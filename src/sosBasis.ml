(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Generic polynomial basis abstraction for SOS decomposition.

   Architecture (three layers):

   1. Algebraic layer (sos.ml): user-facing polynomial expressions and SOS
      constraints — independent of the chosen basis.

   2. Polynomial representation layer (this file): an abstract BASIS interface
      parameterised by a scalar type S, with concrete implementations for
      Classical and Hermite bases (and future families).

      [prod_to_classical] and [to_classical] return coefficients of type
      [Coeff.t = S.t], so the precision of the classical expansion is
      determined by the caller.  Hermite's internal arithmetic always uses
      exact [Q.t] rationals; the conversion to [S.t] happens via [S.of_q]
      at the point of output.

      [prod] (product in the same basis) always uses exact [Q.t] rationals
      through [LINEXPR], as the product formula has integer coefficients.

   3. SOS-to-SDP encoding layer (sos.ml): uses only the abstract BASIS
      interface, so changing the basis never requires touching the encoding.
      Coefficients are converted to [float] for the SDP solver via
      [Coeff.to_float] at the SDP boundary.

   This module originates from sos/monomials.ml (branch old_hermite, 2014)
   and is redesigned for the current module system. *)


(* ===================================================================== *)
(*  Module types                                                          *)
(* ===================================================================== *)

(** A monomial type: ordered basis elements with printing. *)
module type MONOMIAL = sig
  type t
  val compare : t -> t -> int
  val pp : string list -> Format.formatter -> t -> unit
end

(** Linear expression over a monomial type, with exact rational coefficients. *)
module type LINEXPR = sig
  module M : MONOMIAL
  (* sorted by M.compare; no zero-coefficient terms *)
  type t = (Q.t * M.t) list
  val zero      : t
  val inject    : M.t -> t
  val add       : t -> t -> t
  val scal_mult : Q.t -> t -> t
  val pp        : string list -> Format.formatter -> t -> unit
end

(** Abstract polynomial basis for SOS decomposition, parameterised by a
    scalar type [Coeff].

    A basis element [b] of type [t] represents a polynomial in the
    associated family (e.g. x_1^a1 * x_2^a2 * ... for Classical, or
    H_{a1}(x1) * H_{a2}(x2) * ... for Hermite).

    [prod b1 b2] returns the product as a linear combination in the *same*
    basis, with exact rational coefficients (via [LINEXPR]).

    [prod_to_classical b1 b2] returns the product as a classical polynomial
    with coefficients in [Coeff.t].  This is the bridge to the
    coefficient-matching SDP encoding in [sos.ml]; its precision is
    determined by the [Coeff] functor argument. *)
module type BASIS = sig
  (** Scalar type for the classical-polynomial expansion. *)
  module Coeff : Scalar.S

  (** Basis-element type (= [Monomial.t] for both Classical and Hermite). *)
  type t

  val compare : t -> t -> int
  val pp      : string list -> Format.formatter -> t -> unit

  (** Linear expressions over this basis (exact [Q.t] coefficients). *)
  module LE : LINEXPR with type M.t = t

  val dim          : t -> int
  val deg          : t -> int
  val list_le      : int -> int -> t list
  val list_eq      : int -> int -> t list
  val nth          : int -> int -> t
  val get_sos_size : int -> int -> int

  (** Product in the same basis (always exact rational). *)
  val prod : t -> t -> LE.t

  (** Product as a classical polynomial with [Coeff.t] coefficients. *)
  val prod_to_classical : t -> t -> (Monomial.t * Coeff.t) list

  (** Single basis element as a classical polynomial with [Coeff.t]
      coefficients. *)
  val to_classical : t -> (Monomial.t * Coeff.t) list

  (** [refine_basis elems classical_support] returns the subset of [elems]
      that may contribute to the classical monomials in [classical_support].
      For bases where no filter is known, returns [elems] unchanged. *)
  val refine_basis : t list -> Monomial.t list -> t list
end


(* ===================================================================== *)
(*  LinExpr functor                                                       *)
(* ===================================================================== *)

module LinExpr (M : MONOMIAL) : LINEXPR with type M.t = M.t = struct
  module M = M
  type t = (Q.t * M.t) list

  let zero   = []
  let inject m = [Q.one, m]

  let add l1 l2 =
    let rec merge l1 l2 = match l1, l2 with
      | [], l | l, [] -> l
      | (s1, m1) :: t1, (s2, m2) :: t2 ->
        let c = M.compare m1 m2 in
        if c < 0 then (s1, m1) :: merge t1 l2
        else if c > 0 then (s2, m2) :: merge l1 t2
        else
          let s = Q.add s1 s2 in
          if Q.equal s Q.zero then merge t1 t2
          else (s, m1) :: merge t1 t2
    in
    merge l1 l2

  let scal_mult s l =
    if Q.equal s Q.zero then []
    else List.filter_map (fun (s', m) ->
      let r = Q.mul s s' in
      if Q.equal r Q.zero then None else Some (r, m)) l

  let pp names fmt l =
    if l = [] then Format.fprintf fmt "0"
    else
      Utils.pp_list ~sep:" + "
        (fun fmt (s, m) ->
           Format.fprintf fmt "%a * %a" Q.pp_print s (M.pp names) m)
        fmt l
end


(* ===================================================================== *)
(*  TensorProduct — shared enumeration logic for multi-variate bases     *)
(* ===================================================================== *)

module TensorProduct = struct

  let cache : (int, int array list list) Hashtbl.t = Hashtbl.create 13

  let rec monomials_at_deg_rec accu (i, v) =
    if i = Array.length v then accu
    else
      let v' = Array.copy v in
      v'.(i) <- v'.(i) + 1;
      (i, v') :: monomials_at_deg_rec accu (i + 1, v)

  let monomials_at_deg dim d =
    let rec loop vs pow =
      if pow = 0 then List.map snd vs
      else loop (List.fold_left monomials_at_deg_rec [] vs) (pow - 1)
    in
    loop [(0, Array.make dim 0)] d

  let fill_cache dim up_to_deg =
    let exist = Hashtbl.mem cache dim in
    let current_set, current_len =
      if exist then
        let s = Hashtbl.find cache dim in s, List.length s
      else [], 0
    in
    if current_len <= up_to_deg then begin
      let new_levels = ref [] in
      for d = up_to_deg downto current_len do
        new_levels := monomials_at_deg dim d :: !new_levels
      done;
      Hashtbl.replace cache dim (current_set @ !new_levels)
    end

  let nth_array dim n =
    fill_cache dim n;
    let levels = Hashtbl.find cache dim in
    let rec scan levels n' = match levels with
      | [] -> failwith "SosBasis.TensorProduct.nth: index out of range"
      | level :: rest ->
        let l = List.length level in
        if n' < l then List.nth level n'
        else scan rest (n' - l)
    in
    scan levels n

  let get_sos_size dim deg =
    let target = (deg / 2) + (deg mod 2) in
    fill_cache dim target;
    let levels = Hashtbl.find cache dim in
    let rec count lvls n acc =
      if n > target then acc
      else match lvls with
        | [] -> acc
        | lv :: rest -> count rest (n + 1) (acc + List.length lv)
    in
    count levels 0 0

  let array_to_list a =
    let l = Array.to_list a in
    let rec trim = function
      | [] -> []
      | h :: t -> let t' = trim t in if t' = [] && h = 0 then [] else h :: t'
    in
    trim l

  let list_le n d =
    fill_cache n d;
    let levels = Hashtbl.find cache n in
    let rec collect levels k acc =
      if k > d then acc
      else match levels with
        | [] -> acc
        | lv :: rest ->
          collect rest (k + 1) (acc @ List.map array_to_list lv)
    in
    collect levels 0 []

  let list_eq n d =
    fill_cache n d;
    let levels = Hashtbl.find cache n in
    try List.map array_to_list (List.nth levels d)
    with _ -> []

  let nth n k = array_to_list (nth_array n k)
end


(* ===================================================================== *)
(*  Classical monomial basis                                              *)
(* ===================================================================== *)

module Classical (S : Scalar.S)
    : BASIS with module Coeff = S and type t = Monomial.t = struct

  module Coeff = S
  type t = Monomial.t

  let compare = Monomial.compare
  let pp names fmt m = Monomial.pp_names names fmt m

  module LE = LinExpr (struct
    type t = Monomial.t
    let compare = Monomial.compare
    let pp names fmt m = Monomial.pp_names names fmt m
  end)

  let dim          = Monomial.nb_vars
  let deg          = Monomial.degree
  let list_le      = Monomial.list_le
  let list_eq      = Monomial.list_eq
  let nth n k      = Monomial.of_list (TensorProduct.nth n k)
  let get_sos_size = TensorProduct.get_sos_size

  let prod b1 b2 = LE.inject (Monomial.mult b1 b2)

  let to_classical b = [(b, S.one)]

  let prod_to_classical b1 b2 = [(Monomial.mult b1 b2, S.one)]

  let refine_basis elems classical_support =
    NewtonPolytope.filter elems classical_support
end


(* ===================================================================== *)
(*  Hermite polynomial basis                                              *)
(* ===================================================================== *)

(* Shared univariate Hermite arithmetic (always in Q.t for exactness).
   This module is independent of the scalar-type functor argument. *)
module HermiteUniVariate = struct

  let binom n k =
    if k < 0 || k > n then Q.zero
    else
      let result = ref Q.one in
      for i = 0 to k - 1 do
        result := Q.mul !result (Q.of_ints (n - i) (i + 1))
      done;
      !result

  let factorial k =
    let r = ref Q.one in
    for i = 2 to k do r := Q.mul !r (Q.of_int i) done;
    !r

  let pow2 k =
    let r = ref Q.one in
    for _ = 1 to k do r := Q.add !r !r done;
    !r

  (** H_m * H_n in the Hermite basis, exact rational coefficients. *)
  let prod_1d m n =
    let lo = min m n in
    let result = ref [] in
    for k = 0 to lo do
      let c = Q.mul (Q.mul (binom m k) (binom n k))
                    (Q.mul (factorial k) (pow2 k)) in
      if not (Q.equal c Q.zero) then
        result := (c, m + n - 2 * k) :: !result
    done;
    List.sort (fun (_, a) (_, b) -> Int.compare b a) !result

  (** H_n(x) as a classical polynomial with exact Q.t coefficients.
      Recurrence: H_{k+1} = 2x H_k - 2k H_{k-1}. *)
  let h1d_to_classical n =
    let mul2x p =
      let len = Array.length p in
      let r = Array.make (len + 1) Q.zero in
      for i = 0 to len - 1 do r.(i + 1) <- Q.mul (Q.of_int 2) p.(i) done;
      r
    in
    let sub_smul s p1 p2 =
      let n1 = Array.length p1 and n2 = Array.length p2 in
      let r = Array.make (max n1 n2) Q.zero in
      for i = 0 to n1 - 1 do r.(i) <- Q.add r.(i) p1.(i) done;
      for i = 0 to n2 - 1 do r.(i) <- Q.sub r.(i) (Q.mul s p2.(i)) done;
      r
    in
    let rec aux prev curr k =
      if k >= n then curr
      else aux curr (sub_smul (Q.of_int (2 * k)) (mul2x curr) prev) (k + 1)
    in
    match n with
    | 0 -> [| Q.one |]
    | 1 -> [| Q.zero; Q.of_int 2 |]
    | _ -> aux [| Q.one |] [| Q.zero; Q.of_int 2 |] 1

  (** Multiply two univariate classical polynomials (Q.t arrays). *)
  let poly1d_mult p1 p2 =
    let n1 = Array.length p1 and n2 = Array.length p2 in
    if n1 = 0 || n2 = 0 then [||]
    else
      let r = Array.make (n1 + n2 - 1) Q.zero in
      for i = 0 to n1 - 1 do
        if not (Q.equal p1.(i) Q.zero) then
          for j = 0 to n2 - 1 do
            r.(i + j) <- Q.add r.(i + j) (Q.mul p1.(i) p2.(j))
          done
      done;
      r

  (** H_{ai}(x) * H_{bi}(x) as a classical polynomial (Q.t array). *)
  let prod_to_classical_1d ai bi =
    poly1d_mult (h1d_to_classical ai) (h1d_to_classical bi)

  (** Tensor-product classical expansion.
      [polys_1d] is [(var_idx, Q.t array)] list — one 1-D polynomial per
      variable.  Returns the multivariate classical polynomial as an
      association list [(Monomial.t, Q.t)], with duplicate monomials merged
      and zero terms removed. *)
  let tensorize_classical polys_1d =
    let result =
      List.fold_left
        (fun acc (var_idx, poly) ->
           let deg_n = Array.length poly in
           List.concat_map
             (fun (mon, c) ->
                let terms = ref [] in
                for d = 0 to deg_n - 1 do
                  let c' = poly.(d) in
                  if not (Q.equal c' Q.zero) then
                    let m' = Monomial.mult mon (Monomial.var ~d var_idx) in
                    terms := (m', Q.mul c c') :: !terms
                done;
                !terms)
             acc)
        [(Monomial.one, Q.one)]
        polys_1d
    in
    let map = List.fold_left
        (fun acc (mon, c) ->
           let c' = try Q.add (Monomial.Map.find mon acc) c
                    with Not_found -> c in
           if Q.equal c' Q.zero then Monomial.Map.remove mon acc
           else Monomial.Map.add mon c' acc)
        Monomial.Map.empty result
    in
    Monomial.Map.bindings map
end


module Hermite (S : Scalar.S)
    : BASIS with module Coeff = S and type t = Monomial.t = struct

  module Coeff = S
  type t = Monomial.t

  let compare = Monomial.compare

  let pp names fmt m =
    let names_a = Array.of_list names in
    let l = Monomial.to_list m in
    if l = [] then Format.fprintf fmt "H_0"
    else begin
      let x_char = Char.code 'x' in
      List.iteri (fun i ai ->
        let name =
          if i < Array.length names_a then names_a.(i)
          else String.make 1 (Char.chr (x_char + i))
        in
        Format.fprintf fmt "H_%d(%s)" ai name
      ) l
    end

  module LE = LinExpr (struct
    type t = Monomial.t
    let compare = Monomial.compare
    let pp = pp
  end)

  let dim          = Monomial.nb_vars
  let deg          = Monomial.degree
  let list_le n d  = List.map Monomial.of_list (TensorProduct.list_le n d)
  let list_eq n d  = List.map Monomial.of_list (TensorProduct.list_eq n d)
  let nth n k      = Monomial.of_list (TensorProduct.nth n k)
  let get_sos_size = TensorProduct.get_sos_size

  (* Tensor product of 1-D Hermite products into a multivariate LE. *)
  let tensorize_1d_results results =
    let initial : (Q.t * int list) list = [(Q.one, [])] in
    let combined =
      List.fold_right
        (fun dim_result acc ->
           List.concat_map
             (fun (c, idx) ->
                List.map (fun (c', idxs) -> Q.mul c c', idx :: idxs) acc)
             dim_result)
        results initial
    in
    let as_le =
      List.filter_map (fun (c, idxs) ->
        if Q.equal c Q.zero then None
        else Some (c, Monomial.of_list idxs))
        combined
    in
    List.fold_left (fun acc (c, m) -> LE.add acc [c, m]) LE.zero as_le

  let prod a b =
    let a = Monomial.to_list a and b = Monomial.to_list b in
    let n = max (List.length a) (List.length b) in
    let pad lst = lst @ List.init (n - List.length lst) (fun _ -> 0) in
    let a = pad a and b = pad b in
    let per_dim = List.map2 HermiteUniVariate.prod_1d a b in
    tensorize_1d_results per_dim

  let to_classical m =
    let m = Monomial.to_list m in
    let polys_1d =
      List.mapi (fun i ai -> i, HermiteUniVariate.h1d_to_classical ai) m
    in
    List.map (fun (mon, q) -> mon, S.of_q q)
      (HermiteUniVariate.tensorize_classical polys_1d)

  let prod_to_classical a b =
    let a = Monomial.to_list a and b = Monomial.to_list b in
    let n = max (List.length a) (List.length b) in
    let pad lst = lst @ List.init (n - List.length lst) (fun _ -> 0) in
    let a = pad a and b = pad b in
    let polys_1d =
      List.mapi
        (fun i (ai, bi) -> i, HermiteUniVariate.prod_to_classical_1d ai bi)
        (List.combine a b)
    in
    List.map (fun (mon, q) -> mon, S.of_q q)
      (HermiteUniVariate.tensorize_classical polys_1d)

  let refine_basis elems _classical_support = elems
end


(* ===================================================================== *)
(*  Named basis selection                                                 *)
(* ===================================================================== *)

type basis_choice = Classical | Hermite
  (* Future: | Chebyshev | Legendre | Bernstein | Laguerre
             | Custom of (module BASIS) *)

(** [Registry(S)] packages the basis-dispatch functions for scalar type [S].
    Instantiate once per [Make(P)] with [S = P.Coeff]. *)
module Registry (S : Scalar.S) = struct
  module C = Classical (S)
  module H = Hermite   (S)

  let prod_to_classical_fn = function
    | Classical -> C.prod_to_classical
    | Hermite   -> H.prod_to_classical

  let list_fns = function
    | Classical -> C.list_le, C.list_eq
    | Hermite   -> H.list_le, H.list_eq

  let refine_basis_fn = function
    | Classical -> C.refine_basis
    | Hermite   -> H.refine_basis
end
