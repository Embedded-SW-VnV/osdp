(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
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

module type S = sig
  module  Coeff : Scalar.S
  type t
  val of_list : (Monomial.t * Coeff.t) list -> t
  val to_list : t -> (Monomial.t * Coeff.t) list
  val zero : t
  val one : t
  val var : ?c:Coeff.t -> ?d:int -> int -> t
  val const : Coeff.t -> t
  val monomial : Monomial.t -> t
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val power : t -> int -> t
  exception Dimension_error
  val compose : t -> t list -> t
  val derive : t -> int -> t
  val eval : t -> Coeff.t list -> Coeff.t
  val compare : t -> t -> int
  val nb_vars : t -> int
  val degree : t -> int
  val degree_list : t -> int list
  val is_homogeneous : t -> bool
  val is_var : t -> (Coeff.t * int * int) option
  val is_const : t -> Coeff.t option
  val is_monomial : t -> Monomial.t option
  val ( ?? ) : int -> t
  val ( ! ) : Coeff.t -> t
  val ( *. ) : Coeff.t -> t -> t
  val ( ~- ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> Coeff.t -> t
  val ( /. ) : Coeff.t -> Coeff.t -> t
  val ( ** ) : t -> int -> t
  val pp : Format.formatter -> t -> unit
  val pp_names : string list -> Format.formatter -> t -> unit
  val merge : (Monomial.t -> Coeff.t option -> Coeff.t option -> Coeff.t option)
              -> t -> t -> t
  val fold : (Monomial.t -> Coeff.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  module MM = Map.Make (Monomial)

  (* monomials sorted by Monomial.compare, all coeffs are non null *)
  type t = Coeff.t MM.t

  let of_list l =
    let l = List.filter (fun (_, s) -> Coeff.(s <> zero)) l in
    List.fold_left
      (fun acc (m, c) ->
       let c = try Coeff.(c + MM.find m acc) with Not_found -> c in
       MM.add m c acc)
      MM.empty l

  let to_list p =
    let l = MM.fold (fun m c l -> (m, c) :: l) p [] in
    List.rev l

  let zero = MM.empty
  let one = MM.singleton Monomial.one Coeff.one

  let var ?c ?d i =
    let c = match c with Some c -> c | None -> Coeff.one in
    of_list [Monomial.var ?d i, c]

  let const c = var ~c ~d:0 0

  let monomial m = MM.singleton m Coeff.one

  let mult_scalar s p =
    if Coeff.(s = zero) then zero
    else MM.map (Coeff.mult s) p

  let map2 f m1 m2 =
    let opt s = if Coeff.(s <> zero) then Some s else None in
    MM.merge
      (fun _ c1 c2 ->
       match c1, c2 with
       | None, None -> None
       | Some c1, None -> opt (f c1 Coeff.zero)
       | None, Some c2 -> opt (f Coeff.zero c2)
       | Some c1, Some c2 -> opt (f c1 c2))
      m1 m2
  let add = map2 Coeff.add
  let sub = map2 Coeff.sub

  let mult p1 p2 =
    let mult_monomial m s mm =
      MM.fold (fun m' s' mm -> MM.add (Monomial.mult m m') Coeff.(s * s') mm)
              mm MM.empty in
    MM.fold (fun m s mm -> add mm (mult_monomial m s p2)) p1 zero

  let rec power p n =
    if n <= 0 then one
    else if n = 1 then p
    else
      let p' = power p (n / 2) in
      if n mod 2 = 0 then mult p' p'
      else mult p (mult p' p')

  exception Dimension_error

  let compose p ql =
    let get =
      let qa = Array.of_list ql in
      fun i -> try qa.(i) with Invalid_argument _ -> raise Dimension_error in
    let compose_monomial (m, s) =
      let mq = List.mapi (fun i d -> get i, d) (Monomial.to_list m) in
      let mq = List.filter (fun (_, d) -> d > 0) mq in
      let mq = List.map (fun (q, d) -> power q d) mq in
      mult_scalar s (List.fold_left mult one mq) in
    MM.fold (fun m s p -> add p (compose_monomial (m, s))) p zero

  let derive p i =
    let p =
      List.map
        (fun (m, c) ->
         let j, m = Monomial.derive m i in
         m, Coeff.(c * of_int j))
        (to_list p) in
    of_list p

  let eval p l =
    let rec pow c n =
      if n <= 0 then Coeff.one
      else
        let c' = pow c (n / 2) in
        if n mod 2 = 0 then Coeff.(c' * c')
        else Coeff.(c * c' * c') in
    let eval_monomial m =
      let rec aux lc ld = match lc, ld with
        | _, [] -> Coeff.one
        | [], _ -> raise (Invalid_argument "Polynomial.eval")
        | c :: tc, d :: td -> Coeff.mult (pow c d) (aux tc td) in
      aux l (Monomial.to_list m) in
    MM.fold
      (fun m c r -> Coeff.(r + c * eval_monomial m))
      p Coeff.zero

  let compare p1 p2 =
    let p =
      MM.merge
        (fun _m c1 c2 ->
         let desome c = match c with Some c -> c | None -> Coeff.zero in
         Some (desome c1, desome c2))
        p1 p2 in
    MM.fold
      (fun _m (c1, c2) cmp -> if cmp <> 0 then cmp else Coeff.compare c1 c2)
      p 0

  let nb_vars p = MM.fold (fun m _ n -> max n (Monomial.nb_vars m)) p 0

  let degree p = MM.fold (fun m _ d -> max d (Monomial.degree m)) p (-1)

  let degree_list p =
    Monomial.to_list (MM.fold (fun m _ d -> Monomial.lcm d m) p Monomial.one)

  let is_homogeneous p =
    try
      let d =
        let m, _ = MM.choose p in
        Monomial.degree m in
      MM.for_all (fun m _ -> Monomial.degree m = d) p
    with Not_found -> true

  let is_var p =
    if MM.cardinal p <> 1 then None else
      let m, c = MM.choose p in
      match Monomial.is_var m with
      | Some (d, i) -> Some (c, d, i)
      | None -> None

  let is_const p =
    let c = MM.cardinal p in
    if c <= 0 then Some Coeff.zero else if c > 1 then None else
      let m, c = MM.choose p in
      match Monomial.to_list m with
      | [] -> Some c
      | _ -> None

  let is_monomial p =
    if MM.cardinal p <> 1 then None else
      let m, c = MM.choose p in
      if Coeff.(c = one) then Some m else None

  let pp_names names fmt = function
    | [] -> Format.fprintf fmt "0"
    | l ->
       let pp_coeff fmt (m, s) =
         if Coeff.(s = one) then
           Format.fprintf fmt "%a" (Monomial.pp_names names) m
         else if Coeff.(s = minus_one) then
           Format.fprintf fmt "-%a" (Monomial.pp_names names) m
         else if Monomial.compare m (Monomial.of_list []) = 0 then
           Format.fprintf fmt "%a" Coeff.pp s
         else
           Format.fprintf fmt "%a * %a" Coeff.pp s (Monomial.pp_names names) m in
       Format.fprintf fmt "@[%a@]"
                      (Utils.pp_list ~sep:"@ + " pp_coeff)
                      l

  let pp_names names fmt p = pp_names names fmt (to_list p)

  let pp = pp_names []

  let ( ?? ) i = var i
  let ( ! ) = const
  let ( *. ) = mult_scalar
  let ( ~- ) = sub zero
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( / ) p c = mult_scalar (Coeff.inv c) p
  let ( /. ) c1 c2 = !c1 / c2
  let ( ** ) = power

  let merge = MM.merge  (* TODO: postprocessing to remove zeros *)
  let fold = MM.fold
end

module Q = Make (Scalar.Q)

module Float = Make (Scalar.Float)
