(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
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
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val power : t -> int -> t
  exception Dimension_error
  val compose : t -> t list -> t
  val nb_vars : t -> int
  val degree : t -> int
  val is_homogeneous : t -> bool
  val pp : ?names:string list -> Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  (* monomials sorted by Monomial.compare, all coeffs are non null and
     all monomial in the list are different *)
  type t = (Monomial.t * Coeff.t) list

  let of_list l =
    let l = List.filter (fun (_, s) -> not (Coeff.is_zero s)) l in
    let l = List.sort (fun (m1, _) (m2, _) -> Monomial.compare m1 m2) l in
    let rec remove_duplicates = function
      | [] -> []
      | ((m, c) as mc) :: t ->
         match remove_duplicates t with
         | [] -> [mc]
         | ((m', c') :: t) as t' ->
            if Monomial.compare m m' = 0 then (m, Coeff.add c c') :: t
            else mc :: t' in
    remove_duplicates l

  let to_list p = p

  let zero = []
  let one = [Monomial.of_list [], Coeff.one]

  let mult_scalar s p =
    if Coeff.is_zero s then []
    else List.map (fun (m, s') -> m, Coeff.mult s s') p

  let rec map2 f l1 l2 = match l1, l2 with
    | [], [] -> []
    | [], (m2, c2) :: t2 -> (m2, f Coeff.zero c2) :: map2 f [] t2
    | (m1, c1) :: t1, [] -> (m1, f c1 Coeff.zero) :: map2 f t1 []
    | (m1, c1) :: t1, (m2, c2) :: t2 ->
       let cmp = Monomial.compare m1 m2 in
       if cmp < 0 then
         (m1, f c1 Coeff.zero) :: map2 f t1 l2
       else if cmp > 0 then
         (m2, f Coeff.zero c2) :: map2 f l1 t2
       else  (* cmp = 0 *)
         let c = f c1 c2 in
         if Coeff.is_zero c then map2 f t1 t2
         else (m1, f c1 c2) :: map2 f t1 t2
  let add = map2 Coeff.add
  let sub = map2 Coeff.sub

  let mult p1 p2 =
    let mult_monomial (m, s) =
      List.map (fun (m', s') -> Monomial.mult m m', Coeff.mult s s') in
    List.fold_left (fun l ms -> add l (mult_monomial ms p2)) zero p1

  let rec power p n =
    if n <= 0 then one
    else
      let p' = power p (n / 2) in
      if n mod 2 = 1 then mult p (mult p' p')
      else mult p' p'

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
    List.fold_left (fun p m -> add p (compose_monomial m)) zero p

  let nb_vars = List.fold_left (fun n (m, _) -> max n (Monomial.nb_vars m)) 0

  let degree = List.fold_left (fun d (m, _) -> max d (Monomial.degree m)) (-1)

  let is_homogeneous = function
    | [] -> true
    | (h, _) :: t ->
       let d = Monomial.degree h in
       List.for_all (fun (m, _) -> Monomial.degree m = d) t

  let pp ?names fmt = function
    | [] -> Format.fprintf fmt "0"
    | l ->
       let pp_coeff fmt (m, s) =
         if Coeff.is_zero (Coeff.sub s Coeff.one) then
           Format.fprintf fmt "%a" (Monomial.pp ?names) m
         else if Coeff.is_zero (Coeff.add s Coeff.one) then
           Format.fprintf fmt "-%a" (Monomial.pp ?names) m
         else if Monomial.compare m (Monomial.of_list []) = 0 then
           Format.fprintf fmt "%a" Coeff.pp s
         else
           Format.fprintf fmt "%a %a" Coeff.pp s (Monomial.pp ?names) m in
       Format.fprintf fmt "@[%a@]"
                      (Utils.fprintf_list ~sep:"@ + " pp_coeff)
                      (List.rev l)
end

module Q = Make (Scalar.Q)

module Float = Make (Scalar.Float)
