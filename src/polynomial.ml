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
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  (* monomials sorted by Monomial.compare, all coeffs are non null and
     all monomial in the list are different *)
  type t = (Monomial.t * Coeff.t) list

  let of_list l =
    let l = List.filter (fun (_, s) -> Coeff.(s <> zero)) l in
    let l = List.sort (fun (m1, _) (m2, _) -> Monomial.compare m1 m2) l in
    let remove_duplicates l =
      let rec aux acc ((m, c) as last) l = match l with
        | [] -> last :: acc
        | ((m', c') as cur) :: l ->
           if Monomial.compare m m' = 0 then aux acc (m, Coeff.(c + c')) l
           else aux (last :: acc) cur l in
      match l with
      | [] -> []
      | h :: t -> List.rev (aux [] h t) in
    remove_duplicates l

  let to_list p = p

  let zero = []
  let one = [Monomial.one, Coeff.one]

  let var ?c ?d i =
    let c = match c with Some c -> c | None -> Coeff.one in
    of_list [Monomial.var ?d i, c]

  let const c = var ~c ~d:0 0

  let monomial m = [m, Coeff.one]
                    
  let mult_scalar s p =
    if Coeff.(s = zero) then []
    else List.map (fun (m, s') -> m, Coeff.(s * s')) p

  let map2 f l1 l2 =
    let rec aux acc l1 l2 = match l1, l2 with
      | [], [] -> acc
      | [], (m2, c2) :: t2 -> aux ((m2, f Coeff.zero c2) :: acc) [] t2
      | (m1, c1) :: t1, [] -> aux ((m1, f c1 Coeff.zero) :: acc) t1 []
      | (m1, c1) :: t1, (m2, c2) :: t2 ->
         let cmp = Monomial.compare m1 m2 in
         if cmp < 0 then aux ((m1, f c1 Coeff.zero) :: acc) t1 l2
         else if cmp > 0 then aux ((m2, f Coeff.zero c2) :: acc) l1 t2
         else  (* cmp = 0 *)
           let c = f c1 c2 in
           if Coeff.(c = zero) then aux acc t1 t2
           else aux ((m1, c) :: acc) t1 t2 in
    List.rev (aux [] l1 l2)
  let add = map2 Coeff.add
  let sub = map2 Coeff.sub

  let mult p1 p2 =
    let mult_monomial (m, s) =
      List.map (fun (m', s') -> Monomial.mult m m', Coeff.(s * s')) in
    List.fold_left (fun l ms -> add l (mult_monomial ms p2)) zero p1

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
    List.fold_left (fun p m -> add p (compose_monomial m)) zero p

  let derive p i =
    let p =
      List.map
        (fun (m, c) ->
         let j, m = Monomial.derive m i in
         m, Coeff.(c * of_int j))
        p in
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
    List.fold_left
      (fun r (m, c) -> Coeff.(r + c * eval_monomial m))
      Coeff.zero p

  let rec compare p1 p2 = match p1, p2 with
    | [], [] -> 0
    | [], (_, c) :: _ -> Coeff.(compare zero c)
    | (_, c) :: _, [] -> Coeff.(compare c zero)
    | (m1, c1) :: t1, (m2, c2) :: t2 ->
       let cmpm = Monomial.compare m1 m2 in
       if cmpm < 0 then Coeff.(compare c1 zero)
       else if cmpm > 0 then Coeff.(compare zero c2)
       else (* cmpm = 0 *)
         let cmpc = Coeff.compare c1 c2 in
         if cmpc <> 0 then cmpc else compare t1 t2

  let nb_vars = List.fold_left (fun n (m, _) -> max n (Monomial.nb_vars m)) 0

  let degree = List.fold_left (fun d (m, _) -> max d (Monomial.degree m)) (-1)

  let is_homogeneous = function
    | [] -> true
    | (h, _) :: t ->
       let d = Monomial.degree h in
       List.for_all (fun (m, _) -> Monomial.degree m = d) t

  let is_var = function
    | [] | _ :: _ :: _ -> None
    | [m, c] ->
       match Monomial.is_var m with
       | Some (d, i) -> Some (c, d, i)
       | None -> None

  let is_const = function
    | [] | _ :: _ :: _ -> None
    | [m, c] ->
       match Monomial.to_list m with
       | [] -> Some c
       | _ -> None

  let is_monomial = function
    | [m, c] -> if Coeff.(c = one) then Some m else None
    | _ -> None
                
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
end

module Q = Make (Scalar.Q)

module Float = Make (Scalar.Float)
