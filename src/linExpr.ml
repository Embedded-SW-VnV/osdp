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
  val of_list : (Ident.t * Coeff.t) list -> Coeff.t -> t
  val to_list : t -> (Ident.t * Coeff.t) list * Coeff.t
  val const : Coeff.t -> t
  val var : Ident.t -> t
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val eq : t -> t -> bool
  val is_const : t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  (* type invariant: lin doesn't contain any zero coefficient *)
  type t = { const : Coeff.t; lin : (Ident.t * Coeff.t) list }

  let of_list l c =
    let l = List.filter (fun (_, s) -> not (Coeff.is_zero s)) l in
    let l = List.sort (fun (i1, _) (i2, _) -> Ident.compare i1 i2) l in
    let rec remove_duplicates = function
      | [] -> []
      | ((i, c) as ic) :: t ->
         match remove_duplicates t with
         | [] -> [ic]
         | ((i', c') :: t) as t' ->
            if Ident.compare i i' = 0 then (i, Coeff.add c c') :: t
            else ic :: t' in
    let l = remove_duplicates l in
    { const = c; lin = l }

  let to_list a = a.lin, a.const

  let const c = { const = c; lin = [] }

  let var id = { const = Coeff.zero; lin = [id, Coeff.one] }

  let mult_scalar s a =
    if Coeff.is_zero s then
      const Coeff.zero
    else
      { const = Coeff.mult s a.const;
        lin = List.map (fun (i, c) -> i, Coeff.mult s c) a.lin }

  let rec map2 f l1 l2 = match l1, l2 with
    | [], [] -> []
    | [], (i2, c2) :: t2 -> (i2, f Coeff.zero c2) :: map2 f [] t2
    | (i1, c1) :: t1, [] -> (i1, f c1 Coeff.zero) :: map2 f t1 []
    | (i1, c1) :: t1, (i2, c2) :: t2 ->
       let cmp = Ident.compare i1 i2 in
       if cmp < 0 then
         (i1, f c1 Coeff.zero) :: map2 f t1 l2
       else if cmp > 0 then
         (i2, f Coeff.zero c2) :: map2 f l1 t2
       else  (* cmp = 0 *)
         let c = f c1 c2 in
         if Coeff.is_zero c then map2 f t1 t2
         else (i1, f c1 c2) :: map2 f t1 t2
  let map2 f a1 a2 =
    { const = f a1.const a2.const; lin = map2 f a1.lin a2.lin }
  let add = map2 Coeff.add
  let sub = map2 Coeff.sub

  let eq a1 a2 =
    let cmp c1 c2 = Coeff.is_zero (Coeff.sub c1 c2) in
    try
      cmp a1.const a2.const
      && List.for_all2 (fun (_, c1) (_, c2) -> cmp c1 c2)  a1.lin a2.lin
    with Invalid_argument _ -> false

  let is_const a = a.lin = []

  let pp fmt a =
    let pp_coeff fmt (x, a) =
      if Coeff.is_zero (Coeff.sub a Coeff.one) then
        Format.fprintf fmt "%a" Ident.pp x
      else if Coeff.is_zero (Coeff.add a Coeff.one) then
        Format.fprintf fmt "-%a" Ident.pp x
      else
        Format.fprintf fmt "%a %a" Coeff.pp a Ident.pp x in
    if is_const a then
      Format.fprintf fmt "%a" Coeff.pp a.const
    else if Coeff.is_zero a.const then
      Format.fprintf fmt "@[%a@]"
                     (Utils.pp_list ~sep:"@ + " pp_coeff)
                     a.lin
    else
      Format.fprintf fmt "@[%a@ + %a@]"
                     (Utils.pp_list ~sep:"@ + " pp_coeff)
                     a.lin
                     Coeff.pp a.const
end

module Q = Make (Scalar.Q) 

module Float = Make (Scalar.Float)

exception Not_linear

module MakeScalar (L : S) : Scalar.S with type t = L.t = struct
  type t = L.t
  let zero = L.const L.Coeff.zero
  let one = L.const L.Coeff.one
  let is_zero e = L.is_const e && let _, c = L.to_list e in L.Coeff.is_zero c
  let of_float _ = assert false  (* should never happen *)
  let to_float _ = assert false  (* should never happen *)
  let add = L.add
  let sub = L.sub
  let mult e1 e2 =
    match L.is_const e1, L.is_const e2 with
    | false, false -> raise Not_linear
    | true, _ ->
       let _, s = L.to_list e1 in
       L.mult_scalar s e2
    | false, true ->
       let _, s = L.to_list e2 in
       L.mult_scalar s e1
  let div _ _ = assert false  (* should never happen *)
  let pp fmt a =
    let lin, const = L.to_list a in
    let l, l' = (if L.Coeff.is_zero const then 0 else 1), List.length lin in
    if l + l' <= 1 then Format.fprintf fmt "%a" L.pp a
    else Format.fprintf fmt "(%a)" L.pp a
end
