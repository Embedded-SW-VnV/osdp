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
  val compare : t -> t -> int
  val is_const : t -> Coeff.t option
  val pp : Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  (* type invariant: lin is sorted by Ident.compare
   * and doesn't contain any zero coefficient *)
  type t = { const : Coeff.t; lin : (Ident.t * Coeff.t) list }

  let of_list l c =
    let l = List.filter (fun (_, s) -> Coeff.compare s Coeff.zero <> 0) l in
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
    if Coeff.compare s Coeff.zero = 0 then
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
         if Coeff.compare c Coeff.zero = 0 then map2 f t1 t2
         else (i1, f c1 c2) :: map2 f t1 t2
  let map2 f a1 a2 =
    { const = f a1.const a2.const; lin = map2 f a1.lin a2.lin }
  let add = map2 Coeff.add
  let sub = map2 Coeff.sub

  let compare a1 a2 =
    let rec compare l1 l2 = match l1, l2 with
      | [], [] -> 0
      | [], (_, c) :: _ -> Coeff.compare Coeff.zero c
      | (_, c) :: _, [] -> Coeff.compare c Coeff.zero
      | (i1, c1) :: t1, (i2, c2) :: t2 ->
         let cmpi = Ident.compare i1 i2 in
         if cmpi < 0 then Coeff.compare c1 Coeff.zero
         else if cmpi > 0 then Coeff.compare Coeff.zero c2
         else (* cmpi = 0 *)
           let cmpc = Coeff.compare c1 c2 in
           if cmpc <> 0 then cmpc else compare t1 t2 in
    let cmpc = Coeff.compare a1.const a2.const in
    if cmpc <> 0 then cmpc else compare a1.lin a2.lin

  let is_const a = if a.lin = [] then Some a.const else None

  let pp fmt a =
    let pp_coeff fmt (x, a) =
      if Coeff.compare a Coeff.one = 0 then
        Format.fprintf fmt "%a" Ident.pp x
      else if Coeff.compare a (Coeff.sub Coeff.zero Coeff.one) = 0 then
        Format.fprintf fmt "-%a" Ident.pp x
      else
        Format.fprintf fmt "%a %a" Coeff.pp a Ident.pp x in
    if is_const a <> None then
      Format.fprintf fmt "%a" Coeff.pp a.const
    else if Coeff.compare a.const Coeff.zero = 0 then
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

module MakeScalar (L : S) : Scalar.S with type t = L.t = Scalar.Make (struct
  type t = L.t
  let compare = L.compare
  let zero = L.const L.Coeff.zero
  let one = L.const L.Coeff.one
  let of_float f = L.const (L.Coeff.of_float f)
  let to_float _ = assert false  (* should never happen *)
  let to_q _ = assert false  (* should never happen *)
  let add = L.add
  let sub = L.sub
  let mult e1 e2 =
    match L.is_const e1, L.is_const e2 with
    | None, None -> raise Not_linear
    | Some s, _ -> L.mult_scalar s e2
    | None, Some s -> L.mult_scalar s e1
  let div _ _ = assert false  (* should never happen *)
  let pp fmt a =
    let lin, const = L.to_list a in
    let l, l' =
      (if L.Coeff.compare const L.Coeff.zero = 0 then 0 else 1),
      List.length lin in
    if l + l' <= 1 then Format.fprintf fmt "%a" L.pp a
    else Format.fprintf fmt "(%a)" L.pp a
end)
