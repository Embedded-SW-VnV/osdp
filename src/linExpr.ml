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
  val of_list : (Ident.t * Coeff.t) list -> Coeff.t -> t
  val to_list : t -> (Ident.t * Coeff.t) list * Coeff.t
  val var : Ident.t -> t
  val const : Coeff.t -> t
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val replace : t -> (Ident.t * t) list -> t
  val remove : t -> Ident.t -> t
  val compare : t -> t -> int
  val is_var : t -> (Ident.t * Coeff.t) option
  val is_const : t -> Coeff.t option
  val choose : t -> (Ident.t * Coeff.t) option
  val pp : Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  module IM = Ident.Map
  
  (* type invariant: lin is sorted by Ident.compare
   * and doesn't contain any zero coefficient *)
  type t = { const : Coeff.t; lin : Coeff.t IM.t }

  let of_list l c =
    let l = List.filter (fun (_, s) -> Coeff.(s <> zero)) l in
    let m =
      List.fold_left
        (fun acc (id, c) ->
           let c = try Coeff.(c + IM.find id acc) with Not_found -> c in
           IM.add id c acc)
        IM.empty l in
    { const = c; lin = m }

  let to_list a = IM.bindings a.lin, a.const

  let var id = { const = Coeff.zero; lin = IM.singleton id Coeff.one }

  let const c = { const = c; lin = IM.empty }

  let mult_scalar s a =
    if Coeff.(s = zero) then const Coeff.zero else
      { const = Coeff.(s * a.const);
        lin = IM.map (fun c -> Coeff.(s * c)) a.lin }

  let map2 f m1 m2 =
    let opt s = if Coeff.(s <> zero) then Some s else None in
    IM.merge
      (fun _ c1 c2 ->
       match c1, c2 with
       | None, None -> None
       | Some c1, None -> opt (f c1 Coeff.zero)
       | None, Some c2 -> opt (f Coeff.zero c2)
       | Some c1, Some c2 -> opt (f c1 c2))
      m1 m2
  let map2 f a1 a2 =
    { const = f a1.const a2.const; lin = map2 f a1.lin a2.lin }
  let add = map2 Coeff.add
  let sub = map2 Coeff.sub

  let replace l ll =
    let m, ll =
      List.fold_left
        (fun (m, ll) (id, l') ->
           try
             let c = IM.find id m in
             IM.remove id m, (c, l') :: ll
           with Not_found -> m, ll)
        (l.lin, []) ll in
    List.fold_left
      (fun l (c, l') ->
         let m =
           IM.fold
             (fun id c' m ->
                let c =
                  try Coeff.(IM.find id m + c * c')
                  with Not_found -> Coeff.(c * c') in
                if Coeff.(c <> zero) then IM.add id c m else IM.remove id m)
             l'.lin l.lin in
         { const = Coeff.(l.const + c * l'.const); lin = m })
      { const = l.const; lin = m } ll

  let remove l i = { const = l.const; lin = IM.remove i l.lin }
  
  let compare a1 a2 =
    let c = Coeff.compare a1.const a2.const in
    if c <> 0 then c else IM.compare Coeff.compare a1.lin a2.lin

  let is_var a =
    if Coeff.(equal a.const zero) then
      match IM.bindings a.lin with
        | [id_c] -> Some id_c
        | _ -> None
    else None

  let is_const a = if IM.is_empty a.lin then Some a.const else None

  let choose l = try Some (IM.choose l.lin) with Not_found -> None
  
  let pp fmt a =
    let pp_coeff fmt (x, a) =
      if Coeff.(a = one) then
        Format.fprintf fmt "%a" Ident.pp x
      else if Coeff.(a = minus_one) then
        Format.fprintf fmt "-%a" Ident.pp x
      else
        Format.fprintf fmt "%a %a" Coeff.pp a Ident.pp x in
    if is_const a <> None then
      Format.fprintf fmt "%a" Coeff.pp a.const
    else if Coeff.(a.const = zero) then
      Format.fprintf fmt "@[%a@]"
                     (Utils.pp_list ~sep:"@ + " pp_coeff)
                     (IM.bindings a.lin)
    else
      Format.fprintf fmt "@[%a@ + %a@]"
                     (Utils.pp_list ~sep:"@ + " pp_coeff)
                     (IM.bindings a.lin)
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
  let of_q x = L.const (L.Coeff.of_q x)
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
      (if L.Coeff.(const = zero) then 0 else 1),
      List.length lin in
    if l + l' <= 1 then Format.fprintf fmt "%a" L.pp a
    else Format.fprintf fmt "(%a)" L.pp a
end)
