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

let fprintf_list ~sep f fmt l =
  let rec aux fmt = function
    | []   -> ()
    | [e]  -> f fmt e
    | x::r -> Format.fprintf fmt "%a%(%)%a" f x sep aux r in
  aux fmt l

let fprintf_array ~sep f fmt a =
  if Array.length a >= 1 then begin
    Format.fprintf fmt "%a" f a.(0);
    for i = 1 to Array.length a - 1 do
      Format.fprintf fmt "%(%)%a" sep f a.(i)
    done
  end

let fprintf_matrix ~begl ~endl ~sepl ~sepc f =
  let print_line fmt l =
    Format.fprintf fmt "%(%)%a%(%)" begl (fprintf_array ~sep:sepc f) l endl in
  fprintf_array ~sep:sepl print_line

let epsilon_under_float = float_of_string "0x1p-1074"

(* Please report any assert false raised in this function. *)
let float_of_q q =
  let n = Z.to_float q.Q.num in
  let d = Z.to_float q.Q.den in
  if Z.equal q.Q.num (Z.of_float n) && Z.equal q.Q.den (Z.of_float d) then
    n /. d  (* if division is good, we get a closest float *)
  else
    let a = n /. d in
    let l, u =
      if a >= 0. then
        (1. -. 8. *. epsilon_float) *. a -. 8. *. epsilon_under_float,
        (1. +. 8. *. epsilon_float) *. a +. 8. *. epsilon_under_float
      else
        (1. +. 8. *. epsilon_float) *. a -. 8. *. epsilon_under_float,
        (1. -. 8. *. epsilon_float) *. a +. 8. *. epsilon_under_float in
    (* Check that we have l <= q <= u. *)
    let () = if Q.lt q (Q.of_float l) then assert false in
    let () = if Q.lt (Q.of_float u) q then assert false in
    (* Refine the bounds q \in [l, u] by dichotomy. *)
    let rec dicho n l u =
      if n > 0 then
        let m = (l +. u) /. 2. in
        if Q.leq (Q.of_float m) q then dicho (n - 1) m u
        else dicho (n - 1) l m
      else
        l, u in
    let l, u = dicho 10 l u in
    (* Check that we have two consecutive floats. *)
    let l, u = match classify_float l, classify_float u with
      | FP_normal, FP_normal ->
         let lf, le = frexp l in
         let uf, ue = frexp u in
         if uf -. lf > epsilon_float then assert false else l, u
      | FP_subnormal, _
      | _, FP_subnormal ->
         if u -. l > epsilon_under_float then assert false else l, u
      | FP_zero, FP_zero -> l, u
      | _ -> assert false in
    (* Keep a closest one. *)
    if Q.leq (Q.sub (Q.of_float u) q) (Q.sub q (Q.of_float l)) then u else l
