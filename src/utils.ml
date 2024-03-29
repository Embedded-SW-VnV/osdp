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

let pp_list ~sep f fmt l =
  let rec aux fmt = function
    | []   -> ()
    | [e]  -> f fmt e
    | x::r -> Format.fprintf fmt "%a%(%)" f x sep; aux fmt r in
  aux fmt l

let pp_array ~sep f fmt a =
  if Array.length a >= 1 then begin
    Format.fprintf fmt "%a" f a.(0);
    for i = 1 to Array.length a - 1 do
      Format.fprintf fmt "%(%)%a" sep f a.(i)
    done
  end

let pp_matrix ~begl ~endl ~sepl ~sepc f =
  let print_line fmt l =
    Format.fprintf fmt "%(%)%a%(%)" begl (pp_array ~sep:sepc f) l endl in
  pp_array ~sep:sepl print_line

(* smallest positive subnormal *)
let epsilon_under_float = min_float *. epsilon_float  (* 0x1p-1074 *)
(* largest subnormal *)
let max_subnormal = min_float -. epsilon_under_float

let q_max_float = Q.of_float max_float
let q_neg_max_float = Q.of_float ( -. max_float)

(* Assuming f1 <= f2, [consecutive_float f1 f2] returns true iff there
   is no float f (normal or subnormal) such that f1 < f < f2. *)
let consecutive_float f1 f2 =
  (* Same assuming also 0 <= f1 <= f2. *)
  let consecutive_float_pos f1 f2 =
    match classify_float f1, classify_float f2 with
    | FP_nan, _ | _, FP_nan -> assert false  (* f1 <= f2 is false *)
    | FP_infinite, FP_infinite -> true
    | FP_infinite, _ -> false
    | _, FP_infinite -> f1 = max_float
    | _, FP_zero -> true
    | FP_zero, _ -> f2 = epsilon_under_float
    | FP_normal, FP_subnormal -> assert false  (* f1 < f2 is false *)
    | FP_subnormal, FP_normal -> f1 = max_subnormal && f2 = min_float
    | FP_subnormal, FP_subnormal -> f2 -. f1 <= epsilon_under_float
    | FP_normal, FP_normal ->
       let (f1, e1), (f2, e2) = frexp f1, frexp f2 in
       (e1 = e2 && f2 -. f1 <= epsilon_float /. 2.)
       || (e1 = e2 - 1 && f1 = 1.0 -. epsilon_float /. 2. && f2 = 0.5) in
  if f1 >= 0. then consecutive_float_pos f1 f2
  else if f2 <= 0. then consecutive_float_pos ( -. f2) ( -. f1)
  else false  (* f1 < 0 < f2 *)

let itv_float_of_q q = match Q.classify q with
  | Q.ZERO -> 0., 0.
  | Q.INF -> infinity, infinity
  | Q.MINF -> neg_infinity, neg_infinity
  | Q.UNDEF -> nan, nan
  | Q.NZERO ->
     if Q.lt q q_neg_max_float then neg_infinity, -. max_float
     else if Q.gt q q_max_float then max_float, infinity
     else
       let l, u =
         let a =
           let f = Z.to_float q.Q.num /. Z.to_float q.Q.den in
           match classify_float f with
           | (FP_normal | FP_subnormal | FP_zero) -> f
           | (FP_infinite | FP_nan) ->
              let nn, nd = Z.numbits q.Q.num, Z.numbits q.Q.den in
              let k = max 0 (nd - nn + 53) in
              let n = Z.div (Z.shift_left q.Q.num k) q.Q.den in
              ldexp (Z.to_float n) (- k) in
         if a >= 0. then
           (1. -. 2. *. epsilon_float) *. a -. 2. *. epsilon_under_float,
           (1. +. 2. *. epsilon_float) *. a +. 2. *. epsilon_under_float
         else
           (1. +. 2. *. epsilon_float) *. a -. 2. *. epsilon_under_float,
           (1. -. 2. *. epsilon_float) *. a +. 2. *. epsilon_under_float in
       (* above computation may overflow *)
       let l, u = max (-. max_float) l, min u max_float in
       (* Check that we have l <= q <= u. *)
       let l = if Q.geq q (Q.of_float l) then l else -. max_float in
       let u = if Q.leq q (Q.of_float u) then u else max_float in
       (* Refine the bounds q \in [l, u] by dichotomy. *)
       let rec dicho l u =
         let m =
           if l > -1. && u < 1. then (l +. u) /. 2. else l /. 2. +. u /. 2. in
         let l, u = if Q.leq (Q.of_float m) q then m, u else l, m in
         if consecutive_float l u then l, u else dicho l u in
       dicho l u

let float_of_q q =
  let n, d = Z.to_float q.Q.num, Z.to_float q.Q.den in
  if
    try Z.equal q.Q.num (Z.of_float n) && Z.equal q.Q.den (Z.of_float d)
    with Z.Overflow -> false
  then
    n /. d  (* if division is good, we get a closest float *)
  else
    let l, u = itv_float_of_q q in
    (* Keep a closest one. *)
    if Q.leq (Q.sub (Q.of_float u) q) (Q.sub q (Q.of_float l)) then u else l

external setround_tonearest : unit -> unit = "setround_tonearest"

let profile f =
  let fnbeg = Unix.time () in
  let r = f () in
  let fnend = Unix.time () in
  r, fnend -. fnbeg
let map f l = List.rev (List.rev_map f l)
