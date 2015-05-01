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

(** Various utility functions. *)

val fprintf_list :
  sep:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e list -> unit

val fprintf_array :
  sep:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e array -> unit

val fprintf_matrix :
  begl:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  endl:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  sepl:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  sepc:('m, 'n, 'o, 'p, 'p, 'm) format6 ->
  (Format.formatter -> 'q -> unit) -> Format.formatter -> 'q array array -> unit

(** [itv_float_of_q q] returns two floats [l, u] such that, when q is
    [Q.undef], [l] and [u] are both [nan], otherwise l <= q <= u and
    there is no float (either normal or subnormal) such that l < f <
    u. *)
val itv_float_of_q : Q.t -> float * float

(** [float_of_q q] returns a float closest to [q]. *)
val float_of_q : Q.t -> float
