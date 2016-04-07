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

val pp_list :
  sep:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val pp_array :
  sep:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit

val pp_matrix :
  begl:(unit, Format.formatter, unit) format ->
  endl:(unit, Format.formatter, unit) format ->
  sepl:(unit, Format.formatter, unit) format ->
  sepc:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array array -> unit

(** [itv_float_of_q q] returns two floats [l, u] such that, when q is
    [Q.undef], [l] and [u] are both [nan], otherwise l <= q <= u and
    there is no float (either normal or subnormal) such that l < f <
    u.

    @raise Z.Overflow to indicate an error (typically when the
    numerator or denominator doesn't fit in a float). *)
val itv_float_of_q : Q.t -> float * float

(** [float_of_q q] returns a float closest to [q].

    @raise Z.Overflow to indicate an error (typically when the
    numerator or denominator doesn't fit in a float). *)
val float_of_q : Q.t -> float

(** [profile f] executes the function [f] and returns both its result
    and the execution time in second. *)
val profile : (unit -> 'a) -> 'a * float

(** tail-recursive version of [List.map] (implemented using
    List.rev_map and List.rev) *)
val map : ('a -> 'b) -> 'a list -> 'b list
