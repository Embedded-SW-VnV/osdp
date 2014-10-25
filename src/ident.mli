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

(** Unique identificators. *)

(** type of identificators. *)
type t

(** Create a new unique identificator. Try to call it as requested and
    append numbers if it already exists. For instance, multiple calls
    with the string "v" will result in v, v0, v1,... Empty string is
    treated as the string "x". *)
val create : string -> t

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

module Set : Set.S with type elt = t

module Map : Map.S with type key = t
