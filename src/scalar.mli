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

(** Type of scalars along with basic scalar operations.

    Useful to build matrices over it (see module
    {{:./Matrix.html}Matrix}). *)

module type S = sig
    type t 

    (** {2 A few values.} *)

  val zero : t
  val one : t
  val is_zero : t -> bool

  (** {2 Conversion functions.} *)

  val of_float : float -> t
  val to_float : t -> float

  (** {2 Arithmetic operations.} *)

  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t

  (** {2 Printing.} *)

  val pp : Format.formatter -> t -> unit
end

module Q : S with type t = Q.t

module Float : S with type t = float
