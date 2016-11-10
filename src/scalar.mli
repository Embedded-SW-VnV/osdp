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

(** Type of scalars along with basic scalar operations.

    Useful to build matrices or polynomials (for instance) over it
    (see modules {{:./Matrix.html}Matrix} and
    {{:./Polynomial.html}Polynomial}). *)

(** A minimalistic module type. *)
module type M = sig
  type t 

  val compare : t -> t -> int
         
  (** {2 A few values.} *)

  val zero : t
  val one : t

  (** {2 Conversion functions.} *)

  val of_float : float -> t
  val to_float : t -> float
  val to_q : t -> Q.t
                        
  (** {2 Arithmetic operations.} *)

  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t

  (** {2 Printing.} *)

  val pp : Format.formatter -> t -> unit
end

(** An extended module type. *)
module type S = sig
  include M

  val minus_one : t
            
  (** {2 More conversion functions.} *)

  val of_int : int -> t

  (** {2 More arithmetic operations.} *)

  val neg : t -> t
  val inv : t -> t
                   
  val equal : t -> t -> bool
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool

  (** Returns -1, 0 or 1 when its argument is respectively < 0, 0 or >
      0. *)
  val sign : t -> int

  (** {2 Prefix and infix operators.} *)

  val ( ~- ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
end

(** Adds extended operations.

    [minus_one] is defined as [M.sub M.zero M.one].

    [of_int n] is defined as [M.of_float (float_of_int n)]. Beware
    that this may overflow for large 63 bits integers.

    [neg x] is defined as [M.sub M.zero x].

    [inv x] is defined as [M.div M.one x].

    Additional comparison operations are defined based on [M.compare]. *)
module Make (M : M) : S with type t = M.t
                  
module Q : S with type t = Q.t

module Float : S with type t = float
