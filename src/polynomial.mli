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

(** Multiple variables polynomials. *)

module type S = sig
  (** Type of coefficients. *)
  module  Coeff : Scalar.S

  (** Type of polynomials. *)
  type t

  (** {2 Conversion functions.} *)

  (** [of_list \[(m_1, a_1);..; (m_n, a_n)\]] builds the polynomial
      a_1 m_1 + ... + a_n m_n. Duplicates or zeros coefficients are
      accepted (for instance 2 x_0 + 0 x_1 + 3 x_0 is a valid input
      for 5 x_0). *)
  val of_list : (Monomial.t * Coeff.t) list -> t

  (** Returns a list sorted in increasing order of
      {{:./Monomial.html#VALcompare}Monomial.compare} without
      duplicates nor zeros. *)
  val to_list : t -> (Monomial.t * Coeff.t) list

  (** {2 A few values.} *)

  val zero : t
  val one : t

  (** {2 Arithmetic operations.} *)
                                         
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t

  (** [power p n] computes p^n, [n] must be non negative. *)
  val power : t -> int -> t

  exception Dimension_error

  (** [compose p \[q_1;...; q_n\]] returns the polynomial p' such that
      p'(x_1,..., x_m) = p(q_1(x_1,..., x_m),..., q_n(x_1,...,
      x_m)).

      @raise Dimension_error if [p] is defined on more than n
      variables. *)
  val compose : t -> t list -> t

  (** {2 Various functions.} *)

  (** Number of variables appearing in the polynomial. *)
  val nb_vars : t -> int

  (** -1 for the null polynomial. *)
  val degree : t -> int

  val is_homogeneous : t -> bool
                                         
  (** {2 Printing.} *)

  (** See {{:./Monomial.html#VALpp}Monomial.pp} for details about
      [names]. *)
  val pp : ?names:string list -> Format.formatter -> t -> unit
  end

module Make (SC : Scalar.S) : S with module Coeff = SC

module Q : S with module Coeff = Scalar.Q

module Float : S with module Coeff = Scalar.Float
