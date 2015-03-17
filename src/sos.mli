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

(** {b Sum of Squares (SOS)} optimization.

    This module takes SOS problems, transforms them to SDP problems,
    calls an SDP solver and rebuilds the result. *)

module type S = sig
  module Poly : Polynomial.S

  (** Scalar or polynomial variables. *)
  type var

  (** Constructors. See the module
      {{:./Polynomial.S.html}Polynomial.S} for details. *)
  type polynomial_expr =
    | Const of Poly.t
    | Var of var
    | Mult_scalar of Poly.Coeff.t * polynomial_expr
    | Add of polynomial_expr * polynomial_expr
    | Sub of polynomial_expr * polynomial_expr
    | Mult of polynomial_expr * polynomial_expr
    | Power of polynomial_expr * int
    | Compose of polynomial_expr * polynomial_expr list

  (** [var s] creates a new scalar variable ([Var v]). *)
  val var : string -> polynomial_expr

  (** [var_poly s n ~homogen:h d] creates a new polynomial variable
      ([Var v]). [n] is the number of variables of the new polynomial
      variable. It must be positive. [h] is [true] if the polynomial
      is homogeneous (i.e., all monomials of same degree (for instance
      x_0 x_1^3 + x_0^4 is homogeneous, x_0 x_1^3 + x_0^3 is not)),
      [false] if the polynomial is fully parameterized. [h] is [false]
      by default. [d] is the degree of the polynomial. It must be
      positive. *)
  val var_poly : string -> int -> ?homogen:bool -> int -> polynomial_expr

  (** [Minimize e] or [Maximize e] or [Purefeas] (just checking
      feasibility). [e] must be an affine combination of scalar
      variables (obtained from [var]). *)
  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas

  type values

  exception Dimension_error

  exception Not_linear

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each polynomial expressions in [l] is
      SOS. Returns both the achieved objective value and a map with
      values for each variable appearing in [l]. The returned map will
      be empty in case of failure (i.e., [SdpRet.t] being not Success
      or PartialSuccess).

      @raise Dimension_error in case [compose] is used with not enough
      variables.

      @raise Not_linear if the objective [obj] or one of the input
      polynomial expressions in [l] is non linear. *)
  val solve : ?solver:Sdp.solver -> obj -> polynomial_expr list ->
              SdpRet.t * (float * float) * values

  (** [value v m] returns the value contained in [m] for
      variable [v].

      @raise Not_found if the given polynomial_expr [v] was not
      obtained with the function [var] or if no value is found for
      [v] in [m]. *)
  val value : polynomial_expr -> values -> Poly.Coeff.t

  (** [value v m] returns the value contained in [m] for polynomial
      variable [v].

      @raise Not_found if the given polynomial_expr [v] was not
      obtained with the function [var_poly] or if no value is found
      for [v] in [m]. *)
  val value_poly : polynomial_expr -> values -> Poly.t

  (** Printer for polynomial expressions. *)
  val pp : Format.formatter -> polynomial_expr -> unit

  (** See {{:./Monomial.html#VALpp_names}Monomial.pp_names} for details about
      [names]. *)
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P

module Float : S with module Poly = Polynomial.Float
