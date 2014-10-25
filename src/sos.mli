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

  type polynomial_var = {
    name : Ident.t;
    nb_vars : int;
    degree : int;  (** must be even *)
    homogeneous : bool }

  (** Constructors. See the module
      {{:./Polynomial.S.html}Polynomial.S} for details. *)
  type polynomial_expr =
    | PLconst of Poly.t
    | PLvar of polynomial_var
    | PLmult_scalar of Ident.t * polynomial_expr
    | PLadd of polynomial_expr * polynomial_expr
    | PLsub of polynomial_expr * polynomial_expr
    | PLmult of polynomial_expr * polynomial_expr
    | PLpower of polynomial_expr * int
    | PLcompose of polynomial_expr * polynomial_expr list

  (** [Minimize var] or [Maximize var] or [Purefeas] (just checking
      feasibility). Ident [var] must appear as scalar variable
      ([PLmult_scalar]) in the constraints. *)
  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type ('a, 'b) value_t = Scalar of 'a | Poly of 'b

  exception Type_error of string

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each polynomial expressions in [l] is
      SOS. Constraints "v is SOS" (i.e., "v is non negative" for
      scalars) are also implicitly added for all variables appearing
      in [l]. Returns both the achieved objective value and a map with
      values for each variable appearing in [l]. The returned map will
      be empty in case of failure.

      @raise Type_error with an explanatory message in case something
      inconsistent is found.

      @raise LinExpr.Not_linear if one of the input polynomial
      expressions in [l] is non linear. *)
  val solve : ?solver:Sdp.solver -> obj_t -> polynomial_expr list ->
              SdpRet.t * (float * float)
              * (Poly.Coeff.t, Poly.t) value_t Ident.Map.t

  (** Printer for polynomial expressions. *)
  val pp : ?names:string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P

module Float : S with module Poly = Polynomial.Float
