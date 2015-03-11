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
    | Const of Poly.t
    | Var of Ident.t  (** scalar variable *)
    | Var_poly of polynomial_var  (** polynomial variable *)
    | Mult_scalar of Poly.Coeff.t * polynomial_expr
    | Add of polynomial_expr * polynomial_expr
    | Sub of polynomial_expr * polynomial_expr
    | Mult of polynomial_expr * polynomial_expr
    | Power of polynomial_expr * int
    | Compose of polynomial_expr * polynomial_expr list

  (** [var s] is equivalent to [Var (Ident.create s)]. *)
  val var : string -> polynomial_expr

  (** [var_poly s n h d] is equivalent to [Var { name = Ident.create
      s; nb_vars = n; degree = d; homogeneous = h}]. [h] is [false] by
      default.

      todo: invalid argument for non positive values (or negative and
      0 ~~> scalar) *)
  val var_poly : string -> int -> ?homogen:bool -> int -> polynomial_expr

  (** [Minimize var] or [Maximize var] or [Purefeas] (just checking
      feasibility). Ident [var] must appear as scalar variable
      ([Mult_scalar]) in the constraints. *)
  type obj = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type value = Scalar of Poly.Coeff.t | Poly of Poly.t

  exception Type_error of string

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each polynomial expressions in [l] is
      SOS. Constraints "v is SOS" (i.e., "v is non negative" for
      scalars) are also implicitly added for all variables appearing
      in [l]. Returns both the achieved objective value and a map with
      values for each variable appearing in [l]. The returned map will
      be empty in case of failure (i.e., [SdpRet.t] being not Success
      or PartialSuccess).

      @raise Type_error with an explanatory message in case something
      inconsistent is found.

      @raise LinExpr.Not_linear if one of the input polynomial
      expressions in [l] is non linear. *)
  val solve : ?solver:Sdp.solver -> obj -> polynomial_expr list ->
              SdpRet.t * (float * float) * value Ident.Map.t

  (** [value (Var id) m] returns the value contained in [m] for
      variable [id].

      @raise Not_found if the given polynomial_expr is not of the form
      [Var id] or if no value is found for [id] in [m]. *)
  val value : polynomial_expr -> value Ident.Map.t -> Poly.Coeff.t

  (** [value (Var_poly pv) m] returns the value contained in [m] for
      polynomial variable [pv].

      @raise Not_found if the given polynomial_expr is not of the form
      [Var_poly pv] or if no value is found for [pv] in [m]. *)
  val value_poly : polynomial_expr -> value Ident.Map.t -> Poly.t

  (** Printer for polynomial expressions. *)
  val pp : Format.formatter -> polynomial_expr -> unit

  (** See {{:./Monomial.html#VALpp_names}Monomial.pp_names} for details about
      [names]. *)
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P

module Float : S with module Poly = Polynomial.Float
