(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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

(** {b Sum of Squares (SOS)} optimization.

    This module takes SOS problems, transforms them to SDP problems,
    calls an SDP solver and rebuilds the result.

    See file {{:../example/demo.ml}example/demo.ml} for examples of use. *)

module type S = sig
  (** {2 Polynomial expressions.} *)

  module Poly : Polynomial.S

  (** Parametric variables. *)
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
    | Derive of polynomial_expr * int

  (** [make s] creates a new parametric variable named [s].
      For instance, [make "lambda"] creates a new scalar parametric
      variable and [make ~n:1 ~d:2 "p"] creates a new polynomial
      parametric variable (p_0 + p_1 x0 + p_2 x0^2 with p_0, p_1 and
      p_2 scalar parametric variables).

      @param n number of variables (default: 1), must be positive

      @param d degree (default: 1), must be positive

      @param homogen creates an homogeneous polynomial (default:
      false), i.e., all monomials of same degree (for instance x_0
      x_1^3 + x_0^4 is homogeneous, x_0 x_1^3 + x_0^3 is not) *)
  val make : ?n:int -> ?d:int -> ?homogen:bool -> string -> polynomial_expr

  (** Functions for above constructors. *)

  val const : Poly.t -> polynomial_expr
  val scalar : Poly.Coeff.t -> polynomial_expr
  val monomial : Monomial.t -> polynomial_expr
  val mult_scalar : Poly.Coeff.t -> polynomial_expr -> polynomial_expr
  val add : polynomial_expr -> polynomial_expr -> polynomial_expr
  val sub : polynomial_expr -> polynomial_expr -> polynomial_expr
  val mult : polynomial_expr -> polynomial_expr -> polynomial_expr
  val power : polynomial_expr -> int -> polynomial_expr
  val compose : polynomial_expr -> polynomial_expr list -> polynomial_expr
  val derive : polynomial_expr -> int -> polynomial_expr

  (** {3 Conversion functions.} *)

  val of_list : (Monomial.t * polynomial_expr) list -> polynomial_expr

  exception Dimension_error

  (** Returns a list sorted in increasing order of
      {{:./Monomial.html#VALcompare}Monomial.compare} without
      duplicates. All polynomial_expr in the returned list are scalars
      (i.e., functions {{:VALnb_vars}nb_vars} and {{:VALdegree}degree}
      below both return 0).

      @raise Dimension_error in case {!val:compose} is used with not
      enough arguments. *)
  val to_list : polynomial_expr -> (Monomial.t * polynomial_expr) list
                                                                  
  (** {3 Various functions.} *)

  (** See the module {{:./Polynomial.S.html}Polynomial.S} for
      details. Beware that the returned values correspond to the
      polynomial_expression and can be larger than the result after
      solving. For instance, considering the expression v x_1^3 +
      x_0^2 with v a variable, [nb_vars] and [degree] will
      respectively return 2 and 3. Although, when asking the
      expression to be SOS, v will be 0 and then nb_vars and degree
      will become respectively 1 and 2 in the result. *)

  (** @raise Dimension_error in case {!val:compose} is used with not
      enough arguments. *)
  val nb_vars : polynomial_expr -> int

  (** @raise Dimension_error in case {!val:compose} is used with not
      enough arguments. *)
  val degree : polynomial_expr -> int

  (** @raise Dimension_error in case {!val:compose} is used with not
      enough arguments. *)
  val is_homogeneous : polynomial_expr -> bool

  (** [param_vars e] returns the list of all parametric variables
      appearing in [e]. *)
  val param_vars : polynomial_expr -> var list

  (** {3 Prefix and infix operators.} *)

  (** To use this operators, it's convenient to use local opens. For
      instance to write the polynomial 2.3 x0^3 x2^2 + x1 + 0.5:

      {[let module Sos = Osdp.Sos.Float in
Sos.(2.3 *. ??0**3 * ??2**2 + ??1 + !0.5)]} *)

  (** See the module {{:./Polynomial.S.html}Polynomial.S} for
      details. *)

  (** {{:#TYPEELTpolynomial_expr.Const}Const} *)
  val ( !! ) : Poly.t -> polynomial_expr
                        
  val ( ?? ) : int -> polynomial_expr

  (** {{:#VALscalar}scalar} *)
  val ( ! ) : Poly.Coeff.t -> polynomial_expr
                        
  val ( *. ) : Poly.Coeff.t -> polynomial_expr -> polynomial_expr
                        
  val ( ~- ) : polynomial_expr -> polynomial_expr
                                 
  val ( + ) : polynomial_expr -> polynomial_expr -> polynomial_expr
                        
  val ( - ) : polynomial_expr -> polynomial_expr -> polynomial_expr
                        
  val ( * ) : polynomial_expr -> polynomial_expr -> polynomial_expr
                        
  val ( / ) : polynomial_expr -> Poly.Coeff.t -> polynomial_expr
                        
  val ( /. ) : Poly.Coeff.t -> Poly.Coeff.t -> polynomial_expr
                        
  val ( ** ) : polynomial_expr -> int -> polynomial_expr

  (** [e1 >= e2] is just syntactic sugar for [e1 - e2]. *)
  val ( >= ) : polynomial_expr -> polynomial_expr -> polynomial_expr
                                           
  (** [e1 <= e2] is just syntactic sugar for [e2 - e1]. *)
  val ( <= ) : polynomial_expr -> polynomial_expr -> polynomial_expr

  (** {3 Printing.} *)

  (** Printer for polynomial expressions. *)
  val pp : Format.formatter -> polynomial_expr -> unit

  (** See {{:./Monomial.html#VALpp_names}Monomial.pp_names} for details about
      [names]. *)
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit

  (** {2 SOS.} *)

  type options = {
    sdp : Sdp.options;  (** default: {{:./Sdp.html#VALdefault}Sdp.default} *)
    verbose : int;  (** verbosity level, non negative integer, 0 (default)
                        means no output (but see sdp.verbose just above) *)
    scale : bool;  (** scale (default: true) *)
    trace_obj : bool;  (** When no objective is set
                           ({{:./Sos.Make.html#TYPEELTobj.Purefeas}Purefeas}
                           below), minimize the trace of the SOS
                           constraints instead of no objective
                           (default: false). *)
    dualize : bool;  (** solve using the dual representation, can be
                         slower but more robust (default: false) *)
    monoms : Monomial.t list list;  (** monomials (default: \[\]) for each
                                        constraint (automatically
                                        determined when list shorter than
                                        constraints list) *)
    pad : float;  (** padding factor (default: 2.), 0. means no padding *)
    pad_list : float list  (** padding factors (dafault: \[\]) for
                               each constraint ([pad] used when list
                               shorter than constraints list) *)
  }

  (** Default values above. *)
  val default : options

  (** [Minimize e] or [Maximize e] or [Purefeas] (just checking
      feasibility). [e] must be an affine combination of scalar
      variables (obtained from [var]). *)
  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas

  type values

  type 'a witness = Monomial.t array * 'a array array
         
  exception Not_linear

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each polynomial expressions in [l] is SOS. If
      [solver] is provided, it will supersed the solver given in
      [options]. Returns a tuple [ret, (pobj, dobj), values,
      witnesses]. If {{:./SdpRet.html#VALis_success}SdpRet.is_success}
      [ret], then the following holds. [pobj] (resp. [dobj]) is the
      achieved primal (resp. dual) objective value. [values] contains
      values for each variable appearing in [l] (to be retrieved
      through following functions {{:#VALvalue}value} and
      {{:#VALvalue_poly}value_poly}). [witnesses] is a list with the
      same length than [l] containing pairs [v, Q] such that [Q] is a
      square matrix and [v] a vector of monomials of the same size and
      the polynomial [v^T Q v] should be close from the corresponding
      polynomial in [l].

      If [ret] is {{:./SdpRet.html#TYPEELTt.Success}SdpRet.Success},
      then all SOS constraints in [l] are indeed satisfied by the
      values returned in [values] (this is checked through the
      function {{:#VALcheck}check} below with [witnesses]).

      @raise Dimension_error in case
      {{:#TYPEELTpolynomial_expr.Compose}Compose} is used with not
      enough variables in [obj] or one of the element of [l].

      @raise Not_linear if the objective [obj] or one of the input
      polynomial expressions in [l] is non linear. *)
  val solve : ?options:options -> ?solver:Sdp.solver ->
              obj -> polynomial_expr list ->
              SdpRet.t * (float * float) * values * float witness list

  (** [value e values] returns the evaluation of polynomial expression
      [e], replacing all [Var] by the correspoding value in [values].

      @raise Not_found if one of the variables appearing in [e] has no
      corresponding value in [values].

      @raise Dimension_error if [e] is not a scalar. *)
  val value : polynomial_expr -> values -> Poly.Coeff.t

  (** [value_poly e values] returns the evaluation of polynomial
      expression [e], replacing all [Var] by the correspoding value in
      [values].

      @raise Not_found if one of the variables appearing in [e] has no
      corresponding value in [values]. *)
  val value_poly : polynomial_expr -> values -> Poly.t

  (** If [check ?options e ?values (v, Q)] returns [true], then [e] is
      SOS. Otherwise, either [e] is not SOS or the difference between
      [e] and v^T Q v is too large or Q is not positive definite
      enough for the proof to succeed. The witness [(v, Q)] is
      typically obtained from the above function
      {{:#VALsolve}solve}. If [e] contains variables, they are
      replaced by the corresponding value in [values] ([values] is
      empty by default).

      Here is how it works. [e] is expected to be the polynomial v^T Q
      v modulo numerical errors. To prove that [e] is SOS despite
      these numerical errors, we first check that the polynomial [e]
      can be expressed as v^T Q' v for some Q' (i.e. that the monomial
      base [v] contains enough monomials). Then [e] can be expressed
      as v^T (Q + R) v where R is a matrix with all coefficients
      bounded by the maximum difference between the coefficients of
      the polynomials [e] and v^T Q v. If all such matrices Q + R are
      positive definite, then [e] is SOS.

      @raise Not_found if [e] contains a variable not present in
      [values]. *)
  val check : ?options:options -> ?values:values -> polynomial_expr ->
              float witness -> bool

  (** TODO: doc (@raise Invalid argument when lists el and wl not same
      length, @raise Not_found when el contain a variable not bound in
      values) *)
  val check_round : ?options:options -> ?values:values ->
                    polynomial_expr list -> float witness list ->
                    (values * Scalar.Q.t witness list) option
end

module Make (P : Polynomial.S) : S with module Poly = P

(** TODO: explain differences between these modules. *)

module Q : S with module Poly = Polynomial.Q

module Float : S with module Poly = Polynomial.Float
