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
    calls an SDP solver and rebuilds the result.

    See file {{:../example/demo.ml}example/demo.ml} for examples of use. *)

module type S = sig
  (** {2 Polynomial expressions.} *)

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
    | Derive of polynomial_expr * int

  (** [var s] creates a new scalar variable ([Var v]). *)
  val var : string -> polynomial_expr

  (** [var_poly s n ~homogen:h d] creates a new polynomial variable
      ([Var v]). [n] is the number of variables of the new polynomial
      variable. It must be positive. [h] is [true] if the polynomial
      is homogeneous (i.e., all monomials of same degree (for instance
      x_0 x_1^3 + x_0^4 is homogeneous, x_0 x_1^3 + x_0^3 is not)),
      [false] if the polynomial is fully parameterized. [h] is [false]
      by default. [d] is the degree of the polynomial. It must be non
      negative. *)
  val var_poly : string -> int -> ?homogen:bool -> int -> polynomial_expr

  (** Functions for above constructors. *)

  val const : Poly.t -> polynomial_expr
  val mult_scalar : Poly.Coeff.t -> polynomial_expr -> polynomial_expr
  val add : polynomial_expr -> polynomial_expr -> polynomial_expr
  val sub : polynomial_expr -> polynomial_expr -> polynomial_expr
  val mult : polynomial_expr -> polynomial_expr -> polynomial_expr
  val power : polynomial_expr -> int -> polynomial_expr
  val compose : polynomial_expr -> polynomial_expr list -> polynomial_expr
  val derive : polynomial_expr -> int -> polynomial_expr
                                                            
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
                                           
  (** {2 SOS.} *)

  type options = {
    sdp : Sdp.options  (** default: {{:./Sdp.html#VALdefault}Sdp.default} *)
  }

  (** Default values above. *)
  val default : options

  (** [Minimize e] or [Maximize e] or [Purefeas] (just checking
      feasibility). [e] must be an affine combination of scalar
      variables (obtained from [var]). *)
  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas

  type values

  type witness = Monomial.t array * float array array
         
  exception Dimension_error

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
              SdpRet.t * (float * float) * values * witness list

  (** [value var values] returns the value contained in [values] for
      variable [var].

      @raise Not_found if the given polynomial_expr [var] was not
      obtained with the function {{:#VALvar}var} or if no value is
      found for [var] in [values]. *)
  val value : polynomial_expr -> values -> Poly.Coeff.t

  (** [value_poly var values] returns the value contained in [values]
      for polynomial variable [var].

      @raise Not_found if the given polynomial_expr [var] was not
      obtained with the function {{:#VALvar_poly}var_poly} or if no
      value is found for [var] in [values]. *)
  val value_poly : polynomial_expr -> values -> Poly.t

  (** If [check e (v, Q)] returns [true], then [e] is SOS. Otherwise,
      either [e] is not SOS or the difference between [e] and v^T Q v
      is too large or Q is not positive definite enough for the proof
      to succeed. The witness [(v, Q)] is typically obtained from the
      above function {{:#VALsolve}solve}.

      Here is how it works. [e] is expected to be the polynomial v^T Q
      v modulo numerical errors. To prove that [e] is SOS despite
      these numerical errors, we first check that the polynomial [e]
      can be expressed as v^T Q' v for some Q' (i.e. that the monomial
      base [v] contains enough monomials). Then [e] can be expressed
      as v^T (Q + R) v where R is a matrix with all coefficients
      bounded by the maximum difference between the coefficients of
      the polynomials [e] and v^T Q v. If all such matrices Q + R are
      positive definite, then [e] is SOS.

      @raise Invalid_argument "Sos.check" if [e] contains variables
      [Var]. *)
  val check : polynomial_expr -> witness -> bool
                                                  
  (** {2 Printing functions.} *)

  (** Printer for polynomial expressions. *)
  val pp : Format.formatter -> polynomial_expr -> unit

  (** See {{:./Monomial.html#VALpp_names}Monomial.pp_names} for details about
      [names]. *)
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P

module Float : S with module Poly = Polynomial.Float
