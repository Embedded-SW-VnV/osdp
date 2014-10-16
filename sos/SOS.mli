(** Sum of Squares (SOS) optimization.

    This module takes SOS problems, transforms them to SDP problems,
    calls an SDP solver and rebuilds the result. *)

module type S = sig
  module Poly : Polynomial.S

  type polynomial_var = {
    name : Ident.t;
    nb_vars : int;
    degree : int;  (** must be even *)
    homogeneous : bool }

  (** Constructors. See the module {!Polynomial.S} for details. *)
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
              float * (Poly.Coeff.t, Poly.t) value_t Ident.Map.t

  (** Printer for polynomial expressions. *)
  val pp : ?names:string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P

module Num : S with module Poly = Polynomial.Num

module Float : S with module Poly = Polynomial.Float

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
