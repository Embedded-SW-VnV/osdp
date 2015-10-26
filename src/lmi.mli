(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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

(** {b Linear Matrix Inequalities (LMI)} optimization.

    This module takes LMI problems, transforms them to SDP problems,
    calls an SDP solver and rebuilds the result.

    See file {{:../example/demo.ml}example/demo.ml} for examples of use. *)

module type S = sig
  (** {2 Matrix expressions.} *)

  module Mat : Matrix.S

  (** Scalar or symmetric matrix variables. *)
  type var

  (** Constructors. See the module {{:./Matrix.S.html}Matrix.S} for
      details. *)
  type matrix_expr =
    | Const of Mat.t
    | Var of var  (** All variables are symmetric square matrices. *)
    | Zeros of int * int
    | Eye of int
    | Kron of int * int * int
    | Kron_sym of int * int * int
    | Block of matrix_expr array array
    | Lift_block of matrix_expr * int * int * int * int
    | Transpose of matrix_expr
    | Minus of matrix_expr
    | Add of matrix_expr * matrix_expr
    | Sub of matrix_expr * matrix_expr
    | Mult of matrix_expr * matrix_expr  (** [mult_scalar] or [mult], according to he size of the first argument. *)

  (** [var s n] creates a new variable ([Var v]). [n] is the size of
      the variable (scalars and matrices of size 1 are considered the
      same). It must be positive. *)
  val var : string -> int -> matrix_expr

  (** [scalar s] returns [Const (Mat.of_list_list [[s]])]. *)
  val scalar : Mat.Coeff.t -> matrix_expr

  (** {2 SOS.} *)

  type options = {
    sdp : Sdp.options;  (** default: {{:./Sdp.html#VALdefault}Sdp.default} *)
    verbose : int  (** verbosity level, non negative integer, 0 (default)
                       means no output (but see sdp.verbose just above) *)
  }
		   
  (** Default values above. *)
  val default : options

  (** [Minimize e] or [Maximize e] or [Purefeas] (just checking
      feasibility). [e] must be a scalar (i.e., a matrix of size
      1). *)
  type obj = Minimize of matrix_expr | Maximize of matrix_expr | Purefeas

  type values

  exception Dimension_error of string

  exception Not_linear

  exception Not_symmetric

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each matrix expressions in [l] is positive
      semi-definite. Returns both the achieved objective value and a
      map with values for each variable appearing in [l]. The returned
      map will be empty in case of failure (i.e., [SdpRet.t] being not
      Success or PartialSuccess).

      @raise Dimension_error with an explanatory message in case something
      inconsistent is found or the type of a variable cannot be
      determined.

      @raise Not_linear if the objective [obj] is not a scalar (1x1
      matrix) or one of the input matrix expressions in [l] is non
      linear.

      @raise Not_symmetric if one of the input matrix expressions in [l]
      is non symmetric. *)
  val solve : ?options:options -> ?solver:Sdp.solver ->
              obj -> matrix_expr list ->
              SdpRet.t * (float * float) * values

  (** [value e values] returns the evaluation of matrix expression
      [e], replacing all [Var] by the corresponding value in [values].

      @raise Not_found if one of the variables appearing in [e] has no
      corresponding value in [values].

      @raise Dimension_error if [e] is not a scalar. *)
  val value : matrix_expr -> values -> Mat.Coeff.t

  (** [value_mat e values] returns the evaluation of matrix expression
      [e], replacing all [Var] by the correspoding value in [values].

      @raise Not_found if one of the variables appearing in [e] has no
      corresponding value in [values]. *)
  val value_mat : matrix_expr -> values -> Mat.t

  (** {2 Printing function.} *)

  (** Printer for LMI. *)
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) : S with module Mat = M

(** TODO: explain differences between these modules. *)

module Q : S with module Mat = Matrix.Q

module Float : S with module Mat = Matrix.Float
