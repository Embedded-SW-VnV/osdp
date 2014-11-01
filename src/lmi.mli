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

(** {b Linear Matrix Inequalities (LMI)} optimization.

    This module takes LMI problems, transforms them to SDP problems,
    calls an SDP solver and rebuilds the result. *)

module type S = sig
  module Mat : Matrix.S

  (** Constructors. See the module {{:./Matrix.S.html}Matrix.S} for
      details. *)
  type matrix_expr =
    | Const of Mat.t
    | Var of Ident.t  (** All matrix variables are symmetric. *)
    | Zeros of int * int
    | Eye of int
    | Kronecker_sym of int * int * int
    | Block of matrix_expr array array
    | Lift_block of matrix_expr * int * int * int * int
    | Transpose of matrix_expr
    | Minus of matrix_expr
    | Scale_const of Mat.Coeff.t * matrix_expr
    | Scale_var of Ident.t * matrix_expr
    | Add of matrix_expr * matrix_expr
    | Sub of matrix_expr * matrix_expr
    | Mult of matrix_expr * matrix_expr

  (** [Minimize var] or [Maximize var] or [Purefeas] (just checking
      feasibility). Ident [var] must appear as scalar variable
      ([Scale_var]) in the LMIs. *)
  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

  exception Type_error of string
  exception Not_symmetric

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each matrix expressions in [l] is positive
      semi-definite. Returns both the achieved objective value and a
      map with values for each variable appearing in [l]. The returned
      map will be empty in case of failure.

      @raise Type_error with an explanatory message in case something
      inconsistent is found or the type of a variable cannot be
      determined.

      @raise LinExpr.Not_linear if one of the input matrix expressions
      in [l] is non linear.

      @raise Not_symmetric if one of the input matrix expressions in [l]
      is non symmetric. *)
  val solve : ?solver:Sdp.solver -> obj_t -> matrix_expr list ->
              SdpRet.t * (float * float) * (Mat.Coeff.t, Mat.t) value_t Ident.Map.t

  (** Printer for LMI. *)
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) : S with module Mat = M

module Float : S with module Mat = Matrix.Float
