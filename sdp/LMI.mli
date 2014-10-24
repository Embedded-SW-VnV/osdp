(** Linear Matrix Inequalities (LMI) optimization.

    This module takes LMI problems, transforms them to SDP problems,
    calls an SDP solver and rebuilds the result. *)

module type S = sig
  module Mat : Matrix.S

  (** Constructors. See the module {!Matrix.S} for details. *)
  type matrix_expr =
    | MEconst of Mat.t
    | MEvar of Ident.t  (** All matrix variables are symmetric. *)
    | MEzeros of int * int
    | MEeye of int
    | MEkronecker_sym of int * int * int
    | MEblock of matrix_expr array array
    | MElift_block of matrix_expr * int * int * int * int
    | MEtranspose of matrix_expr
    | MEminus of matrix_expr
    | MEscale_const of Mat.Elem.t * matrix_expr
    | MEscale_var of Ident.t * matrix_expr
    | MEadd of matrix_expr * matrix_expr
    | MEsub of matrix_expr * matrix_expr
    | MEmult of matrix_expr * matrix_expr

  (** [Minimize var] or [Maximize var] or [Purefeas] (just checking
      feasibility). Ident [var] must appear as scalar variable
      ([MEscale_var]) in the LMIs. *)
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
              SdpRet.t * (float * float) * (Mat.Elem.t, Mat.t) value_t Ident.Map.t

  (** Printer for LMI. *)
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) : S with module Mat = M

module NumLMI : S with module Mat = Matrix.NumMat

module Float : S with module Mat = Matrix.FloatMat

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
