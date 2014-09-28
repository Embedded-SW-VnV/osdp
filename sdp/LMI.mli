(** Interface towards SDP solvers providing high level LMI constructs.

    This module takes LMI problems, transforms them as dual and primal
    problems and calls an SDP solver. *)

module type S = sig
  module Mat : Matrix.S
  module LinExpr : LinExpr.S

(*  type lmi_obj_t = (objKind * Ident.t) option*)
    
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
    | MEmult_const of Mat.Elem.t * matrix_expr
    | MEmult_scalar of Ident.t * matrix_expr
    | MEadd of matrix_expr * matrix_expr
    | MEsub of matrix_expr * matrix_expr
    | MEmult of matrix_expr * matrix_expr

  (** Type of variables. *)
  type ty =
    | TIscal  (** scalar variable *)
    | TImat of int option  (** matrix variable and size (if known) *)

  exception Type_error of string

  (** Infers types of variables (i.e., whether they are scalars or
      matrices and, in the latter case, size of the matrices).

      @raise Type_error with an explanatory message in case something
      inconsistent is found. *)
  val type_check : matrix_expr list -> ty Ident.Map.t

  exception Not_linear

  (** Decomposes all matrix variables into a matrix of new scalar
      variables and returns a matrix of linear expressions in those
      scalar variables. Also returns a mapping [m]. All new variables
      [sv] map to [v, (i, j)] in [m] where [v] is the matrix variable
      they are part of and [i] and [j] their indices (starting from 0)
      in [v]. Only upper triangular coeffs are provided since all
      matrix variables [v] are symmetric. The second argument should
      be the result of {!type_check} on the first argument.

      @raise Type_error in case the type of a variable is unknown.

      @raise Not_linear if one of the input matrix expressions is non
      linear. *)
  val scalarize : matrix_expr list -> ty Ident.Map.t ->
                  LinExpr.t array array list * (Ident.t * (int * int)) Ident.Map.t

  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas
  type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

  exception Not_symmetric

  (** [solve obj l] tries to optimise the objective [obj] under the
      constraint that each matrix expressions in [l] is positive
      semi-definite. Returns both the achieved objective value and a
      map with values for each variable appearing in [l]. The returned
      map will be empty in case of failure.

      @raise Type_error with an explanatory message in case something
      inconsistent is found or the type of a variable cannot be
      determined.

      @raise Not_linear if one of the input matrix expressions in [l]
      is non linear.

      @raise Not_symmetric if one of the input matrix expressions in [l]
      is non symmetric. *)
  val solve : obj_t -> matrix_expr list ->
              float * (Mat.Elem.t, Mat.t) value_t Ident.Map.t

  (** Printer for LMI *)
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) (LE : LinExpr.S with module Coeff = M.Elem) :
  S with module Mat = M and module LinExpr = LE

module NumLMI :
  S with module Mat = Matrix.NumMat and module LinExpr = LinExpr.Num

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
