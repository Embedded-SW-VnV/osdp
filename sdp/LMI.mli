(** This module is an interface towards SDP solvers providing high level LMI
    constructs. It takes LMI problems, transform them as dual and primal
    problems and call SDP. *)

type ident = string
type objKind = Minimize | Maximize
type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

module type Sig = 
sig
  module Mat: Matrix.S

  (* Unknown matrices are always symetrical *)
  type matrix_expr 
      
(** Objective to be maximized by the solver: None denotes no objective, Some
    (positive_sign, v) denotes the value v if positive_sign is true, -v
    otherwise. *)
  type lmi_obj_t = (objKind * ident) option
    
    
(** Printers for LMI *)
 val pp_matrix_expr : Format.formatter -> matrix_expr -> unit
    
 (** Constructors *)
 val zeros: int -> int -> matrix_expr
 val eye: int -> matrix_expr
 val diag: matrix_expr list -> matrix_expr 
 val const_mult: Mat.elt -> matrix_expr -> matrix_expr
 val scal_mult: string -> matrix_expr -> matrix_expr
 val symmat: string * int ->  matrix_expr
 val const_mat: Mat.t -> matrix_expr
 val trans_const_mat: Mat.t -> matrix_expr
 val kronecker_sym: ?lift:(Mat.t -> Mat.t) -> int -> int -> int -> matrix_expr
 val add: matrix_expr -> matrix_expr -> matrix_expr 
 val sub: matrix_expr -> matrix_expr -> matrix_expr 
 val mult: matrix_expr -> matrix_expr -> matrix_expr
 val of_array_array: matrix_expr array array -> matrix_expr    

  (* val rewrite_mat_as_scalar: matrix_expr -> matrix_expr  *)

(** [solve lmi obj] solves the provided LMI trying to maximize the objective
    obj. It returns the solver return value plus a list with result, mapping each unknown variable to its
    value *)
  val solve: matrix_expr list -> lmi_obj_t -> float * (ident * (Mat.elt, Mat.t) value_t) list option
end

module Num : Sig with module Mat = Matrix.Num 

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
