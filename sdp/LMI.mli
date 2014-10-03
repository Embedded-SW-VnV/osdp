(** This module is an interface towards SDP solvers providing high level LMI
    constructs. It takes LMI problems, transform them as dual and primal
    problems and call SDP. *)

type objKind = Minimize | Maximize
type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

module Ident :
sig
  type t
  val create: string -> t
  val cmp: t -> t -> int
  val fprintf: Format.formatter -> t -> unit
end

type v_t = Var of Ident.t * int * int | ScalId of Ident.t 

module type Sig = 
sig
  module Mat: Matrix.S

  (* Unknown matrices are always symetrical *)
  type matrix_expr 
      
(** Objective to be maximized by the solver: None denotes no objective, Some
    (positive_sign, v) denotes the value v if positive_sign is true, -v
    otherwise. *)
  type lmi_obj_t = objKind * (v_t * Mat.elt) list

  type var = v_t

(** Printers for LMI *)
 val pp_matrix_expr : Format.formatter -> matrix_expr -> unit
 val pp_var : Format.formatter -> var -> unit
    
 (** Constructors *)
 val zeros: int -> int -> matrix_expr
 val eye: int -> matrix_expr
 val diag: matrix_expr list -> matrix_expr 
 val const_mult: Mat.elt -> matrix_expr -> matrix_expr
 val scal_mult: v_t -> matrix_expr -> matrix_expr
 val symmat: Ident.t * int ->  matrix_expr
 val const_mat: Mat.t -> matrix_expr
 val trans_const_mat: Mat.t -> matrix_expr
 val kronecker_sym: ?lift:(Mat.t -> Mat.t) -> int -> int -> int -> matrix_expr
 val add: matrix_expr -> matrix_expr -> matrix_expr 
 val sub: matrix_expr -> matrix_expr -> matrix_expr 
 val mult: matrix_expr -> matrix_expr -> matrix_expr
 val of_array_array: matrix_expr array array -> matrix_expr    
 val sym_mat_of_var: int -> Mat.elt list -> Mat.t
 val vars_of_sym_mat: Ident.t -> int -> var list 
  (* val rewrite_mat_as_scalar: matrix_expr -> matrix_expr  *)

(** [solve vars lmi obj] solves the provided LMI on variables vars trying to maximize the objective
    obj. It returns the solver return value plus a list with result, mapping each unknown variable to its
    value *)
  val solve: 
    v_t list list ->  (* list of variables *)
    matrix_expr list -> (* list of sdp matrix expression *)
    lmi_obj_t -> (* Objective *)
    float * (v_t (*Ident.t*) * (Mat.elt, Mat.t) value_t) list option 

  val get_var_id: var -> Ident.t
  val get_var_indices: var -> (int * int) option
end

module Num_mat : Sig with module Mat = Matrix.Num_mat

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
