module type ElemType = sig 
  type t 
  val of_int: int -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  val div: t -> t -> t
  val sign: t -> int
  val pp: Format.formatter -> t -> unit
  val to_float: t -> float
  val of_float: float -> t
  val compare: t -> t -> int
  val eq: t -> t -> bool
  val to_string: t -> string
end

module type S = sig
  module Elem: ElemType
  type elt = Elem.t
  type t

  exception Matrix_dimension_error 
    
  val matrix_of_list_list : elt list list -> t
  val matrix_to_list_list : t -> elt list list
    
  val matrix_of_array_array : elt array array -> t
  val matrix_to_array_array : t -> elt array array
    
  (* block A B C D = [A, B; C, D] *)
  val block_matrix : t -> t -> t -> t -> t
    
  val zeros_matrix : int -> int -> t
    
  val ident_matrix : int -> t
  val kronecker_sym_matrix: int -> int -> int -> t

  val ( ~:. ) : t -> t
  val transpose_matrix : t -> t
    
  (* uminus A = -A *)
  val ( ~: ) : t -> t
  val minus_matrix : t -> t
    
  val ( */: ) : elt -> t -> t
  val mult_scalar_matrix : elt -> t -> t

  val ( +: ) : t -> t -> t
  val add_matrix : t -> t -> t
    
  val ( -: ) : t -> t -> t
  val sub_matrix : t -> t -> t
    
  val ( *: ) : t -> t -> t
  val mult_matrix : t -> t -> t
    
(* returns the same matrix, without its rows and columns containing only 0 *)
  val matrix_remove_0_row_cols : t -> t

  val copy: t -> t
  val transpose: t -> t

  val pp_matrix : Format.formatter -> t -> unit 
  val pp_elem : Format.formatter -> elt -> unit
  val nb_lines: t -> int
  val nb_cols: t -> int

  (** gauss_split m for a matrix m of size l x c returns (n, m1, m2) where n is
      the rank of the input matrix (n <= l), m1 its row space, ie. a matrix of
      size l' x c where l' <= l characterizing the same space as m but with no
      linear dependencies, and m2 a matrix of size l x l' mapping original
      dimensions in m to the ones of m1 *)
  val gauss_split: t -> int * t * t

  val lift_block: t -> int -> int -> int -> int -> t
end

module Make : functor (ET : ElemType) -> S (* with type elt = ET.t *)

(* Matrix with Num.num coefficients *)
module Num: S with type Elem.t = Num.num (* elt = Num.num *)

(* Matrix with float coefficients *)
module Float: S with type Elem.t = float (*elt = float *)

val num_of_string: string -> Num.elt

(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
