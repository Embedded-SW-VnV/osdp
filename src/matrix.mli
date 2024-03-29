(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
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

(** Matrices and basic operations. *)

module type S = sig
  (** Type of coefficients. *)
  module Coeff : Scalar.S  (* TODO : renommer en Coeff *)

  (** Type of matrices. *)
  type t

  (** Most of the functions in this module can raise this exception. *)
  exception Dimension_error of string

  (** {2 Conversion functions.} *)

  (** [of_list_list l] returns the matrix with lines corresponding to
      the elements of the list [l]. All lists in [l] must have the
      same length. *)
  val of_list_list : Coeff.t list list -> t

  val to_list_list : t -> Coeff.t list list

  (** [of_array_array a] returns the matrix with lines corresponding
      to the elements of [a]. All arrays in [a] must have the same
      size. A copy of the array is made internally. *)
  val of_array_array : Coeff.t array array -> t

  (** The returned array is a copy that can be freely modified by the
      user. *)
  val to_array_array : t -> Coeff.t array array

  (** {2 Construction functions.} *)

  (** [zeros n m] builds a matrix of size n x m with all coefficients
      equal to [Coeff.of_int O]. [n] and [m] must be non negative. *)
  val zeros : int -> int -> t

  (** [eye n] builds the identity matrix of size n (i.e., a square
      matrix of size n with coefficients (i, j) equal to [Coeff.of_int
      O] when i != j and [Coeff.of_int 1] when i = j). [n] must be non
      negative. *)
  val eye : int -> t

  (** [kron n i j] builds the square matrix of size n with all coefficients
      equal to zero (i.e., [Coeff.of_int 0] except coefficients (i, j) which is
      one (i.e., [Coeff.of_int 1]). [n], [i] and [j] must satisfy 0 <= i < n and
      0 <= j < n. *)
  val kron : int -> int -> int -> t

  (** [kron_sym n i j] builds the square matrix of size n with
      all coefficients equal to zero (i.e., [Coeff.of_int 0] except
      coefficients (i, j) and (j, i) which are one (i.e., [Coeff.of_int
      1]). [n], [i] and [j] must satisfy 0 <= i < n and 0 <= j < n. *)
  val kron_sym : int -> int -> int -> t

  (** [block a] returns the block matrix corresponding to the array
      [a]. For instance [block \[|\[|a; b|\]; \[|c; d|\]|\]] builds
      the block matrix \[a, b; c, d\]. The array [a] must have a
      positive size. All arrays in array [a] must have the same
      positive size. Matrices dimensions must be consistent (for
      instance, in the previous example, [a] and [b] must have the
      same number of rows and [a] and [c] the same number of
      columns). *)
  val block : t array array -> t

  (** [lift_block m i j k l] returns a matrix of size i x j with zeros
      everywhere except starting from indices k x l where matrix m is
      copied. The parameters must satisfy 0 <= k, 0 <= l, k + nb_lines
      m <= i and l + nb_cols m <= j. *)
  val lift_block : t -> int -> int -> int -> int -> t

  (** {2 Matrix operations.} *)

  (** Matrix transposition. *)
  val transpose : t -> t

  (** Unary minus. *)
  val minus : t -> t

  (** [mult_scalar s m] multiplies matrix [m] by scalar [s]. *)
  val mult_scalar : Coeff.t -> t -> t

  (** Matrix addition. Both matrices must have the same size. *)
  val add : t -> t -> t

  (** Matrix subtraction. Both matrices must have the same size. *)
  val sub : t -> t -> t

  (** Matrix multiplication. First matrix must have as many columns as
      the second as rows. *)
  val mult : t -> t -> t

  (** [power m n] computes m^n, n must be non negative. *)
  val power : t -> int -> t

  (** {2 Various operations.} *)

  val nb_lines : t -> int
  val nb_cols : t -> int

  val is_symmetric : t -> bool

  (** Returns the same matrix, without its rows and columns containing
      only 0. *)
  val remove_0_row_cols : t -> t

  (** [gauss_split m] for a matrix [m] of size l x c returns [(n, m1,
      m2)] where [n] is the rank of the input matrix (n <= l), [m1]
      its row space, i.e., a matrix of size l' x c where l' <= l
      characterizing the same space as [m] but with no linear
      dependencies, and [m2] a matrix of size l' x l mapping original
      dimensions in [m] to the ones of [m1] (m1 = m2 x m). *)
  val gauss_split : t -> int * t * t

  (** {2 Prefix and infix operators.} *)

  (** To use this operators, it's convenient to use local opens. For
      instance to write the matrix operations m1 * m2 + I_3x3:

      {[let module M = Osdp.Matrix.Float in M.(m1 * m2 + eye 3)]} *)

  (** Same as {{:#VALtranspose}transpose}. *)
  val ( ~: ) : t -> t

  (** Same as {{:#VALminus}minus}. *)
  val ( ~- ) : t -> t

  (** Same as {{:#VALmult_scalar}mult_scalar}. *)
  val ( *. ) : Coeff.t -> t -> t

  (** Same as {{:#VALadd}add}. *)
  val ( + ) : t -> t -> t

  (** Same as {{:#VALsub}sub}. *)
  val ( - ) : t -> t -> t

  (** Same as {{:#VALmult}mult}. *)
  val ( * ) : t -> t -> t

  (** Same as {{:#VALpower}power}. *)
  val ( ** ) : t -> int -> t

  (** {2 Printing.} *)

  val pp : Format.formatter -> t -> unit
end

module Make (ET : Scalar.S) : S with module Coeff = ET

(** Matrix with Q.t coefficients from the library Zarith. *)
module Q : S with module Coeff = Scalar.Q

(** Matrix with float coefficients *)
module Float : S with module Coeff = Scalar.Float
