(** This module is an interface towards the C library CSDP
    (semidefinite programming optimization procedure).
    You may be interested in the slightly higher level interface Sdp.mli. *)

(** Matrices, line by line.
    Type invariant: all lines have the smae size. *)
type matrix = float array array

(** Block diagonal matrices. *)
type block_diag_matrix = matrix list

(** [solve obj constraints] solves the SDP problem:
    max{ tr(obj X) | tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd }
    with [\[(A_1, a_1);...; (A_n, a_n)\]] the [constraints] list.
    It returns both the maximum and a witness for X (primal) and y (dual).
    All block diagonal matrices in [obj] and [constraints] should have
    the same shape, i.e. same number of blocks and corresponding blocks
    of same size. The resulting block diagonal matrix X will also have
    this shape. *)
val solve : block_diag_matrix -> (block_diag_matrix * float) list ->
  float * (block_diag_matrix * float array)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
