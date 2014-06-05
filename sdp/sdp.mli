(** This module is an interface towards the C library CSDP
    (semidefinite programming optimization procedure).
    This is just a somewhat higher level interface to Csdp.mli. *)

(** Matrices, line by line.
    Type invariant: all lines have the same size. *)
type matrix = float array array

(** Block diagonal matrices (sparse representation, forgetting null blocks).
    For instance, \[(1, m1), (3, m2)\] will be transformed into [m1; 0; m2].
    There is no requirement for indices to be sorted. *)
type block_diag_matrix = (int * matrix) list

(** [solve obj constraints] solves the SDP problem:
    max{ tr(obj X) | tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd }
    with [\[(A_1, a_1);...; (A_n, a_n)\]] the [constraints] list.
    It returns both the maximum and a witness for X (primal) and y (dual).
    There is no requirement for indices in [obj] and [A_i] to be
    present in every input. However, if an indice is present in one input,
    potential matrices for that indice in other inputs will be padded with 0
    on right and bottom to have the same size.
    There is no requirement for any indice to be present in any input, for
    instance there is no need for the indices 0 or 1 to appear, the first
    block of the output will just be the first indice present in the input. *)
val solve : block_diag_matrix -> (block_diag_matrix * float) list ->
  float * (block_diag_matrix * float array)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
