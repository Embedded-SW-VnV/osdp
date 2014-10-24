(** Slightly higher level interface to the {!Csdp} or {!Moseksdp}
    module. *)

(** Primal-dual correspondence: the primal problem {[max tr(C X)
    tr(A_1 X) = a_1
    .
    .
    .
    tr(A_n X) = a_n
    X psd]}
    (X psd meaning X positive semi-definite)
    corresponds to the dual problem {[min a^T y
    \sum_i y_i A_i - C psd.]}
    C, A_i and a_i are parameters whereas X and y are variables. *)

type solver = Csdp | Mosek

(** Matrices, line by line. Must be symmetric.
    Type invariant: all lines have the same size. *)
type matrix = float array array

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. There is no requirement for indices to be
    sorted. *)
type block_diag_matrix = (int * matrix) list

(** Matrices. Sparse representation as triplet [(i, j, x)] meaning
    that the coefficient at line [i] and column [j] has value [x]. All
    forgotten coefficients are assumed to be [0.0]. Since matrices are
    symmetric, only the lower triangular part (j <= i) must be
    given. No duplicates are allowed. *)
type matrix_sparse = (int * int * float) list

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. There is no requirement for indices to be
    sorted. *)
type block_diag_matrix_sparse = (int * matrix_sparse) list

val matrix_of_sparse : matrix_sparse -> matrix
val matrix_to_sparse : matrix -> matrix_sparse

val block_diag_of_sparse : block_diag_matrix_sparse -> block_diag_matrix
val block_diag_to_sparse : block_diag_matrix -> block_diag_matrix_sparse

(** [solve obj constraints] solves the SDP problem: max\{ tr(obj X) |
    tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd \} with [\[(A_1,
    a_1);...; (A_n, a_n)\]] the [constraints] list.  It returns both
    the maximum and a witness for X (primal) and y (dual). See above
    for details. There is no requirement for indices in [obj] and
    [A_i] to be present in every input. However, if an indice is
    present in one input, potential matrices for that indice in other
    inputs will be padded with 0 on right and bottom to have the same
    size. There is no requirement for any indice to be present in any
    input, for instance there is no need for the indices 0 or 1 to
    appear, the first block of the output will just be the first
    indice present in the input. *)
val solve : ?solver:solver -> block_diag_matrix ->
            (block_diag_matrix * float) list ->
            SdpRet.t * (float * float) * (block_diag_matrix * float array)

(** Same as {!solve} with sparse matrices as input. *)
val solve_sparse : ?solver:solver -> block_diag_matrix_sparse ->
                   (block_diag_matrix_sparse * float) list ->
                   SdpRet.t * (float * float) * (block_diag_matrix * float array)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
