(** Interface towards the C library of Mosek for SDP.

    Mosek SDP is a semidefinite programming optimization
    procedure. You may be interested in the slightly higher level
    interface {!Sdp}. *)

(** See {!Sdp} for definition of SDP with primal and dual. *)

(** Matrices. Sparse representation as triplet [(i, j, x)] meaning
    that the coefficient at line [i] and column [j] has value [x]. All
    forgotten coefficients are assumed to be [0.0]. Since matrices are
    symmetric, only the lower triangular part (j <= i) must be
    given. No duplicates are allowed. *)
type matrix = (int * int * float) list

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. There is no requirement for indices to be
    sorted. *)
type block_diag_matrix = (int * matrix) list

(** [solve obj constraints] solves the SDP problem: max\{ tr(obj X) |
    tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd \} with [\[(A_1,
    a_1);...; (A_n, a_n)\]] the [constraints] list.  It returns both
    the maximum and a witness for X (primal) and y (dual, see
    {!Sdp}). *)
val solve : block_diag_matrix -> (block_diag_matrix * float) list ->
            SdpRet.t * (float * float) * (float array array list * float array)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
