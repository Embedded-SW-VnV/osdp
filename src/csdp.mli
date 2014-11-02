(** Interface towards the C library CSDP.

    CSDP is a semidefinite programming optimization procedure. You may
    be interested in the slightly higher level interface
    {{:./Sdp.html}Sdp}. *)

(** See {{:./Sdp.html}Sdp} for definition of SDP with primal and dual. *)

(** Matrices, line by line. Must be symmetric.
    Type invariant: all lines have the same size. *)
type matrix = float array array

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. There is no requirement for indices to be
    sorted. *)
type block_diag_matrix = (int * matrix) list

(** [solve obj constraints] solves the SDP problem: max\{ tr(obj X) |
    tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd \} with [\[(A_1,
    a_1);...; (A_n, a_n)\]] the [constraints] list. It returns both
    the primal and dual objective values and a witness for X (primal)
    and y (dual, see {{:./Sdp.html}Sdp}). The block diagonal matrix
    returned for X contains exactly the indices that appear in the
    objective or one of the constraints. Size of each diagonal block
    in X is the maximum size appearing for that block in the objective
    or one of the constraints. The array returned for y has the same
    size and same order than the input list of constraints. *)
val solve : block_diag_matrix -> (block_diag_matrix * float) list ->
            SdpRet.t * (float * float) * (block_diag_matrix * float array)
