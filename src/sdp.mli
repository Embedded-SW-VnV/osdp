(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {b Common interface for SDP} ({{:./Csdp.html}Csdp} or
    {{:./Moseksdp.html}Moseksdp}). *)

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

(** {2 Block diagonal dense and sparse symmetric matrices.} *)

(** Matrices, line by line. Must be symmetric.
    Type invariant: all lines have the same size. *)
type matrix = float array array

(** Matrices. Sparse representation as triplet [(i, j, x)] meaning
    that the coefficient at line [i] >= 0 and column [j] >= 0 has
    value [x]. All forgotten coefficients are assumed to be
    [0.0]. Since matrices are symmetric, only the lower triangular
    part (j <= i) must be given. No duplicates are allowed. *)
type sparse_matrix = (int * int * float) list

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. There is no requirement for indices to be
    sorted. *)
type 'a block_diag = (int * 'a) list

val matrix_of_sparse : sparse_matrix -> matrix
val matrix_to_sparse : matrix -> sparse_matrix

val block_diag_of_sparse : sparse_matrix block_diag -> matrix block_diag
val block_diag_to_sparse : matrix block_diag -> sparse_matrix block_diag

(** {2 SDP.} *)

type solver = Csdp | Mosek

(** Objective (matrix C). *)
type 'a obj = 'a block_diag

(** Constraints (tr(A_i X) = a_i), [Le (A_i, a_i)] will automatically
    be translated as tr(A_i X) + s_i = a_i by adding slack variables
    s_i >= 0. Similarly [Ge (A_i, a_i)] gives tr(A_i X) - s_i = a_i
    with s_i >= 0. *)
type 'a constr =
  | Eq of 'a block_diag * float
  | Le of 'a block_diag * float
  | Ge of 'a block_diag * float

(** [solve obj constraints] solves the SDP problem: max\{ tr(obj X) |
    tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd \} with [\[(A_1,
    a_1);...; (A_n, a_n)\]] the [constraints] list.  It returns both
    the primal and dual objective values and a witness for X (primal)
    and y (dual). See above for details. There is no requirement for
    indices in [obj] and [A_i] to be present in every input. However,
    if an indice is present in one input, potential matrices for that
    indice in other inputs will be padded with 0 on right and bottom
    to have the same size. There is no requirement for any indice to
    be present in any input, for instance there is no need for the
    indices 0 or 1 to appear. In case of success (or partial success),
    the block diagonal matrix returned for X contains exactly the
    indices that appear in the objective or one of the
    constraints. Size of each diagonal block in X is the maximum size
    appearing for that block in the objective or one of the
    constraints. In case of success (or partial success), the array
    returned for y has the same size and same order than the input
    list of constraints. *)
val solve : ?solver:solver -> matrix obj -> matrix constr list ->
            SdpRet.t * (float * float) * (matrix block_diag * float array)

(** Same as {!solve} with sparse matrices as input. This can be more
    efficient with solver handling sparse matrices (for instance
    Mosek). Otherwise, this is equivalent to {!solve} after conversion
    with {!matrix_of_sparse}. *)
val solve_sparse : ?solver:solver -> sparse_matrix obj ->
                   sparse_matrix constr list ->
                   SdpRet.t * (float * float) * (matrix block_diag * float array)
