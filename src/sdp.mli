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

(** Slightly higher level interface to the {{:./Csdp.html}Csdp} or
    {{:./Moseksdp.html}Moseksdp} module. *)

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
    the primal and dual objective values and a witness for X (primal)
    and y (dual). See above for details. There is no requirement for
    indices in [obj] and [A_i] to be present in every input. However,
    if an indice is present in one input, potential matrices for that
    indice in other inputs will be padded with 0 on right and bottom
    to have the same size. There is no requirement for any indice to
    be present in any input, for instance there is no need for the
    indices 0 or 1 to appear, the first block of the output will just
    be the first indice present in the input. *)
val solve : ?solver:solver -> block_diag_matrix ->
            (block_diag_matrix * float) list ->
            SdpRet.t * (float * float) * (block_diag_matrix * float array)

(** Same as {!solve} with sparse matrices as input. This can be more
    efficient with solver handling sparse matrices (fo instance
    Mosek). Otherwise, this is equivalent to {!solve} after conversion
    with {!matrix_of_sparse}. *)
val solve_sparse : ?solver:solver -> block_diag_matrix_sparse ->
                   (block_diag_matrix_sparse * float) list ->
                   SdpRet.t * (float * float) * (block_diag_matrix * float array)
