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

(** {b Common interface for SDP} ({{:./Csdp.html}Csdp} or
    {{:./Moseksdp.html}Moseksdp} or {{:./Sdpa.html}Sdpa}). *)

(** Primal-dual correspondence: the primal problem {[max tr(C X)
    tr(A_1 X) = b_1
    .
    .
    .
    tr(A_m X) = b_m
    X psd]}
    (X psd meaning X positive semi-definite)
    corresponds to the dual problem {[min b^T y
    \sum_i y_i A_i - C = Z psd]}
    where C, A_i and X are symmetric matrices, C, A_i and b_i are
    parameters whereas X (resp. y) is the primal (resp. dual)
    variable. *)

(** {2 Block diagonal sparse and dense symmetric matrices.} *)

(** Matrices. Sparse representation as triplet [(i, j, x)] meaning
    that the coefficient at line [i] >= 0 and column [j] >= 0 has
    value [x]. All forgotten coefficients are assumed to be
    [0.0]. Since matrices are symmetric, only the lower triangular
    part (j <= i) must be given. No duplicates are allowed. *)
type sparse_matrix = (int * int * float) list

(** Matrices. Dense representation, line by line. Must be symmetric.
    Type invariant: all lines have the same size. *)
type matrix = float array array

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. No duplicates are allowed. There is no
    requirement for indices to be sorted. *)
type 'a block_diag = (int * 'a) list

val matrix_of_sparse : sparse_matrix -> matrix
val matrix_to_sparse : matrix -> sparse_matrix

val block_diag_of_sparse : sparse_matrix block_diag -> matrix block_diag
val block_diag_to_sparse : matrix block_diag -> sparse_matrix block_diag

(** {2 SDP.} *)

type solver = Csdp | Mosek | Sdpa | SdpaGmp | SdpaDd

(** Options for calling SDP solvers (currently, options other than
    solver are only handled by Sdpa\{,Gmp\}). *)
type options = {
  solver : solver;  (** default: Csdp *)
  verbose : int;  (** verbosity level, non negative integer, 0 (default)
                      means no output (only for SDPA* and Mosek) *)
  max_iteration : int;  (** maxIteration (default: 100) *)
  stop_criterion : float;  (** epsilonStar and epsilonDash (default: 1.0E-7) *)
  initial : float;  (** lambdaStar (default: 1.0E2) *)
  precision : int  (** precision (only for SDPA-GMP, default: 200) *)
}

(** Default values above. *)
val default : options

(** Objective (matrix C). *)
type 'a obj = 'a block_diag

(** Constraints (tr(A_i X) = b_i), [Le (A_i, b_i)] means tr(A_i X) <=
    b_i and will automatically be translated internally as tr(A_i X) +
    s_i = b_i by adding slack variables s_i >= 0. Similarly [Ge (A_i,
    b_i)] means tr(A_i X) >= b_i and gives tr(A_i X) - s_i = b_i with
    s_i >= 0. *)
type 'a constr =
  | Eq of 'a block_diag * float
  | Le of 'a block_diag * float
  | Ge of 'a block_diag * float

(** [solve_sparse obj constraints] solves the SDP problem: max\{
    tr(obj X) | constraints, X psd \}. If [solver] is provided, it
    will supersed the solver given in [options]. It returns both the
    primal and dual objective values and a witness for X (primal) and
    y and Z (dual). See above for details. There is no requirement for
    indices in [obj] and [A_i] to be present in every input. In case
    of success (or partial success), the block diagonal matrices
    returned for X and Z contain exactly the indices, sorted by
    increasing order, that appear in the objective or one of the
    constraints. Size of each diagonal block in X is the maximum size
    appearing for that block in the objective or one of the
    constraints. In case of success (or partial success), the array
    returned for y has the same size and same order than the input
    list of constraints.

    @raise Invalid_argument "non symmetric matrix" in case the
    objective or one of the constraints is non symmetric. *)
val solve_sparse : ?options:options -> ?solver:solver ->
                   ?init:(matrix block_diag
                          * float array * matrix block_diag) ->
                   sparse_matrix obj -> sparse_matrix constr list ->
                   SdpRet.t * (float * float)
                   * (matrix block_diag * float array * matrix block_diag)

(** Same as {!solve_sparse} with dense matrices as input. This can be
    more convenient for small problems. Otherwise, this is equivalent
    to {!solve_sparse} after conversion with {!matrix_to_sparse}. If
    an indice is present in one input, potential matrices for that
    indice in other inputs will be padded with 0 on right and bottom
    to have the same size.

    @raise Invalid_argument "non symmetric matrix" in case the
    objective or one of the constraints is not a sparse matrix. *)
val solve : ?options:options -> ?solver:solver ->
            ?init:(matrix block_diag * float array * matrix block_diag) ->
            matrix obj -> matrix constr list ->
            SdpRet.t * (float * float)
            * (matrix block_diag * float array * matrix block_diag)

(** {2 Extended formulation.} *)
                                                  
(** Primal-dual correspondence: the primal problem {[max c^T x + tr(C X)
    b_1^- <= a_1^T x + tr(A_1 X) <= b_1^+
    .
    .
    .
    b_m^- <= a_m^T x + tr(A_m X) <= b_m^+
    d_1^- <= x_1 <= d_1^+
    .
    .
    .
    d_n^- <= x_n <= d_n^+
    X psd]}
    (X psd meaning X positive semi-definite)
    corresponds to the dual problem {[min (b^+)^T s^+ - (b^-)^T s^- + (d^+)^T t^+ - (d^-)^T t^-
    y = s^+ - s^-
    \sum_i y_i A_i - C = Z psd
    [a_1,..., a_n] y - c + t^+ - t^- = 0
    s^+, s^-, t^+, t^- >= 0]}
    where C, A_i and X are symmetric matrices, C, A_i, a_i, b_i and d
    are parameters whereas x, X, y, s^+, s^-, t^+ and t^- are
    variables.

    Note that the simple formulation on top is a particular case of
    this for n = 0 and b^- = b^+.

    The bounds b_i^- and d_j^- (resp. b_i^+ and d_j^+) can be
    [neg_infinity] (resp. [infinity]), in which case, the
    corresponding s or t in the dual is 0 (and the corresponding term
    disappears from the dual objective). *)

(** No duplicates are allowed. *)
type vector = (int * float) list

(** Objective (vector c and matrix C). *)
type 'a obj_ext = vector * 'a block_diag

(** Constraints. [(a_i, A_i, b_i_m, b_i_p)] encodes the constraint
    b_i_m <= a_i^T x + tr(A_i X) <= b_i_p. [b_i_m] can be
    [neg_infinity] and [b_i_p] can be [infinity]. *)
type 'a constr_ext = vector * 'a block_diag * float * float

(** Bounds on scalar variables. Each [(i, lb, ub)] in the list encodes
    the constraint lb <= x_i <= ub. [lb] can be [neg_infinity] and
    [ub] can be [infinity]. No duplicated indices i are allowed. *)
type bounds = (int * float * float) list

(** [solve_ext_sparse obj constraints bounds] solves the SDP problem:
    max\{ obj | constraints, bounds, X psd \}. If [solver] is
    provided, it will supersed the solver given in [options]. It
    returns both the primal and dual objective values and a witness
    for (x, X) (primal) and (y, Z) (dual). See above for
    details. Variables x_i not bounded in [bounds] are assumed to be
    unbounded (i.e., bounded by [neg_infinity] and [infinity]). Bounds
    in [bounds] about variables x_i not appearing in the objective or
    constraints may be ignored. There is no requirement for indices in
    [obj] and [a_i, A_i] to be present in every input. In case of
    success (or partial success), the vector (resp. block diagonal
    matrix) returned for x (resp. X, Z) contains exactly the indices,
    sorted by increasing order, that appear in the linear
    (resp. matrix) part of the objective or one of the
    constraints. Size of each diagonal block in X or Z is the maximum
    size appearing for that block in the objective or one of the
    constraints. In case of success (or partial success), the array
    returned for y has the same size and same order than the input
    list of constraints.

    @raise Invalid_argument "non symmetric matrix" in case the
    objective or one of the constraints is non symmetric. *)
val solve_ext_sparse : ?options:options -> ?solver:solver ->
                       sparse_matrix obj_ext ->
                       sparse_matrix constr_ext list -> bounds ->
                       SdpRet.t * (float * float)
                       * (vector * matrix block_diag
                          * float array * matrix block_diag)

(** Same as {!solve_ext_sparse} with dense matrices as input. This can
    be more convenient for small problems. Otherwise, this is
    equivalent to {!solve_ext_sparse} after conversion with
    {!matrix_to_sparse}. If an indice is present in one input,
    potential matrices for that indice in other inputs will be padded
    with 0 on right and bottom to have the same size.

    @raise Invalid_argument "non symmetric matrix" in case the
    objective or one of the constraints is not a sparse matrix. *)
val solve_ext : ?options:options -> ?solver:solver -> matrix obj_ext ->
                matrix constr_ext list -> bounds ->
                SdpRet.t * (float * float)
                * (vector * matrix block_diag * float array * matrix block_diag)

(** {2 Printing functions.} *)

val pp_sparse_matrix : Format.formatter -> sparse_matrix -> unit

val pp_matrix : Format.formatter -> matrix -> unit

val pp_block_diag : (Format.formatter -> 'a -> unit) ->
                    Format.formatter -> 'a block_diag -> unit

val pp_obj : (Format.formatter -> 'a -> unit) ->
             Format.formatter -> 'a obj -> unit

val pp_constr : (Format.formatter -> 'a -> unit) ->
                Format.formatter -> 'a constr -> unit

val pp_sparse : Format.formatter ->
                (sparse_matrix obj * sparse_matrix constr list) -> unit

val pp : Format.formatter -> (matrix obj * matrix constr list) -> unit

val pp_vector : Format.formatter -> vector -> unit

val pp_obj_ext : (Format.formatter -> 'a -> unit) ->
                 Format.formatter -> 'a obj_ext -> unit

val pp_constr_ext : (Format.formatter -> 'a -> unit) ->
                    Format.formatter -> 'a constr_ext -> unit

val pp_bounds :  Format.formatter -> bounds -> unit

val pp_ext_sparse : Format.formatter ->
                    (sparse_matrix obj_ext *
                     sparse_matrix constr_ext list * bounds) -> unit

val pp_ext : Format.formatter ->
             (matrix obj_ext * matrix constr_ext list * bounds) -> unit

val pp_ext_sparse_sedumi : Format.formatter ->
                           (sparse_matrix obj_ext *
                              sparse_matrix constr_ext list * bounds) -> unit

val pp_ext_sedumi : Format.formatter ->
                    (matrix obj_ext * matrix constr_ext list * bounds) -> unit

(** {2 Miscellaneous functions.} *)

(** [pfeas_stop_crit (b_1,..., b_m)] returns [eps] used as primal
    feasibility error stopping criteria by solver [solver] on a
    problem with given [b_i] (i.e., if the solver successfully
    terminates, |tr(A_i X) - b_i| <= eps should hold for each
    constraint i). *)
val pfeas_stop_crit : ?options:options -> ?solver:solver -> float list -> float
