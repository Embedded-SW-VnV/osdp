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

(** Interface towards SDPA\{,-GMP\}.

    SDPA is a semidefinite programming optimization procedure. You may
    be interested in the slightly higher level interface
    {{:./Sdp.html}Sdp}. *)

(** See {{:./Sdp.html}Sdp} for definition of SDP with primal and dual. *)

(** Matrices. Sparse representation as triplet [(i, j, x)] meaning
    that the coefficient at line [i] >= 0 and column [j] >= 0 has
    value [x]. All forgotten coefficients are assumed to be
    [0.0]. Since matrices are symmetric, only the lower triangular
    part (j <= i) must be given. No duplicates are allowed. *)
type matrix = (int * int * float) list

(** Block diagonal matrices (sparse representation, forgetting null
    blocks). For instance, [\[(1, m1), (3, m2)\]] will be transformed
    into [\[m1; 0; m2\]]. No duplicates are allowed. There is no
    requirement for indices to be sorted. *)
type block_diag_matrix = (int * matrix) list

type solver = Sdpa | SdpaGmp | SdpaDd

(** Options for calling SDPA. *)
type options = {
  solver : solver;  (** default: Sdpa *)
  verbose : int;  (** verbosity level, non negative integer, 0 (default)
                      means no output *)
  max_iteration : int;  (** maxIteration (default: 100) *)
  stop_criterion : float;  (** epsilonStar and epsilonDash (default: 1.0E-7) *)
  initial : float;  (** lambdaStar (default: 1.0E2) *)
  precision : int  (** precision (only for SDPA-GMP, default: 200) *)
}

(** Default values above. *)
val default : options

(** [solve obj constraints] solves the SDP problem: max\{ tr(obj X) |
    tr(A_1 X) = a_1,..., tr(A_n X) = a_n, X psd \} with [\[(A_1,
    a_1);...; (A_n, a_n)\]] the [constraints] list. It returns both
    the primal and dual objective values and a witness for X (primal)
    and y and Z (dual, see {{:./Sdp.html}Sdp}). In case of success (or
    partial success), the block diagonal matrices returned for X and Z
    contain exactly the indices, sorted by increasing order, that
    appear in the objective or one of the constraints. Size of each
    diagonal block in X or Z is the maximum size appearing for that
    block in the objective or one of the constraints. In case of
    success (or partial success), the array returned for y has the
    same size and same order than the input list of constraints. *)
val solve : ?options:options ->
            ?init:((int * float array array) list
                   * float array * (int * float array array) list) ->
            block_diag_matrix -> (block_diag_matrix * float) list ->
            SdpRet.t * (float * float)
            * ((int * float array array) list
               * float array * (int * float array array) list)
