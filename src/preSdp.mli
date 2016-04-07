(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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

(** Preprocessed SDP (interface to the module {{:./Sdp.html}Sdp}).

    See the module {{:./Sdp.html}Sdp} for details. This just call the
    corresponding function in {{:./Sdp.html}Sdp} after a preprocessing
    meant to eliminate redundant free variables. *)

module type S = sig
  (** The scalars used for preprocessing (the SDP solver uses floats anyway). *)
  module Scalar : Scalar.S

  (** C.f. {{:./Sdp.html#TYPEvector}Sdp.vector} *)
  type vector = (int * Scalar.t) list

  (** C.f. {{:./Sdp.html#TYPEobj_ext}Sdp.obj_ext} *)
  type 'a obj_ext = vector * 'a Sdp.block_diag

  (** C.f. {{:./Sdp.html#TYPEconstr_ext}Sdp.constr_ext} *)
  type 'a constr_ext = vector * 'a Sdp.block_diag * Scalar.t (* b *) * Scalar.t (* b *) * float (* padding (original bound was b + padding) *)

  (** See {{:./Sdp.html#VALsolve_ext_sparse}Sdp.solve_ext_sparse} for
      details. The only difference is that redundant free variables
      are handled before calling the SDP solver. That is, when a
      constraint of the form x_i + \sum_j x_j = 0 appears, the SDP
      solver is only asked about the variables x_j and the variable
      x_i will be recomputed from its answer as -\sum_j x_j. *)
  val solve_ext_sparse : ?options:Sdp.options -> ?solver:Sdp.solver ->
                         Sdp.sparse_matrix obj_ext ->
                         Sdp.sparse_matrix constr_ext list -> Sdp.bounds ->
                         float list ->
                         SdpRet.t * (float * float)
                         * (vector * Sdp.matrix Sdp.block_diag
                            * float array * Sdp.matrix Sdp.block_diag)
end

module Make (S : Scalar.S) : S with module Scalar = S

(** TODO: explain differences between these modules. *)

module Q : S with module Scalar = Scalar.Q

module Float : S with module Scalar = Scalar.Float
