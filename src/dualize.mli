(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2017  P. Roux
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

(** TODO: doc *)

(** TODO: factorize defs and printing with PreSdp and Sdp *)

(** Preprocessed SDP (interface to the module {{:./Sdp.html}Sdp}).

    See the module {{:./Sdp.html}Sdp} for details. This just call the
    corresponding function in {{:./Sdp.html}Sdp} after a preprocessing
    meant to eliminate redundant free variables. *)

module type S = sig
  (** The scalars used for preprocessing (the SDP solver uses floats anyway). *)
  module Scalar : Scalar.S

  (** {2 Extended formulation.} *)

  (** C.f. {{:./Sdp.html#TYPEvector}Sdp.vector} *)
  type vector = (int * Scalar.t) list

  (** C.f. {{:./Sdp.html#TYPEobj_ext}Sdp.obj_ext} *)
  type 'a obj_ext = vector * 'a Sdp.block_diag

  (** C.f. {{:./Sdp.html#TYPEconstr_ext}Sdp.constr_ext} *)
  type 'a constr_ext = vector * 'a Sdp.block_diag * Scalar.t * Scalar.t

  (** See {{:./Sdp.html#VALsolve_ext_sparse}Sdp.solve_ext_sparse} for
      details. TODO : explain, explain we return only primal solution *)
  val solve_ext_sparse : ?options:Sdp.options -> ?solver:Sdp.solver ->
                         Sdp.sparse_matrix obj_ext ->
                         Sdp.sparse_matrix constr_ext list -> Sdp.bounds ->
                         SdpRet.t * (float * float)
                         * (vector * Sdp.matrix Sdp.block_diag)

  (** TODO: doc *)
  module ScalarLinExpr : LinExpr.S with module Coeff = Scalar

  type 'a details_val = DV of 'a | DVexpr of ScalarLinExpr.t

  type details =
    (int * Ident.t) list * Ident.t array array Sdp.block_diag
    * float details_val Ident.Map.t

  val solve_ext_sparse_details :
    ?options:Sdp.options -> ?solver:Sdp.solver ->
    Sdp.sparse_matrix obj_ext ->
    Sdp.sparse_matrix constr_ext list -> Sdp.bounds ->
    SdpRet.t * (float * float)
    * (vector * Sdp.matrix Sdp.block_diag)
    * details

  (** {2 Printing functions.} *)

  val pp_obj_ext : (Format.formatter -> 'a -> unit) ->
                   Format.formatter -> 'a obj_ext -> unit

  val pp_constr_ext : (Format.formatter -> 'a -> unit) ->
                      Format.formatter -> 'a constr_ext -> unit

  val pp_ext_sparse : Format.formatter ->
                      (Sdp.sparse_matrix obj_ext *
                       Sdp.sparse_matrix constr_ext list * Sdp.bounds) -> unit
end

module Make (S : Scalar.S) : S with module Scalar = S

(** TODO: explain differences between these modules. *)

module Q : S with module Scalar = Scalar.Q

module Float : S with module Scalar = Scalar.Float
