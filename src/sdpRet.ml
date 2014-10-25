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

(** Return codes for SDP. *)

type t =
  | Success
  | PartialSuccess  (** Problem solved with reduced accuracy. *)
  | PrimalInfeasible
  | DualInfeasible
  | NearPrimalInfeasible
  | NearDualInfeasible
  | MaxIterReached
  | LackOfProgress
  | Unknown

let is_success = function
  | Success
  | PartialSuccess -> true
  | PrimalInfeasible
  | DualInfeasible
  | NearPrimalInfeasible
  | NearDualInfeasible
  | MaxIterReached
  | LackOfProgress
  | Unknown -> false

let pp fmt t =
  Format.fprintf
    fmt
    (match t with
     | Success -> "Success"
     | PartialSuccess -> "PartialSuccess"
     | PrimalInfeasible -> "PrimalInfeasible"
     | DualInfeasible -> "DualInfeasible"
     | NearPrimalInfeasible -> "NearPrimalInfeasible"
     | NearDualInfeasible -> "NearDualInfeasible"
     | MaxIterReached -> "MaxIterReached"
     | LackOfProgress -> "LackOfProgress"
     | Unknown -> "Unknown")
