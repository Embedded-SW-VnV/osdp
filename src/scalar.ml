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

module type S = sig
  type t 
  val zero : t
  val one : t
  val is_zero : t -> bool
  val of_float : float -> t
  val to_float : t -> float
  val to_q : t -> Q.t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module Q : S with type t = Q.t = struct 
  type t = Q.t
  let zero = Q.zero
  let one = Q.one
  let is_zero n = Q.equal n zero
  let of_float = Q.of_float
  let to_float = Utils.float_of_q
  let to_q x = x
  let add = Q.add
  let sub = Q.sub
  let mult = Q.mul
  let div = Q.div
  let pp = Q.pp_print
end

module Float : S with type t = float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let is_zero f = f = 0.
  let of_float x = x
  let to_float x = x
  let to_q x = Q.of_float x
  let add = ( +. )
  let sub = ( -. )
  let mult = ( *. )
  let div = ( /. )
  let pp = Format.pp_print_float
end
