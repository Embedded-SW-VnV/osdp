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

module type M = sig
  type t
  val compare : t -> t -> int
  val zero : t
  val one : t
  val of_float : float -> t
  val to_float : t -> float
  val to_q : t -> Q.t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  include M
  val minus_one : t
  val of_int : int -> t
  val neg : t -> t
  val inv : t -> t
  val equal : t -> t -> bool
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val sign : t -> int
  val ( ~- ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
end

module Make (M : M) = struct
  include M

  let minus_one = sub zero one

  let of_int n = of_float (float_of_int n)

  let neg x = sub zero x
  let inv x = div one x

  let equal x y = compare x y = 0
  let leq x y = compare x y <= 0
  let geq x y = compare x y >= 0
  let lt x y = compare x y < 0
  let gt x y = compare x y > 0

  let sign x =
    let c = compare x zero in
    if c < 0 then -1 else if c > 0 then 1 else 0

  let ( ~- ) = neg
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( / ) = div
  let ( = ) = equal
  let ( <> ) x y = not (equal x y)
  let ( <= ) = leq
  let ( >= ) = geq
  let ( < ) = lt
  let ( > ) = gt
end

module Q : S with type t = Q.t = Make (struct 
  type t = Q.t
  let compare = Q.compare
  let zero = Q.zero
  let one = Q.one
  let of_float = Q.of_float
  let to_float = Utils.float_of_q
  let to_q x = x
  let add = Q.add
  let sub = Q.sub
  let mult = Q.mul
  let div = Q.div
  let pp fmt q =
    Format.fprintf fmt "%a/%a" Z.pp_print q.Q.num Z.pp_print q.Q.den
    (* Format.fprintf fmt "\"%a\"/\"%a\"" Z.pp_print q.Q.num Z.pp_print q.Q.den *)
end)

module Float : S with type t = float = Make (struct
  type t = float
  let compare = compare
  let zero = 0.
  let one = 1.
  let of_float x = x
  let to_float x = x
  let to_q x = Q.of_float x
  let add = ( +. )
  let sub = ( -. )
  let mult = ( *. )
  let div = ( /. )
  let pp fmt f = Format.fprintf fmt "%s" (Posdef.string_of_float_bin f)
end)
