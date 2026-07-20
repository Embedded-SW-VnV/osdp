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

(** Newton polytope optimization. *)

(** [filter s p] returns the list of the s_i in [s] such that 2s_i is
    in the convex hull of the p_1,..., p_n in [p]. The returned list
    is sorted according to {!val:Monomial.compare}. C.f., Johan
    Löfberg, Pre- and Post-Processing Sum-of-Squares Programs in
    Practice, IEEE Transactions on Automatic Control, 2009. *)
val filter : Monomial.t list -> Monomial.t list -> Monomial.t list
