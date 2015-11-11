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

(** Proving positive definiteness of matrices. *)

(** Takes as input a square interval matrix [m] of size nxn. If it
    returns [true], then all symmetric matrices in this interval are
    positive definite (otherwise, one of these matrices is either not
    symmetric positive definite or its smallest eigenvalue is too
    small for the proof to succeed). If [m] is not symmetric or one of
    its elements is not finite, [false] is returned. *)
val check_itv : (float * float) array array -> bool

(** Takes as input a square matrix of Q.t of size nxn and returns 1 if it
    manages to prove that the matrix is symmetric positive definite and 0
    otherwise (i.e. the matrix is either not symmetric positive definite
    or its smallest eigenvalue is too small for the proof to succeed). *)
val check : Q.t array array -> bool

(** Same as [check] but complete (returns true iff the input is
    positive definite). This is however implemented with rational
    arithmetic and may be dramatically slower. *)
val check_complete : Q.t array array -> bool
