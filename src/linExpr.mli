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

(** Affine expressions. *)

module type S = sig
  (** Type of coefficients. *)
  module  Coeff : Scalar.S

  (** Type of affine expressions. *)
  type t

  (** {2 Conversion functions.} *)

  (** [of_list \[(x_1, a_1);..; (x_n, a_n)\] c] builds the affine
      expression a_1 x_1 + ... + a_n x_n + c. Duplicates or zeros
      coefficients are accepted (for instance 2 x_0 + 0 x_1 + 3 x_0 is
      a valid input for 5 x_0). *)
  val of_list : (Ident.t * Coeff.t) list -> Coeff.t -> t

  (** Returns a list sorted in increasing order of
      {{:./Ident.html#VALcompare}Ident.compare} without duplicates nor
      zeros. *)
  val to_list : t -> (Ident.t * Coeff.t) list * Coeff.t

  (** {2 A few values.} *)

  (** Same as {!of_list} with the list [\[ident, Coeff.one\]] and
      constant [Coeff.zero]. *)
  val var : Ident.t -> t

  (** Same as {!of_list} with an empty list. *)
  val const : Coeff.t -> t

  (** {2 Arithmetic operations.} *)
                                         
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t

  (** [compose le \[v_1, l_1;...; v_n, l_n\]] replaces each variable
      [v_i] by [l_i] in [l]. No duplicates are allowed. *)
  val replace : t -> (Ident.t * t) list -> t

  val remove : t -> Ident.t -> t
  
  (** {2 Various operations.} *)

  val compare : t -> t -> int
                                         
  val is_var : t -> (Ident.t * Coeff.t) option

  val is_const : t -> Coeff.t option

  (** Returns one of the (non zero) coefficients in the linear
      expression (if any). *)
  val choose : t -> (Ident.t * Coeff.t) option
  
  (** {2 Printing.} *)
                                         
  val pp : Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC

module Q : S with module Coeff = Scalar.Q

module Float : S with module Coeff = Scalar.Float

exception Not_linear

(** Gives an interface {{:./Scalar.S.html}Scalar.S} to linear
    expressions. Since {{:./Scalar.S.html#VALto_float}to_float} and
    {{:./Scalar.S.html#VALdiv}div} have no meaning for linear
    expressions, they are implemented as [assert false]. You should
    never use them. Simialr behavior for the moment for comparison
    functions. Moreover, the product of two linear expressions is a
    linear expression only when at least one of them is a
    constant. {!Not_linear} is raised otherwise. *)
module MakeScalar (L : S) : Scalar.S with type t = L.t
