(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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

(** {b Polynomial basis abstraction for SOS decomposition},
    parameterised by a scalar coefficient type.

    Three-layer architecture:
    {ol
      {li Algebraic layer ([sos.ml]): user-facing polynomial expressions and
          SOS constraints — independent of the chosen basis.}
      {li Polynomial representation layer (this module): abstract {!BASIS}
          interface, parameterised by a scalar type [Coeff : Scalar.S], with
          concrete {!Classical}, {!Hermite} and {!HermiteNormalized}
          implementations (and future orthogonal families).}
      {li SOS-to-SDP encoding layer ([sos.ml]): converts SOS constraints
          into affine SDP constraints.  Instantiates {!Registry} with the
          polynomial's coefficient type, then converts to [float] at the SDP
          boundary via [Coeff.to_float].}}

    The module originates from [sos/monomials.ml] (branch [old_hermite],
    2014). *)


(** {2 Module types} *)

module type MONOMIAL = sig
  type t
  val compare : t -> t -> int
  val pp : string list -> Format.formatter -> t -> unit
end

(** Linear expressions over a monomial type with exact rational coefficients.
    Used by {!BASIS.prod} to express products in the same basis. *)
module type LINEXPR = sig
  module M : MONOMIAL
  (* sorted by M.compare; no zero-coefficient entries *)
  type t = (Q.t * M.t) list
  val zero      : t
  val inject    : M.t -> t
  val add       : t -> t -> t
  val scal_mult : Q.t -> t -> t
  val pp        : string list -> Format.formatter -> t -> unit
end

(** Abstract polynomial basis for SOS decomposition, parameterised by
    [Coeff : Scalar.S].

    [prod b1 b2] returns the product in the *same* basis with exact rational
    coefficients (via {!LINEXPR}).  [prod_to_classical] and [to_classical]
    return the classical expansion with [Coeff.t] coefficients: for
    {!Classical} this is [Coeff.one]; for {!Hermite} the exact rational
    coefficients are computed internally then converted via [Coeff.of_q]. *)
module type BASIS = sig
  (** Scalar type for the classical-polynomial expansion. *)
  module Coeff : Scalar.S

  type t
  val compare : t -> t -> int
  val pp      : string list -> Format.formatter -> t -> unit

  (** Linear expressions over this basis (exact [Q.t] coefficients). *)
  module LE   : LINEXPR with type M.t = t

  val dim          : t -> int
  val deg          : t -> int
  val list_le      : int -> int -> t list
  val list_eq      : int -> int -> t list
  val nth          : int -> int -> t
  val get_sos_size : int -> int -> int

  (** Product in the same basis, always exact rational. *)
  val prod : t -> t -> LE.t

  (** Product as a classical polynomial with [Coeff.t] coefficients. *)
  val prod_to_classical : t -> t -> (Monomial.t * Coeff.t) list

  (** Single basis element as a classical polynomial with [Coeff.t]
      coefficients. *)
  val to_classical : t -> (Monomial.t * Coeff.t) list

  (** [refine_basis elems classical_support] returns the subset of [elems]
      that may contribute to the monomials in [classical_support].
      For bases where no filter is known, returns [elems] unchanged. *)
  val refine_basis : t list -> Monomial.t list -> t list
end


(** {2 LinExpr functor} *)

module LinExpr (M : MONOMIAL) : LINEXPR with type M.t = M.t


(** {2 Concrete bases} *)

(** Classical (power-product) polynomial basis, parameterised by scalar type.

    Basis element [[a0; a1; ...; a_{k-1}]] represents [x_0^{a0} * ...].
    Product rule: [prod b1 b2 = [(1, b1*b2)]].
    [to_classical b = [(b, Coeff.one)]] and
    [prod_to_classical b1 b2 = [(b1*b2, Coeff.one)]].

    [refine_basis] delegates to the Newton polytope filter. *)
module Classical (S : Scalar.S) : BASIS with module Coeff = S
                                         and type t = Monomial.t

(** Probabilist's (monic) Hermite polynomial basis, parameterised by
    scalar type.

    [He_0 = 1], [He_1 = x], [He_{n+1} = x He_n - n He_{n-1}]: leading
    coefficient is always [1] (unlike the physicist's [H_n], whose leading
    coefficient is [2^n]). Basis element [[a0; ...; a_{k-1}]] represents
    [He_{a0}(x_0) * ... * He_{a_{k-1}}(x_{k-1})].

    [prod] uses the exact product formula with [Q.t] rationals.
    [to_classical] and [prod_to_classical] compute internally in [Q.t]
    and convert each coefficient via [S.of_q]. *)
module Hermite (S : Scalar.S) : BASIS with module Coeff = S
                                       and type t = Monomial.t

(** Orthonormal rescaling of {!Hermite}: same monic [He_n] family, but
    [to_classical]/[prod_to_classical] are rescaled so that
    [E_gamma[He_n^2] = 1] under the standard normal measure [gamma]
    (since [E_gamma[He_m He_n] = n! delta_mn] for monic [He_n], the
    rescaling factor is [1 / sqrt(a0! * ... * a_{k-1}!)] for basis index
    [[a0; ...; a_{k-1}]]).

    These constants are generically irrational, so unlike {!Hermite} this
    cannot be computed exactly in [Q.t]: the rescaling is applied directly
    in [float] and lifted into [Coeff.t] via [S.of_float]. Meaningful
    mostly for [S = Scalar.Float]; instantiating with [S = Scalar.Q] still
    type-checks but only yields a float-precision approximation of the
    normalization, not an exact rational. [prod]/[LE] are inherited
    unchanged (unnormalized) from {!Hermite}, since they are basis-index
    bookkeeping never consumed by the SOS-to-SDP encoding in [sos.ml]. *)
module HermiteNormalized (S : Scalar.S) : BASIS with module Coeff = S
                                                  and type t = Monomial.t


(** {2 Named basis selection} *)

(** Runtime selector passed via [options.basis] in [sos.ml]. *)
type basis_choice = Classical | Hermite | HermiteNormalized

(** [Registry(S)] packages the dispatch functions for scalar type [S].
    Instantiated in [sos.ml] with [P.Coeff] so that all arithmetic stays in
    the polynomial's scalar ring up to the SDP boundary. *)
module Registry (S : Scalar.S) : sig
  val prod_to_classical_fn :
    basis_choice -> Monomial.t -> Monomial.t -> (Monomial.t * S.t) list

  val list_fns :
    basis_choice ->
    (int -> int -> Monomial.t list) * (int -> int -> Monomial.t list)

  val refine_basis_fn :
    basis_choice -> Monomial.t list -> Monomial.t list -> Monomial.t list
end
