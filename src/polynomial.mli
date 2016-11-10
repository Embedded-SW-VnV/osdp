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

(** Multiple variables polynomials. *)

module type S = sig
  (** Type of coefficients. *)
  module  Coeff : Scalar.S

  (** Type of polynomials. *)
  type t

  (** {2 Conversion functions.} *)

  (** [of_list \[(m_1, a_1);..; (m_n, a_n)\]] builds the polynomial
      a_1 m_1 + ... + a_n m_n. Duplicates or zeros coefficients are
      accepted (for instance 2 x_0 + 0 x_1 + 3 x_0 is a valid input
      for 5 x_0). *)
  val of_list : (Monomial.t * Coeff.t) list -> t

  (** Returns a list sorted in increasing order of
      {{:./Monomial.html#VALcompare}Monomial.compare} without
      duplicates nor zeros. *)
  val to_list : t -> (Monomial.t * Coeff.t) list

  (** {2 A few values.} *)

  val zero : t
  val one : t

  (** [var ?c ?d i] returns c xi^d, this is equivalent to [of_list [c,
      Monomial.var ~d i]]. [c] is [Coeff.one] and [d] is [1] by
      default. [i] and [d] must be non negative. *)
  val var : ?c:Coeff.t -> ?d:int -> int -> t
 
  (** [const c] is equivalent to [var ~c ~d:0 0]. *)
  val const : Coeff.t -> t
                                             
  (** [monomial m] is equivalent to [of_list [m, Coeff.one]]. *)
  val monomial : Monomial.t -> t
                                             
  (** {2 Arithmetic operations.} *)
                                         
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t

  (** [power p n] computes p^n, [n] must be non negative. *)
  val power : t -> int -> t

  exception Dimension_error

  (** [compose p \[q_1;...; q_n\]] returns the polynomial p' such that
      p'(x_1,..., x_m) = p(q_1(x_1,..., x_m),..., q_n(x_1,...,
      x_m)).

      @raise Dimension_error if [p] is defined on more than n
      variables. *)
  val compose : t -> t list -> t

  (** [derive p i] returns the derivative of the polynomial p with
      respect to variable [i] (indices starting from 0). [i] must be
      non negative. *)
  val derive : t -> int -> t

  (** [eval p \[x_1;...; x_n\]] returns p(x_1,..., x_n).

      @raise Invalid_argument "Polynomial.eval" if [n] is less than
      [nb_vars p]. *)
  val eval : t -> Coeff.t list -> Coeff.t

  (** {2 Various functions.} *)

  val compare : t -> t -> int

  (** [nb_vars p] returns the largest index (starting from 0) of a
      variable appearing in [p] plus one (0 if none). For instance,
      [nb_var p] returns 5 if [p] is the polynomial x_2^2 + x_4 (even if
      variables x_0, x_1 and x_3 don't actually appear in [p]) *)
  val nb_vars : t -> int

  (** -1 for the null polynomial. *)
  val degree : t -> int

  val is_homogeneous : t -> bool

  (** [is_var p] returns [Some (c, d, i)] if [p] is a polynomial of
      the form c xi^d and [None] otherwise. *)
  val is_var : t -> (Coeff.t * int * int) option
                              
  (** [is_const p] returns [Some c] if [p] is the constant polynomial
      c and [None] otherwise. *)
  val is_const : t -> Coeff.t option
                              
  (** [is_monomial p] returns [Some m] if [p] is a monmial x_0^i_0
      ... x_n^i_n and [None] otherwise. *)
  val is_monomial : t -> Monomial.t option
                              
  (** {2 Prefix and infix operators.} *)

  (** To use this operators, it's convenient to use local opens. For
      instance to write the polynomial 2.3 x0^3 x2^2 + x1 + 0.5:

      {[let module P = Osdp.Polynomial.Float in
P.(2.3 *. ??0**3 * ??2**2 + ??1 + !0.5)]} *)

  (** {{:#VALvar}var} *)
  val ( ?? ) : int -> t

  (** {{:#VALconst}const} *)
  val ( ! ) : Coeff.t -> t
                        
  (** {{:#VALmult_scalar}mult_scalar} *)
  val ( *. ) : Coeff.t -> t -> t

  (** Unary minus, [~- p] is syntactic sugar for [sub zero p]. *)
  val ( ~- ) : t -> t
                                 
  (** {{:#VALadd}add} *)
  val ( + ) : t -> t -> t
                        
  (** {{:#VALsub}sub} *)
  val ( - ) : t -> t -> t
                        
  (** {{:#VALmult}mult} *)
  val ( * ) : t -> t -> t
                        
  (** [p / c] is equivaent to [mult_scalar (Coeff.div Coeff.one c)
      p]. *)
  val ( / ) : t -> Coeff.t -> t
                        
  (** [c1 /. c2] is equivaent to [!c1 / c2]. *)
  val ( /. ) : Coeff.t -> Coeff.t -> t
                        
  (** {{:#VALpower}power} *)
  val ( ** ) : t -> int -> t
                        
  (** {2 Printing.} *)

  val pp : Format.formatter -> t -> unit

  (** See {{:./Monomial.html#VALpp}Monomial.pp} for details about
      [names]. *)
  val pp_names : string list -> Format.formatter -> t -> unit

  val merge : (Monomial.t -> Coeff.t option -> Coeff.t option -> Coeff.t option)
              -> t -> t -> t
  val fold : (Monomial.t -> Coeff.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (SC : Scalar.S) : S with module Coeff = SC

module Q : S with module Coeff = Scalar.Q

module Float : S with module Coeff = Scalar.Float
