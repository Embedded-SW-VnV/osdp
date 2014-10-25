(** Multiple variables polynomials. *)

module type S = sig
  (** Type of coefficients. *)
  module  Coeff : Scalar.S

  (** Type of polynomials. *)
  type t

  (** {2 Conversion functions.} *)

  (** [of_list \[(m_1, a_1);..; (m_n, a_n)\]] builds the polynomial
      a_1 m_1 + ... + a_n m_n.

      TODO: duplicates accepted *)
  val of_list : (Monomial.t * Coeff.t) list -> t

  (** TODO: return sorted list without duplicates *)
  val to_list : t -> (Monomial.t * Coeff.t) list

  (** {2 A few values.} *)

  val zero : t
  val one : t

  (** {2 Arithmetic operations.} *)
                                         
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t

  (** [power p n] computes p^n, [n] must be positive. *)
  val power : t -> int -> t

  exception Dimension_error

  (** [compose p \[q_1;...; q_n\]] returns the polynomial p' such that
      p'(x_1,..., x_m) = p(q_1(x_1,..., x_m),..., q_n(x_1,...,
      x_m)).

      @raise Dimension_error if [p] is defined on more than n
      variables. *)
  val compose : t -> t list -> t

  (** {2 Various functions.} *)

  (** Number of variables appearing in the polynomial. *)
  val nb_vars : t -> int

  (** -1 for the null polynomial. *)
  val degree : t -> int

  val is_homogeneous : t -> bool
                                         
  (** {2 Printing.} *)

  (** See {!Monomial.pp} for details about [names]. *)                                         
  val pp : ?names:string list -> Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC

module Q : S with module Coeff = Scalar.Q

module Float : S with module Coeff = Scalar.Float

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
