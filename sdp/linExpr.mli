(** Affine expressions. *)

module type S = sig
  (** Type of coefficients. *)
  module  Coeff : Scalar.S

  (** Type of affine expressions. *)
  type t

  (** {2 Conversion functions.} *)

  (** [of_list \[(x_1, a_1);..; (x_n, a_n)\] c] builds the affine
      expression a_1 x_1 + ... + a_n x_n + c. *)
  val of_list : (Ident.t * Coeff.t) list -> Coeff.t -> t

  val to_list : t -> (Ident.t * Coeff.t) list * Coeff.t

  (** Same as {!of_list} with an empty list. *)
  val const : Coeff.t -> t

  (** Same as {!of_list} with the list [\[ident, Coeff.one\]] and
      constant [Coeff.zero]. *)
  val var : Ident.t -> t

  (** {2 Arithmetic operations.} *)
                                         
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t

  (** {2 Various operations.} *)

  val eq : t -> t -> bool
                                         
  val is_const : t -> bool

  (** {2 Printing.} *)
                                         
  val pp : Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC

module Num : S with module Coeff = Scalar.Num

module Float : S with module Coeff = Scalar.Float

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
