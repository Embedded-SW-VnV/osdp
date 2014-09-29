(** Type of scalars along with basic scalar operations.

    Useful to build matrices over it (see module {!Matrix}). *)

module type S = sig
  type t 

  (** {2 A few values.} *)

  val zero : t
  val one : t
  val is_zero : t -> bool

  (** {2 Conversion functions.} *)

  val of_float : float -> t
  val to_float : t -> float

  (** {2 Arithmetic operations.} *)

  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t

  (** {2 Printing.} *)

  val pp : Format.formatter -> t -> unit
end

module Num : S with type t = Num.num

module Float : S with type t = float
