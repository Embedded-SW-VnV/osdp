(** Return codes for SDP. *)

type t =
  | Success
  | PartialSuccess  (** Problem solved with reduced accuracy. *)
  | PrimalInfeasible
  | DualInfeasible
  | NearPrimalInfeasible
  | NearDualInfeasible
  | MaxIterReached
  | LackOfProgress
  | Unknown

(** [is_success t] returns [true] if and only if [t] is {!Success} or
    {!PartialSuccess}. *)
val is_success : t -> bool

val pp : Format.formatter -> t -> unit

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
