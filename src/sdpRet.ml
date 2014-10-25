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

let is_success = function
  | Success
  | PartialSuccess -> true
  | PrimalInfeasible
  | DualInfeasible
  | NearPrimalInfeasible
  | NearDualInfeasible
  | MaxIterReached
  | LackOfProgress
  | Unknown -> false

let pp fmt t =
  Format.fprintf
    fmt
    (match t with
     | Success -> "Success"
     | PartialSuccess -> "PartialSuccess"
     | PrimalInfeasible -> "PrimalInfeasible"
     | DualInfeasible -> "DualInfeasible"
     | NearPrimalInfeasible -> "NearPrimalInfeasible"
     | NearDualInfeasible -> "NearDualInfeasible"
     | MaxIterReached -> "MaxIterReached"
     | LackOfProgress -> "LackOfProgress"
     | Unknown -> "Unknown")

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
