(** Unique identificators. *)

(** type of identificators. *)
type t
type ident = t

(** Create a new unique identificator. Try to call it as requested and
    append numbers if it already exists. For instance, multiple calls
    with the string "v" will result in v, v0, v1,... Empty string is
    treated as the string "x". *)
val create : string -> t
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit

module Set : Set.S with type elt = ident

module Map : Map.S with type key = ident
