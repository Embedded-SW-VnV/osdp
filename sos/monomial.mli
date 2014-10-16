(** Monomials of multiple variables (e.g., x^2 y z^3). *)

type t

(** [of_list l] produces the monomial corresponding to list [l]. For
    instance, considering the variables x0, x1, x2 and x3,
    [of_list\[3; 4; 0; 1\]] gives the monomial x0^3 x x1^4 x x3. *)
val of_list : int list -> t
val to_list : t -> int list

val compare : t -> t -> int

val nb_vars : t -> int

val degree : t -> int

(** [mult m1 m2] multiplies the two monomials [m1] and [m2]. If one of
    them is defined on less variables, the undefined exponents are
    considered as 0 (for instance [mult (of_list \[1; 2\]) (of_list
    \[3; 4; 5\])] returns [m] such that [to_list m = \[4; 6; 5\]]). *)
val mult : t -> t -> t

(** [list_eq n d] provides the list of all monomials with [n]
    variables of degree equal [d] (for instance, [list_eq 3 2] can
    return [\[x0^2; x0 x1; x0 x2; x1^2; x1 x2; x2^2\]]). [n] must be
    positive and [d] must be non negative. *)
val list_eq : int -> int -> t list

(** [list_le n d] provides the list of all monomials with [n]
    variables of degree less or equal [d] (for instance, [list_le 3 2]
    can return [\[1; x0; x1; x2; x0^2; x0 x1; x0 x2; x1^2; x1 x2;
    x2^2\]]). [n] and [d] must be non negative. *)
val list_le : int -> int -> t list

(** Pretty printing. If [names] is not provided or is to short,
    variables will be printed as x0, x1,... Provided a too short
    [names] list is not advisable however as the generated names may
    collide with the provided ones. *)
val pp : ?names:string list -> Format.formatter -> t -> unit
