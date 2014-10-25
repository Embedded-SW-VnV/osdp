module type S = sig
  type t 
  val zero : t
  val one : t
  val is_zero : t -> bool
  val of_float : float -> t
  val to_float : t -> float
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module Q : S with type t = Q.t = struct 
  type t = Q.t
  let zero = Q.zero
  let one = Q.one
  let is_zero n = Q.equal n zero
  let of_float = Q.of_float
  let to_float q =
    let s = Q.to_string q in
    try
      let i = String.index s '/' in
      let n = float_of_string (String.sub s 0 i) in
      let d =
        let l = String.length s - i - 1 in
        float_of_string (String.sub s (i + 1) l) in
      n /. d
    with Not_found -> float_of_string s
  let add = Q.add
  let sub = Q.sub
  let mult = Q.mul
  let div = Q.div
  let pp = Q.pp_print
end

module Float : S with type t = float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let is_zero f = f = 0.
  let of_float x = x
  let to_float x = x
  let add = ( +. )
  let sub = ( -. )
  let mult = ( *. )
  let div = ( /. )
  let pp = Format.pp_print_float
end
