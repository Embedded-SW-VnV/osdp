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

module Num : S with type t = Num.num = struct 
  type t = Num.num
  let zero = Num.num_of_int 0
  let one = Num.num_of_int 1
  let is_zero n = Num.sign_num n = 0
  let of_float = Utils.num_of_float
  let to_float = Num.float_of_num
  let add = Num.add_num
  let sub = Num.sub_num
  let mult = Num.mult_num
  let div = Num.div_num
  let pp fmt num = Format.fprintf fmt "%s" (Num.string_of_num num)
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
