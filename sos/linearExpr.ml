open Utils
module Ident = LMI.Ident
open Monomials

exception Incompatible_dim

(*******************************************************************************)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*******************************************************************************)

(* Numerical scalar using in polynomials *)    
module N = 
struct
  module Ext = Matrix.Num_mat.Elem
  include Ext
  type ext_t =t 
  let of_rat a b = div (of_int a) (of_int b)
  let ext_mult = mult
  let ext_of_int = of_int
  let ext_is_zero = is_zero
 
end

(*
  (* For debug purpose *)
  let pp_xvn pp_x fmt xvn =
    match xvn with
      [] -> Format.fprintf fmt "0"
    | _ -> fprintf_list ~sep:" + " 
      (fun fmt (l,c) -> 
	pp_x Format.str_formatter c;
	let c_str = Format.flush_str_formatter () in
	if c_str = "1" then
	  pp_levarsnum fmt l 
	else
	  Format.fprintf fmt "%a * %s" pp_levarsnum l c_str
      )
      fmt 
      xvn
*)

module type COEFF =
sig
  type t
  type ext_t
  val ext_is_zero: ext_t -> bool
  val ext_mult: ext_t -> t -> t
  val is_zero: t -> bool
  val add: t -> t -> t
  val ext_of_int: int -> ext_t
  val pp: Format.formatter -> t -> unit
end

module type VARS =
sig      
  type t
  module Set: Set.S with type elt = t
  val pp: ?names:string array option -> Format.formatter -> t -> unit
  val compare: t -> t -> int
end

module type LE =
sig
  type t 
  type ext_t
  module Coeff : COEFF
  module V : VARS
  val get_vars: t -> V.Set.t
  val ext_is_zero: ext_t -> bool
  val ext_mult: ext_t -> t -> t
  val is_zero: t -> bool
  val add: t -> t -> t
  val ext_of_int: int -> ext_t
  val pp: Format.formatter -> t -> unit
  val inject: (Coeff.t * V.t) list -> t
  val extract: t -> (Coeff.t * V.t) list
  val map: (Coeff.t * V.t -> 'a) -> t -> 'a list
  val zero: t
end

(* Linear expr on (Coeff * V) *)
module MakeLE = functor (Coeff: COEFF) -> functor (V: VARS) -> (
struct
  module Coeff = Coeff
  module V = V

  type elem_t = Coeff.t * V.t
  type t = elem_t list
  (* Generic functions to ease the addition of polynomials, using merge_sorted_lists *)
  let cmp_gen f ((_,x):elem_t) (_,y) = f x y 
  let add_gen isz fadd ((s1,x):elem_t) (s2,_) = let s = fadd s1 s2 in if isz s then None else Some (s, x)
      
  (* For debug purpose *)
  let pp fmt l =
    match l with
      [] -> Format.fprintf fmt "0"
    | _ -> fprintf_list ~sep:" + " 
      (fun fmt (c,v) -> 
	V.pp Format.str_formatter v;
	let v_str = Format.flush_str_formatter () in
	if v_str = "1" then
	  Coeff.pp fmt c 
	else
	  Format.fprintf fmt "%a * %s" Coeff.pp c v_str
      )
      fmt 
      l
      
  let ext_mult s (l:t) = 
    if Coeff.ext_is_zero s then 
      [] 
    else 
      List.map (fun (c,v) -> Coeff.ext_mult s c, v) l 
	
  let add = merge_sorted_lists (cmp_gen V.compare) (add_gen Coeff.is_zero Coeff.add)
    
  let sub l1 l2 = add l1 (ext_mult (Coeff.ext_of_int (-1) ) l2)
    
  let is_zero (l:t)  = (=) [] l
    
  let get_vars (l:t) = List.fold_left (fun accu (_,v) -> V.Set.add v accu) V.Set.empty  l

  type ext_t = Coeff.ext_t
  let ext_of_int = Coeff.ext_of_int      
  let ext_is_zero = Coeff.ext_is_zero
  let inject x = x
  let extract x = x
  let zero = []
  let map = List.map

end : LE with type Coeff.t = Coeff.t and type V.t = V.t and type ext_t = Coeff.ext_t
)
(*

module Make =functor 
    (N:sig 
      type t 
      type ext_t
      val is_zero: t -> bool
      val ext_is_zero: ext_t -> bool
      val ext_mult: ext_t -> t -> t
      val add: t -> t -> t
      val ext_of_int: int -> ext_t
      val pp: Format.formatter -> t -> unit
    end) ->
struct

 

  module VN =
  struct
    include MakeLE 
      (struct
	include N(*	type t = N.t
	type ext_t = N.t
	let is_zero = N.is_zero
	let ext_mult = N.ext_mult
	let add = N.add
	let ext_of_int = N.of_int*)
       end)
      
      (struct
	include Vars
	module Set = Set.Make (Vars)
	let pp ?(names:string array option=None) = pp 
       end)
      
    (* We overwrite get_vars to remove Cst *)
    let get_vars l = V.Set.remove Vars.Cst (get_vars l) 

end



end

module Main = Make (N)
*)
