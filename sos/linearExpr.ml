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

module F = 
struct
  module Ext = Matrix.Float.Elem
  include Ext
  type ext_t =t 
  let of_rat a b = div (of_int a) (of_int b)
  let ext_mult = mult
  let ext_of_int = of_int
  let ext_is_zero = is_zero
end

module Vars =
struct
   type t = | SOSVar of Ident.t * int
	    | PolyVar of Ident.t * int
	    | SDPVar of LMI.Num_mat.var 
	    | Cst (* homogeneization used to encode affine part *)

   (* Used to drive the ordering of Vars: variable registered should be smaller
      than unregistered ones *)
   let base_vars : LMI.Num_mat.var list ref = ref []
     
   (* Variables should be registered early to avoid messing with the order of VN
      expressions *)
   let register_base_vars vars =
     base_vars := vars @ !base_vars
       

  let compare x y = match x,y with 
    | Cst , (SOSVar _ | PolyVar _ | SDPVar _ ) -> 1 
    | (SOSVar _ | PolyVar _ | SDPVar _ ) , Cst -> -1 
    | SDPVar v1, SDPVar v2 -> 
      (* Format.eprintf "v1: %a, %b, v2: %a, %b@."  *)
      (* 	LMI.Num_mat.pp_var v1 (List.mem v1 !base_vars) *)
      (* 	LMI.Num_mat.pp_var v2 (List.mem v2 !base_vars); *)
      if List.mem v1 !base_vars && not (List.mem v2 !base_vars) then 1 else compare x y
    | _ -> compare x y

  let pp fmt v = match v with
    | SOSVar (v, d) -> Format.fprintf fmt "sos(%a)" Ident.fprintf v
    | PolyVar (v, d) -> Format.fprintf fmt "poly(%a)" Ident.fprintf v
    | SDPVar v -> Format.fprintf fmt "%a" LMI.Num_mat.pp_var v
    | Cst -> Format.fprintf fmt "CST"
end 

(*******************************************************************************)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*******************************************************************************)


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
  val sub: t -> t -> t
  val ext_of_int: int -> ext_t
  val pp: Format.formatter -> t -> unit
  val inject: (Coeff.t * V.t) list -> t
  val extract: t -> (Coeff.t * V.t) list
  val map: (Coeff.t * V.t -> 'a) -> t -> 'a list
  val zero: t
end

(*******************************************************************************)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*******************************************************************************)

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


(*module LE = LinearExpr.Make (LinearExpr.N)*)
  (*  module LEV = LinearExprVars.Make (N)*)
module VN = struct
  include MakeLE (N) (struct
    include Vars
    module Set = Set.Make (Vars)
    let pp ?(names:string array option=None) = pp 
  end)
    
  (* We overwrite get_vars to remove Cst *)
  let get_vars l = V.Set.remove Vars.Cst (get_vars l) 

end

module VF = struct
  include MakeLE (F) (struct
    include Vars
    module Set = Set.Make (Vars)
    let pp ?(names:string array option=None) = pp 
  end)
end
