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
  let of_rat a b = div (of_int a) (of_int b)
  let ext_mult = mult
  let fprintf = pp 
end



(* Generic functions to ease the addition of polynomials, using merge_sorted_lists *)
let cmp f (_,x) (_,y) = f x y 
let add isz fadd (s1,x) (s2,_) = let s = fadd s1 s2 in if isz s then None else Some (s, x)

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

module Make = functor () -> functor () ->
struct

end
