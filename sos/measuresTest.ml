open Monomials
open LinearExpr

module C = ClassicalMonomialBasis 
module H = HermiteMonomialBasis 
module SOS = Sos.Make (C)
module N = LinearExpr.N
module Vars = LinearExpr.Vars


let f () =
  let dim = 2 in
  let deg = 6 in
  Format.printf "%i" (C.get_base_size dim deg)

let nl2 ()  =
  let dim = 2 in
  let d = 6 in

  (* monomials used *)
  let cst = [|0;0|] in
  let x = [|1;0|] in
  let xy = [|1;1|] in
  let y = [|0;1|] in
  let x3 = [|3;0|] in
  let x2 = [|2;0|] in
  let y2 = [|0;2|] in
  
  (* We define a function from R^2 to R^2 *)
  let f : t_cpoly list = 
    [
      [N.of_rat 1 2, x; N.of_int 1 , xy]; (* (x+2*x*y)/2 *)
      [N.of_rat 1 2, y; N.of_int (-1), x3] (* (y-2*x^3)/2 *)
    ]
  in

  (* The balls g1 and g0 *)
  (*  g1 := 1 - x2 - y2 *)
  let g1 : t_cpoly = 
    [N.of_int 1, cst; N.of_int (-1), x2; N.of_int (-1), y2 ] 
  in 
  (* g0 := - (x - 1/2)^2 - (y - 1/2)^2 *)
  let g0 : t_cpoly = 
    [N.of_rat (-1) 2, cst; 
     N.of_int (-1), x2; N.of_int 1, x; 
     N.of_int (-1), y2; N.of_int 1, y ] 
  in 

  (* We have v and w polynomials of degree d *)
  let v_id = Ident.create "v" in
  let v = Vars.PolyVar (v_id, d) in
  let w_id = Ident.create "w" in
  let w = Vars.PolyVar (w_id, d) in

  let monomials = SOS.M.get_monomials dim d in
(*  let pow = Moments.momball monomials in
  Format.eprintf "@.Pow: [@[<v>%a]@]@." (Utils.fprintf_list ~sep:"@ " (fun fmt (m,p) -> Format.fprintf fmt "%a: %f" (SOS.M.fprintf ~names:None) m p)) (List.map2 (fun x y -> x,y) monomials pow);
*)	   
  () 

let run () =
  nl2 ()
