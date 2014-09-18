open Monomials

module Num = 
struct
  include Matrix.Num_mat.Elem
  let of_rat a b = div (of_int a) (of_int b)
  let fprintf = pp 
end


module C = ClassicalMonomialBasis (Num)
module H = HermiteMonomialBasis (Num)

module Test =
  functor (M: MONOMIAL_BASIS) ->
struct
let _ =
  
let nb_vars = 3 in 
let names = [|"x"; "y"; "z"|] in
let deg = 10 in
let nb_mon = M.get_sos_deg nb_vars deg in

Format.printf "nb monomials: %i@.@?" nb_mon;
let rec aux n accu = 
  if n > 0 then
    aux (n-1) ((M.nth nb_vars (n-1))::accu)
  else 
    accu
in
let base = aux nb_mon [] in
Format.printf "Base monomiale SOS en dimension %i de degré %i:@.@[<h>%a@]@."
  nb_vars 
  deg
  (Utils.fprintf_list ~sep:",@," (M.fprintf names)) base;

Format.printf "Computing products of monomials@.";
let rec aux l1 l2 =
match l1, l2 with
| hd1::tl1, hd2::tl2 ->
  List.iter (fun m2 -> 
    let res = M.prod hd1 m2 in
    Format.printf "%a * %a = %a@."
      (M.fprintf names) hd1
      (M.fprintf names) m2
      (M.LE.fprintf names) res;
    ()
      
  ) l2;
  aux tl1 tl2
| [], [] -> 
  ()
| _ -> assert false
  ;  
  
in
aux base base
end


let _ =
  let nb_vars = 10 in 
  let deg = 16 in
  let nb_mon = C.get_sos_deg nb_vars deg in

()
(*module TC =  Test(C)

module TH = Test(H)
*)


(*module X = LinearExpr.PolyScalar*)

