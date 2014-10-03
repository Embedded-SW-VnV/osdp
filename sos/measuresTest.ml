open Monomials
open LinearExpr
open Sos

module C = ClassicalMonomialBasis 
module H = HermiteS
module SOS = Sos.Make (C)
module N = LinearExpr.N
module Vars = LinearExpr.Vars
module CN = Sos.CN

let poly_cst_one dim = 
    CN.inject ([CN.ext_of_int 1, C.nth dim 0])

let f () =
  let dim = 2 in
  let deg = 20 in
  Format.printf "%i" (C.get_base_size dim deg)

let ext_product (v: VN.t) (e: CN.t) : CVN.t =
  List.fold_left 
    (fun (accu: CVN.t) (v',c) -> 
      CVN.add accu (CVN.ext_mult v' (CVN.inject [v, c]))   
    ) CVN.zero (CN.extract e)


let product (e1:CN.t) (e2:CN.t) : CN.t = 
  let prod_elem (n1,m1) (n2,m2) =
    match C.prod m1 m2 with
    |[_, m] -> N.mult n1 n2, m (* We even do not look at the coefficient since
				  it should be equal to 1 *)
    | _ -> assert false (* should not happen with classical polynomials *)
  in
  List.fold_left (
    fun accu elem_e1 ->
      List.fold_left (
	fun accu' elem_e2 ->
	  CN.add 
	    accu' 
	    (CN.inject [prod_elem elem_e1 elem_e2])
      ) accu (CN.extract e2)
	
  ) CN.zero (CN.extract e1) 
  

(* Compute the composition f o g: expression expr^order in CN.t *)
let rec expand dim order expr =
  if order < 0 then assert false 
  else if order = 0 then poly_cst_one dim
  else if order = 1 then expr 
  else product expr (expand dim (order-1) expr)


let image_measure dim deg v_id (f:CN.t list) : Sos.expr = 
  let compute_monomial_img m idx var_idx : Sos.expr = (* substitute each dim i with order n by f(i)^n *)
    let exprl = 
      Array.mapi (fun idx order -> expand dim order (List.nth f idx)) m in
    let expr = match Array.to_list exprl with
      | [] -> assert false (* should not happen : 0-dimensional systems *)
      | [x] -> x 
      | hd::tl -> List.fold_left product hd tl 
    in 
(*     ext_product (VN.inject [N.of_int 1, Vars.SDPVar var_idx]) expr *)
      expr *% Sos.Var (Vars.SDPVar var_idx)

  in
  if List.length f <> dim then
    assert false;
  let monomials_C = C.get_monomials dim deg in
  let v_coeffs = SOS.get_poly_vars v_id in
  match monomials_C, v_coeffs with
  | [], _ | _ , [] -> assert false (* how could this happen ? *)
  | [m], [var_idx] -> compute_monomial_img m 0 var_idx
  | hd_m::tl_m, var_0::_ -> 
    let res, _ = 
      List.fold_left (
	fun (res, idx) monomial ->
	  let var_idx = List.nth v_coeffs idx in
	  res +% (compute_monomial_img monomial idx var_idx), idx+1  
    ) (compute_monomial_img hd_m 0 var_0, 1) tl_m 
    in
    res
  
let scal_prod_lebesgue w dim deg =
  let vars = SOS.get_poly_vars w in
  let monomials = SOS.M.get_monomials dim deg in
  let pow = Moments.momball monomials in 
  Format.eprintf "@.Pow: [@[<v>%a]@]@." 
    (Utils.fprintf_list ~sep:"@ " 
       (fun fmt (m,p) -> match p with | Some p -> Format.fprintf fmt "%a: %f" (SOS.M.pp ~names:None) m p | _ -> Format.fprintf fmt "%a: 0" (SOS.M.pp ~names:None) m)
    ) 
    (List.map2 (fun x y -> x,y) monomials pow);
  List.fold_left2 (fun res v p ->
    match p with
    | None -> res
    | Some f -> VN.add res (VN.inject [Utils.num_of_float f, Vars.SDPVar v])
  ) VN.zero vars pow

let nl2 ()  =
  let dim = 2 in
  let d = 2 in

  (* monomials used *)
  let cst = [|0;0|] in
  let x = [|1;0|] in
  let xy = [|1;1|] in
  let y = [|0;1|] in
  let x3 = [|3;0|] in
  let x2 = [|2;0|] in
  let y2 = [|0;2|] in
  
  (* We define a function from R^2 to R^2 *)
  let f : CN.t list = 
    [
      CN.inject [N.of_rat 1 2, x; N.of_int 1 , xy]; (* (x+2*x*y)/2 *)
      CN.inject [N.of_rat 1 2, y; N.of_int (-1), x3] (* (y-2*x^3)/2 *)
    ]
  in

  let alpha = CN.inject [N.of_int 1, cst] in
  let one = CN.inject [N.of_int 1, cst] in

  (* The balls g1 and g0 *)
  (*  g1 := 1 - x2 - y2 *)
  let g1 : CN.t = 
    CN.inject [N.of_int 1, cst; N.of_int (-1), x2; N.of_int (-1), y2 ] 
  in 
  (* g0 := - (x - 1/2)^2 - (y - 1/2)^2 *)
  let g0 : CN.t = 
    CN.inject [N.of_rat (-1) 2, cst; 
     N.of_int (-1), x2; N.of_int 1, x; 
     N.of_int (-1), y2; N.of_int 1, y ] 
  in 

  (* We have v and w polynomials of degree d *)
  let v_id, v = SOS.new_poly_var "v" dim d in
  let w_id, w = SOS.new_poly_var "w" dim d in


  (* We declare the following SOS polynomials *)
  let q11, q11_expr = SOS.new_sos_var "q11" dim (d-2) in  
  let q21, q21_expr = SOS.new_sos_var "q21" dim (d-2) in  
  let q31, q31_expr = SOS.new_sos_var "q31" dim (d-2) in  
  let q41, q41_expr = SOS.new_sos_var "q41" dim (d-2) in  

  (* Image measure *)
  let vf : expr = image_measure dim d v_id f in
(*   Format.eprintf "F#\\mu = %a@." CVN.pp vf;*)
  
  (* SOS constraints *)
  let expr_q1 = ((alpha *% vf) -% (!% v)) -%  (g1 *% !% q11_expr) in
  let expr_q2 = (!% v) -%  (g0 *% !% q21_expr) in
  let expr_q3 = ((!% w -% (?% one)) -% !% v) -%  (g1 *% !% q31_expr) in
  let expr_q4 = !% w -%  (g1 *% !% q41_expr) in
  let q1, q1_expr, lmi_q1, cons_q1 = SOS.sos dim "Q1" expr_q1 in
  (* Format.eprintf "Q1 sos: %i constraints!@." (List.length cons_q1); *)

  let q2, q2_expr, lmi_q2, cons_q2 = SOS.sos dim "Q2" expr_q2 in
  (* Format.eprintf "Q2 sos: %i constraints!@." (List.length cons_q2); *)
  let q3, q3_expr, lmi_q3, cons_q3 = SOS.sos dim "Q3" expr_q3 in
  (* Format.eprintf "Q3 sos: %i constraints!@." (List.length cons_q3); *)
  let q4, q4_expr, lmi_q4, cons_q4 = SOS.sos dim "Q4" expr_q4 in
  (* Format.eprintf "Q4 sos: %i constraints!@." (List.length cons_q4); *)

  (* Minimizing the pb:
    we iterate over variables,xxx  
  *)

  (* minimize wc' * z. *)
  let obj_cons = 
    List.map 
      (fun (c,v) -> match v with Vars.SDPVar v -> v, c | _ -> assert false)
      (VN.extract (scal_prod_lebesgue w_id dim d))
  in
  let obj = LMI.Minimize, obj_cons  in


  let sos_vars = [(*q1; q2; q3; q4; *) q11; q21; q31; q41] in

  let constraints_as_lmi = [] in (*List.fold_left  (
    fun res cons ->
      let lmi = cons in
      let neg_lmi = cons in
      lmi::neg_lmi::res
  ) [] (obj_cons::cons_q1@cons_q2@cons_q3@cons_q4)
  
  in
*)
  (* let sos_constraints_to_lmi =  xxx*)

  (* in *)
  (* List.map () [cons1; ] *)
  let vn_to_mat_expr vn = (* every thing is in dimension 1 *)
    List.fold_left (fun accu (n,v) ->
      match v with
      | Vars.SDPVar v -> 
	let new_term = LMI.Num_mat.scal_mult v (LMI.Num_mat.const_mult n (LMI.Num_mat.eye 1)) in
	LMI.Num_mat.add accu new_term  
      | Vars.Cst -> LMI.Num_mat.add accu (LMI.Num_mat.const_mult n (LMI.Num_mat.eye 1))
      | _ -> Format.eprintf "Vars: [%a]@.@?" Vars.pp v; assert false (* bizarre *)
	
    ) (LMI.Num_mat.zeros 1 1) (VN.extract vn)
  in
  let lmis = (List.map (fun id -> LMI.Num_mat.symmat (id, SOS.get_sos_dim id)) sos_vars)@[lmi_q1; lmi_q2; lmi_q3; lmi_q4]@(List.map vn_to_mat_expr constraints_as_lmi) in

  let sos_vars_expr : Vars.t list = [q1_expr; q2_expr; q3_expr; q4_expr; q11_expr; q21_expr; q31_expr; q41_expr] in
  let expand_var v = 
    match v with
    | Vars.SDPVar v -> [v]
    | Vars.SOSVar (id, i) -> SOS.get_sos_free_vars id
    | Vars.PolyVar (id, i) -> SOS.get_poly_vars id
    | Vars.Cst -> assert false
  in
  let vars = List.map expand_var (v::w::sos_vars_expr) in
  Format.eprintf "Call LMI.solve@.@?";
 let _ = LMI.Num_mat.solve vars lmis obj in
  Format.eprintf "Called LMI.solve@.@?";

()

(* Pistes d'ameliorations:
- structures de données efficcaces poru conserver des linformations en triangulaire superieur
- il faut ordonner les variables: les premieres dimensions doivent etre les variables SOS contraintes, sos(expr) exprimées en fonctions des variables SOS existantes
- on ne gardera que la partie "flat" de la diagonalisée
  \
   \
    \
     | ici
     | ici

- dans la production du probleme, les variables a 0 sont retirées. Mais on les garde pour reconstruire le primal (dual).
*)

let run () =
  nl2 ()
