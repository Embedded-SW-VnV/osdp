open Utils
module Ident = LMI.Ident
open Monomials
    

(*************************      Module Types          *************************)

(* module type DIMEN = *)
(* sig *)
(*   val names : string array  *)
(* end *)


module type LINEXPR = 
sig
  (* type base_t *)
  type expr
  val sos : expr
    
end



(*************************  Polynomial and Linear Expressions   ***************)


(* Polynomial over the classical basis of dimension dim
   Coeff can be either Matrix elements or linear expressions over free variables
*)


(* module PolyScalar = *)
(*   functor (S: SCALAR) ->  *)
(* struct *)
  
(*     (\* [(a1, [|v1;v2;v3|]); (a12 [|v'1;v'2;v'3|])] denotes the polynomial *)
(*        a1*x^v1*y^v2* z^v3 + a2*x^v1'* y^v2'* z^v3'  *)
(*        The list should be sorted  *)
(*     *\) *)
  
(*   module C = ClassicalMonomialBasis (S) *)
(*   type t = (S.t * C.t) list   *)

(*   let comp = fun (_, m1) (_, m2) -> C.cmp m1 m2 *)

(*   let rec plus =  *)
(*     merge_sorted_lists *)
(*       comp *)
(*       (fun (c1, m1) (c2, m2) ->  *)
(* 	let r = Coeff.add c1 c2 in *)
(* 	if Coeff.is_zero r then None  *)
(* 	else Some (r, m1)) *)

(*   let mul cst s =  *)
(*     if Coeff.is_zero cst then *)
(*       []  *)
(*     else *)
(*       List.map (fun (c,m) -> Coeff.mult cst c, m) s *)

(*   let one : t= [Coeff.of_int 1, C.nth 0] *)

(*   let zero = [] *)

(*   let of_int i = [Coeff.of_int i, C.nth 0] *)
    
(*   let prod s1 s2 =  *)
(*     let rec aux s1 accu = *)
(*       match s1 with *)
(*       | [] -> accu *)
(*       | (c,m)::tl ->  *)
(* 	let hd_x_s2 = List.map (fun (c', m') ->  *)
(* 	  let prod_m_m' = match C.prod m m' with [e] -> e | _ -> failwith "Since we are using classical basis, product of monomes should return a single monome" in *)
(* 	  Coeff.mult c c', prod_m_m'  *)
(* 	) s2 in *)
(* 	let accu' = *)
(* 	  merge_sorted_lists  *)
(* 	    comp  *)
(* 	    (fun (c1, m1) (c2, m2) -> Coeff.add c1 c2, m1) *)
(* 	    accu hd_x_s2 in *)
(* 	aux tl accu' *)
(*     in *)
(*     aux s1 [] *)

(*   let fprintf names pp_coeff = *)
(*     fprintf_list  *)
(*       ~sep:" + "  *)
(*       (fun fmt (c, m) -> Format.fprintf fmt "%a * %a" pp_coeff c (C.fprintf names) m )   *)

(* end *)

module Make = functor (BT : Matrix.ElemType) ->
struct


module NumCoeff : SCALAR = struct
  include BT
  let of_rat a b = div (of_int a) (of_int b)
  (* let is_zero = BT.eq (BT.of_int 0)     *)
  let fprintf = pp
end

module C = ClassicalMonomialBasis (NumCoeff)
module NumPolyScalar = PolyExpr (C)

module Vars = 
struct
  type t = SOSVar of Ident.t | SDPVar of Ident.t
  let cmp = compare
  let fprintf fmt v = match v with
    | SOSVar v -> Format.fprintf fmt "sos(%a)" Ident.fprintf v
    | SDPVar v -> Format.fprintf fmt "sdp(%a)" Ident.fprintf v
end

module LinPolyExpr = LinExpr (NumPolyScalar) (Vars)


(* Linear Polynomial Expression:
     define polynomial expressions, linear in their (polynomial) variables
  *)
  module LinPolyExpr =
    functor (M: MONOMIAL_BASIS) ->
  struct

    module S = NumPolyScalar(M)

    type scalar_t = NumPolyScalar.t (* polynomial *)


    type expr = | SOSVar of Ident.t (* SOSVar(id) : Polynomial variable id (should have been 
				       declared with a given degree n *)
		| SDPVar of Ident.t
      		| Scalar of scalar_t (* Polynomial with known coefficients, no free (sdp) vars *)
		| Add of expr * expr 
		| Sub of expr * expr
		| ScalMul of scalar_t * expr
		    
    let hash_sosvars : 
	(Ident.t, 
	 LMI.Num_mat.var list * 
	   (LMI.Num_mat.Mat.elt list -> LMI.Num_mat.Mat.t) * 
	   LMI.Num_mat.matrix_expr *
	   ((int * int) * (M.t)) list
	) Hashtbl.t  
	= Hashtbl.create 13

      
    (* We bind a new polynomial variable.  new_sos_var "foo" 3 returns (id, build,
       sdp_vars): Ident.t * (BT.t list -> scalar_t) * LMI.Sig.matrix_expr

       id is a new identifier

       build is a map from a list of BT.t to the polynomial representation

       sdp_vars is the sdp matrix
    *)
      
    let new_sos_var name deg dim =
      let id = Ident.create name in
      let deg_monomials = M.get_sos_deg deg in
      let vars = LMI.Num_mat.vars_of_sym_mat id deg in
      let expr = 
	List.map 
	  (fun v -> match v with 
	  | Var (vid, i, j) -> 
	    let monomials = M.prod (M.nth i, M.nth j) in
	    List.map 
	      (fun (coeff, m) -> if i = j then coeff,m else (BT.mult (BT.of_int 2) coeff m)) 
	      monomials
	  | _ -> assert false) vars 
      in
      let build_poly elems = LMI.Num_mat.sym_mat_of_var deg_monomials elems in
      let unknown_sdp = LMI.Num_mat.symmat (id, deg_monomials) in
      Hashtbl.add hash_sosvars id (vars, build_poly, unknown_sdp, expr);
      id
	
    let get_sos_vars id = match Hashtbl.find id hash_sosvars with a, _, _, _ -> a
    let get_sos_mat  id = match Hashtbl.find id hash_sosvars with _, a, _, _ -> a
    let get_sos_poly id = match Hashtbl.find id hash_sosvars with _, _, a, _ -> a
    let get_sos_expr id = match Hashtbl.find id hash_sosvars with _, _, _, a -> a
      

    (* STEP 1

       return a simplified expr as an affine system: (scalar, unknown) list *
       affine_part (ie. a scalar) 
       The list sorted by the unknown indices

       expr is simplified: - product of scalars gives new scalars; 
       we obtain (polyscalar, sosvar) list * polyscalar; then

    *)
    let rec simplify expr : (scalar_t * Ident.t) list * scalar_t =
      match expr with
      | SOSVar v -> [S.one,  v] , S.zero
      | SDPVar v -> [S.one,  v] , S.zero
      | Scalar s -> [], s
      | Add (e1, e2) -> 
	let vect1, aff1 = simplify e1 and
	    vect2, aff2 = simplify e2 in
	merge_sorted_lists 
	  (fun (_,v1) (_,v2) -> Ident.cmp v1 v2) 
	  (fun (s1, v1) (s2, v2) -> (S.plus s1 s2), v1) 
	  vect1 vect2, 
	S.plus aff1 aff2

      | Sub (e1, e2) -> simplify (Add (e1, ScalMul(S.of_int (-1), e2)))

      | ScalMul (s,e) ->
	let vect, aff = simplify e in
	List.map (fun (s', v) -> S.prod s s', v) vect , S.prod s aff



(* TODO *)





    type poly_elem_t = Cst of B.t | Var of var_t (* var doit contenir les variables SDP en id,i,j et les variables libres de dimen, eg. x, y, etc *)
    type poly_t = (poly_elem_t list) list  (* sum of products *)

 (*   let polymult p1 p2 = xxx

    (* Evaluate the polynomial corresponding to poly1 * poly2 * m *)
    let monomialpolymult poly m =
      List.fold (fun ml prod -> match prod with
      | Cst c -> List.map (fun (c', m) -> BT.mult c c', m) ml 
      | Var (SOSVar _) ->failwith "should have been eliminated already"
      | Var (SDPVar _) -> (* only valid is m=M0 *)
	
      | Var id -> 
 prod * res ) [1, m] poly
 *) 
    (* monomial_expr expr returns *)
    let monomial_expr (vector, aff) = 
      let merge = merge_sorted_lists 
	(fun  (_, m1) (_, m2) -> M.cmp m1 m2) 
	(fun (e1,m1) (e2,m2) -> (xxxadd e1 e2) , m1) 
      in 
      (* First we express aff as aff * M_0 *)
      let init = [aff, M.nth 0] in
      (* Then we iterate on each element of vector: it associates a polynomial to
	 a sos or sdp var (but not a regular free variable) *)
      let m_vector = 
	List.fold_left (fun res (poly, var) ->
	  match var with
	  | SDPVar v -> (* the SDP var v corresponds to the expression poly * v *)
	    let poly' = polymult poly (LinVarExpr v) in 
	    merge res (monomialpolymult poly' (M.nth 0))
	  | SOSVar v -> (* the sos var v is replaced by its polynomial expression in M, pv(M). *)
	    let m_expr = get_sos_expr v in
	    List.fold_left (fun res (c,m) -> 
	      let poly' = polymult poly c in
	      let new_elem = monomialpolymult poly' m in
	      merge res new_elem
	    ) [] m_expr 
      ) init vector
      in
    (* Cleaning it *)
 (*     let m_vector = List.fold_left ( *)
()
	
    (* Compute the polynomial representing (M1 ... Mdeg) Mat (M1 .. Mdeg)  *)
    let compute_prod_basis deg =
      let id = "tmp_var" in
      let vars = LMI.vars_of_sym_mat id deg in
      assert false;
      ()
	

    (* get_monomial_cons i svar: returns the constraint expression associated to
       i-th monomial of the base, expressed over the variables of svar. It is an
       expression over svar matrix element. Typically a_jj or 2 * a_ij, but it can
       be a sum for exotic bases like Hermite polynomials.
    *)
    let get_monomial_cons idx sos_var =
      (* Both list are sorted: lowest elements first *)
      let rec merge_lists coeff2 new_elems result =
	let merge = merge_lists coeff2 in
	let coeff_mult c = if coeff2 then 2 * c else c in
	match new_elems, result with
	| [], _ -> result
	| _, [] -> 
	  if coeff2 then 
	    List.map (fun (coeff,idx) -> 2 * coeff, idx) new_elems
	  else 
	    new_elems
	| (elem_coeff, elem_idx)::tl, (hd_coeff, hd_idx)::tl_res ->
	  if elem_idx = hd_idx then
	    ((coeff_mult elem_coeff) + hd_coeff, hd_idx)::(merge tl tl_res)
	  else if elem_idx < hd_ix then
	    (coeff_mult elem_coeff, elem_idx)::(merge tl result)
	  else 
	    (hd_coeff, hd_idx)::(merge new_elems result)
      in

      let vars = get_sos_vars sos_var in
      (* iterate on the variables: V_ij is associated to coeff 2, and product of
	 base Mi * Mj
	 V_ii have coeff 1.
	 We do both in a single fold
      *)
      List.fold_left (fun accu Var(id, i, j) -> 
	let monomial_product = M.prod i j in
	(* We add each element to accu with the coeff 2 is i<>j (1 otherwise). 
	   Both list are sorted 
	*)
	merge_lists (i<>j) monomial_product accu
      ) [] vars

    (* sos expr: expr ne va pas etre naturellement exprimé dans la base M, mais
       c'est a nous de l'exprimer comme une somme de carré dans M: Mt A M *)
  	
    (* sos name expr expresses expr as a sum of square as a new sos var.  It
       returns a symmetric matrix to be proved SDP along with constraints to be
       satisfied.  The expression is linear in the sos variables. The algorithm is the following: 

       1. expr is simplified: - product of scalars gives new scalars. It remains linear 
          in (sdp|sos)vars; 
          we obtain (numpolyscalar, sdpvar|sosvar) list * numpolyscalar; then

       2. each sosvar is replaced by a polynomial expression in the monomial basis M:
       (numpolyscalar, (sdpvaraffineexpr, M.t) list) list * numpolyscalar

       2.1 it requires to generate the (sdpaffinescalar, M.t) list for a sosvar

       2.2 if not yet computed, the product of basis is computed to at least the degree of sosvar
       eg with variables {x, y, z} and degree up to 2, we have all products of 
       (M0xM0yM0z, M1xM0yM0z,M0xM1yM0z,M0xM0yM1z) with itself constraints associated with it 

       3. it is simplified and expressed over the monomial basis:
       we obtain a ((SDPVAR or B.t)expr, M.t) list
       it needs the capability to compute polyscalar * M.t -> B.t * M.t

       4. the main expression is expressed in the product base as SOS: Mt A M
       the product of bases to the degree of expr is computed. 
       it produces a polynomial in M.t where scalar are polynomials overs sdp vars in A

       5. by identification, we associate (B.t|vars) elemens of (3) to polynomials overs
       sdp vars of (4) It induces a new set of constraints between scalar
       coefficients and free coeeficient variables of unknown polynomials.
       eg. 3 xy + 7 x + 6y^2: xy is searched in the product base, it returns
       the variable "p456" with multiplier 2: 2 * p456 = 3. Similarly y returns
       the var "p34" with multiplier 2: 2 * p34 = 7. For monomial y^2, the var
       is associated to a multiplier 1 (diagonal coeff): p324 = 6.

       6. eventually the symetric matrix is returned with the set of equality
       constraint generated
       
    *)
    let sos name expr = 
      let expr = simplify expr in
      let vector_monomial, (degree:int) = monomial_expr expr in
      let sos_var = new_sos_var name degree in
      let constraints, _ = 
	Array.fold_left (
	  fun (constraints, idx) coeff ->
	    let new_constraint = coeff, get_monomial_cons idx sos_var in
	    new_constraint::constraints, idx+1
	) ([], 0) vector_monomials 
      in
      sos_var, constraints
  end


  (* Tests *)
  module MonBase3 = ClassicalMonomialBasis(struct let dim = 3 end)
  let nth = MonBase3.nth

  let _ =
    List.iter 
      (fun el -> 
	MonBase3.fprintf Format.std_formatter el [|"x";"y";"z"|];
	Format.pp_print_newline Format.std_formatter ()
      ) (List.map nth [0;1;2;3;4;5;6;7;8;9;10])
(*
  module LinearPolynomialExpr =
  functor (MonomialBase: sig
  type t 
  val int -> t
  end ->
  struct
  type base_t = polynome de deg d sur un ensemble de variable V et une base de monome B de degre au plus d
  degre d'un monome

  type expr = 
  | Const of base_t
  | ConstMult of base_t * expr
  | Add of expr * expr
  | Sub of expr * expr

  let const =      
  
  end
*)


(* OLD 
   (* Compute the number of monomials of degree = d with v variables *)
   let rec nb_mon_of_fix_deg v d =
   match v,d  with
   | 1, _ -> 1
   | _, 1 -> v
   | _ -> nb_mon_of_fix_deg (v-1) d + nb_mon_of_fix_deg v (d-1)

   let nth k = 
   let new_array = Array.make (dim) 0 in
   let rec aux k deg max = 
   if k < max then (
   Format.printf "%i-th vecteur of deg %i@." k deg;
	(*Array.set new_array k 1*)()
   )
   else (* k >= accu *)
   aux (k-max) (deg+1) (nb_mon_of_fix_deg dim (deg+1))
   in
   if k > 0 then
   aux (k-1) 1 dim;
   new_array   


*)
end
