open Utils
module Ident = LMI.Ident
open Monomials

(* Numerical scalar using in polynomials *)    
module N = 
struct
  module Ext = Matrix.Num_mat.Elem
  include Ext
  let of_rat a b = div (of_int a) (of_int b)
  let ext_mult = mult
  let fprintf = pp 
end

(* Variables: either scalar (sdp vars), polynomial or SOS polynomial *)
module Vars = 
struct
  type t = | SOSVar of Ident.t * int
	   | PolyVar of Ident.t * int
	   | SDPVar of LMI.Num_mat.var 

  let cmp = compare

  let fprintf fmt v = match v with
    | SOSVar (v, d) -> Format.fprintf fmt "sos(%a)" Ident.fprintf v
    | PolyVar (v, d) -> Format.fprintf fmt "poly(%a)" Ident.fprintf v
    | SDPVar v -> Format.fprintf fmt "sdp(%a)" LMI.Num_mat.pp_var v
end

module C = ClassicalMonomialBasis 
module H = HermiteMonomialBasis 

(* Polynomial in C *)
type t_cpoly = (N.t * C.t) list

(* Linear expressions over variables in Vars with numerical scalars *)
type t_levarsnum = (N.t * Vars.t option) list

let pp_levarsnum fmt c = 
match c with | [] -> Format.fprintf fmt "0" | _ ->
  fprintf_list ~sep:" + " (fun fmt (n,vopt) -> match vopt with None -> N.fprintf fmt n | Some v -> Format.fprintf fmt "%a * %a" Vars.fprintf v N.fprintf n ) fmt c
  
(* Polynomials in the classical basis with scalar in LEVarsNum *)
type t_cvn = (t_levarsnum * C.t) list

(* For debug purpose *)
let pp_xvn pp_x names fmt xvn =
  fprintf_list ~sep:" + " (fun fmt (l,c) -> Format.fprintf fmt "(%a * %a)" pp_levarsnum l (pp_x names) c) fmt xvn

let pp_cvn = pp_xvn C.fprintf

(* Type of expressions: it should be linear in SOSVar, SDPVar, PolyVar *)
type expr = | SOSVar of Ident.t * int (* SOSVar(id, d) : Positive Polynomial variable 
					 id of degree d *)
	    | PolyVar of Ident.t * int 
	    | SDPVar of LMI.Num_mat.var
      	    | Scalar of t_cpoly (* Polynomial with known coefficients, no free (sdp) vars *)
	    | Add of expr * expr 
	    | Sub of expr * expr
	    | ScalMul of t_cpoly * expr


module Make =
  functor (M: MONOMIAL_BASIS) ->
   struct

  (* Polynomials in base M with scalar in LEVarsNum *)
  (* module MVN = PolyLinExpr (LEVarsNum) (M) *)
  type t_mvn = (t_levarsnum * M.t) list 

  let pp_mvn = pp_xvn M.fprintf
    
  (* Polynomials in the classical basis with scalar in MVN *)
  (* module CMVN = PolyLinExpr (MVN) (C)  *)
  type t_cmvn = (t_mvn * C.t) list


  (* Generic functions to ease the addition of polynomials, using merge_sorted_lists *)
  let cmp f = fun (_,x) (_,y) -> f x y 
  let add isz fadd = fun (s1,x) (s2,_) -> let s = fadd s1 s2 in if isz s then None else Some (s, x)

  (* Additions of polynomials: Instanciated functions *)
  let levarsnum_add = merge_sorted_lists (cmp (compare)) (add (N.eq (N.of_int 0)) N.add)
  let levarsnum_is_zero x = x = []
  let mvn_add = merge_sorted_lists (cmp M.cmp) (add levarsnum_is_zero levarsnum_add)
  let cvn_add = merge_sorted_lists (cmp C.cmp) (add levarsnum_is_zero levarsnum_add) 

    (* Hash table to store the computed generated sos variables *)
   let hash_sosvars : 
	(Ident.t, 
	 int *
	   LMI.Num_mat.var list * 
	   (LMI.Num_mat.Mat.elt list -> LMI.Num_mat.Mat.t) * 
	   LMI.Num_mat.matrix_expr *
	   t_mvn
	) Hashtbl.t  
	= Hashtbl.create 13

   (* copy of LMI.sym_mat_of_var *)
   let pp_sos dim fmt (vl:LMI.Num_mat.var list) = 
     let expected_length = (dim*dim + dim)/2 in
     if List.length vl != expected_length then
       raise (Failure ("Invalid input list for pp_sos: expecting " 
		       ^ string_of_int expected_length ^ ", we have " 
		       ^ string_of_int (List.length vl)));
     let new_mat = Array.make_matrix dim dim (None) in
     let i = ref 0 and j = ref 0 in
     List.iter (fun elem -> 
    (* Report.debugf ~kind:"sdp" 
       (fun fmt -> "%i, %i@.@?" !i !j); *)
       if !i > dim then assert false;
       (if !i = !j then
	   new_mat.(!i).(!j) <- Some elem
	else
	   (new_mat.(!i).(!j) <- Some elem;
	    new_mat.(!j).(!i) <- Some elem)
       );
    (* Incrementing i and j *)
       if !j = dim-1 then
	 (incr i; j := !i)
       else
	 (incr j)
     ) vl;
     Format.fprintf fmt "@[<v>[";
     Array.iter (fun m -> Format.fprintf fmt "@[<h>%t@]@ "
       (fun fmt -> Array.iter (fun v -> match v with None -> Format.fprintf fmt "0@ 
" | Some v -> Format.fprintf fmt "%a@ " LMI.Num_mat.pp_var v) m)
     ) new_mat;
     Format.fprintf fmt "@ ]@]"




    (* Vieille explication: 
       We bind a new polynomial variable.  new_sos_var
       "foo" 3 returns (id, build, sdp_vars): Ident.t * (BT.t list -> scalar_t)
       * LMI.Sig.matrix_expr

       id is a new identifier

       build is a map from a list of BT.t to the polynomial representation

       sdp_vars is the sdp matrix
    *)
      
    let new_sos_var name deg dim : Ident.t =
      let id = Ident.create name in
      let deg_monomials = M.get_sos_deg dim deg in
      Format.eprintf "nb element base monomiale: %i@." deg_monomials;
      let vars = LMI.Num_mat.vars_of_sym_mat id deg_monomials in
      Format.eprintf "nb vars: %i@." (List.length vars);
      let expr : t_mvn = 
	List.fold_left  
	  (fun res v -> mvn_add res 
	    (match LMI.Num_mat.get_var_indices v with 
	    | Some (i, j) -> 
	      let v = Vars.SDPVar v in
	      let monomials = try M.prod (M.nth dim i) (M.nth dim j) with Failure _ -> assert false in
	      List.map
		(fun (coeff, m) -> 
		  if i = j then
		    [coeff, Some v],m 
		  else
		    ([N.mult (N.of_int 2) coeff, Some v], m)
		) 
		monomials
	    | _ -> assert false)
	    
	  ) [] vars 
      in
      let build_poly elems = LMI.Num_mat.sym_mat_of_var deg_monomials elems in
      let unknown_sdp = LMI.Num_mat.symmat (id, deg_monomials) in
      Hashtbl.add hash_sosvars id (deg_monomials, vars, build_poly, unknown_sdp, expr);
      id
	
    let get_sos_dim  id = match Hashtbl.find hash_sosvars id with a, _, _, _, _ -> a
    let get_sos_vars id = match Hashtbl.find hash_sosvars id with _, a, _, _, _ -> a
    let get_sos_mat  id = match Hashtbl.find hash_sosvars id with _, _, a, _, _ -> a
    let get_sos_var  id = match Hashtbl.find hash_sosvars id with _, _, _, a, _ -> a
    let get_sos_expr id = match Hashtbl.find hash_sosvars id with _, _, _, _, a -> a


      
             

    (* STEP 1: parse the expression and return a cvn polynomial.  *)
    let rec simplify dim expr : t_cvn =
      let zero_c dim = Array.make dim 0 in
      match expr with
      | SOSVar (v, d)  -> [[N.of_int 1,  Some (Vars.SOSVar (v, d))] , zero_c dim]
      | SDPVar v -> [[N.of_int 1,  Some (Vars.SDPVar v)] , zero_c dim]
      | Scalar s -> List.map (fun (n,m) -> [n, None] ,m) s
      | PolyVar _ -> assert false (* todo *)
      | Add (e1, e2) -> cvn_add (simplify dim e1) (simplify dim e2)

      | Sub (e1, e2) -> 
	let minus_one_poly : t_cpoly = (N.of_int (-1), zero_c dim)::[] in
	let minus_e2 = ScalMul(minus_one_poly, e2) in
	simplify dim (Add (e1, minus_e2))

      | ScalMul (s,e) ->
	List.fold_left  (
	  fun res (le, m) ->
	    List.fold_left (
	      fun res' (n1,m') ->
		let prod_m_m' = C.prod m m' in
		cvn_add res' (List.map (fun (n2,m) -> 
		  let le' = List.map (fun (c,v) -> N.mult n1 (N.mult n2 c), v) le in
		  le', m) prod_m_m')
	    ) res s 
	) [] (simplify dim e) 


    (*   STEP 2: convert a cvn into a mvn:

	 CVN -> CMVN -- chaque variable est remplacée par un polynome en M
	 - les scalaires s par s * M(0)
	 - les variables polynomiales par un element de MVN
	 
	 CMVN -> MVN -- on deplie les equations et on recaster en base M
    *)
    let unfold_poly dim cvn =

      let cmvn : t_cmvn = List.map (
	fun (s,m) -> (* s: linear expr over vars, m: a classical monomial *)
	  List.fold_left ( 
	    fun accu (c,v) -> (* c: a numerical coeff, v a variable *)
	      let new_term =
		match v with
		| Some (Vars.SDPVar _) -> [[(c, v)], try M.nth dim 0 with Failure _ -> assert false] (* the constant monomial : 1 *)
		| Some (Vars.SOSVar (id, d)) -> 
		  List.map (
		    fun (v, m) -> 
		      List.map (fun (n,v) -> N.mult c n, v ) v, m
		  ) 
		    (get_sos_expr id)
		| Some (Vars.PolyVar (id, d)) -> assert false (* generer un polynome de degre d 
								 en la base M *)
		| None -> [[c, None], try M.nth dim 0 with Failure _ -> assert false]
	      in
	      mvn_add new_term accu
	  ) [] s,
	  m 
      ) 
	(cvn: t_cvn) (* a polynomial in classical basis, with linear expressions
			over var as coefficients *)
      in

      let mvn, degree = 
	List.fold_left  (
	  fun (res, deg) (s, mc) -> 
	    List.fold_left (
	      fun (res', deg) (expr_v, mm) -> 
		let m_expr = M.var_prod mc mm in
		let new_term, deg' = 
		  List.fold_right (fun (c,m) (accu,deg) -> 
		    (List.map 
		       (fun (c', v) -> N.mult c c', v ) expr_v, m
		    )::accu, max deg (M.deg m)) m_expr ([], deg)
		in
		mvn_add res' new_term , deg'
	    ) (res, deg) s
	) ([], 0) cmvn
      in

      mvn, degree
   

(* TODO 
   each time we introduce (x,[])
   if x is a singleton (coeff, var) we store that var = 0
   and we simplify accu: every occurence of var is replaced by 0. If we obtain a new var = 0, this is removed from accu and stored in the zero table.

   if it is more complex, x is simplidied using the list of zero vars and store into accu
*)
    let sos dim names id expr =
      (* We use two hashtbl: 
	 idtbl: pairid to (c1, c2)
	 var_id: var to pairid list
	 zerotbl: List of var
      *)
      let cpt = ref 0 in
      let idtbl = Hashtbl.create 13 in
      let vartbl = Hashtbl.create 13 in
      let zerotbl = ref [] in

      let clean c = List.filter (fun (_,v) -> not (List.mem v !zerotbl)) c in

      let rec add_zero_var v =
	zerotbl := v :: !zerotbl;
	let ids = Hashtbl.find_all vartbl v in
	(* We remove all v elements of vartbl *)
	List.iter (fun _ -> Hashtbl.remove vartbl v) ids;
	(* We clean and store the constraints *)
	List.iter (fun id ->
	  (* Format.eprintf "add_zero_var id=%i@.@?" id; *)
	  try 
	    let c1, c2 = Hashtbl.find idtbl id in
	    match clean c1, clean c2 with
	      [], [] -> (* this case should not happen *) ()
	    | [_,v'], [] | [], [_, v'] -> (
	    (* this constraint has to be removed and v added to zerotbl *) 
	      Hashtbl.remove idtbl id;
	      add_zero_var v'	      
	    )
	    | l1, l2 -> (* both lists are non empty, we replace id *)
	      Hashtbl.replace idtbl id (l1, l2)
	  with Not_found -> ()
	) ids
      in
      
      let rec add_cons c1 c2 =
	match c1, c2 with
	| [], [_,v] 
	| [_,v], [] -> (* case 1 *)
	  if List.mem v !zerotbl then
	    () (* the constraint is already here *)
	  else  (* We register the variable and clean the existing data *)
	    add_zero_var v
	| [], (_::_ as l)
	| (_::_ as l), [] -> ( (* case 2 *)
	  let l' = clean l in 
	  match l' with
	  | [] -> () (* strange case. Can it happen ? *)
	  | [_,v] -> add_cons l' [] (* case 2.1 *)
	  | _ -> let new_id = incr cpt; !cpt in
		 Hashtbl.add idtbl new_id (l', []); (* the cons l' = 0 is added *)
		 (* a link is stored from each variable v of l' to od *)
		 List.iter (fun (_,v) -> Hashtbl.add vartbl v new_id) l       
	)
	| l1, l2 -> ((* We clean c1 c2 and add it to the hashtbls *)
	  let l1', l2' = clean l1, clean l2 in
	  match l1', l2' with
	  | [], [_] | [_], [] -> add_cons l1' l2' (* -> case 1 *)
	  | [], (_::_ as l') | (_::_ as l'), [] -> add_cons l' [] (* -> case 2.1 *)
	  | _ -> let  new_id = incr cpt; !cpt in
		 Hashtbl.add idtbl new_id (l1', l2'); (* the cons l1' = l2' is added *)
		 (* a link is stored from each variable v of l1' and l2' to id *)
		 List.iter (fun (_,v) -> Hashtbl.add vartbl v new_id) (l1'@l2')      
	)
      in

      let rec build_constraints mvn1 mvn2 =
	  match mvn1, mvn2 with
	| [], [] -> ()
	| (_::_ as l), []
	| [], (_::_ as l) -> List.iter (fun (c,_) -> add_cons c []) l
	| (c1, m1)::tl1, (c2, m2)::tl2 -> (
	  let order = M.cmp m1 m2 in
	  (* Format.eprintf "Comparing %a with %a: %i@." (M.fprintf names) m1 (M.fprintf names) m2 order; *)
	  if order = 0 then (
	    Format.printf "%a = %a@."
	      pp_levarsnum c1 pp_levarsnum c2;
	    add_cons c1 c2;
	    build_constraints tl1 tl2
	  )
	  else if order < 0 then (* m1 < m2 *) (
	    add_cons c1 [];
	    build_constraints tl1 mvn2 
	  )
	  else (
	    add_cons c2 [];
	    build_constraints mvn1 tl2 
	  )
	)
      in
      
      if dim <> Array.length names then
	assert false;
      let cvn = simplify dim expr in 
      Format.eprintf "cvn = %a@." (pp_cvn names) cvn;
      let mvn, degree = unfold_poly dim cvn in
      Format.eprintf "d(mvn)=%i, mvn = %a@." degree (pp_mvn names) mvn;
      Format.eprintf "new sos var: deg=%i, dim=%i@." degree dim;
      let sos_var = new_sos_var id (2*degree) dim in
      let sos_expr : t_mvn = get_sos_expr sos_var in
      let _ = build_constraints mvn sos_expr in
      let constraints = 
	(Hashtbl.fold (fun _ cons accu -> cons::accu) idtbl []) @ 
	  (List.map (fun c -> [N.of_int 1, c], []) !zerotbl)
      in
      List.iter (fun (c1, c2) -> 
	Format.printf "%a = %a@."
	    pp_levarsnum c1 pp_levarsnum c2
      ) constraints;
      sos_var, constraints
   	
   end


(*
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
*)	

    (* get_monomial_cons i svar: returns the constraint expression associated to
       i-th monomial of the base, expressed over the variables of svar. It is an
       expression over svar matrix element. Typically a_jj or 2 * a_ij, but it can
       be a sum for exotic bases like Hermite polynomials.
    *)
(*    let get_monomial_cons idx sos_var =
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

*)      
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
  (*  let sos name expr = 
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
  *)
(*

  (* Tests *)
  module MonBase3 = ClassicalMonomialBasis (* (struct let dim = 3 end)*)
  let nth = MonBase3.nth

  let _ =
    List.iter 
      (fun el -> 
	let dim = 4 in
	MonBase3.fprintf [|"x";"y";"z"|] Format.std_formatter el;
	Format.pp_print_newline Format.std_formatter ()
      ) (List.map (nth 4) [0;1;2;3;4;5;6;7;8;9;10])

*)
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

