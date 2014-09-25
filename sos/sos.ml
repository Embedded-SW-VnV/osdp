(*
Take a list of expressions that have to be SOS
Each  expression is expressed over sos_unknown of fixed degree in a provided monomial basis
It returns
- a set of sdp vars
- a list of constraints of those
- a list of mapping from sdp vars to SOS

each sos unknown is expressed as a SOS over the monomial basis. 
on deplie les equations sur nos variables SOS : base^t P base 
et on reconstruit une version SOS sur la base

on a besoin pour ca de structures de donnee efficaces pour 
- etant donne un monome, savoir l'exprimer dans la base SOS, par ex. comme expression des variables sdp: monome en x = 2*b, donc si on cree le monome avec le coeff 2: 2*x, on doit avoir b = 1
- etant donne deux expressions, calculer le produit des deux: savoir efficace le deplier en monomes et reidentifier les variables sdp associées.
- si la base en exotique, ca peut demander de renormaliser pour retomber dans la base

it requires a hashtbl from monomial to set of indices ...
*)

let sos_to_sdp expressions vars degree basis = ()


(*
dans une matrice M sym matrix of dim n *n, appliqué à une base monomiale B^n
le coeff associé au monome x_a * x_b où x_a,x_b \in B^n
est:
si exist x_c \in B^n et k \in [1,n], tq x_a * x_b = x_c^k
alors coeff(x_a * x_b] = [M()
*)



(*
on a deux polynome

*)


module Make = functor (M: MONOMIAL_BASIS) ->
struct
  module M = M

  (*******************************************************************************)
  (*                                                                             *)
  (*                                                                             *)
  (*                                                                             *)
  (*                                                                             *)
  (*                                                                             *)
  (*                                                                             *)
  (*******************************************************************************)
module C = ClassicalMonomialBasis 
module H = HermiteMonomialBasis 
module LEV = LinearExprVars.Make (N)

(* Polynomial in C *)
type t_cpoly = (N.t * C.t) list
    
(* Polynomials in the classical basis with scalar in LEVarsNum *)
type t_cvn = (LEV.t_levarsnum * C.t) list


let pp_cvn ?(names=None) = pp_xvn (C.fprintf ~names:names)

  (* Polynomials in base M with scalar in LEVarsNum *)
  (* module MVN = PolyLinExpr (LEVarsNum) (M) *)
  type t_mvn = (LEV.t_levarsnum * M.t) list 

  let pp_mvn ?(names=None) = pp_xvn (M.fprintf ~names:names)
    
  (* Polynomials in the classical basis with scalar in MVN *)
  (* module CMVN = PolyLinExpr (MVN) (C)  *)
  type t_cmvn = (t_mvn * C.t) list


  let mvn_add l1 l2 = 
    let res = merge_sorted_lists (cmp M.cmp) (add levarsnum_is_zero levarsnum_add) l1 l2 in
    (* Format.eprintf "Merging l1 with l2@.l1 = %a@.l2=%a@.ress=%a@.@?" *)
    (*   (pp_mvn ~names:None) l1 (pp_mvn ~names:None) l2 (pp_mvn ~names:None) res; *)
    res

  let cvn_add = merge_sorted_lists (cmp C.cmp) (add levarsnum_is_zero levarsnum_add) 
    
end



(* Type of expressions: it should be linear in SOSVar, SDPVar, PolyVar *)
type expr = | SOSVar of Ident.t * int (* SOSVar(id, d) : Positive Polynomial variable 
					 id of degree d *)
	    | PolyVar of Ident.t * int 
	    | SDPVar of LMI.Num_mat.var
      	    | Scalar of t_cpoly (* Polynomial with known coefficients, no free (sdp) vars *)
	    | Add of expr * expr 
	    | Sub of expr * expr
	    | ScalMul of t_cpoly * expr



   (*******************************************************************************)
   (*                                                                             *)
   (*                                                                             *)
   (*                                                                             *)
   (*                                                                             *)
   (*                                                                             *)
   (*                                                                             *)
   (*******************************************************************************)

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
    
let new_sos_var name deg dim : Ident.t * expr =
  let id = Ident.create name in
  let deg_monomials = M.get_sos_base_size dim deg in
  Format.eprintf "nb element base monomiale: %i@." deg_monomials;
  let vars = LMI.Num_mat.vars_of_sym_mat id deg_monomials in
  Format.eprintf "nb vars: %i@." (List.length vars);
  let expr : t_mvn = 
    List.fold_left  
      (fun res v -> 
	mvn_add res 
	  (match LMI.Num_mat.get_var_indices v with 
	  | Some (i, j) -> 
	    let v = Vars.SDPVar v in
	    let monomials = try M.prod (M.nth dim i) (M.nth dim j) with Failure _ -> assert false in
	    List.map
	      (fun (coeff, m) -> 
		if i = j then
		  [coeff, v],m 
		else
		  ([N.mult (N.of_int 2) coeff, v], m)
	      ) 
	      monomials
	  | _ -> assert false)
	  
      ) [] vars 
  in
  let build_poly elems = LMI.Num_mat.sym_mat_of_var deg_monomials elems in
  let unknown_sdp = LMI.Num_mat.symmat (id, deg_monomials) in
  Hashtbl.add hash_sosvars id (deg_monomials, vars, build_poly, unknown_sdp, expr);
  id, SOSVar(id, dim)
    
let get_sos_dim  id = match Hashtbl.find hash_sosvars id with a, _, _, _, _ -> a
let get_sos_vars id = match Hashtbl.find hash_sosvars id with _, a, _, _, _ -> a
let get_sos_mat  id = match Hashtbl.find hash_sosvars id with _, _, a, _, _ -> a
let get_sos_var  id = match Hashtbl.find hash_sosvars id with _, _, _, a, _ -> a
let get_sos_expr id = match Hashtbl.find hash_sosvars id with _, _, _, _, a -> a


  
    (*******************************************************************************)
    (*                                                                             *)
    (*                                                                             *)
    (*                                                                             *)
    (*                                                                             *)
    (*                                                                             *)
    (*                                                                             *)
    (*******************************************************************************)
  

    (* STEP 1: parse the expression and return a cvn polynomial.  *)
let rec simplify dim expr : t_cvn =
  let zero_c dim = Array.make dim 0 in
  match expr with
  | SOSVar (v, d)  -> [[N.of_int 1,  Vars.SOSVar (v, d)] , zero_c dim]
  | SDPVar v -> [[N.of_int 1,  Vars.SDPVar v] , zero_c dim]
  | Scalar s -> List.map (fun (n,m) -> [n, Vars.Cst] ,m) s (* Constant polynomial in C *)
  | PolyVar _ -> assert false (* todo *)
  | Add (e1, e2) -> cvn_add (simplify dim e1) (simplify dim e2)

  | Sub (e1, e2) -> 
    let minus_one_poly : t_cpoly = (N.of_int (-1), zero_c dim)::[] in
    let minus_e2 = ScalMul(minus_one_poly, e2) in
    simplify dim (Add (e1, minus_e2))

  | ScalMul (s,e) ->
    List.fold_left  (
      fun res (le, m) ->
	if C.dim m <> dim then raise Incompatible_dim;
	List.fold_left (
	  fun res' (n1,m') ->
	    if C.dim m' <> dim then raise Incompatible_dim;
	    let prod_m_m' = C.prod m m' in
	    cvn_add res' (List.map (fun (n2,m) -> 
	      let le' = levarsnum_ext_mult (N.mult n1 n2) le in
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
      List.fold_right ( 
	fun (c,v) accu -> (* c: a numerical coeff, v a variable *)
	  let new_term =
	    match v with
	    | Vars.SDPVar _ -> [[(c, v)], try M.nth dim 0 with Failure _ -> assert false] (* the constant monomial : 1 *)
	    | Vars.SOSVar (id, d) -> 
	      List.map (fun (v, m) -> levarsnum_ext_mult c v, m) (get_sos_expr id)
	    | Vars.PolyVar (id, d) -> assert false (* TODO generer un polynome de degre d 
						      en la base M *)
	    | Vars.Cst -> [[c, Vars.Cst], try M.nth dim 0 with Failure _ -> assert false]
	  in
	  mvn_add new_term accu
      ) s [],
      m 
  ) 
    (cvn: t_cvn) (* a polynomial in classical basis, with linear expressions
		    over var as coefficients *)
  in

  let mvn, degree = 
    List.fold_right  (
      fun (s, mc) (res, deg) -> 
	List.fold_right (
	  fun (expr_v, mm) (res', deg) -> 
	    let m_expr = M.var_prod mc mm in
	    let new_term, deg' = 
	      List.fold_right (fun (c,m) (accu,deg) -> 
		(levarsnum_ext_mult c expr_v, m
		)::accu, max deg (M.deg m)) m_expr ([], deg)
	    in
	    mvn_add res' new_term , deg'
	) s (res, deg) 
    ) cmvn ([], 0) 
  in

  mvn, degree


    (* TODO 
       each time we introduce (x,[])
       if x is a singleton (coeff, var) we store that var = 0
       and we simplify accu: every occurence of var is replaced by 0. If we obtain a new var = 0, this is removed from accu and stored in the zero table.

       if it is more complex, x is simplidied using the list of zero vars and store into accu
    *)
let sos ?(names=None) dim id expr =

  let rec build_constraints mvn1 mvn2 =
    match mvn1, mvn2 with
    | [], [] -> ()
    | (_::_ as l), []
    | [], (_::_ as l) -> List.iter (fun (c,_) -> VNConstraints.add_cons c []) l
    | (c1, m1)::tl1, (c2, m2)::tl2 -> (
      let order = M.cmp m1 m2 in
	  (* Format.eprintf "Comparing %a with %a: %i@." (M.fprintf names) m1 (M.fprintf names) m2 order; *)
      if order = 0 then (
	    (* Format.eprintf "%a = %a@." *)
	    (*   pp_levarsnum c1 pp_levarsnum c2; *)
	VNConstraints.add_cons c1 c2;
	build_constraints tl1 tl2
      )
      else if order < 0 then (* m1 < m2 *) (
	VNConstraints.add_cons c1 [];
	build_constraints tl1 mvn2 
      )
      else (
	VNConstraints.add_cons c2 [];
	build_constraints mvn1 tl2 
      )
    )
  in
  let _ = 
    match names with 
    | Some a -> if dim <> Array.length a then assert false
    | _ -> ()
  in
  
  let cvn = simplify dim expr in 
      (* Format.eprintf "cvn = %a@." (pp_cvn ~names:names) cvn; *)
  let mvn, degree = unfold_poly dim cvn in
      (* Format.eprintf "d(mvn)=%i, mvn = %a@." degree (pp_mvn ~names:names) mvn; *)
  Format.eprintf "new sos var: deg=%i, dim=%i@." degree dim;
  let sos_var = new_sos_var id degree dim in
  let sos_expr : t_mvn = get_sos_expr sos_var in
      (* Format.eprintf "sos expr mvn = %a@." (pp_mvn ~names:names) sos_expr; *)
  let _ = build_constraints mvn sos_expr in
  let constraints = VNConstraints.get_cons () in
      (* List.iter (fun c ->  *)
      (* 	Format.printf "%a = 0@." pp_levarsnum c *)
      (* ) constraints; *)
  sos_var, constraints
