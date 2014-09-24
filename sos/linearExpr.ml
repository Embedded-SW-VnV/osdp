open Utils
module Ident = LMI.Ident
open Monomials

exception Incompatible_dim

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
	   | Cst (* homogeneization used to encode affine part *)

  let cmp x y = match x,y with 
    | Cst , (SOSVar _ | PolyVar _ | SDPVar _ ) -> -1 
    | (SOSVar _ | PolyVar _ | SDPVar _ ) , Cst -> 1 
    | _ -> compare x y

  let compare = cmp

  let fprintf fmt v = match v with
    | SOSVar (v, d) -> Format.fprintf fmt "sos(%a)" Ident.fprintf v
    | PolyVar (v, d) -> Format.fprintf fmt "poly(%a)" Ident.fprintf v
    | SDPVar v -> Format.fprintf fmt "%a" LMI.Num_mat.pp_var v
    | Cst -> ()
end
module VarSet = Set.Make (Vars)

module C = ClassicalMonomialBasis 
module H = HermiteMonomialBasis 

(* Polynomial in C *)
type t_cpoly = (N.t * C.t) list

(* Linear expressions over variables in Vars with numerical scalars *)
type t_levarsnum = (N.t * Vars.t) list

let pp_levarsnum fmt c = 
match c with 
| [] -> Format.fprintf fmt "0"
| [n, Vars.Cst] -> N.fprintf fmt n        
| (n1,Vars.Cst)::(n, v)::[] when N.is_zero n1 ->
  Format.fprintf fmt "%a * %a" N.fprintf n Vars.fprintf v      
| _ ->
  Format.fprintf fmt "(%a)"
    (fprintf_list ~sep:" + " 
    (fun fmt (n,v) -> 
      match v with
      | Vars.Cst -> N.fprintf fmt n 
      | _ -> if N.compare (N.of_int 1) n = 0 then
	  Vars.fprintf fmt v 
	else 
	  Format.fprintf fmt "%a * %a" N.fprintf n Vars.fprintf v
    )) c
    
(* Polynomials in the classical basis with scalar in LEVarsNum *)
type t_cvn = (t_levarsnum * C.t) list

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

let pp_cvn ?(names=None) = pp_xvn (C.fprintf ~names:names)

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

  let pp_mvn ?(names=None) = pp_xvn (M.fprintf ~names:names)
    
  (* Polynomials in the classical basis with scalar in MVN *)
  (* module CMVN = PolyLinExpr (MVN) (C)  *)
  type t_cmvn = (t_mvn * C.t) list


  (* Generic functions to ease the addition of polynomials, using merge_sorted_lists *)
  let cmp f = fun (_,x) (_,y) -> f x y 
  let add isz fadd = fun (s1,x) (s2,_) -> let s = fadd s1 s2 in if isz s then None else Some (s, x)

  (* Additions of polynomials: Instanciated functions *)
  let levarsnum_ext_mult s l =
    if N.is_zero s then [] 
    else 
      List.map (fun (c, v) -> N.ext_mult s c, v ) l

  let levarsnum_add = merge_sorted_lists (cmp (compare)) (add N.is_zero N.add)
  let levarsnum_sub l1 l2 =
    let l2' = levarsnum_ext_mult (N.of_int (-1)) l2 in
    levarsnum_add l1 l2'

  let levarsnum_is_zero = (=) []
  let levarsnum_get_vars l = 
    List.fold_left (fun accu (_,v) -> VarSet.add v accu) VarSet.empty  (List.tl l)

  let mvn_add l1 l2 = 
    let res = merge_sorted_lists (cmp M.cmp) (add levarsnum_is_zero levarsnum_add) l1 l2 in
    (* Format.eprintf "Merging l1 with l2@.l1 = %a@.l2=%a@.ress=%a@.@?" *)
    (*   (pp_mvn ~names:None) l1 (pp_mvn ~names:None) l2 (pp_mvn ~names:None) res; *)
    res

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

    (* Set of functions to accumulate linear constraints among vars *)
    module VNConstraints =
    struct
      
      (* We use two hashtbl: 
	 idtbl: pairid to (c1, c2)
	 var_id: var to pairid list
	 zerotbl: List of var
      *)
      let deftbl: (Vars.t, t_levarsnum) Hashtbl.t = Hashtbl.create 13 
      let varusetbl: (Vars.t, Vars.t list) Hashtbl.t = Hashtbl.create 13 
      let zerotbl = ref [] 

      let print_summary fmt = 
	Format.fprintf fmt "def: @[<v>";
	Hashtbl.iter (fun v e -> Format.fprintf fmt "%a = %a@ " Vars.fprintf v pp_levarsnum e) deftbl;
	Format.fprintf fmt "@]@.";
	Format.fprintf fmt "varuse: @[<v>";
	Hashtbl.iter (fun v vl -> Format.fprintf fmt "%a used in %a@ " Vars.fprintf v (fprintf_list ~sep:", " Vars.fprintf) vl) varusetbl;
	Format.fprintf fmt "@]@.";
	Format.fprintf fmt "zero vars: [%a]@." (fprintf_list ~sep:", " Vars.fprintf) !zerotbl


      let is_registered v = Hashtbl.mem deftbl v

      let get_def v = try Hashtbl.find deftbl v with Not_found -> assert false
      let register_def v e = Hashtbl.replace deftbl v e 
      let del_def = Hashtbl.remove deftbl

      let get_uses v = try Hashtbl.find varusetbl v with Not_found -> []

      let register_use v v' = 
	if Hashtbl.mem varusetbl v then
	  let uses = get_uses v in 
	  (if not (List.mem v' uses) then Hashtbl.replace varusetbl v (v'::uses) ) 
	else
	  Hashtbl.add varusetbl v [v']

      let del_use v v' = 
	Hashtbl.replace varusetbl v (List.filter (fun var -> Vars.cmp var v' <> 0) (get_uses v))

      let clean c = 
	List.filter (fun (_,v) -> not (List.mem v !zerotbl)) c 
      
      let rec add_zero_var v =
	if List.mem v !zerotbl then
	    () (* the constraint is already here *)
	  else (
	    (* We register the variable and clean the existing data *)
	    zerotbl := v :: !zerotbl;
	    List.iter (fun v' -> 
	      del_use v v';
	      let v'_l = clean (get_def v') in
	      del_def v';
	      add_def_var v' v'_l
	    ) (get_uses v)
	  );
	(* Format.eprintf "added zero var: %a@.%t@." Vars.fprintf v print_summary; *)
	()

      and add_def_var v l =
	register_def v l;
	(* Variables of l are declared as being used in v *)
	List.iter (fun (_,v') -> match v' with Vars.Cst -> () | _ -> register_use v' v) l;
	(* for all variable v' < v, we substitute occurence of v by l *)
	List.iter (
	  fun v' -> 
	    if Vars.cmp v' v < 0 then (
	      del_use v v';
	      (* we replace (n,v) by n * l in definion of v' *)
	      let v'_l = List.fold_left (
		fun accu ((n',v'') as e) -> 
		     if Vars.cmp v'' v = 0 then
		       levarsnum_add (levarsnum_ext_mult n' l) accu
		     else
		       levarsnum_add [e] accu
	      ) [] (get_def v') in
	      del_def v';
	      add_def_var v' v'_l
	    )
	) (get_uses v)


	(*   (\* Since we want now to replace v by l in all existing definitions, *)
	(*      we obtain every use of v *\) *)
	(*   let defs_to_update = get_uses v in *)
	(*   (\* For each of them, we subsitute v by l and delete the link. If the *)
	(*      resulting expression is empty, the variable is declared as zero *\) *)
	(*   List.iter (fun v' -> *)
	(*     let v'_l = get_def v' in *)
	(*     let vars_v'_l = levarsnum_get_vars v'_l in *)
	(*     let coeff_v, _ = List.find (fun (coeff, variable) -> (Vars.cmp variable (Some v)) = 0) v'_l in  *)
	(*     let new_l = levarsnum_add v'_l (levarsnum_ext_mult coeff_v ((N.of_int (-1), Some v)::l)) in *)
	(*     let vars_new_l = levarsnum_get_vars v'_l in *)
	(*     (\* Any variables that disappeard should be removed from the vartbl to v' *\) *)
	(*     VarSet.iter (fun v'' -> del_use v'' v') (VarSet.diff vars_v'_l vars_new_l);	       *)
	(*     match new_l with  *)
	(*     | [] -> add_zero_var v' *)
	(*     | _ -> register_def v' new_l *)
	(*   ) defs_to_update  *)
	(* ); *)
	    ;
	(* Format.eprintf "added def var: %a = %a@.%t@." Vars.fprintf v pp_levarsnum l print_summary; *)
	()

      and add_cons c1 c2 =
	(* Format.eprintf "adding cons: %a = %a@.%t@." pp_levarsnum c1  pp_levarsnum c2 print_summary; *)

	let cl = clean(levarsnum_sub c1 c2) in
	let cl = List.fold_left (fun accu (n,v) ->
	  if is_registered v then
	    let vl = get_def v in
	    levarsnum_add (levarsnum_ext_mult n vl) accu
	  else
	    levarsnum_add [n,v] accu
	) [] cl 
	in
	(* Format.eprintf "sub %a@." pp_levarsnum cl; *)
	match cl with 
	| [] -> assert false (* correspond to 0 = 0. I think it should not happen *)
	| [_, Vars.Cst] -> assert false (* should really never happen, ie. 3 = 0 *)
	| [_, v] -> add_zero_var v

	| ((_, Vars.Cst) as cst)::(n,v)::tl -> (* Affine expression, special treatment for cst *)
	  let l' = levarsnum_ext_mult (N.div (N.of_int (-1)) n) (cst::tl) in
	  add_def_var v l'

	| (n,v)::l -> (* Linear expression, no affine part *)
	  let l' = levarsnum_ext_mult (N.div (N.of_int (-1)) n) l in
	  add_def_var v l'	 

      let get_cons () = 
	(Hashtbl.fold (fun var expr accu -> (levarsnum_add [N.of_int (-1), var] expr)::accu) deftbl []) @ 
	  (List.map (fun c -> [N.of_int 1, c]) !zerotbl)
      
    end   

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

