module Ident =
struct
  type t = int

  let cpt = ref 0
  let name_hash: (string, int) Hashtbl.t = Hashtbl.create 11
  let id_hash: (int, string) Hashtbl.t = Hashtbl.create 11

  let rec create name : int=
    if Hashtbl.mem name_hash name then
      create (name ^ "_")
    else (
      incr cpt; 
      let id = !cpt in
      Hashtbl.add id_hash id name;
      Hashtbl.add name_hash name id;
      id
    )

  let get_name = Hashtbl.find id_hash 
  let find_id = Hashtbl.find name_hash 
  let fprintf fmt id = Format.fprintf fmt "%s" (get_name id)
  let cmp = compare
end

type objKind = Minimize | Maximize
type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

type v_t = Var of Ident.t * int * int | ScalId of Ident.t 

let pp_var fmt v = 
  match v with
    Var (s, i, j) -> Format.fprintf fmt "%a_%i_%i" Ident.fprintf s (i+1) (j+1)
  | ScalId s -> Ident.fprintf fmt s


module type Sig = 
sig
  module Mat: Matrix.S
  type matrix_expr 
  type lmi_obj_t = objKind * (v_t * Mat.elt) list
  type var = v_t
  val pp_matrix_expr : Format.formatter -> matrix_expr -> unit
  val pp_var : Format.formatter -> var -> unit
  val zeros : int -> int -> matrix_expr
  val eye: int -> matrix_expr
  val diag: matrix_expr list -> matrix_expr 
  val const_mult: Mat.elt -> matrix_expr -> matrix_expr
  val scal_mult: v_t -> matrix_expr -> matrix_expr
  val symmat: Ident.t * int ->  matrix_expr
  val const_mat: Mat.t -> matrix_expr
  val trans_const_mat: Mat.t -> matrix_expr
  val kronecker_sym: ?lift:(Mat.t -> Mat.t) -> int -> int -> int -> matrix_expr
  val add: matrix_expr -> matrix_expr -> matrix_expr 
  val sub: matrix_expr -> matrix_expr -> matrix_expr 
  val mult: matrix_expr -> matrix_expr -> matrix_expr
  val of_array_array: matrix_expr array array -> matrix_expr
  val sym_mat_of_var: int -> Mat.elt list -> Mat.t
  val vars_of_sym_mat: Ident.t -> int -> var list 
  val solve: v_t list list -> matrix_expr list -> lmi_obj_t -> float * (v_t (*Ident.t*) * (Mat.elt, Mat.t) value_t) list option
  val get_var_id: var -> Ident.t
  val get_var_indices: var -> (int * int) option
end 

module IdentSet = 
struct
  include Set.Make (struct type t = Ident.t let compare = Ident.cmp end)
  let pp fmt s = Utils.fprintf_list ~sep:", " Ident.fprintf fmt (elements s)
end


module VarSet = 
struct
  include Set.Make (struct type t = v_t let compare = compare end)
  let pp fmt s = Utils.fprintf_list ~sep:", " pp_var fmt (elements s)
end


module VarSetSet = 
struct
  include Set.Make (struct type t = VarSet.t let compare = compare end)
  let pp fmt s = Utils.fprintf_list ~sep:"; " VarSet.pp fmt (elements s)
end

module IdentDimSet = 
  Set.Make (struct type t = Ident.t * int let compare = compare end)



module Make = functor (Mat: Matrix.S) -> 
struct
  type elt = Mat.elt
  module Mat = Mat

  type var = v_t

  type scalar_t = Cst of elt | SIdent of Ident.t | MVar of Ident.t * int * int


(* Unknown matrices are always symetrical *)
  type matrix_expr = 
  | MatMult of matrix_expr * matrix_expr
  | MatAdd of matrix_expr * matrix_expr
  | MatSub of matrix_expr * matrix_expr
  | MatScalMult of scalar_t * matrix_expr
  | MatCst of Mat.t
  | MatVarSym of Ident.t * int 
  | BlockLMI of matrix_expr array array * (int * int array * int * int array)

(** Objective to be maximized by the solver: None denotes no objective, Some
    (positive_sign, v) denotes the value v if positive_sign is true, -v
    otherwise. *)
type lmi_obj_t = objKind * (v_t * Mat.elt) list

let rec get_dim lmi =
  match lmi with
  | MatMult (e1, e2) -> 
    let n1, _ = get_dim e1 in
    let _, m2 = get_dim e2 in
    n1, m2

  | MatAdd (e1, _) | MatSub (e1, _) 
  | MatScalMult (_, e1) -> get_dim e1
  | MatCst mat -> Mat.nb_lines mat, Mat.nb_cols mat
  | MatVarSym (id, dim) -> dim, dim
  | BlockLMI (_, (n, _, m, _)) -> n, m
    
let pp_scalar fmt s =
match s with
| Cst f -> Mat.Elem.pp fmt f
| SIdent id -> Ident.fprintf fmt id
| MVar (id, i, j) -> Format.fprintf fmt "%a(%i,%i)" Ident.fprintf id i j

let rec pp_matrix_expr fmt e =
  let pp = pp_matrix_expr in
  let pp_paren fmt e =
    match e with 
    | MatAdd _ | MatSub _ -> 
      Format.fprintf fmt "(%a)" pp e
    | _ -> pp fmt e
  in
  match e with
  | MatMult (e1, e2) ->
    Format.fprintf fmt "%a %a" pp_paren e1 pp_paren e2
  | MatAdd (e1, e2) -> 
    Format.fprintf fmt "%a + %a" pp e1 pp e2
  | MatSub (e1, e2) -> 
    Format.fprintf fmt "%a - %a" pp e1 pp e2
  | MatScalMult (s, e1) -> 
    Format.fprintf fmt "%a %a" pp_scalar s pp_paren e1
  | MatCst mat -> Mat.pp_matrix fmt mat
  | MatVarSym (id, dim) -> Ident.fprintf fmt id
  | BlockLMI (a, _) -> 
    Format.fprintf fmt "@[<v>[ %a ]@]" 
      (Utils.fprintf_list ~sep:";@ " 
	 (fun fmt a_i -> 
	   Utils.fprintf_list ~sep:", @  " pp fmt (Array.to_list a_i)))
      (Array.to_list a)

let pp_var = pp_var

let rec get_mat_var_sym e =
  match e with
  | MatMult (e1, e2) 
  | MatAdd (e1, e2) 
  | MatSub (e1, e2) -> 
    IdentDimSet.union (get_mat_var_sym e1) (get_mat_var_sym e2)
  | MatScalMult (s, e1) -> (get_mat_var_sym e1) 
  | MatCst _ -> IdentDimSet.empty
  | MatVarSym (id, dim) -> IdentDimSet.singleton (id, dim)
  | BlockLMI (a, _) -> 
    List.fold_left (
      fun accu a_i ->
	List.fold_left (fun accu2 e -> IdentDimSet.union (get_mat_var_sym e) accu2) 
	  accu 
	  (Array.to_list a_i)
    ) IdentDimSet.empty (Array.to_list a)
(*

let rec get_scal_var e =
  match e with
  | MatMult (e1, e2) 
  | MatAdd (e1, e2) ->     IdentSet.union (get_scal_var e1) (get_scal_var e2)
  | MatSub (e1, e2) ->     assert false
  | MatScalMult (SIdent s, e1) -> IdentSet.add s (get_scal_var e1) 
  | MatScalMult (_, e1) -> get_scal_var e1
  | MatCst _ ->            IdentSet.empty
  | MatVarSym (id, dim) -> assert false (* Should not exist anymore when this function is called *)
*)

(*
let rec get_vars e =
  match e with
  | MatMult (e1, e2) 
  | MatAdd (e1, e2) 
  | MatSub (e1, e2) ->     IdentDimSet.union (get_vars e1) (get_vars e2)
  | MatScalMult (SIdent s, e1) -> IdentDimSet.add (s,0) (get_vars e1) 
  | MatScalMult (MVar (id, i, j), e1) -> IdentDimSet.add (idxxxx,0) (get_vars e1) 
  | MatScalMult (_, e1) -> get_vars e1
  | MatCst _ ->            IdentDimSet.empty
  | MatVarSym (id, dim) -> IdentDimSet.singleton (id, dim)
  | BlockLMI (a, _) -> 
    List.fold_left (
      fun accu a_i ->
	List.fold_left (fun accu2 e -> IdentDimSet.union (get_vars e) accu2) 
	  accu 
	  (Array.to_list a_i)
    ) IdentDimSet.empty (Array.to_list a)
*)

let new_var id i j = Var (id, i, j)
let get_var id i j = if j <= i then Var(id, i, j) else Var(id, j, i) 
let new_var_name v = match v with | Var (id, i, j) -> (Ident.get_name id) ^ "_" ^ string_of_int (i+1) ^ "_" ^ string_of_int (j+1) | ScalId s -> Ident.get_name s

(* A symetrical matrix of dim n x n is characterized by (n^2 + n)/2 variables *)
let vars_of_sym_mat id dim = 
  let res = 
  let rec aux i j =
    let new_v = new_var id i j in
    let tail = 
      if i = dim-1 && j = dim-1 then 
	[] 
      else if j = dim-1 then
	aux (i+1) (i+1)
      else
	aux i (j+1)
    in
    new_v :: tail
  in
  aux 0 0 

  in
  (* Report.debugf ~kind:"sdp" 
	  (fun fmt -> "Vars of sym mat: %s (dim %i) -> [%a]@.@?" id dim (Utils.fprintf_list ~sep:", " (fun fmt v -> Format.pp_print_string fmt (new_var_name v) )) res); *)
  res

(* The nth variable in list when matrix is of size dim, characterize the element
   (i,j) and (j,i) where i=

   size=3, list=[a,b,c,d,e,f]
   0 -> (0,0)
   1 -> (0,1) and (1,0)
   2 -> (0,2) and (2,0)
   3 -> (1,1) 
   4 -> (1,2) and (2,1)
   5 -> (2,2)
*)
let sym_mat_of_var dim list =
  let expected_length = (dim*dim + dim)/2 in
  if List.length list != expected_length then
    raise (Failure ("Invalid input list for sym_mat_of_var: expecting " 
		    ^ string_of_int expected_length ^ ", we have " 
		    ^ string_of_int (List.length list)));

  let new_mat = Array.make_matrix dim dim (Mat.Elem.of_int 0) in
  let i = ref 0 and j = ref 0 in
  List.iter (fun elem -> 
    (* Report.debugf ~kind:"sdp" 
	  (fun fmt -> "%i, %i@.@?" !i !j); *)
    if !i > dim then assert false;
    (if !i = !j then
	new_mat.(!i).(!j) <- elem
     else
	(new_mat.(!i).(!j) <- elem;
	 new_mat.(!j).(!i) <- elem)
    );
    (* Incrementing i and j *)
    if !j = dim-1 then
      (incr i; j := !i)
    else
      (incr j)
  ) list;
  Mat.matrix_of_array_array new_mat
    
let kronecker_sym ?lift:(lift=fun x -> x) dim i j =
  (* Report.debugf ~kind:"sdp" 
	  (fun fmt -> "kro: %i %i %i@.@?" dim i j); *)
    MatCst (lift (Mat.kronecker_sym_matrix dim i j))


(* The algorithm is the follow: 
   - we iterate (down) through the AST to replace unknown
     matrices by sum of (unknown scalar) times E_ij 
     ( zeros everywhere except in (i,j) )
   - we iterate (up) from the leaves to the root to extract 
     unknown scalar and evaluate (eval) the constant parts 
   - when block matrices are met through this second 
     phase, the same up algo is applied on each block 
     before it is replaced by a sum of lifted blocks 
*)
let rewrite_mat_as_scalar matrix_expr =

  let rec eval e =
    let res = 
      match e with
    | MatMult (e1, e2) -> (
      match e1, e2 with
      | MatCst m1, MatCst m2 -> 
	MatCst (Mat.mult_matrix m1 m2)
      | _ -> e
    )
    | MatAdd (e1, e2) -> (
      match e1, e2 with
      | MatCst m1, MatCst m2 -> 
	MatCst (Mat.add_matrix m1 m2)
      | _ -> e
    )
    | MatScalMult (s, e1) -> (
      match s, e1 with
      | Cst c, MatCst m1 -> 
	MatCst (Mat.mult_scalar_matrix c m1)
      | _ -> e
    )
    | MatCst x -> MatCst x

    | MatSub _
    | MatVarSym _ 
    | BlockLMI _ -> assert false (* Should be removed now *)
    in
    (* Format.eprintf "Evaluating expr %a = %a@."  *)
    (*   pp_matrix_expr e *)
    (*   pp_matrix_expr res; *)
    
    res
  in      

  let var_to_kronecker dim v = 
    match v with Var (id, i, j) -> 
      MatScalMult (MVar(id, i, j), (try kronecker_sym dim i j with Failure id -> raise (Failure id))) 
    | ScalId _ -> assert false
    
  in

  let rec down e = 
    match e with
    | MatMult (e1, e2) -> MatMult (down e1, down e2) 
    | MatAdd (e1, e2) -> MatAdd  (down e1, down e2)
    | MatSub (e1, e2) -> MatAdd (down e1, MatScalMult (Cst (Mat.Elem.of_int (-1)), down e2))
    | MatScalMult (s, e1) -> (
      let res = MatScalMult (s, down e1) in
     (* ( match s with *)
     (* 	SIdent "lambda_1" -> Format.eprintf "down LAMBDA1 * %a@." pp_matrix_expr res *)
     (*  | _ -> ()); *)
      res)
    | MatCst x -> MatCst x
    | MatVarSym (id, dim) -> (
      let vars = try vars_of_sym_mat id dim with _ -> assert false in
      match vars with
      | [] -> assert false
      | [ v ] -> (try( var_to_kronecker dim v) with _ -> assert false)
      | hd:: tl -> 
	List.fold_left 
	  (fun accu v -> MatAdd (accu, try var_to_kronecker dim v with _ -> assert false)) 
	  (try var_to_kronecker dim hd with Failure id -> raise (Failure id)) 
	  tl 
    )  
    | BlockLMI (a, (n, dim_lines, m, dim_cols)) -> (
      (* Full copy of matrix a and apply down on each element *)
      let copy_a = Array.copy a in
      for i = 0 to Array.length a -1 do
	copy_a.(i) <- Array.copy copy_a.(i) ;
	for j = 0 to Array.length copy_a.(i) - 1 do 
	  copy_a.(i).(j) <- down a.(i).(j);
	done
      done;
      BlockLMI(copy_a, (n, dim_lines, m, dim_cols))
    )
      
  in

  let up e =

    let lift i j (n, dim_lines, m, dim_cols) =
      let pos_line = Array.fold_left (+) 0 (try Array.sub dim_lines 0 i with _ -> assert false) in
      let pos_col = Array.fold_left (+) 0 (try Array.sub dim_cols 0 j with _ -> assert false) in
      fun mat -> 
	match mat with 
	  MatCst mat -> MatCst (Mat.lift_block mat n m pos_line pos_col)
	| _ -> assert false 
    in

    (* let flatten_block a (n, dim_lines, m, dim_cols) =  *)
    (*   let flatten = ref [] in *)
    (*   for i = 0 to (Array.length dim_lines) - 1 do *)
    (* 	for j = 0 to (Array.length dim_cols) - 1 do  *)
    (* 	  try	     *)
    (* 	    let pos_line = Array.fold_left (+) 0 (try Array.sub dim_lines 0 i with _ -> assert false) in *)
    (* 	    let pos_col = Array.fold_left (+) 0 (try Array.sub dim_cols 0 j with _ -> assert false) in *)
    (* 	    let lift_fun mat = lift (Mat.lift_block mat n m pos_line pos_col (\* with _ -> raise (Failure "coucou") *\) ) in *)
    (* 	    let a_i_j = down ~lift:(lift_fun) a.(i).(j) in *)
    (* 	    flatten := a_i_j :: !flatten *)
    (* 	  with Invalid_argument _ ->  assert false	done; *)
    (*   done; *)
    (*   match !flatten with *)
    (*   | [] -> assert false *)
    (*   | [e] -> e *)
    (*   | hd::tl -> List.fold_left (fun accu e -> MatAdd (e, accu)) hd tl *)
    (* in *)

    let rec up e =
      let r = 
      match e with
      | MatMult (e1, e2) -> (
	 (* Report.debugf ~kind:"sdp" 
	  (fun fmt -> "up mat mult %a  %a@.@?" pp_matrix_expr e1 pp_matrix_expr e2);  *)
	match up e1, up e2 with
	| [v1, e1'], [v2, e2'] -> [VarSet.union v1 v2 , eval (MatMult (e1', e2'))]
	| [v, e], (_::_ as l) when VarSet.is_empty v ->
 	  List.map (fun (vs, e') -> vs, eval (MatMult (e, e'))) l
	| (_::_ as l), [v, e] when VarSet.is_empty v -> 
	  List.map (fun (vs, e') -> vs, eval (MatMult (e', e))) l
	| _ -> assert false (* Non linear expression *)
      )
      | MatAdd (e1, e2) -> combine_additive_op (up e1) (up e2)
      | MatScalMult (s, e1) -> (
	let up_e1 = up e1 in
	
	(* (match s with *)
	(*   SIdent "lambda_1" -> Format.eprintf "UP LAMBDA1 * %a@."    *)
	(*     (Utils.fprintf_list ~sep:"; " (fun fmt (vs, e) -> Format.fprintf fmt "%a: %a" VarSet.pp vs pp_matrix_expr e) ) *)
	(*     up_e1 *)
	(* | _ -> ()); *)
	    


	 (* Report.debugf ~kind:"sdp" 
	  (fun fmt -> "up mat scal mult %a  %a@.@?" pp_scalar s pp_matrix_expr e1) ; *)
	   
	 
	  match s, up_e1 with 
	  | SIdent s, [v, e'] when VarSet.is_empty v -> 
	    [VarSet.singleton (ScalId s), e']
	  | MVar (s,i,j), [v, e'] when VarSet.is_empty v ->
	    [VarSet.singleton (Var (s,i,j)), e']
	  | SIdent _,( _::_ as vmap)
	  | MVar _, (_::_ as vmap) -> (
	    Report.debugf ~kind:"sdp" 
	      (fun fmt -> Format.fprintf fmt "Non linear expression: %a * [%a]"
	      pp_scalar s
	      (Utils.fprintf_list ~sep:"; " (fun fmt (vs, e) -> Format.fprintf fmt "%a: %a" VarSet.pp vs pp_matrix_expr e) )
	      (List.filter (fun (vs, _) -> not (VarSet.is_empty vs)) vmap )
	  );
 
	    assert false (* Non linear expression *) 
	  )
	  | Cst c, e' -> List.map (fun (vs, e'') -> vs, eval (MatScalMult (s, e''))) e'
	  | _, [] -> assert false (* should not happen, by construction we have a non empty list as second argument *)
      )
      | MatCst c -> [VarSet.empty , MatCst c]
      | BlockLMI (a, dim) -> (try
	(* Each block has to be analyzed with 'up'. We can work inplace *)
	(* Then each constant expression associated to a block element is
	   evaluated *)
	(* Finally each constant matrix is lifted to the global dim and the
	   block is transformed as a sum *)
	let _, res =
	  Array.fold_right (
	  fun line (i, accu) ->
	    let _, new_line = 
	      Array.fold_right (
		fun elem (j, line_accu) ->
		  let vlist = up elem in
		  let vlist' = List.map (fun (vs, mat) -> vs, lift i j dim (eval mat)) vlist in
		  j-1, combine_additive_op vlist' line_accu
	      ) line (Array.length line - 1, [])		
	    in
	    i-1, (combine_additive_op new_line accu)
	) a (Array.length a - 1, [])
	in
	res
	with Invalid_argument msg -> raise (Invalid_argument ("icic" ^ msg)))
      | MatSub _ 
      | MatVarSym _ -> 
	assert false (* should have been remove by down *)
      in
      (* Report.debugf ~kind:"sdp" 
	  (fun fmt -> Format.fprintf fmt "up: %a -> %a@." *)
      (* 	pp_matrix_expr e *)
      (* 	(Utils.fprintf_list ~sep:"; " (fun fmt (vs, e') -> Format.fprintf fmt "(%a) -> %a" VarSet.pp vs pp_matrix_expr e')) r); *)
      r

    and combine_additive_op e1 e2 =
      let vars_of_elems e =
	List.fold_right VarSetSet.add (List.map fst e) VarSetSet.empty
      in
      (* let e1' = up e1 in *)
      let vars1 = vars_of_elems e1 in
      (* let e2' = up e2 in *)
      let vars2 = vars_of_elems e2 in

      let common = VarSetSet.inter vars1 vars2 in

      let remain1 = VarSetSet.diff vars1 common in
      let remain1_elems = List.filter (fun (vs, _) -> VarSetSet.mem vs remain1) e1 in
      let remain2 = VarSetSet.diff vars2 common in
      let remain2_elems = List.filter (fun (vs, _) -> VarSetSet.mem vs remain2) e2 in
      
      let common_elems = 
	List.fold_left (
	  fun accu vs -> 
	    let vs_e1 = List.assoc vs e1 in
	    let vs_e2 = List.assoc vs e2 in
	    (vs, eval (MatAdd (vs_e1, vs_e2))):: accu
	) [] (VarSetSet.elements common) 
      in
      Report.debugf ~kind:"sdp" ~level:10
	  (fun fmt -> Format.fprintf fmt "Common: [%a], remain1: [%a], remain2: [%a]@."
      	VarSetSet.pp common
      	VarSetSet.pp remain1
      	VarSetSet.pp remain2
      )	;
      
      let res = remain1_elems @ remain2_elems @ common_elems in
      Report.debugf ~kind:"sdp" ~level:10
	(fun fmt -> Format.fprintf fmt "Combine addidive: %a + %a = %a@."
	  (Utils.fprintf_list ~sep:"; " (fun fmt (vs, e) -> Format.fprintf fmt "%a: %a" VarSet.pp vs pp_matrix_expr e) ) e1
	  (Utils.fprintf_list ~sep:"; " (fun fmt (vs, e) -> Format.fprintf fmt "%a: %a" VarSet.pp vs pp_matrix_expr e) ) e2
	  (Utils.fprintf_list ~sep:"; " (fun fmt (vs, e) -> Format.fprintf fmt "%a: %a" VarSet.pp vs pp_matrix_expr e) ) res
	)	;
      
      res
    in
    let e' = up e in 
    e'
  in
  let d = down matrix_expr in
  Report.debugf ~kind:"sdp" ~level:10
	  (fun fmt -> Format.fprintf fmt "Down: %a@.============@." pp_matrix_expr d);
  let u = up d in
  Report.debugf ~kind:"sdp" ~level:10
	  (fun fmt -> Format.fprintf fmt "Up: %a@." (Utils.fprintf_list ~sep:"@ " (fun fmt (vs, e) -> Format.fprintf fmt "%a: %a" VarSet.pp vs pp_matrix_expr e)) u);
  (* Cleaning *)
  List.map (fun (v, e) ->
    let m =
      match e with
      | MatCst m -> m
      | _ -> assert false (* TODO raise Failure and catch it later *)
    in
    v, m
  ) u


(** Constructors *)
let zeros n m = MatCst (Mat.zeros_matrix n m)
let eye n = MatCst (Mat.ident_matrix n)
let kronecker_sym_matrix dim i j = MatCst (Mat.kronecker_sym_matrix dim i j)

let const_mult c e = MatScalMult ((Cst c), e)

let scal_mult v e = match v with
  | ScalId id -> MatScalMult ((SIdent id), e)
  | Var (id, i, j) -> MatScalMult (MVar (id, i, j), e)

let symmat (id, dim) = MatVarSym (id, dim)
let const_mat m = MatCst m
let trans_const_mat m = MatCst (Mat.transpose_matrix m)
let add e1 e2 = MatAdd (e1, e2)
let sub e1 e2 = MatSub (e1, e2)
let mult e1 e2 = MatMult (e1, e2)

(* For each var in sorted_vars, associate the value in dual_sol. Then rebuilt matrices. *)
let rebuild_dual dual_sol sorted_vars =
if List.length (List.flatten sorted_vars) != Array.length dual_sol then
  assert false (* TODO should return a Failure ?. It should never happen *)
else
  let _, res = 
    List.fold_right (
     fun var_list (cpt, accu)-> 
       let new_cpt, vars, elems = 
	 List.fold_right 
	   (fun v (cpt, accu_vars, accu_elems) -> cpt-1 , v::accu_vars, dual_sol.(cpt)::accu_elems) 
	   var_list 
	   (cpt, [], [])
       in
       let exported_val = 
	 match List.rev vars, elems with
       | [ScalId x], [y] -> (
	 (* Report.debugf ~kind:"sdp" ~level:10 *)
	   Format.eprintf "%t"
	   (fun fmt -> Format.fprintf fmt "%a elem= %a@.@?" 
	     Ident.fprintf x Format.pp_print_float y
	   );
	 (ScalId x, Scalar (Mat.Elem.of_float y)) :: accu
       )
       | (Var (id, i, j)::_ ), _ when i=j -> (
	   let conv_elems = List.map Mat.Elem.of_float elems in
	   Format.eprintf "%t"
	   (* Report.debugf ~kind:"sdp" ~level:10 *)
	  (fun fmt -> Format.fprintf fmt "%a elems= [%a]@.@?" 
	     Ident.fprintf id
	     (* (Utils.fprintf_list ~sep:", " Base.pp) conv_elems *)
	     (Utils.fprintf_list ~sep:", " Format.pp_print_float) elems
	  );
	   let mat = sym_mat_of_var (i+1) conv_elems in
	   (Var (id,0,0) , Mat mat)::accu  
	 )
       | _ -> 
	 (List.map2 (fun v e -> 
	   Format.eprintf "%t"
	   (* Report.debugf ~kind:"sdp" ~level:10 *)
	  (fun fmt -> Format.fprintf fmt "%t elem= %a@.@?" 
	    (fun fmt -> match v with Var (id,i,j) -> Format.fprintf fmt "%a(%i,%i)" Ident.fprintf id i j)
	     (* (Utils.fprintf_list ~sep:", " Base.pp) conv_elems *)
	     Format.pp_print_float e
	  );

	   v,Scalar (Mat.Elem.of_float e)) vars elems) @  accu
       in
       new_cpt, exported_val
   ) sorted_vars (-1 + Array.length dual_sol,[]) 
  in
  res

(*let prod_vect vecta vectb =
  let lengtha = Array.length vecta in
  if lengtha != Array.length vectb then
    raise (Failure "incomptaible length for vector products")
  else (
    let res = ref (Mat.Elem.of_int 0) in
    for i = 0 to lengtha - 1 do
      res := Mat.Elem.add !res (Mat.Elem.mult vecta.(i) vectb.(i))
    done;
    !res
  )
*)

(*
let add_vect vecta vectb =
  let lengtha = Array.length vecta in
  if lengtha != Array.length vectb then
    raise (Failure "incomptaible length for vector products")
  else ()
*)

(* let sub_vect vecta vectb = *)
(*   let vectb' = Array.map (fun e -> MatScalMult (Cst (Base.of_int (-1))) xxxx  vectb in *)
(*   add_vect vecta vectb *)

(* let get_line i m = *)
(* match m with *)
(* | MatMult (e1, e2) ->  *)
(* | MatAdd (e1, e2) -> add_vect (get_line i e1) (get_line i e2) *)
(* | MatSub (e1, e2) -> sub_vect ((get_line i e1) (get_line i e2) *)
(* | MatScalMult (s, e1) -> MatScalMult (s, down e1) *)
(* | MatCst x -> (Mat.matrix_to_array_array x).(i).(j) *)
(* | MatVarSym (id, dim) -> *)
  

let of_array_array a =
  (* We check the dimension *)
  let cols_dim = 
    let first_line = a.(0) in
    Report.debugf ~kind:"sdp" ~level:10
	  (fun fmt -> Format.fprintf fmt "Cols: first line dims: %a@."
	    (fun fmt -> Array.iter (fun e -> Format.fprintf fmt "%a: %i, %i" pp_matrix_expr e (fst (get_dim e)) (snd (get_dim e)) )) first_line);
    Array.map (fun elem -> snd (get_dim elem)) first_line
  in
  let nb_cols = Array.length a  in
  let lines_dim = 
    let first_col = Array.map (fun l -> l.(0)) a in
    Array.map (fun elem -> fst (get_dim elem)) first_col
  in
  let nb_lines = Array.length a.(0) in
  (* Report.debugf ~kind:"sdp"  *)
  (* 	  (fun fmt -> Format.fprintf fmt "Expected dim:@.lines: %a@.cols:%a@." (Utils.fprintf_list ~sep:", " Format.pp_print_int) (Array.to_list lines_dim) (Utils.fprintf_list ~sep:", " Format.pp_print_int) (Array.to_list cols_dim)); *)
  for i = 1 to nb_lines - 1 do
    for j = 1 to nb_cols - 1 do
    let dim_l, dim_c = get_dim a.(i).(j) in
      (* Report.debugf ~kind:"sdp"  *)
      (* 	  (fun fmt -> Format.fprintf fmt "Checking block %i, %i: dim is %i, %i, expecting %i, %i@." i j dim_l dim_c lines_dim.(i) cols_dim.(j)); *)
    if dim_l <> lines_dim.(i) || dim_c <> cols_dim.(j) then (
      Report.debugf ~kind:"sdp" 
	  (fun fmt -> Format.fprintf fmt "Failure: dimen error in block matrix LMI expression; block %i, %i@. Dim is %i, %i, expecting %i, %i@." i j dim_l dim_c lines_dim.(i) cols_dim.(j));
      raise (Failure "dimen error in block matrix LMI expression")
    )
    done
  done;

  let sum a = Array.fold_left ((+)) 0 a in

  let total_lines = sum lines_dim in
  let total_cols = sum cols_dim in

  BlockLMI (a, (total_lines, lines_dim, total_cols, cols_dim))


let diag block_list = 
  let nb_elems = List.length block_list in
  let block_array = Array.of_list block_list in
  let matrix = Array.make_matrix nb_elems nb_elems (zeros 1 1) in
  for i = 0 to nb_elems - 1 do
    for j = 0 to nb_elems - 1 do
      if i = j then 
	matrix.(i).(j) <- block_array.(i)
      else
	let nb_lines, _ = get_dim block_array.(i) in
	let _, nb_cols = get_dim block_array.(j) in
	matrix.(i).(j) <- zeros nb_lines nb_cols
    done;
  done;
  of_array_array matrix



(* Dual -> Primal *)
let solve (sorted_vars: v_t list list) lmi_list (obj_kind, obj_varl) =
  
(* Objective is normalized: it is expressed as coefficients over SDP variables *)


  (* 1. we iterate through lmi_list to gather variables and split them as
     scalar, it will be our solution. *)
(*  let vars = List.fold_left 
    (fun accu s -> IdentDimSet.union accu (get_vars s)) 
    IdentDimSet.empty lmi_list 
  in
  
  let sorted_vars : v_t list list = 
    List.map 
      (fun (v, dim) -> if dim = 0 then [ScalId v] else vars_of_sym_mat v dim) 
      (IdentDimSet.elements vars) 
  in
*)

  (* We check that objective is part of the variables *)
 (* (match objective with
  | Some (_, o) -> 
    if not (List.exists (List.mem o) sorted_vars) then 
      raise (Failure "LMI: objective is not part of the free variable of the LMIs.")
  | _ -> ()
  );
 *)

  (* 2. each lmi is rephrased over those scalars. the associated constant part
     is negated and returned (-C) *)
  let _, neg_constants, (lmi_unfolded: (VarSet.t *  Mat.t) list list) = 
    List.fold_right  (fun lmi (cpt, csts, unfolded) ->
      let assoc_list = try rewrite_mat_as_scalar lmi  
	with Invalid_argument ex -> (Report.debugf ~kind:"sdp" 
	  (fun fmt -> Format.fprintf fmt "Failure in step 1.@."); raise (Invalid_argument ex))
      in
      (* Format.eprintf "LMI:@.initial lmi: %a@. Rewritten as@.%a@." *)
      (* 	pp_matrix_expr lmi *)
      (* 	(Utils.fprintf_list ~sep:"@." (fun fmt (vs, elem) -> Format.fprintf fmt "%a * %a" VarSet.pp vs Mat.pp_matrix elem )) *)
      (* 	assoc_list *)
      (* ; *)
      let constant_blocks = 
	if List.mem_assoc VarSet.empty assoc_list then
	  let const_block = List.assoc VarSet.empty assoc_list in
	  let neg_const_block = Mat.mult_scalar_matrix (Mat.Elem.of_int (-1)) const_block in
	  (cpt, neg_const_block)::csts
	else
	  csts
      in
      cpt+1, constant_blocks, assoc_list::unfolded
    ) lmi_list (1, [], [])
  in
  (* 3. We iterate through the scalar variables and for each of them provide the
     block matrix of each lmi (do you understand that?)  It is also associated
     to the float a_i ((+/-) 1 for the objective variable, 0 for the others This
     is CONSTRAINTS *)
  let constraints = List.map (
    fun (v : v_t) -> 
      let target = 
	let modifier = match obj_kind with
	    Minimize -> 1.
	  | Maximize -> -1.
	in  (* non objective variable are associated to 0. *)
	if List.mem_assoc v obj_varl then modifier *. Mat.Elem.to_float (List.assoc v obj_varl) else 0.
      in
      let _, blocks = List.fold_right  (
	fun elems (cpt, accu) ->
	  if List.mem_assoc (VarSet.singleton v) elems then
	    cpt+ 1, (cpt, (List.assoc (VarSet.singleton v) elems)) :: accu
	  else 
	    cpt+1, accu
      ) lmi_unfolded (1, [])
      in
      blocks, target
  ) (List.flatten sorted_vars)
  in

  (* 4. We call Sdp.solve OBJ LIST *)
  let to_floats b = List.map (fun (i, m) -> 
    let m = Mat.matrix_to_list_list m in
    let m = List.map (List.map Mat.Elem.to_float) m in
    let m = Matrix.Float.matrix_of_list_list m in
    let m = Matrix.Float.matrix_to_array_array m in
    i, m)
    b 
  in
  let neg_constants' = to_floats neg_constants in
  let constraints' = List.map (fun (m, f) -> to_floats m, f) constraints in
  let res , (primal_sol, dual_sol) = Sdp.solve neg_constants' constraints' in
  Report.debugf ~kind:"sdp" ~level:10
	  (fun fmt -> Format.fprintf fmt "sdp=%f @.@?" res);

  (* Extract from dual_sol the value of each params and rebuilt the unknown
     matrices and lambda *)
  if res = infinity || res = neg_infinity then
    res, None
  else
    res, Some (rebuild_dual dual_sol sorted_vars)
    
let get_var_id v =
  match v with
  | Var (v,_,_) -> v
  | ScalId v -> v

let get_var_indices v =
  match v with
  | Var (_,i,j) -> Some(i,j)
  | ScalId _ -> None


end 

module Num_mat = Make (Matrix.Num_mat)


(*
let mat_map f = List.map (fun row -> List.map f row)


let a = 
  Matrix.Num.matrix_of_list_list (mat_map Matrix.num_of_string  [
    ["0.9379"; "-0.0381"; "-0.0414"];
    ["-0.0404";"0.968";"-0.0179"];
    ["0.0142"; "-0.0197"; "0.9823"];
  ])

let b = 
  Matrix.Num.matrix_of_list_list (mat_map Matrix.num_of_string [
    ["0.0237"];
    ["0.0143"];
    ["0.0077"];
  ]
  )

let _ =
  Global.policy_verbosity := 10;
  Global.sdp_verbosity := 10;
  let dim_p = Matrix.Num.nb_lines a in
  let nb_inputs = Matrix.Num.nb_cols b in
  let n = 1 in
  let mat_n = Num.of_array_array 
    [|
      [| Num.zeros dim_p dim_p; Num.zeros dim_p nb_inputs; Num.zeros dim_p 1; |];
      [| Num.zeros nb_inputs dim_p; 
	 Num.const_mult 
	   (Matrix.Num.Elem.of_int (-1)) 
	   (Num.kronecker_sym nb_inputs (n-1) (n-1)); 
	 Num.zeros nb_inputs 1; |];
      [| Num.zeros 1 dim_p; Num.zeros 1 nb_inputs; Num.eye 1 |]
    |]
  in
  let res = Num.rewrite_mat_as_scalar mat_n in
  Format.eprintf "LMI:@.initial lmi: %a@. Rewritten as@.%a@."
    Num.pp_matrix_expr mat_n
    (Utils.fprintf_list ~sep:"@." (fun fmt (vs, elem) -> Format.fprintf fmt "%a * %a" VarSet.pp vs Matrix.Num.pp_matrix elem ))
    res;
  ()
  *)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
