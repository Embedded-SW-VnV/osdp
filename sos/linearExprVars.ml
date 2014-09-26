
(*******************************************************************************)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*******************************************************************************)


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


module Make =functor (N:sig end) ->
struct

(* Linear expressions over variables in Vars with numerical scalars *)
type t_levarsnum = (N.t * Vars.t) list

let pp_levarsnum fmt c = 
match c with 
| [] -> Format.fprintf fmt "0"
| [n, Vars.Cst] -> N.fprintf fmt n        
(*| (n1,Vars.Cst)::(n, v)::[] when N.is_zero n1 ->
  Format.fprintf fmt "%a * %a" N.fprintf n Vars.fprintf v      
*)
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

    
(*******************************************************************************)
(*                                                                             *)
(* Module used to accumulate constraints and store them in a compact           *)
(* way.                                                                        *)
(*                                                                             *)
(*                                                                             *)
(*                                                                             *)
(*******************************************************************************)

    (* Set of functions to accumulate linear constraints among vars *)
    module Constraints =
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
end
