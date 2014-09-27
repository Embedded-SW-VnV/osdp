open Utils
module Vars = LinearExpr.Vars
module VN = LinearExpr.VN
    
      (* We use two hashtbl: 
	 idtbl: pairid to (c1, c2)
	 var_id: var to pairid list
	 zerotbl: List of var
      *)
      let deftbl: (Vars.t, VN.t) Hashtbl.t = Hashtbl.create 13 
      let varusetbl: (Vars.t, Vars.t list) Hashtbl.t = Hashtbl.create 13 
      let zerotbl = ref [] 

      let print_summary fmt = 
	let pp_v = Vars.pp in
	Format.fprintf fmt "def: @[<v>";
	Hashtbl.iter (fun v e -> Format.fprintf fmt "%a = %a@ " pp_v v VN.pp e) deftbl;
	Format.fprintf fmt "@]@.";
	Format.fprintf fmt "varuse: @[<v>";
	Hashtbl.iter (fun v vl -> Format.fprintf fmt "%a used in %a@ " pp_v v (fprintf_list ~sep:", " pp_v) vl) varusetbl;
	Format.fprintf fmt "@]@.";
	Format.fprintf fmt "zero vars: [%a]@." (fprintf_list ~sep:", " pp_v) !zerotbl


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
	Hashtbl.replace varusetbl v (List.filter (fun var -> Vars.compare var v' <> 0) (get_uses v))

      let clean c = VN.inject (List.filter (fun (_,v) -> not (List.mem v !zerotbl)) (VN.extract c))
      
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
	(* Format.eprintf "added zero var: %a@.%t@." pp_v v print_summary; *)
	()

      and add_def_var v l =
	register_def v l;
	(* Variables of l are declared as being used in v *)
	List.iter (fun (_,v') -> match v' with Vars.Cst -> () | _ -> register_use v' v) (VN.extract l);
	(* for all variable v' < v, we substitute occurence of v by l *)
	List.iter (
	  fun v' -> 
	    if Vars.compare v' v < 0 then (
	      del_use v v';
	      (* we replace (n,v) by n * l in definion of v' *)
	      let v'_l = List.fold_left (
		fun accu ((n',v'') as e) -> 
		     if Vars.compare v'' v = 0 then
		       VN.add (VN.ext_mult n' l) accu
		     else
		       VN.add (VN.inject [e]) accu
	      ) VN.zero (VN.extract (get_def v')) in
	      del_def v';
	      add_def_var v' v'_l
	    )
	) (get_uses v)

      and add_cons c1 c2 =
	(* Format.eprintf "adding cons: %a = %a@.%t@." pp_levarsnum c1  pp_levarsnum c2 print_summary; *)

	let cl = clean(VN.sub c1 c2) in
	let cl = List.fold_left (fun accu (n,v) ->
	  if is_registered v then
	    let vl = get_def v in
	    VN.add (VN.ext_mult n vl) accu
	  else
	    VN.add (VN.inject [n,v]) accu
	) VN.zero (VN.extract cl)
	in
	(* Format.eprintf "sub %a@." pp_levarsnum cl; *)
	match VN.extract cl with 
	| [] -> assert false (* correspond to 0 = 0. I think it should not happen *)
	| [_, Vars.Cst] -> assert false (* should really never happen, ie. 3 = 0 *)
	| [_, v] -> add_zero_var v

	| ((_, Vars.Cst) as cst)::(n,v)::tl -> (* Affine expression, special treatment for cst *)
	  let l' = VN.ext_mult (LinearExpr.N.div (LinearExpr.N.of_int (-1)) n) (VN.inject (cst::tl)) in
	  add_def_var v l'

	| (n,v)::l -> (* Linear expression, no affine part *)
	  let l' = VN.ext_mult (LinearExpr.N.div (LinearExpr.N.of_int (-1)) n) (VN.inject l) in
	  add_def_var v l'	 

      let get_cons () =
	let res =  
	(Hashtbl.fold (fun var expr accu -> (VN.add (VN.inject [LinearExpr.N.of_int (-1), var]) expr)::accu) deftbl []) @ 
	  (List.map (fun c -> VN.inject [LinearExpr.N.of_int 1, c]) !zerotbl)
	in
	  Hashtbl.reset deftbl;
	  Hashtbl.reset varusetbl;
	  zerotbl := [];

	res
      
