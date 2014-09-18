open Utils

module type ORDEREDNAMEDMONOMIAL =
sig
  type t (* A monomial *)
  val cmp: t -> t -> int
  val fprintf: string array -> Format.formatter -> t ->  unit
end

module type MONOMIAL =
sig
  include  ORDEREDNAMEDMONOMIAL

  (* Returns the n-th monomial of the basis over dim variables . Starts at 0. 0
     should be the only monomial of degree 0 *)
  val nth: int -> int -> t

  (*  val get_id: t -> int (* Return the idx of the monomials in the basis *)
  *)

  (* We should have nth (get_id x) = x and get_id (nth x) = x *)

  (* TODO: add functions to 
     - obtain the classical rep of each monomial
     - compute the product of monomials   
  *)
  
end 

module type SCALAR =
sig
  type t
  val of_int: int -> t
  val of_rat: int -> int -> t
  val add: t -> t -> t
  val mult: t -> t -> t
  val is_zero: t -> bool
  val fprintf: Format.formatter -> t -> unit
end

module type LINEXPR = 
sig
  module S: SCALAR
  module M: ORDEREDNAMEDMONOMIAL
  type t = (S.t * M.t) list (* sorted by M.cmp *)
  val add: t -> t -> t
  val scal_mult: S.t -> t -> t
  val fprintf: string array -> Format.formatter -> t -> unit
end

 module type MONOMIAL_BASIS = 
   (* functor (S: SCALAR) ->  *)
   (*   functor (M: MONOMIAL) ->  *)
 sig 
   module S: SCALAR
   include MONOMIAL
   module LE : LINEXPR with type S.t = S.t and type M.t = t
   val get_sos_deg: int -> int -> int
   (* val fprintf: string array -> Format.formatter -> t ->  unit *)
   val prod: t -> t -> LE.t 
 (*   val ext_prod: LE.S.t -> t -> LE.t *)
 end

(*
module type POLYEXPR =
sig
  include LINEXPR
  val mult: t -> t -> t
end
 *)

module LinExpr =
  functor (S: SCALAR) ->
    functor (M: ORDEREDNAMEDMONOMIAL) ->
struct
  module S = S
  module M = M
  type t =  (S.t * M.t) list (* sorted by M.cmp *)

  let add = 
    merge_sorted_lists 
      (fun (_,m1)  (_,m2) -> M.cmp m1 m2) 
      (fun (s1,m) (s2,_) -> let r_s = S.add s1 s2 in 
			    if S.is_zero r_s then None else Some (r_s , m)) 

  let scal_mult s le = if S.is_zero s then [] else List.map (fun (s',m) -> S.mult s s', m) le

  let fprintf names = 
    Utils.fprintf_list ~sep:" + " 
      (fun fmt (s,m) -> Format.fprintf fmt "%a * %a" S.fprintf s (M.fprintf names) m) 
end 


(*
module PolyExpr =
    functor (MB: MONOMIAL_BASIS) -> (
struct
  include LinExpr (MB.S) (MB) 

(* TODO check order of elements to have a sorted list and test the function *)
  let mult pe1 pe2 =
    List.fold_right (fun (s, m) res ->
      List.fold_right (fun (s', m') res2 -> 
	let m_e = MB.prod m m' in
	let ss' = S.mult s s' in
	let le = List.map (fun (s'', m) -> S.mult ss' s'', m) m_e in
	add le res2) pe1 res
    ) pe2 []

  let is_zero pe = pe = []
  let of_int 
end:POLYEXPR)
*)
(*************************      Monomial Modules      *************************)

module TensorProduct =
struct
    
  type t = int array (* array of size: dim + 1 *)

  let base : (int, int array list list) Hashtbl.t = Hashtbl.create 13

  let cmp m1 m2 = (* m1 and m2 should be of same size *)
    compare m1 m2


  let fprintf pp_univar_monomial names fmt monomial = 
    assert (Array.length names = Array.length monomial);
      (*Format.fprintf fmt "[%a]" (fprintf_list ~sep:"; " Format.pp_print_int)
	(Array.to_list monomial)*)
    if Array.fold_left (fun accu pow -> accu && pow = 0) true monomial then
      Format.fprintf fmt "1"
    else
      List.iter2 (fun pow name -> 
	if pow > 0 then
	  pp_univar_monomial fmt name pow
	else 
	  () 
      ) (Array.to_list monomial) (Array.to_list names)


  let pp_debug dim fmt m= 
    let names = (Array.init dim (fun i -> "x" ^ string_of_int i)) in
    fprintf (fun fmt name pow -> Format.fprintf fmt "%s^%i" name pow) names fmt m

  let pp_debug_base fmt =
    Hashtbl.iter (fun d set -> 
      let flattened_set = List.flatten set in
      Format.fprintf fmt "%i -> (%i elements) [@[<v 5>%a]@]" 
	d 
	(List.length flattened_set)
	(Utils.fprintf_list ~sep:",@," (pp_debug d)) flattened_set
    ) base    
  
  (* Compute basis *)
  let rec monomes_rec accu (i, vector) =
    if i = Array.length vector then accu else
      let cp = Array.copy vector in 
      let new_monome = i, (cp.(i) <- cp.(i) + 1; cp) in
      new_monome :: monomes_rec accu (i+1, vector) 
	
	
  let monomes dim power =
    let rec loop vectors pow =
      if pow = 0 then
	List.map snd vectors
      else
	loop (List.fold_left monomes_rec [] vectors) (pow-1)
    in loop [0, Array.make dim 0] power
    
    
  let gen_base dim deg =
    Format.eprintf "Gen base dim=%i deg=%i@."dim deg;
    Format.eprintf "Avant: %t@." pp_debug_base;
    let exist = Hashtbl.mem base dim in
    let run, set, st = 
      if exist then
	let set = Hashtbl.find base dim in
	let st = List.length set in
	st < deg , set, st
      else 
	true, [], -1
    in
    if run then (
      let monomials = ref [] in
      for i = deg downto st+1 do
	monomials := (monomes dim i) :: !monomials
      done;
      Hashtbl.add base dim (set @ !monomials)
    );
    Format.eprintf "Apres: %t@." pp_debug_base
    

  let nth dim n  = 
    if Hashtbl.mem base dim then
      let set = Hashtbl.find base dim in
      let rec aux remaining n' =
	match remaining with
	| hd::tl -> 
	  let l = List.length hd in
	  if n' < l then
	    List.nth hd n'
	  else
	    aux tl (n'-l)
	| [] -> (
	  Format.eprintf "Looking for %i-th element of dim %i of the base %t@.@?"
	    n dim pp_debug_base; 
	  assert false (* TODO handle exception, generate bigger base or fail *)
	)
      in
      aux set n
    else (
      Format.eprintf "nth (dim=%i, n=%i) in base@.%t@.@?" dim n pp_debug_base;
      assert false (* arrive si la base n'a pas ete calculé du tout pour dim *)
    )

    (* Compute the size of the monomial basis when targeting a SOS
       polynomial of degree deg *)
      let get_sos_deg dim deg = 
	let target_deg = (deg / 2) + (deg mod 2) in
	gen_base dim target_deg;
	let set = Hashtbl.find base dim in	
	let rec aux n (base: t list) = 
	  if n > target_deg then 
	    base 
	  else 
	    (aux (n+1) ((List.nth set n)@base))
	in
	let base = aux 0 [] in
	List.length base
end

module ClassicalMonomialBasis =
functor (S : SCALAR) -> (
struct
  module S = S
  include TensorProduct
    
  let pp_univar_monomial fmt name pow =
	if pow > 1 then
	  Format.fprintf fmt "%s^%i" name pow
	else
	  Format.pp_print_string fmt name
	    
  let fprintf = fprintf pp_univar_monomial

  module LE = LinExpr 
    (S) 
    (struct include TensorProduct let fprintf = fprintf pp_univar_monomial end)
      
  let prod m1 m2 = 
    let l1, l2 = Array.length m1, Array.length m2 in
	if l1 <> l2 then
	  assert false
	else
	  [S.of_int 1, Array.init l1 (fun i -> m1.(i) + m2.(i))]

      
end:MONOMIAL_BASIS)

module HermiteMonomialBasis =
functor (S : SCALAR) -> (
struct
  module S = S
  include TensorProduct

  let pp_univar_monomial fmt name pow =
    Format.fprintf fmt "H_%i(%s)" pow name
      
  let fprintf = fprintf pp_univar_monomial
    

  (* Single dimension computation: the list are ordered in decreasing order to
     ease the computations *)
    
  (* Linear expression of single dimension monomials *)
  module MonoLE = LinExpr (S) 
    (struct type t = int 
	    let cmp x y = - (compare x y)
	    let fprintf names fmt monomial = fprintf names fmt [|monomial|]  
     end) 

  (* Computes xH0 = 1/2 H1,  xHn = nH{n-1} + 1/2 H{n+1} *)
  let prod_var_mono n : MonoLE.t =
    let half = S.of_rat 1 2 in
    if n = 0 then
      (half, 1)::[]
    else
	(half, n + 1)::(S.of_int n, n - 1)::[]

  (* Test *)	  
  let test () =
    List.iter (fun n -> 
      let m = prod_var_mono n in
      Format.printf "xH%i(x) = %a@." n (MonoLE.fprintf [|"x"|]) m
    ) [0;1;2;3;4;5;6;7;8;9;10]

  let prod_var_mono_serie (nlist: MonoLE.t) : MonoLE.t = (* nlist should be sorted by n decreasing *)
    let rec aux nlist res =
      match nlist, res with 
      | [], _ -> res
      | (s,n)::tl, [] -> ((MonoLE.scal_mult s (aux tl (prod_var_mono n))):MonoLE.t) 
      | (s,n)::tl, _ -> let xn: MonoLE.t = MonoLE.scal_mult s (prod_var_mono n) in 
			let res': MonoLE.t= 		      merge_sorted_lists
			(fun (_,m1) (_,m2) -> -(compare m1 m2)) 
			(fun (s1,m) (s2,_) -> let s = S.add s1 s2 in
					      if S.is_zero s then None else Some (s,m)) xn res
			in
		    aux tl res'
		    
    in
    aux nlist []


  (* Test *)	  
  let test ()  =
    let le1:MonoLE.t = [(S.of_int 1, 4); (S.of_int 1, 3); (S.of_int 1, 2); (S.of_int 1, 1)] in
    List.iter (fun (nl: MonoLE.t) -> 
      let m = prod_var_mono_serie nl in
      Format.printf "x * (%a) = %a@." (MonoLE.fprintf [|"x"|]) nl (MonoLE.fprintf [|"x"|]) m
    ) ([le1]: MonoLE.t list)


  let prod_mono_monomials n1 n2 =
    let inject n = [S.of_int 1, n] in
    let xs, c = if n1 <= n2 then inject n1, n2 else inject n2, n1 in
    let rec aux i c0 c1 nd =
      if i >= c + 2 then
	c0, c1
      else
	let i' = i + 1 in
	let nd' = nd - 1 in
	let c0' = MonoLE.scal_mult (S.of_int (-2 * (nd'-1)) ) c1 in
	let c1' = MonoLE.add c0 (MonoLE.scal_mult (S.of_int 2) (prod_var_mono_serie c1)) in
	aux i' c0' c1' nd'
    in 
    match c with 
    | 0 -> [S.of_int 1, 0]
    | 1 -> let c0, c1 = [], xs in
	   MonoLE.add c0 (MonoLE.scal_mult (S.of_int 2) (prod_var_mono_serie c1))
    | _ -> let c0, c1 = aux 3 [] xs (c+1) in
	   MonoLE.add c0 (MonoLE.scal_mult (S.of_int 2) (prod_var_mono_serie c1))

  (* Test *)
  let test () =
    List.iter (fun (x,y) ->
      let r = prod_mono_monomials x y in 
      let names = [|"x"|] in
      let pp = fprintf  names in
      Format.printf "%a * %a = %a@." pp [|x|] pp [|y|] (MonoLE.fprintf names) r
    )
      [(1,1);(0,2);(2,3); (5,6) ]

  (* Tensorized functions *)
      
  module LE = LinExpr
    (S) 
    (struct 
      include TensorProduct 
      let fprintf = fprintf pp_univar_monomial 
     end)


  (*  Lift of prod_var_mono to Tensor product *)
  let prod_var idx m = 
    (* idx should be a valid dimension *)
    if idx < 0 || idx > Array.length m then assert false;
    let order_idx_m = m.(idx) in
    let res_idx = List.rev (prod_var_mono order_idx_m) in (* we reorder the list to 
							     have it increasing *)
    List.map 
      (fun (coeff, new_order) -> 
	let m' = Array.copy m in 
	m'.(idx) <- new_order; coeff, m'
      ) res_idx
      

  let rec tensorize (ll : MonoLE.t list) : LE.t  =
    match ll with
    | [] -> assert false 
    | [l] -> ((List.map (fun (s, m) -> s, [|m|] ) l): LE.t)
    | hd::tl -> 
      List.fold_right 
	(fun (s, m_array) (res:LE.t) -> 
	  List.fold_right 
	    (fun (shd, mhd) res2 -> 
	      (S.mult shd s, Array.append [|mhd|] m_array)::res2)
	    hd res
	) 
	(tensorize tl) ([]:LE.t)



  let prod m1 m2 = 
    let l1, l2 = Array.length m1, Array.length m2 in
    if l1 <> l2 then
      assert false
    else
      let res_by_dim = 
	List.map2
	  (fun mono1 mono2 -> prod_mono_monomials mono1 mono2) 
	  (Array.to_list m1)
	  (Array.to_list m2)
      in
      tensorize res_by_dim

end:MONOMIAL_BASIS)
