open Utils

module SNum = struct
  include Matrix.Num_mat.Elem
  let of_rat a b = div (of_int a) (of_int b)

end

module type MONOMIAL =
sig
  type t (* A monomial *)
  val compare: t -> t -> int
  val pp: ?names:string array option -> Format.formatter -> t ->  unit
end

(*
module type RING =
sig
  type t
  module Ext : sig type t end
  val of_int: int -> t
  val of_rat: int -> int -> t
  val is_zero: t -> bool
  val add: t -> t -> t
  val mult: t -> t -> t
  val ext_mult: Ext.t -> t -> t
  val fprintf: Format.formatter -> t -> unit
end
*)

module type LINEXPR = 
sig
  module M: MONOMIAL
  type t = (SNum.t * M.t) list (* sorted by M.cmp *)
  val add: t -> t -> t
  val scal_mult: SNum.t -> t -> t
  val pp: ?names:string array option -> Format.formatter -> t -> unit
end

module type MONOMIAL_BASIS = 
sig 
(*  module S: RING*)
  include MONOMIAL
  module LE : LINEXPR with type M.t = t
			     
  (* Returns the n-th monomial of the basis over dim variables . Starts at 0. 0
     should be the only monomial of degree 0 *)
  val nth: int -> int -> t

  val dim: t -> int
  val deg: t -> int
    
  (* dim deg *)
  val get_base_size: int -> int -> int
  val get_sos_base_size: int -> int -> int
  val prod: t -> t -> LE.t 
  (* var_prod c m : multiply m by the polynomial x^c[0]y^c[1]... *)
  val var_prod: int array -> t -> LE.t 

(* get_monimials dim deg returns the list of monomials of dim up to deg *)
  val get_monomials: int -> int -> t list

end

module LinExpr =
(*  functor (S: RING) ->*)
    functor (M: MONOMIAL) ->
struct
(*  module S = S*)
  module M = M
  type t =  (SNum.t * M.t) list (* sorted by M.cmp *)

  let inject n = [SNum.of_int 1, n] 

  let add = 
    merge_sorted_lists 
      (fun (_,m1)  (_,m2) -> M.compare m1 m2) 
      (fun (s1,m) (s2,_) -> let r_s = SNum.add s1 s2 in 
			    if SNum.is_zero r_s then None else Some (r_s , m)) 

  let scal_mult s le = if SNum.is_zero s then [] else List.map (fun (s',m) -> SNum.mult s s', m) le

  let pp ?(names=None) = 
    Utils.fprintf_list ~sep:" + " 
      (fun fmt (s,m) -> Format.fprintf fmt "%a * %a" SNum.pp s (M.pp ~names:names) m) 
end 


(*************************      Monomial Modules      *************************)

module TensorProduct =
struct
    
  type t = int array (* array of size: dim + 1 *)

  let dim = Array.length
  let deg = Array.fold_left ((+)) 0     

  let base : (int, int array list list) Hashtbl.t = Hashtbl.create 13

  let compare m1 m2 = (* m1 and m2 should be of same size *)
    compare m1 m2


  let pp pp_univar_monomial ?(names=None) fmt monomial = 
    let names =
      match names with
      | None -> let x_char = Char.code 'x' in 
		Array.init (Array.length monomial) (fun id -> String.make 1 (Char.chr (x_char + id)))
      | Some a -> a
    in
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
    pp (fun fmt name pow -> Format.fprintf fmt "%s^%i" name pow) fmt m

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
    (* Format.eprintf "Gen base dim=%i deg=%i@."dim deg; *)
(*    Format.eprintf "Avant: %t@." pp_debug_base;*)
    let exist = Hashtbl.mem base dim in
    let run, set, st = 
      if exist then
	let set = Hashtbl.find base dim in
	let st = List.length set in
	st < deg+1 , set, st
      else 
	true, [], -1
    in
    if run then (
      let monomials = ref [] in
      for i = deg+1 downto st+1 do
	monomials := (monomes dim i) :: !monomials
      done;
      Hashtbl.add base dim (set @ !monomials)
    );
(*    Format.eprintf "Apres: %t@." pp_debug_base;*)
    ()
    
  let get_monomials dim deg =
    let rec select i l =
      match i, l with
      | 0, x::_ -> x
      | _, hd::tl -> hd@(select (i-1) tl)
      | _ -> Format.eprintf "select: %i in list of length %i@.@?" i (List.length l); assert false
    in
    if not (Hashtbl.mem base dim) then
      gen_base dim deg;
    select deg (Hashtbl.find base dim)
    
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

  let get_base_size dim deg =
    gen_base dim deg;
    let set = Hashtbl.find base dim in	
    (* Format.eprintf "filling hashtbl: target deg = %i, nb of already computed deg: %i@." target_deg (List.length set);  *)
    let rec aux n base = 
      if n > deg then 
	base 
      else 
	(aux (n+1) ((try base + List.length (List.nth set n) with Failure _ -> Format.eprintf "n=%i@?@." n; assert false)))
    in
(*    Format.eprintf "%t" pp_debug_base;*)
    aux 0 0 

    (* Compute the size of the monomial basis when targeting a SOS
       polynomial of degree deg *)
  let get_sos_base_size dim deg = 
    let target_deg = (deg / 2) + (deg mod 2) in
    (* Format.eprintf "dim=%i, deg=%i, deg vise: %i@." dim deg target_deg; *)
    get_base_size dim target_deg
end

module ClassicalMonomialBasis =
(* functor (S : RING) -> ( *)
(struct
  (* module S = S *)
  include TensorProduct
 
  let pp_univar_monomial fmt name pow =
	if pow > 1 then
	  Format.fprintf fmt "%s^%i" name pow
	else
	  Format.pp_print_string fmt name
	    
  let pp = pp pp_univar_monomial

  module LE = LinExpr 
    (* (S)  *)
    (struct include TensorProduct 
	    let pp ?(names=None) = pp pp_univar_monomial ~names:names 
(*let fprintf = fprintf pp_univar_monomial *)

end)
      
  let prod m1 m2 = 
    let l1, l2 = Array.length m1, Array.length m2 in
	if l1 <> l2 then
	  assert false
	else
	  LE.inject (Array.init l1 (fun i -> m1.(i) + m2.(i)))


  let var_prod = prod
      
end:MONOMIAL_BASIS with type t = int array)

type hermite_t = HPhysicists | HStatistic

module HermiteMonomialBasisMake =
  functor (HKind: sig val kind: hermite_t end) ->
(* functor (S : RING) ->  *)(
struct
  (* module S = S *)
  include TensorProduct

  (* Single dimension computation: the list are ordered in decreasing order to
     ease the computations *)
  module UniVariate =
  struct
    
    let pp_univar_monomial fmt name pow =
      Format.fprintf fmt "H_%i(%s)" pow name

  (* Linear expression of single dimension monomials *)
    module LE = LinExpr (* (S) *) 
      (struct type t = int 
	      let compare x y = - (compare x y)
	      let pp ?(names=None) fmt monomial = pp pp_univar_monomial ~names:names fmt [|monomial|]  
       end) 
      
  (* Computes 
     xH0 = 1/2 H1,  xHn = nH{n-1} + 1/2 H{n+1} (in physicists)
     xH0 = H1,  xHn = nH{n-1} + H{n+1}         (in statistic)

  *)
  let prod_var_mono n : LE.t =
    let half = if HKind.kind = HPhysicists then SNum.of_rat 1 2 else SNum.of_int 1 in
    if n = 0 then
      (half, 1)::[]
    else
	(half, n + 1)::(SNum.of_int n, n - 1)::[]

  (* Test *)	  
  let test =
    List.iter (fun n -> 
      let m = prod_var_mono n in
      Format.printf "xH%i(x) = %a@." n (LE.pp ~names:(Some [|"x"|])) m
    ) [0;1;2;3;4;5;6;7;8;9;10]

  (* Computes x(sum xxx)*)
  let prod_var_mono_serie (nlist: LE.t) : LE.t = (* nlist should be sorted by n decreasing *)
    let rec aux nlist res =
      match nlist, res with 
      | [], _ -> res
      | (s,n)::tl, [] -> ((LE.scal_mult s (aux tl (prod_var_mono n))):LE.t) 
      | (s,n)::tl, _ -> let xn: LE.t = LE.scal_mult s (prod_var_mono n) in 
			let res': LE.t= 		      merge_sorted_lists
			(fun (_,m1) (_,m2) -> -(compare m1 m2)) 
			(fun (s1,m) (s2,_) -> let s = SNum.add s1 s2 in
					      if SNum.is_zero s then None else Some (s,m)) xn res
			in
		    aux tl res'
		    
    in
    aux nlist []


  (* Test *)	  
  let test ()  =
    let le1:LE.t = [(SNum.of_int 1, 4); (SNum.of_int 1, 3); (SNum.of_int 1, 2); (SNum.of_int 1, 1)] in
    List.iter (fun (nl: LE.t) -> 
      let m = prod_var_mono_serie nl in
      Format.printf "x * (%a) = %a@." (LE.pp ~names:(Some [|"x"|])) nl (LE.pp ~names:(Some [|"x"|])) m
    ) ([le1]: LE.t list)


  let prod_mono_monomials n1 n2 =
    let scale = if HKind.kind = HPhysicists then SNum.of_int 2 else SNum.of_int 1 in
    let xs, c = if n1 <= n2 then LE.inject n1, n2 else LE.inject n2, n1 in
    let rec aux i c0 c1 nd =
      if i >= c + 2 then
	c0, c1
      else
	let i' = i + 1 in
	let nd' = nd - 1 in
	let c0' = LE.scal_mult (SNum.mult scale (SNum.of_int (-(nd'-1))) ) c1 in
	let c1' = LE.add c0 (LE.scal_mult scale (prod_var_mono_serie c1)) in
	aux i' c0' c1' nd'
    in 
    match c with 
    | 0 -> LE.inject 0
    | 1 -> let c0, c1 = [], xs in
	   LE.add c0 (LE.scal_mult scale (prod_var_mono_serie c1))
    | _ -> let c0, c1 = aux 3 [] xs (c+1) in
	   LE.add c0 (LE.scal_mult scale (prod_var_mono_serie c1))

  (* Test *)
  let test  =
    List.iter (fun (x,y) ->
      let r = prod_mono_monomials x y in 
      let names = Some [|"x"|] in
      let pp = pp pp_univar_monomial ~names:names in
      Format.printf "%a * %a = %a@." pp [|x|] pp [|y|] (LE.pp ~names:names) r
    )
      [(*(1,1);(0,2);(2,3); (5,6);*) (2,2) ]
      
  end

  let pp = pp UniVariate.pp_univar_monomial
    

    

  (* Tensorized functions *)
      
  module LE = LinExpr
    (* (S)  *)
    (struct 
      include TensorProduct 
      let pp = pp UniVariate.pp_univar_monomial 
     end)


  (*  Lift of prod_var_mono to Tensor product *)
  let prod_var_single_dim idx m = 
    (* idx should be a valid dimension *)
    if idx < 0 || idx > Array.length m then assert false;
    let order_idx_m = m.(idx) in
    let res_idx = List.rev (UniVariate.prod_var_mono order_idx_m) in (* we reorder the list to 
							     have it increasing *)
    List.map 
      (fun (coeff, new_order) -> 
	let m' = Array.copy m in 
	m'.(idx) <- new_order; coeff, m'
      ) res_idx

  exception Idx of int
  let var_prod a m = 
    let rec aux (m:LE.t) =
      let idx =
	try
	  Array.iteri (fun i ci -> if ci > 0 then (a.(i) <- -1 + a.(i); raise (Idx i))) a;
	  None
	with Idx i -> Some i
      in
      match idx with
      | None -> m
      | Some i -> aux (List.fold_left (fun res (s,m) -> LE.add (prod_var_single_dim i m) res) [] m)

    in
    aux (LE.inject m)
      

  let rec tensorize (ll : UniVariate.LE.t list) : LE.t  =
    match ll with
    | [] -> assert false 
    | [l] -> ((List.map (fun (s, m) -> s, [|m|] ) l): LE.t)
    | hd::tl -> 
      List.fold_right 
	(fun (s, m_array) (res:LE.t) -> 
	  List.fold_right 
	    (fun (shd, mhd) res2 -> 
	      (SNum.mult shd s, Array.append [|mhd|] m_array)::res2)
	    hd res
	) 
	(tensorize tl) ([]:LE.t)



  let prod m1 m2 = 
    let l1, l2 = Array.length m1, Array.length m2 in
    if l1 <> l2 then
      assert false
    else
      let res = 
	let res_by_dim = 
	  List.map2
	    (fun mono1 mono2 -> UniVariate.prod_mono_monomials mono1 mono2) 
	    (Array.to_list m1)
	    (Array.to_list m2)
	in
	tensorize res_by_dim
      in
      List.sort (fun (_,x) (_,y) -> compare x y) res

end)

module HermiteP = HermiteMonomialBasisMake(struct let kind = HPhysicists end)
module HermiteS = HermiteMonomialBasisMake(struct let kind = HStatistic end)
