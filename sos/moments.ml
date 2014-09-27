open Num

  let pi = acos (-1.) 
  let sqrt_pi = sqrt pi 

  let rec fact n = 
    match n with 
    | 0 | 1 -> 1 
    | n when n > 0 -> n * fact (n-1) 
    | _ -> assert false

  (* Implementation of gamma for integers (factorial) and n + 1/2 with n
     integer. Computations are performed with Num until the last moment where
     the product with sqrt(pi) is computed in floats. *)
  let gamma (x: Num.num) : float = 
    if x <=/ (Num.num_of_int 0) then assert false; (* NaN *)
    (* We check that either x is integer or it is y/2 with y integer *)
    if Num.is_integer_num x then (* Gamma(x) = (x-1)! *)
      float_of_int (fact (-1 + int_of_num x))
    else 
      let y = (Num.num_of_int 2) */ x in 
      if Num.is_integer_num y then
	let x' = int_of_num (Num.floor_num x) in (* x = x' + 1/2 *)
	let big_term = (* 2n! / (4^n * n!) *)
	    (float_of_int (fact(2 * x'))) 
	  /. 
	    ( (4. ** ( float_of_int x' ) ) *. (float_of_int (fact x') ) )
	in
	big_term *. sqrt_pi

      else assert false (* only handle integer and integer/2 *)

(* Using def of thm 3.1 in J. B. Lasserre, E. S. Zeron. Solving a class of
   multivariate integration problems via Laplace techniques. Applicationes
   Mathematicae 28(4):391-405, 2001.

   The term 2^-n should be removed as stated in 
   http://homepages.laas.fr/henrion/Papers/innerpmi.pdf (page 9)

   z is choosen 1 to remain in the unit ball
*)
let momball monomials =
  let noi = Num.num_of_int in
  let dim = Array.length (List.hd monomials) in
  List.map (
    fun t -> 
      let l, sum_l = Array.fold_left (fun (l,sum) e -> 
	e::l, (noi e) +/ sum)
	([], noi 0) t
      in
      if List.for_all (fun x -> x mod 2 = 0) l then
	let num = (* \Pi_i \Gamma ( (ai+1) / 2 ) *)
	  List.fold_left 
	    (fun res ai -> 
	      res *. (gamma (((noi ai) +/ (noi 1)) // (noi 2)))
	    ) 1. l 
	in
	let denum = (* \Gamma ( 1 + (n + sum ai) / 2 ) *)
	  gamma (noi 1 +/ ( (noi dim +/ sum_l) // (noi 2)) ) 
	in
	Some (num /. denum)
      else
	None
  ) monomials



(* Compute \int H(i)(x)*H(j)(y) H(i')(x)*H(j')(y) d \gauss = d e^{- (x^2 + y^2)/2}  
   = \int H(i)(x)*H(j)(y) H(i')(x)*H(j')(y) d e^{- x^2/2} . e^{-y^2/2}
   = \int H(i)(x) H(i')(x) d e^{- x^2/2}  \times \int H(j)(y)H(j')(y) d e^{-y^2/2}
   
   For Statistical Hermite: with the 'real' gaussian measure, we have
   if i=i' then \int H(i)(x)H(i')(x) = \sqrt(2 \pi ) i!
   else 0   

 *)

let hermite_e_scal_prod hermite_pair_monomials =
  List.map (fun (xlist, ylist) ->
    if xlist = ylist then
      let prod =
	Array.fold_left (fun res i -> res * (fact i)) 1 xlist in
      let dim = Array.length xlist in
      let cst = (sqrt (2. *. pi)) ** (float_of_int dim) in
      cst *. (float_of_int prod)
    else
      0.
)




