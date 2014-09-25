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
	num /. denum
      else
	0.
  ) monomials

