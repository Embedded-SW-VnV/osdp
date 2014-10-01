(* (\* test SOS : F(x, y) = 2x^4+2x^3y-x^2y^2+5y^4 *)
(*    = (sqrt(2)x^2-1.455122518091861y^2+0.707106781186547xy)^2 *)
(*      + (1.697827569967575y^2+0.606025616617623xy)^2 *)
(*      + (1.499479893830934xy)^2 *\) *)
(* let _ = *)
(*   let mC = Array.make_matrix 3 3 0. in *)
(*   let mA1 = Array.make_matrix 3 3 0. in *)
(*   mA1.(0).(0) <- 1.; *)
(*   let a1 = 2. in *)
(*   let mA2 = Array.make_matrix 3 3 0. in *)
(*   mA2.(1).(1) <- 1.; *)
(*   let a2 = 5. in *)
(*   let mA3 = Array.make_matrix 3 3 0. in *)
(*   mA3.(2).(2) <- 1.; *)
(*   mA3.(0).(1) <- 1.; mA3.(1).(0) <- 1.; *)
(*   let a3 = -1. in *)
(*   let mA4 = Array.make_matrix 3 3 0. in *)
(*   mA4.(0).(2) <- 1.; mA4.(2).(0) <- 1.; *)
(*   let a4 = 2. in *)
(*   let mA5 = Array.make_matrix 3 3 0. in *)
(*   mA5.(1).(2) <- 1.; mA5.(2).(1) <- 1.; *)
(*   let a5 = 0. in *)
(*   let obj = [mC] in *)
(*   let cstrs = [[mA1], a1; [mA2], a2; [mA3], a3; [mA4], a4; [mA5], a5] in *)
(*   let res, (mX, _) = Csdp.solve obj cstrs in *)
(*   Format.printf "res = %f@." res; *)
(*   Format.printf "X = "; *)
(*   List.iter *)
(*     (fun a -> *)
(*        Format.printf "[@[%a@]]@." *)
(*          (Utils.fprintf_array ~sep:";@ " *)
(*             (fun fmt -> Format.fprintf fmt "@[%a@]" *)
(*                (Utils.fprintf_array ~sep:",@ " (fun fmt -> Format.fprintf fmt "%f")))) a) mX *)

(* (\* Test LMI *\) *)
(* open LMI.NumLMI *)

(* let _ = *)
(*   let ratio i j = Num.div_num (Num.num_of_int i) (Num.num_of_int j) in *)
(*   let a = Matrix.NumMat.of_list_list *)
(*             [[ratio 15 10; ratio (-7) 10]; [ratio 1 1; ratio 0 1]] in *)
(*   let p = Ident.create "P" in *)
(*   let lambda = Scalar.Num.of_float 0.7 in *)
(*   let e1 = MEsub (MEvar p, MEscale_const (Scalar.Num.of_float 0.00001, MEeye 2)) in *)
(*   let e2 = MEsub (MEscale_const (lambda, MEvar p), *)
(*                   MEmult (MEmult (MEtranspose (MEconst a), MEvar p), MEconst a)) in *)
(*   (\* let q = Ident.create "Q" in *\) *)
(*   (\* let lambda = Ident.create "lambda" in *\) *)
(*   (\* let e1 = MEsub (MEmult (MEmult (MEtranspose (MEconst a), MEvar p), MEconst a), *\) *)
(*   (\*                MEvar p) in *\) *)
(*   (\* let e2 = MEadd (MEvar q, MEmult_scalar (lambda, MEeye 3)) in *\) *)
(*   Format.printf "%a\n%!" pp e1; *)
(*   Format.printf "%a\n%!" pp e2; *)
(*   let el = [e1; e2] in *)
(*   let res, vars = solve Purefeas el in *)
(*   Format.printf "res = %f@." res; *)
(*   if Ident.Map.is_empty vars then *)
(*     Format.printf "erreur SDP@." *)
(*   else *)
(*     Ident.Map.iter *)
(*       (fun id v -> *)
(*          match v with *)
(*          | Scalar e -> Format.printf "%a = %a@." Ident.pp id Scalar.Num.pp e *)
(*          | Mat m -> Format.printf "%a = %a@." Ident.pp id Matrix.NumMat.pp m) *)
(*       vars *)

open SOS.Num

let _ =
  let l = Monomial.list_le 3 2 in
  Format.printf "@[[%a@]]@." (Utils.fprintf_list ~sep:",@ " Monomial.pp) l;
  let pol_of_list l =
    let l = List.map (fun (s, m) -> Monomial.of_list m, Num.num_of_int s) l in
    Polynomial.Num.of_list l in
  let p1 = pol_of_list [2, [4]; 2, [3; 1]; -1, [2; 2]; 5, [0; 4]] in
  Format.printf "p1 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p1;
  let p2 = Polynomial.Num.mult_scalar (Num.num_of_int 2) p1 in
  Format.printf "p2 = 2 p1 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p2;
  let p3 = pol_of_list [3, [1; 1]; -2, [0; 3]; -2, [4]] in
  Format.printf "p3 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p3;
  let p4 = Polynomial.Num.add p1 p3 in
  Format.printf "p4 = p1 + p3 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p4;
  let p5 = Polynomial.Num.power p1 2 in
  Format.printf "p5 = p1^2 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p5;
  let p6 = pol_of_list [1, [2]] in
  Format.printf "p6 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p6;
  let p7 = pol_of_list [1, [0; 2]] in
  Format.printf "p7 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p7;
  let p8 = pol_of_list [1, [1; 1]] in
  Format.printf "p8 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p8;
  let l1 = Polynomial.Num.mult_scalar
             (Num.div_num (Num.num_of_int 1) (Num.num_of_int 10))
             (pol_of_list [15, [1]; -7, [0; 1]]) in
  Format.printf "l1 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) l1;
  let l2 = pol_of_list [1, [1]] in
  Format.printf "l2 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) l2;
  let p9 = Polynomial.Num.compose p6 [l1; l2] in
  Format.printf "p9 = p6(l1, l2) = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p9;
  let p10 = Polynomial.Num.compose p7 [l1; l2] in
  Format.printf "p10 = p7(l1, l2) = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p10;
  let p11 = Polynomial.Num.compose p8 [l1; l2] in
  Format.printf "p11 = p8(l1, l2) = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p11;
  let l = Monomial.list_eq 2 2 in
  Format.printf "@[[%a@]]@." (Utils.fprintf_list ~sep:",@ " Monomial.pp) l;
  let p = { name = Ident.create "p";
            nb_vars = 2;
            degree = 4;
            homogeneous = true } in
  (* solve Purefeas [PLvar p]; *)
  let pe = PLcompose (PLvar p, [PLconst l1; PLconst l2]) in
  Format.printf "pe = @[%a@]@." (pp ~names:[]) pe;
  solve Purefeas [PLcompose (PLvar p, [PLconst l1; PLconst l2])]

(* Local Variables: *)
(* compile-command:"make -C .. test" *)
(* End: *)
