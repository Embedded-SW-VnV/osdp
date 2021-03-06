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
(*   let b = Matrix.NumMat.of_list_list *)
(*             [[ratio 16 10]; [ratio 0 10]] in *)
(*   let p = Ident.create "P" in *)
(*   let dim_a = 2 in *)
(*   let dim_b = 1 in *)
(*   let lambda = Scalar.Num.of_float 0.84 in *)
(*   let tau_id = Ident.create "tau" in *)
(*   let lambda'_id = Ident.create "lambda'" in *)
(*   let i0 = MEblock [|[|MEeye dim_a; MEzeros (dim_a, dim_b)|]|] in *)
(*   let ab = MEblock [|[|MEconst a; MEconst b|]|] in *)
(*   let e1 = MEsub (MEsub (MEblock [|[|MEminus (MEmult (MEmult (MEtranspose ab, MEvar p), ab)); MEzeros (dim_a + dim_b, 1)|]; *)
(*                                    [|MEzeros (1, dim_a + dim_b); MEeye 1|]|], *)
(*                          MEscale_const (lambda, *)
(*                                         MEblock [|[|MEminus (MEmult (MEmult (MEtranspose i0, MEvar p), i0)); MEzeros (dim_a + dim_b, 1)|]; *)
(*                                                   [|MEzeros (1, dim_a + dim_b); MEeye 1|]|])), *)
(*                   MEscale_var (tau_id, MEblock [|[|MEminus (MEkronecker_sym (dim_a + dim_b, dim_a, dim_a)); MEzeros (dim_a + dim_b, 1)|]; *)
(*                                                  [|MEzeros (1, dim_a + dim_b); MEeye 1|]|])) in *)
(*   let e2 = MEsub (MEvar p, MEscale_var (lambda'_id, MEeye dim_a)) in *)
(*   let e3 = MEscale_var (tau_id, MEeye 1) in *)
(*   Format.printf "e1 = %a\n%!" pp e1; *)
(*   Format.printf "e2 = %a\n%!" pp e2; *)
(*   let el = [e1; e2] in *)
(*   let res, vars = solve (Maximize lambda'_id) el in *)
(*   Format.printf "res = %f@." res; *)
(*   if Ident.Map.is_empty vars then *)
(*     Format.printf "erreur SDP@." *)
(*   else *)
(*     Ident.Map.iter *)
(*       (fun id v -> *)
(*          match v with *)
(*          | Scalar e -> Format.printf "%a = %a (%g)@." Ident.pp id Scalar.Num.pp e (Scalar.Num.to_float e) *)
(*          | Mat m -> Format.printf "%a = %a@." Ident.pp id Matrix.NumMat.pp m) *)
(*       vars; *)
(*   let Mat m = Ident.Map.find p vars in *)
(*   let lambda''_id = Ident.create "lambda''" in *)
(*   let e3 = MEsub (MEconst m, *)
(*                   MEscale_var (lambda''_id, *)
(*                                MEconst (Matrix.NumMat.of_list_list *)
(*                                           [[ratio 1 10000; ratio 0 1]; *)
(*                                            [ratio 0 1; ratio 1 1]]))) in *)
(*   Format.printf "e3 = %a@." pp e3; *)
(*   let res, vars = solve (Maximize lambda''_id) [e3] in *)
(*   Format.printf "res = %f@." res; *)
(*   if Ident.Map.is_empty vars then *)
(*     Format.printf "erreur SDP@." *)
(*   else *)
(*     Ident.Map.iter *)
(*       (fun id v -> *)
(*          match v with *)
(*          | Scalar e -> Format.printf "%a = %a (%g)@." Ident.pp id Scalar.Num.pp e (Scalar.Num.to_float e) *)
(*          | Mat m -> Format.printf "%a = %a@." Ident.pp id Matrix.NumMat.pp m) *)
(*       vars *)
(*       (\* bounds: |x| <= 16.06, |y| <= 17.52 *)
(*          (.01909721391381722505 x^2 -.03124999027762423442 x y + .01604166373440409423 y^2 <= 1) *\) *)
(*       (\* PI + x^2 + itv: |x| <= 15.98, |y| <= 15.98 *)
(*          (.01885522766262599791 x^2 -.03101999134118256506 x y + .01602705872706839881 y^2 <= 1.00032 *)
(*           /\ x^2 <= 255.157) *\) *)

(* (\* Test SOS *\) *)
(* open SOS.Num *)

(* let _ = *)
(*   let l = Monomial.list_le 3 2 in *)
(*   Format.printf "@[[%a@]]@." (Utils.fprintf_list ~sep:",@ " Monomial.pp) l; *)
(*   let pol_of_list l = *)
(*     let l = List.map (fun (s, m) -> Monomial.of_list m, Num.num_of_int s) l in *)
(*     Polynomial.Num.of_list l in *)
(*   let p1 = pol_of_list [2, [4]; 2, [3; 1]; -1, [2; 2]; 5, [0; 4]] in *)
(*   Format.printf "p1 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p1; *)
(*   let p2 = Polynomial.Num.mult_scalar (Num.num_of_int 2) p1 in *)
(*   Format.printf "p2 = 2 p1 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p2; *)
(*   let p3 = pol_of_list [3, [1; 1]; -2, [0; 3]; -2, [4]] in *)
(*   Format.printf "p3 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p3; *)
(*   let p4 = Polynomial.Num.add p1 p3 in *)
(*   Format.printf "p4 = p1 + p3 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p4; *)
(*   let p5 = Polynomial.Num.power p1 2 in *)
(*   Format.printf "p5 = p1^2 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p5; *)
(*   let p6 = pol_of_list [1, [2]] in *)
(*   Format.printf "p6 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p6; *)
(*   let p7 = pol_of_list [1, [0; 2]] in *)
(*   Format.printf "p7 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p7; *)
(*   let p8 = pol_of_list [1, [1; 1]] in *)
(*   Format.printf "p8 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p8; *)
(*   let l1 = Polynomial.Num.mult_scalar *)
(*              (Num.div_num (Num.num_of_int 1) (Num.num_of_int 10)) *)
(*              (pol_of_list [15, [1]; -7, [0; 1]]) in *)
(*   Format.printf "l1 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) l1; *)
(*   let l2 = pol_of_list [1, [1]] in *)
(*   Format.printf "l2 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) l2; *)
(*   let p9 = Polynomial.Num.compose p6 [l1; l2] in *)
(*   Format.printf "p9 = p6(l1, l2) = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p9; *)
(*   let p10 = Polynomial.Num.compose p7 [l1; l2] in *)
(*   Format.printf "p10 = p7(l1, l2) = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p10; *)
(*   let p11 = Polynomial.Num.compose p8 [l1; l2] in *)
(*   Format.printf "p11 = p8(l1, l2) = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p11; *)
(*   let l = Monomial.list_eq 2 2 in *)
(*   Format.printf "@[[%a@]]@." (Utils.fprintf_list ~sep:",@ " Monomial.pp) l; *)
(*   let p12 = pol_of_list [1, [4]; 1, [0; 4]] in *)
(*   Format.printf "p12 = %a@." (Polynomial.Num.pp ~names:["x"; "y"]) p12; *)
(*   let p = { name = Ident.create "p"; *)
(*             nb_vars = 2; *)
(*             degree = 4; *)
(*             homogeneous = true } in *)
(*   (\* solve Purefeas [PLvar p]; *\) *)
(*   let pe1 = PLcompose (PLvar p, [PLconst l1; PLconst l2]) in *)
(*   Format.printf "pe1 = @[%a@]@." (pp ~names:[]) pe1; *)
(*   let lambda_id = Ident.create "lambda" in *)
(*   let pe2 = PLsub (PLvar p, PLmult_scalar (lambda_id, PLconst p12)) in *)
(*   Format.printf "pe2 = @[%a@]@." (pp ~names:[]) pe2; *)
(*   let res, vars = solve Purefeas [pe1; pe2] in *)
(*   Format.printf "res = %f@." res; *)
(*   if Ident.Map.is_empty vars then *)
(*     Format.printf "erreur SDP@." *)
(*   else *)
(*     Ident.Map.iter *)
(*       (fun id v -> *)
(*          match v with *)
(*          | Scalar e -> Format.printf "%a = %a@." Ident.pp id Scalar.Num.pp e *)
(*          | Poly p -> Format.printf "%a = %a@." Ident.pp id (Polynomial.Num.pp ~names:["x"; "y"]) p) *)
(*       vars *)

(* (\* Test SOS 2 *\) *)
(* open SOS.Float *)

(* (\* let test lambda = *\) *)
(* let _ = *)
(*   let lambda = 0.84 in *)
(*   let names = ["x"; "y"; "u"] in *)
(*   let pp_poly = Poly.pp ~names in *)
(*   let pol_of_list l = *)
(*     Poly.of_list (List.map (fun (s, m) -> Monomial.of_list m, s) l) in *)
(*   let cp f = pol_of_list [f, []] in *)
(*   let deg = 4 in *)
(*   let p = { name = Ident.create "p"; *)
(*             nb_vars = 2; *)
(*             degree = deg; *)
(*             homogeneous = true } in *)
(*   Format.printf "cp lambda = %a@." pp_poly (cp lambda); *)
(*   let tau_id = Ident.create "tau" in *)
(*   let lambda'_id = Ident.create "lambda'" in *)
(*   let l1 = pol_of_list [1.5, [1]; -0.7, [0; 1]; 1.6, [0; 0; 1]] in *)
(*   Format.printf "l1 = %a@." pp_poly l1; *)
(*   let l1' = pol_of_list [1.5, [1]; -0.7, [0; 1]] in *)
(*   Format.printf "l1' = %a@." pp_poly l1'; *)
(*   let l2 = pol_of_list [1., [1]] in *)
(*   Format.printf "l2 = %a@." pp_poly l2; *)
(*   let pe1 = PLsub (PLsub (PLsub (PLconst (cp 1.), *)
(*                                  PLcompose (PLvar p, [PLconst l1; PLconst l2])), *)
(*                           PLmult (PLconst (cp lambda), *)
(*                                   PLsub (PLconst (cp 1.), PLvar p))), *)
(*                    PLmult_scalar (tau_id, *)
(*                                   PLsub (PLconst (cp 1.), *)
(*                                          PLconst (pol_of_list [1., [0; 0; deg]])))) in *)
(*   Format.printf "pe1 = %a@." (pp ~names) pe1; *)
(*   let pe1' = PLsub (PLmult (PLconst (cp lambda), PLvar p), *)
(*                     PLcompose (PLvar p, [PLconst l1'; PLconst l2])) in *)
(*   Format.printf "pe1' = %a@." (pp ~names) pe1'; *)
(*   let pe2 = PLsub (PLvar p, *)
(*                    PLmult_scalar (lambda'_id, *)
(*                                   PLconst (pol_of_list [1., [deg]; 1., [0; deg]]))) in *)
(*   Format.printf "pe2 = %a@." (pp ~names) pe2; *)
(*   let pe2' = PLsub (PLvar p, *)
(*                    PLmult (PLconst (cp 0.00001), *)
(*                            PLconst (pol_of_list [1., [deg]; 1., [0; deg]]))) in *)
(*   Format.printf "pe2' = %a@." (pp ~names) pe2'; *)
(*   let res, vars = solve (Maximize lambda'_id) [pe1; pe2] in *)
(*   Format.printf "res = %f@." res; *)
(*   if Ident.Map.is_empty vars then *)
(*     Format.printf "erreur SDP@." *)
(*   else *)
(*     Ident.Map.iter *)
(*       (fun id v -> *)
(*          match v with *)
(*          | Scalar e -> Format.printf "%a = %a@." Ident.pp id Scalar.Float.pp e *)
(*          | Poly p -> Format.printf "%a = %a@." Ident.pp id pp_poly p) *)
(*       vars; *)
(*   let Poly p = Ident.Map.find p.name vars in *)
(*   let lambda''_id = Ident.create "lambda''" in *)
(*   let pe3 = PLsub (PLconst p, *)
(*                    PLmult_scalar (lambda''_id, *)
(*                                   PLconst (pol_of_list [1., [deg]; 0.0001, [0; deg]]))) in *)
(*   Format.printf "pe3 = %a@." (pp ~names) pe3; *)
(*   let res, vars = solve (Maximize lambda''_id) [pe3] in *)
(*   Format.printf "res = %f@." res; *)
(*   if Ident.Map.is_empty vars then *)
(*     Format.printf "erreur SDP@." *)
(*   else *)
(*     Ident.Map.iter *)
(*       (fun id v -> *)
(*          match v with *)
(*          | Scalar e -> Format.printf "%a = %a@." Ident.pp id Scalar.Float.pp e *)
(*          | Poly p -> Format.printf "%a = %a@." Ident.pp id pp_poly p) *)
(*       vars *)
(*       (\* bounds (degree 4): |x| <= 15.57, |y| <= 16.64 *)
(*          (0.000299013671468 y^4 + -0.00116129107886 x y^3 + 0.0018468906396 *)
(*           x^2 y^2 + -0.00139235326831 x^3 y + 0.000425080699248 x^4 <= 1) *\) *)
(*       (\* bounds (degree 6): |x| <= 15.44, |y| <= 16.28 *)
(*          (5.67584790715e-06 y^6 + -3.30073547184e-05 x y^5 *)
(*           + 8.41566023528e-05 x^2 y^4 + -0.000119401915527 x^3 y^3 *)
(*           + 9.93706446912e-05 x^4 y^2 + -4.60592130938e-05 x^5 y *)
(*           + 9.34269976033e-06 x^6 <= 1) *\) *)
(*       (\* bounds (degree 8 (numerical problems ?)): |x| <= 16.64, |y| <= 17.65 *)
(*          (6.92661602697e-08 y^8 + -5.37224572792e-07 x y^7 *)
(*           + 1.89408048343e-06 x^2 y^6 + -3.9480321158e-06 x^3 y^5 *)
(*           + 5.30962943268e-06 x^4 y^4 + -4.71135602647e-06 x^5 y^3 *)
(*           + 2.69251953052e-06 x^6 y^2 + -9.06892465936e-07 x^7 y *)
(*           + 1.38236764031e-07 x^8 <= 1) *\) *)
(*       (\* bounds (degree 10 (false)): |x| <= 12.32, |y| <= 14.03 *)
(*          (5.34096105338e-10 y^10 + -6.19049102191e-09 x y^9 *)
(*           + 3.28183155997e-08 x^2 y^8 + -1.03925168567e-07 x^3 y^7 *)
(*           + 2.16175011125e-07 x^4 y^6 + -3.07271725778e-07 x^5 y^5 *)
(*           + 3.01847942477e-07 x^6 y^4 + -2.02733551642e-07 x^7 y^3 *)
(*           + 8.95507588387e-08 x^8 y^2 + -2.36814750873e-08 x^9 y *)
(*           + 2.87630122667e-09 x^10 <= 1)*\) *)

(* let _ = *)
(*   let lambda = ref 0.48 in *)
(*   let best_lambda = ref neg_infinity in *)
(*   let best_lambda' = ref neg_infinity in *)
(*   while !lambda < 1. do *)
(*     let l' = test !lambda in *)
(*     if l' > !best_lambda' then begin *)
(*       best_lambda := !lambda; *)
(*       best_lambda' := l' *)
(*     end; *)
(*     lambda := !lambda +. 0.1 *)
(*   done; *)
(*   Format.printf "best_lambda = %f (best_lambda' = %f)@." *)
(*                 !best_lambda !best_lambda' *)

(* test MOSEK *)
(* test SOS : F(x, y) = 2x^4+2x^3y-x^2y^2+5y^4
   = (sqrt(2)x^2-1.455122518091861y^2+0.707106781186547xy)^2
     + (1.697827569967575y^2+0.606025616617623xy)^2
     + (1.499479893830934xy)^2 *)
let _ =
  let mA1 = [0, [0, 0, 1.]] in
  let a1 = 2. in
  let mA2 = [0, [1, 1, 1.]] in
  let a2 = 5. in
  let mA3 = [0, [1, 0, 1.; 2, 2, 1.]] in
  let a3 = -1. in
  let mA4 = [0, [2, 0, 1.]] in
  let a4 = 2. in
  let mA5 = [0, [2, 1, 1.]] in
  let a5 = 0. in
  let obj = [] in
  let cstrs = [mA1, a1; mA2, a2; mA3, a3; mA4, a4; mA5, a5] in
  let _, (res, _), (mX, y) = Moseksdp.solve obj cstrs in
  Format.printf "res = %f@." res;
  Format.printf "X = ";
  List.iter
    (fun a ->
       Format.printf "[@[%a@]]@."
         (Utils.fprintf_array ~sep:";@ "
            (fun fmt -> Format.fprintf fmt "@[%a@]"
               (Utils.fprintf_array ~sep:",@ " (fun fmt -> Format.fprintf fmt "%g")))) a) mX;
  Format.printf
    "y = [|@[%a@]|]@."
    (Utils.fprintf_array ~sep:";@ " (fun fmt -> Format.fprintf fmt "%g")) y

(* Local Variables: *)
(* compile-command:"make -C .. test" *)
(* End: *)
