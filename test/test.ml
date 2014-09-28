(* open LMI *)

(* let _ =  *)
(* 	let mat_map f = List.map (fun row -> List.map f row) in *)

(* 	let a =  *)
(*   Matrix.NumMat.of_list_list (mat_map Matrix.num_of_string  [ *)
(*     ["0.9379"; "-0.0381"; "-0.0414"]; *)
(*     ["-0.0404";"0.968";"-0.0179"]; *)
(*     ["0.0142"; "-0.0197"; "0.9823"]; *)
(*   ]) in *)

(* 	let b =  *)
(*   Matrix.NumMat.of_list_list (mat_map Matrix.num_of_string [ *)
(*     ["0.0237"]; *)
(*     ["0.0143"]; *)
(*     ["0.0077"]; *)
(*   ] *)
(*   ) in *)
(* 	let c = NumLMI.kronecker_sym 10 4 6 in *)
(* 	NumLMI.pp_expr (Format.std_formatter) c; *)
(* 	Format.printf "\n"; *)
(* 	exit 0 *)

(* test SOS : F(x, y) = 2x^4+2x^3y-x^2y^2+5y^4
   = (sqrt(2)x^2-1.455122518091861y^2+0.707106781186547xy)^2
     + (1.697827569967575y^2+0.606025616617623xy)^2
     + (1.499479893830934xy)^2 *)
let _ =
  let mC = Array.make_matrix 3 3 0. in
  let mA1 = Array.make_matrix 3 3 0. in
  mA1.(0).(0) <- 1.;
  let a1 = 2. in
  let mA2 = Array.make_matrix 3 3 0. in
  mA2.(1).(1) <- 1.;
  let a2 = 5. in
  let mA3 = Array.make_matrix 3 3 0. in
  mA3.(2).(2) <- 1.;
  mA3.(0).(1) <- 1.; mA3.(1).(0) <- 1.;
  let a3 = -1. in
  let mA4 = Array.make_matrix 3 3 0. in
  mA4.(0).(2) <- 1.; mA4.(2).(0) <- 1.;
  let a4 = 2. in
  let mA5 = Array.make_matrix 3 3 0. in
  mA5.(1).(2) <- 1.; mA5.(2).(1) <- 1.;
  let a5 = 0. in
  let obj = [mC] in
  let cstrs = [[mA1], a1; [mA2], a2; [mA3], a3; [mA4], a4; [mA5], a5] in
  let res, (mX, _) = Csdp.solve obj cstrs in
  Format.printf "res = %f@." res;
  Format.printf "X = ";
  List.iter
    (fun a ->
       Format.printf "[@[%a@]]@."
         (Utils.fprintf_array ~sep:";@ "
            (fun fmt -> Format.fprintf fmt "@[%a@]"
               (Utils.fprintf_array ~sep:",@ " (fun fmt -> Format.fprintf fmt "%f")))) a) mX

let _ =
  Format.printf "\n------------\n@."

open LMI.NumLMI

let _ =
  let ratio i j = Num.div_num (Num.num_of_int i) (Num.num_of_int j) in
  let a = Matrix.NumMat.of_list_list
            [[ratio 15 10; ratio (-7) 10]; [ratio 1 1; ratio 0 1]] in
  let p = Ident.create "P" in
  let lambda = Scalar.Num.of_float 0.7 in
  let e1 = MEsub (MEvar p, MEmult_const (Scalar.Num.of_float 0.00001, MEeye 2)) in
  let e2 = MEsub (MEmult_const (lambda, MEvar p),
                  MEmult (MEmult (MEtranspose (MEconst a), MEvar p), MEconst a)) in
  (* let q = Ident.create "Q" in *)
  (* let lambda = Ident.create "lambda" in *)
  (* let e1 = MEsub (MEmult (MEmult (MEtranspose (MEconst a), MEvar p), MEconst a), *)
  (*                MEvar p) in *)
  (* let e2 = MEadd (MEvar q, MEmult_scalar (lambda, MEeye 3)) in *)
  Format.printf "%a\n%!" pp e1;
  Format.printf "%a\n%!" pp e2;
  let el = [e1; e2] in
  let t = type_check el in
  Ident.Map.iter
    (fun i t ->
       let st = match t with
         | TIscal -> "scalar"
         | TImat (Some n) -> "square matrix of size " ^ string_of_int n
         | _ -> assert false in
       Format.printf "%a : %s\n%!" Ident.pp i st)
    t;
  let al, m = scalarize el t in
  List.iter
    (fun a ->
       Format.printf "[@[%a@]]@."
         (Utils.fprintf_array ~sep:";@ "
            (fun fmt -> Format.fprintf fmt "@[%a@]"
               (Utils.fprintf_array ~sep:",@ " LinExpr.pp))) a) al;
  Ident.Map.iter
    (fun sid (mid, (i, j)) ->
       Format.printf "%a ~~> %a (%i, %i)@." Ident.pp sid Ident.pp mid i j)
    m;
  let res, vars = solve Purefeas el in
  Format.printf "res = %f@." res;
  if Ident.Map.is_empty vars then
    Format.printf "erreur SDP@."
  else
    Ident.Map.iter
      (fun id v ->
         match v with
         | Scalar e -> Format.printf "%a = %a@." Ident.pp id Scalar.Num.pp e
         | Mat m -> Format.printf "%a = %a@." Ident.pp id Matrix.NumMat.pp m)
      vars
