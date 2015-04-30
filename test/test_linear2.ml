let solver = Sdp.Mosek

let ratio i j = Num.div_num (Num.num_of_int i) (Num.num_of_int j)

let a_1 = [[ratio 15 10; ratio (-7) 10]; [ratio 1 1; ratio 0 1]]
let b_1 = [[ratio 16 10]; [ratio 0 10]]

let a_2 = [[ratio 15 10; ratio (-7) 10; ratio (-7) 10; ratio 4 10];
           [ratio 1 1; ratio 0 1; ratio 0 1; ratio 0 1];
           [ratio 0 1; ratio 0 1; ratio 0 1; ratio 0 1];
           [ratio 0 1; ratio 0 1; ratio 1 1; ratio 0 1]]
let b_2 = [[ratio 5 10]; [ratio 0 1]; [ratio 1 1]; [ratio 0 1]]

let a_3 = [[ratio 499 1000; ratio (-5) 100];[ratio 1 100; ratio 1 1]]
let b_3 = [[ratio 1 1]; [ratio 0 1]]

let a_4 = [[ratio 9379 10000; ratio (-381) 10000; ratio (-414) 10000];
	   [ratio (-404) 10000; ratio 968 1000; ratio (-179) 10000]; 
	   [ratio (-142) 10000; ratio (-197) 10000; ratio 9823 10000]]
let b_4 = [[ratio 237 10000]; [ratio 143 10000]; [ratio 77 10000]]

let a_5 = [[ratio 6227 10000;    ratio 3871 10000;    ratio (-113) 1000;   ratio 102  10000];
           [ratio (-3407) 10000; ratio 9103 10000;    ratio (-3388) 10000; ratio 649  10000];
           [ratio 918 10000;     ratio (-265) 10000;  ratio (-7319) 10000; ratio 2669 10000];
           [ratio 2643 10000;    ratio (-1298) 10000; ratio (-9903) 10000; ratio 3331 10000]]
let b_5 = [[ratio 3064 10000;   ratio 1826  10000]; 
           [ratio (-54) 10000;  ratio 6731  10000]; 
           [ratio 494 10000;    ratio 16138 10000]; 
           [ratio (-531) 10000; ratio 4012  10000]]

let a_6 = [[ ratio 4250 10000; ratio    0 1;     ratio       0 1;     ratio    0 1;     ratio       0 1];
           [ ratio 3167 10000; ratio 1016 10000; ratio (-4444) 10000; ratio    0 1;     ratio       0 1];
           [ ratio 1278 10000; ratio 4444 10000; ratio    8207 10000; ratio    0 1;     ratio       0 1];
           [ ratio  365 10000; ratio 1270 10000; ratio    5202 10000; ratio 4163 10000; ratio (-5714) 10000];
           [ ratio  147 10000; ratio  512 10000; ratio    2099 10000; ratio 5714 10000; ratio    7694 10000]]
let b_6 = [[ ratio 8131 10000];
           [ ratio 1807 10000];
           [ ratio  729 10000];
           [ ratio  208 10000];
           [ ratio   84 10000]]

let examples = [a_1, b_1; a_2, b_2; a_3, b_3; a_4, b_4; a_5, b_5; a_6, b_6]

(* Test LMI *)
open LMI.NumLMI

let test_quadratic a b project =
  let a = Mat.of_list_list a in
  let dim_a = Mat.nb_cols a in
  let b = Mat.of_list_list b in
  let dim_b = Mat.nb_cols b in

  let p_id = Ident.create "p" in

  let test_feas_lambda lambda =
    let e1 = MEsub (MEscale_const (Mat.Elem.of_float lambda, MEvar p_id),
                    MEmult (MEmult (MEtranspose (MEconst a), MEvar p_id), MEconst a)) in
    let e2 = MEsub (MEvar p_id, MEeye dim_a) in
    (* Format.printf "e1 = %a\n%!" pp e1; *)
    (* Format.printf "e2 = %a\n%!" pp e2; *)
    let _, _, vars = solve ~solver Purefeas [e1; e2] in
    if Ident.Map.is_empty vars then None
    else
      match Ident.Map.find p_id vars with Mat p -> Some p | _ -> assert false in

  let normalize m =
    let m = Mat.to_list_list m in
    let mc =
      List.fold_left
        (fun m c -> max m (abs_float (Mat.Elem.to_float c)))
        0. (List.flatten m) in
    let mc = Mat.Elem.of_float mc in
    let m = List.map (List.map (fun c -> Mat.Elem.div c mc)) m in
    Mat.of_list_list m in

  let test_radius_lambda p lambda =
    let i0 = MEblock [|[|MEeye dim_a; MEzeros (dim_a, dim_b)|]|] in
    let ab = MEblock [|[|MEconst a; MEconst b|]|] in
    let e1 = MEsub (MEblock [|[|MEminus (MEmult (MEmult (MEtranspose ab, MEconst p), ab)); MEzeros (dim_a + dim_b, 1)|];
                              [|MEzeros (1, dim_a + dim_b); MEscale_const (lambda, MEeye 1)|]|],
                    MEscale_var (Ident.create "tau",
                                 MEblock [|[|MEminus (MEmult (MEmult (MEtranspose i0, MEconst p), i0)); MEzeros (dim_a + dim_b, 1)|];
                                           [|MEzeros (1, dim_a + dim_b); MEscale_const (lambda, MEeye 1)|]|])) in
    let e1, e3l =
      let rec loop n e1 e3l =
        if n >= dim_b then
          e1, e3l
        else
          let tau_id = Ident.create "tau" in
          let e1 = MEsub (e1,
                          MEscale_var (tau_id,
                                       MEblock [|[|MEminus (MEkronecker_sym (dim_a + dim_b, dim_a + n, dim_a + n)); MEzeros (dim_a + dim_b, 1)|];
                                                 [|MEzeros (1, dim_a + dim_b); MEeye 1|]|])) in
          let e3 = MEscale_var (tau_id, MEeye 1) in
          loop (n + 1) e1 (e3 :: e3l) in
      loop 0 e1 [] in
    (* Format.printf "e1 = %a\n%!" pp e1; *)
    (* List.iteri (fun i e3 -> Format.printf "e3 %i = %a\n%!" i pp e3) e3l; *)
    let el = e1 :: e3l in
    let ret, _, _ = solve ~solver Purefeas el in
    SdpRet.is_success ret in

  let project_dim p lambda i =
    let lambda''_id = Ident.create "lambda''" in
    let e = MEsub (MEscale_const (Mat.Elem.div Mat.Elem.one lambda, MEconst p),
                   MEscale_var (lambda''_id,
                                MEadd (MEscale_const (ratio 999 1000,
                                                      MEkronecker_sym (dim_a, i, i)),
                                       MEscale_const (ratio 1 1000,
                                                      MEeye dim_a)))) in
    (* Format.printf "e = %a@." pp e; *)
    let _, (res, _), _ = solve ~solver (Maximize lambda''_id) [e] in
    let lambda'' = if res = infinity || res = neg_infinity then 0. else res in
    sqrt (1. /. lambda'') in

  let nb_dicho_p = 10 in
  let nb_dicho_lambda = 10 in
  let rec dicho n tau_min_p min max =
    let mid = (min +. max) /. 2. in
    let tau_min_p, min, max = match test_feas_lambda mid with
      | None -> tau_min_p, mid, max
      | Some p -> Some (mid, p), min, mid in
    if n > 0 then dicho (n - 1) tau_min_p min max else tau_min_p in
  match dicho nb_dicho_p None 0. 1. with
  | None -> failwith "unable to find tau_min"
  | Some (tau_min, p) ->
     if not project then tau_min
     else
       begin
         Format.printf "tau_min = %g@." tau_min;
         let p = normalize p in
         Format.printf "p = %a@." Mat.pp p;
         let last_lambda, lambda =
           let rec kleene last_lambda lambda =
             if test_radius_lambda p lambda then last_lambda, lambda
             else kleene lambda (Num.mult_num (Num.num_of_int 2) lambda) in
           kleene (Num.num_of_int 0) (Num.num_of_int 1) in
         Format.printf "last_lambda = %g, lambda = %g@."
                       (Num.float_of_num last_lambda) (Num.float_of_num lambda);
         let rec dicho n min max =
           let mid = Num.div_num (Num.add_num min max) (Num.num_of_int 2) in
           let min, max = if test_radius_lambda p mid then min, mid else mid, max in
           if n > 0 then dicho (n - 1) min max else max in
         let lambda = dicho nb_dicho_lambda last_lambda lambda in
         Format.printf "lambda = %g@." (Num.float_of_num lambda);
         let rec loop i =
           if i >= dim_a then []
           else project_dim p lambda i :: loop (i + 1) in
         let bounds = loop 0 in
         Format.printf "bounds = %a@." (Utils.fprintf_list
                                          ~sep:", "
                                          (fun fmt -> Format.fprintf fmt "%.2f"))
                       bounds;
         tau_min
       end

(* Test SOS *)
open SOS.Float

let test_SOS a b deg =
  let nb_dicho_lambda = 12 in
  let tau_min = test_quadratic a b false in
  let tau_min = tau_min ** (float_of_int deg /. 2.) in
  let tau_min = (90. *. tau_min +. 10.) /. 100. in
  Format.printf "tau_min = %g@." tau_min;
  let p_id = Ident.create "p" in
  let nb_vars = List.length a in
  let nb_inputs = List.length (List.hd b) in
  let names = [] in
  let pp_poly = Poly.pp ~names in
  let pol_of_list l =
    Poly.of_list (List.map (fun (s, m) -> Monomial.of_list m, s) l) in
  let cp f = pol_of_list [f, []] in  (* constant polynomial *)
  let vp f n =  (* f x_n ^ deg *)
    pol_of_list [f, Array.to_list (Array.make n 0) @ [deg]] in
  let l =
    a
    |> List.map
         (fun a ->
          List.map Scalar.Num.to_float a
          |> (List.fold_left (fun (m, l) c -> 0 :: m, (c, m) :: l) ([1], []))
          |> snd |> pol_of_list) in
  (* List.iteri (fun i l -> Format.printf "l%i = %a@." i pp_poly l) l; *)
  let p = { name = p_id;
            nb_vars = nb_vars;
            degree = deg;
            homogeneous = true } in
  let p_norm =
    let rec loop n =
      if n >= nb_vars then Poly.zero
      else Poly.add (vp 0.00000001 n) (loop (n + 1)) in
    loop 0 in
  let pe1 = PLsub (PLmult (PLconst (cp tau_min), PLadd (PLvar p, PLconst p_norm)),
                   PLcompose (PLadd (PLvar p, PLconst p_norm),
                              List.map (fun p -> PLconst p) l)) in
  Format.printf "pe1 = %a@." (pp ~names) pe1;
  (* let pe2 = *)
  (*   let p_norm = *)
  (*     let rec loop n = *)
  (*       if n >= nb_vars then Poly.zero *)
  (*       else Poly.add (vp 0.00000001 n) (loop (n + 1)) in *)
  (*     loop 0 in *)
  (*   PLsub (PLvar p, PLconst p_norm) in *)
  (* Format.printf "pe2 = %a@." (pp ~names) pe2; *)
  let _, _, vars = solve ~solver Purefeas [pe1(* ; pe2 *)] in
  let p =
    if Ident.Map.is_empty vars then None
    else
      match Ident.Map.find p_id vars with Poly p -> Some p | _ -> assert false in
  match p with
  | None -> failwith "unable to find template P"
  | Some p ->
     let p = Poly.add p p_norm in
     let normalize p =
       let p = Poly.to_list p in
       let mp = List.fold_left (fun m (_, c) -> max m (abs_float c)) 0. p in
       let p = List.map (fun (m, c) -> m, c /. mp) p in
       Poly.of_list p in
     Format.printf "p = %a@." pp_poly p;
     let p = normalize p in
     Format.printf "p = %a@." pp_poly p;
     let l =
       List.combine a b
       |> List.map
         (fun (a, b) ->
          a @ b
          |> List.map Scalar.Num.to_float
          |> List.fold_left (fun (m, l) c -> 0 :: m, (c, m) :: l) ([1], [])
          |> snd |> pol_of_list) in
     List.iteri (fun i l -> Format.printf "l%i = %a@." i pp_poly l) l;
     let test_radius_lambda p lambda =
       let pe1 = PLsub (PLsub (PLconst (cp lambda),
                               PLcompose (PLconst p, List.map (fun p -> PLconst p) l)),
                        PLmult_scalar (Ident.create "lambda",
                                       PLsub (PLconst (cp lambda), PLconst p))) in
       let pe1 =
         let rec loop n pe1 =
           if n >= nb_inputs then
             pe1
           else
             let pe1 =
               PLsub (pe1,
                      PLmult_scalar (Ident.create "tau",
                                     PLsub (PLconst (cp 1.),
                                            PLconst (vp 1. (nb_vars + n))))) in
             loop (n + 1) pe1 in
         loop 0 pe1 in
       (* Format.printf "pe1 = %a@." (pp ~names) pe1; *)
       Format.printf "testing radius %g@." lambda;
       let ret, _, _ = solve ~solver Purefeas [pe1(* ; pe2 *)] in
       let res = SdpRet.is_success ret in
       Format.printf "res = %b@." res;
       res in
     let last_lambda, lambda =
       let rec kleene last_lambda lambda =
         if test_radius_lambda p lambda then last_lambda, lambda
         else kleene lambda (2. *. lambda) in
       kleene 0. 1024. in
     Format.printf "last_lambda = %g, lambda = %g@." last_lambda lambda;
     let rec dicho n min max =
       let mid = (min +. max) /. 2. in
       let min, max = if test_radius_lambda p mid then min, mid else mid, max in
       if n > 0 then dicho (n - 1) min max else max in
     let lambda = dicho nb_dicho_lambda last_lambda lambda in
     Format.printf "lambda = %g@." lambda;
     let p = Poly.mult_scalar (1. /. lambda) p in
     Format.printf "p = %a@." pp_poly p;
     let project_dim p i =
       let lambda''_id = Ident.create "lambda''" in
       let p_proj =
         let rec loop n =
           if n >= nb_vars then Poly.zero
           else Poly.add (vp (if n = i then 1. else 0.0001) n) (loop (n + 1)) in
         loop 0 in
       let pe = PLsub (PLconst p, PLmult_scalar (lambda''_id, PLconst p_proj)) in
       (* Format.printf "pe = %a@." (pp ~names) pe; *)
       let _, _, vars = solve ~solver (Maximize lambda''_id) [pe] in
       let lambda'' =
         try
           match Ident.Map.find lambda''_id vars with
           | Scalar lambda'' -> lambda''
           | _ -> infinity
         with Not_found -> infinity in
       Format.printf "lambda'' = %g@." lambda'';
       exp (-. log lambda'' /. float_of_int deg) in
     let rec loop i =
       if i >= nb_vars then []
       else project_dim p i :: loop (i + 1) in
     let bounds = loop 0 in
     Format.printf "bounds = %a@." (Utils.fprintf_list
                                      ~sep:", "
                                      (fun fmt -> Format.fprintf fmt "%.2f"))
                   bounds
                 
let _ =
  let usage () =
    Format.printf "Usage: %s <n> <Q|S<n>>@." Sys.argv.(0);
    exit 0 in
  if Array.length Sys.argv <> 3 then usage ();
  let n = int_of_string Sys.argv.(1) in
  let a, b = List.nth examples (n - 1) in
  match Sys.argv.(2).[0] with
  | 'Q' -> let _ = test_quadratic a b true in ()
  | 'S' ->
     let deg =
       Sys.argv.(2).[0] <- ' ';
       int_of_string (String.trim Sys.argv.(2)) in
     Format.printf "deg = %i@." deg;
     test_SOS a b deg
  | _ -> usage ()

(* Local Variables: *)
(* compile-command:"make -C .. test_linear2" *)
(* End: *)
