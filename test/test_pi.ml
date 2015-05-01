(*********************)
(* Policy iterations *)
(*********************)

(* Types defining policy iteration problems. *)

type cstr =
  | LeConst of Osdp.Polynomial.Float.t * float  (* <= f *)
  | LeVar of Osdp.Polynomial.Float.t * int  (* <= ith equation (starting from 0) *)

(* max {p | cstrs } *)
type expr = Osdp.Polynomial.Float.t * cstr list

(* An equation is the max of each expression. *)
type eq = expr list

(* Defining z := [Osdp.Monomial.list_eq nb_vars deg] or
   [Osdp.Monomial.list_le nb_vars deg] according to homogeneous
   (resp. true or false), returns [sz, l] with sz the size of z and l
   the list of all (monomial, (i, j)) such that monomial = z_i * z_j
   sorted by increasing order of monomial (Osdp.Monomial.compare). *)
let list_monom_coords nb_vars deg homogeneous =
  let base =
    let l =  
      if homogeneous then Osdp.Monomial.list_eq nb_vars deg
      else Osdp.Monomial.list_le nb_vars deg in
    Array.of_list l in
  let sz = Array.length base in
  let l = ref [] in
  for i = sz - 1 downto 0 do
    for j = 0 to i do
      l := (Osdp.Monomial.mult base.(i) base.(j), (i, j)) :: !l
    done
  done;
  sz, List.stable_sort (fun (m, _) (m', _) -> Osdp.Monomial.compare m m') !l

(* Returns a symmetric matrix M such that [p] = z^T * M * z (see
   definition of z above). *)
let mat_of_poly nb_vars deg homogeneous p =
  let sz, l = list_monom_coords nb_vars deg homogeneous in
  let rec remove_duplicates l = match l with
    | [] | [_] -> l
    | ((m1, _) as h1) :: (((m2, _) :: t2) as t1) ->
       if Osdp.Monomial.compare m1 m2 = 0 then remove_duplicates (h1 :: t2)
       else h1 :: remove_duplicates t1 in
  let l = remove_duplicates l in
  (* Format.printf *)
  (*   "l = @[%a@]@." *)
  (*   (Osdp.Utils.fprintf_list *)
  (*      ~sep:",@ " *)
  (*      (fun fmt (m, (i, j)) -> *)
  (*       Format.fprintf fmt "(%a, (%d, %d))" Osdp.Monomial.pp m i j)) *)
  (*   l; *)
  let a = Array.make_matrix sz sz 0. in
  (* let p0 = p in *)
  let rec match_poly l p = match l, p with
    | _, [] -> ()
    | [], (m, c) :: _ ->
       (* Format.printf "nb_vars = %d, deg = %d, homogeneous = %B@." nb_vars deg homogeneous; *)
       (* Format.printf "p = %a@." Osdp.Polynomial.Float.pp p0; *)
       (* Format.printf "m = %a, c = %g@." Osdp.Monomial.pp m c; *)
       raise (Invalid_argument
                "nb_vars, deg or homogeneous inconsistant with p")
    | (ml, (i, j)) :: tl, (mp, c) :: tp ->
       if Osdp.Monomial.compare ml mp = 0 then
         begin
           if i = j then a.(i).(i) <- c
           else begin a.(i).(j) <- c /. 2.; a.(j).(i) <- c /. 2. end;
           match_poly tl tp
         end
       else match_poly tl p in
  match_poly l (Osdp.Polynomial.Float.to_list p); a

(* Returns a list of all tuples (i, j), (i', j') such that z_i * z_j =
   z_i' * z_j' (no transitive closure: if z_i z_j = z_i' z_j' = z_i''
   z_j'', only z_i z_j = z_i' z_j' and z_i z_j = z_i'' z_j'' will be
   returned but not z_i z_j = z_i'' z_j''). *)
let eqs_of_params nb_vars deg homogeneous =
  let sz, l = list_monom_coords nb_vars deg homogeneous in
  let rec split = function
    | [] -> [] | [(_, ij)] -> [[ij]]
    | (m1, ij1) :: (((m2, _) :: _) as t1) ->
       if Osdp.Monomial.compare m1 m2 = 0 then
         match split t1 with
         | [] -> assert false
         | ijl :: t -> (ij1 :: ijl) :: t
       else [ij1] :: split t1 in
  let distrib = function
    | [] -> []
    | h :: t -> List.map (fun h' -> h, h') t in
  sz, split l |> List.map distrib |> List.filter ((<>) []) |> List.flatten

(* Types for linear min policies *)

(* An expression (c, a) represents the linear expression c + \sum_{i
    <= Array.length a} a.(i) * x_i *)
type min_policy_expr = float * float array

(* An equation is the max of each expression. *)
type min_policy_eq = min_policy_expr list

(* A system of equations. Type invariant: no expression in the system
   can have more variables than the number of equations (list length)
   in the system. *)
type min_policy_system = min_policy_eq list

let fprintf_min_policy_expr fmt (c, a) =
  let need_plus = ref false in
  for i = 0 to Array.length a - 1 do
    if a.(i) > 0.00000001 || a.(i) < -0.00000001 then begin
      if !need_plus then begin
        if a.(i) = 1. then Format.fprintf fmt "+"
        else if a.(i) = -1. then Format.fprintf fmt "-"
        else Format.fprintf fmt  "%+g*" a.(i)
      end else begin
        if a.(i) = 1. then ()
        else if a.(i) = -1. then Format.fprintf fmt "-"
        else Format.fprintf fmt "%g*" a.(i)
      end;
      Format.fprintf fmt "a%d" (i + 1);
      need_plus := true
    end
  done;
  if not !need_plus then
    Format.fprintf fmt "%g" c
  else if c > 0.00000001 || c < -0.00000001 then
    Format.fprintf fmt "%+g" c

let fprintf_min_policy_eq fmt eq =
  Format.fprintf fmt "@[%a@]"
    (Osdp.Utils.fprintf_list ~sep:"@ \\/ " fprintf_min_policy_expr) eq

let fprintf_min_policy_system fmt eqs =
  Format.fprintf fmt "@[<v>%a@]"
    (Osdp.Utils.fprintf_list ~sep:"@ " fprintf_min_policy_eq) eqs

let relax_sdp_solve ~(solver : Osdp.Sdp.solver) (obj : Osdp.Polynomial.Float.t)
                    (cstrs : (Osdp.Polynomial.Float.t * float) list)
    : Osdp.SdpRet.t * (float * float) * (Osdp.Sdp.matrix Osdp.Sdp.block_diag * float array) =
  let nb_vars, deg, homogeneous =
    List.fold_left
      (fun (n, d, h) (p, _) ->
       max n (Osdp.Polynomial.Float.nb_vars p),
       max d (Osdp.Polynomial.Float.degree p),
       h && Osdp.Polynomial.Float.is_homogeneous p)
      (0, 0, true)
      ((obj, 0.) :: cstrs) in
  let deg = (deg + 1) / 2 in
  let eqs =
    (* equalities among monomials (for instance x^2 * y^2 = (x y) * (x y)) *)
    let sz, eqs = eqs_of_params nb_vars deg homogeneous in
    let cstr_of_eq ((i, j), (i', j')) =
      let a = Array.make_matrix sz sz 0. in
      if i = j then a.(i).(i) <- 1.
      else begin a.(i).(j) <- 0.5; a.(j).(i) <- 0.5 end;
      if i' = j' then a.(i').(i') <- -1.
      else begin a.(i').(j') <- -0.5; a.(j').(i') <- -0.5 end;
      Osdp.Sdp.Eq ([0, a], 0.) in
    if homogeneous then List.map cstr_of_eq eqs
    else
      (* first line and column are for linear terms *)
      let cstr1 = Osdp.Sdp.Eq ([0, [|[|1.|]|]], 1.) in
      cstr1 :: List.map cstr_of_eq eqs in
  let cstrs =
    let cstr_of_cstr (p, f) =
      Osdp.Sdp.Le ([0, mat_of_poly nb_vars deg homogeneous p], f) in
    List.map cstr_of_cstr cstrs @ eqs in
  let obj = [0, mat_of_poly nb_vars deg homogeneous obj] in
  Osdp.Sdp.solve ~solver obj cstrs

(* Find a good min policy at point rho. *)
let improve_strat_eqs ~(solver : Osdp.Sdp.solver) (eqs : eq list) (rho : float list) : min_policy_system =
  let improve_strat_expr (obj, cstrs) =
    try
      let trans, add_cstrs, cstrs =
        List.fold_left
          (fun (trans, add_cstrs, cstrs) cstr ->
           match cstr with
           | LeConst (e, n) -> trans, add_cstrs, ((e, n) :: cstrs)
           | LeVar (e, i) ->
              let f = List.nth rho i in
              if f = neg_infinity then raise Exit
              else if f = infinity then trans, add_cstrs, cstrs
              else (i :: trans), ((e, f) :: add_cstrs), cstrs)
          ([], [], [])
          cstrs in
      if add_cstrs = [] then
        let _, (_, res), _ = relax_sdp_solve ~solver obj cstrs in
        1.0000001 *. res, [||]
      else
        let ret, (_, res), (_, y) = relax_sdp_solve ~solver obj (add_cstrs @ cstrs) in
        (* Format.printf "ret = %a@." Osdp.SdpRet.pp ret; *)
        if not (Osdp.SdpRet.is_success ret) then
          infinity, Array.make (List.length add_cstrs) 0.
        else
          begin
            (* Format.printf *)
            (*   "y = @[%a@]@." *)
            (*   (Osdp.Utils.fprintf_array ~sep:",@ " Format.pp_print_float) y; *)
            let mu, lambda_list =
              let y = Array.sub y 0 (List.length add_cstrs) in
              let c, _ =
                List.fold_left
                  (fun (c, i) (_, f) -> c -. y.(i) *. f, i + 1)
                  (res, 0) add_cstrs in
              c, Array.to_list y in
            let lambda = Array.make (List.fold_left max 0 trans + 1) 0. in
            List.iter2 (Array.set lambda) trans lambda_list;
            let mu = 1.00004 *. mu in
            for i = 0 to Array.length lambda - 1 do
              lambda.(i) <- lambda.(i) *. 1.00004
            done;
            mu, lambda
          end
    with Exit -> neg_infinity, [||] in
  List.map (List.map improve_strat_expr) eqs

(* Compute least fixpoint of a min policy using linear programming (GLPK). *)
let min_policy_lfp eqs =
  let min_policy_finite_lfp eqs =
    let n = List.length eqs in
    let zcoeffs = Array.make n 1. in
    let nbcstr = List.fold_left (fun nb eq -> nb + List.length eq) 0 eqs in
    let constrs =
      let constrs = Array.make_matrix nbcstr n 0. in
      let _ = List.fold_left (fun (ieq, icstr) eq ->
        let icstr = List.fold_left (fun icstr (_, a) ->
          for i = 0 to Array.length a - 1 do
            constrs.(icstr).(i) <- a.(i)
          done;
          constrs.(icstr).(ieq) <- constrs.(icstr).(ieq) -. 1.;
          icstr + 1) icstr eq in
        (ieq + 1, icstr)) (0, 0) eqs in
      constrs in
    let pbounds =
      let pbounds = Array.make nbcstr (0., 0.) in
      let _ = List.fold_left (fun icstr eq ->
        let icstr = List.fold_left (fun icstr (c, _) ->
          pbounds.(icstr) <- (neg_infinity, -.c);
          icstr + 1) icstr eq in
        icstr) 0 eqs in
      pbounds in
    let xbounds = Array.make n (neg_infinity, infinity) in
    let lp = Glpk.make_problem Glpk.Minimize zcoeffs constrs pbounds xbounds in
    try
      Glpk.set_message_level lp 1;
      Glpk.use_presolver lp true;
      Glpk.simplex lp;
      Glpk.get_col_primals lp
    with Glpk.No_primal_feasible_solution ->
      Array.make (List.length eqs) infinity in
  let eqs = List.map (fun eq ->
    if List.exists (fun (c, _) -> c = infinity) eq then [infinity, [||]]
    else
      let eq' = List.filter (fun (c, _) -> c <> neg_infinity) eq in
      if eq' = [] then [neg_infinity, [||]] else eq')
    eqs in
  (* TODO we should do n Kleene iterations to eliminate all +oo. *)
  let trans, finite_eqs =
    let trans, finite_eqs, _ = List.fold_left
      (fun (trans, finite_eqs, i) eq ->
        if eq = [infinity, [||]] then trans, finite_eqs, i + 1
        else (i :: trans), (eq :: finite_eqs), i + 1)
      ([], [], 0) eqs in
    let trans, finite_eqs = List.rev trans, List.rev finite_eqs in
    let translate_eq = List.map (fun (c, a) ->
      let a' = Array.make (List.length trans) 0. in
      let _ = List.fold_left
        (fun i j ->
          if j < Array.length a then a'.(i) <- a.(j);
          i + 1)
        0 trans in
      c, a') in
    trans, List.map translate_eq finite_eqs in
  let finite_res =
    if finite_eqs = [] then [||]
    else min_policy_finite_lfp finite_eqs in
  let res = Array.make (List.length eqs) infinity in
  let _ = List.fold_left
    (fun i j ->
      res.(j) <- finite_res.(i);
      i + 1)
    0 trans in
  List.map (( *.) 1.00005) (Array.to_list res)

(* Perform min policy iterations on given equation system
 * from given post fixpoint. *)
let min_policy_from_postfixpoint ~(solver : Osdp.Sdp.solver) (eqs : eq list) (rho : float list) : float list =
  let progress = List.exists2
    (fun x x' ->
      (abs_float x > 0.0001 && abs_float ((x -. x') /. x) > 0.0001)
      || (x = infinity && x' <> infinity)) in
  let max_iter = 12 in
  let rec iter n rho =
    let pi = improve_strat_eqs ~solver eqs rho in
    Format.printf "pi%d: %a@." n fprintf_min_policy_system pi;
    let rho' = min_policy_lfp pi in
    Format.printf
      "x%d: @[(%a)@]@." n
      (Osdp.Utils.fprintf_list ~sep:",@ " Format.pp_print_float)
      rho';
    if n < max_iter && List.exists (( <> ) infinity) rho'
       && progress rho rho' then iter (n + 1) rho'
    else rho' in
  iter 0 rho

let solve_pi ~(solver : Osdp.Sdp.solver) (eqs : eq list) : float list =
  (* Well, we could do Kleene iterations with a widening with thresholds
   * to get a good postfixpoint to start with but it is easier to just start
   * with +oo for every template except the ellipsoid. *)
  let init_lambda = 2. in
  let postfixpoint =
    let sz = List.length eqs in
    init_lambda :: Array.to_list (Array.make (sz - 1) infinity) in
  Format.printf
    "Starting min policy iterations from postfixpoint: @[%a@]@."
    (Osdp.Utils.fprintf_list ~sep:",@ " Format.pp_print_float)
    postfixpoint;
  min_policy_from_postfixpoint ~solver eqs postfixpoint

(***********************)
(* Template generation *)
(***********************)

open Osdp.Lmi.Float

let test_quadratic ~solver a b =
  let a = Mat.of_list_list a in
  let dim_a = Mat.nb_cols a in
  let b = Mat.of_list_list b in
  let dim_b = Mat.nb_cols b in

  let p_id = Osdp.Ident.create "p" in
  let lambda'_id = Osdp.Ident.create "lambda'" in

  let test_feas_lambda lambda =
    let e1 = Sub (Scale_const (lambda, Var p_id),
                  Mult (Mult (Transpose (Const a), Var p_id), Const a)) in
    let e2 = Sub (Var p_id, Eye dim_a) in
    (* Format.printf "e1 = %a\n%!" pp e1; *)
    (* Format.printf "e2 = %a\n%!" pp e2; *)
    let _, _, vars = solve ~solver Purefeas [e1; e2] in
    let res = not (Osdp.Ident.Map.is_empty vars) in
    res in

  let test_radius_lambda lambda =
    let i0 = Block [|[|Eye dim_a; Zeros (dim_a, dim_b)|]|] in
    let ab = Block [|[|Const a; Const b|]|] in
    let e1 = Sub (Block [|[|Minus (Mult (Mult (Transpose ab, Var p_id), ab)); Zeros (dim_a + dim_b, 1)|];
                          [|Zeros (1, dim_a + dim_b); Eye 1|]|],
                    Scale_const (lambda,
                                 Block [|[|Minus (Mult (Mult (Transpose i0, Var p_id), i0)); Zeros (dim_a + dim_b, 1)|];
                                         [|Zeros (1, dim_a + dim_b); Eye 1|]|])) in
    let e1, e3l =
      let rec loop n e1 e3l =
        if n >= dim_b then
          e1, e3l
        else
          let tau_id = Osdp.Ident.create "tau" in
          let e1 = Sub (e1,
                        Scale_var (tau_id,
                                   Block [|[|Minus (Kronecker_sym (dim_a + dim_b, dim_a + n, dim_a + n)); Zeros (dim_a + dim_b, 1)|];
                                           [|Zeros (1, dim_a + dim_b); Eye 1|]|])) in
          let e3 = Scale_var (tau_id, Eye 1) in
          loop (n + 1) e1 (e3 :: e3l) in
      loop 0 e1 [] in
    let e2 = Sub (Var p_id, Scale_var (lambda'_id, Eye dim_a)) in
    (* Format.printf "e1 = %a\n%!" pp e1; *)
    (* Format.printf "e2 = %a\n%!" pp e2; *)
    (* List.iteri (fun i e3 -> Format.printf "e3 %i = %a\n%!" i pp e3) e3l; *)
    let el = e1 :: e2 :: e3l in
    solve ~solver (Maximize lambda'_id) el in

  let nb_dicho = 10 in
  let nb_try = 12 in
  let rec dicho n min max =
    Format.printf "min, max = %g, %g@." min max;
    let mid = (min +. max) /. 2. in
    let min, max = if test_feas_lambda mid then min, mid else mid, max in
    if n > 0 then dicho (n - 1) min max else max in
  let tau_min = dicho nb_dicho 0. 1. in
  (* Format.printf "tau_min = %g@." tau_min; *)
  let best_tau = ref tau_min in
  let best_radius = ref neg_infinity in
  let best_vars = ref Osdp.Ident.Map.empty in
  let tau = ref tau_min in
  for i = 0 to nb_try - 1 do
    (* Format.printf "trying %g@." !tau; *)
    let _, _, vars = test_radius_lambda !tau in
    let radius =
      try
        match Osdp.Ident.Map.find lambda'_id vars with
        | Scalar r -> r
        | _ -> neg_infinity
      with Not_found -> neg_infinity in
    (* Format.printf "gives %g@." radius; *)
    if radius > !best_radius then
      begin
        best_tau := !tau;
        best_radius := radius;
        best_vars := vars
      end;
    tau := !tau +. (1. -. tau_min) /. float_of_int nb_try
  done;
  (* Format.printf "best_tau = %g@." !best_tau; *)
  (* Format.printf "best_radius = %g@." !best_radius; *)
  !best_tau

open Osdp.Sos.Float

let test_SOS ~solver a b deg =
  let lambda = test_quadratic ~solver a b in
  let p_id = Osdp.Ident.create "p" in
  let lambda'_id = Osdp.Ident.create "lambda'" in
  let nb_vars = List.length a in
  let nb_inputs = List.length (List.hd b) in
  let pol_of_list l =
    Poly.of_list (List.map (fun (s, m) -> Osdp.Monomial.of_list m, s) l) in
  let cp f = pol_of_list [f, []] in  (* constant polynomial *)
  let vp f n =  (* f x_n ^ deg *)
    pol_of_list [f, Array.to_list (Array.make n 0) @ [deg]] in
  let l =
    List.combine a b
    |> List.map
         (fun (a, b) ->
          (a @ b)
          |> (List.fold_left (fun (m, l) c -> 0 :: m, (c, m) :: l) ([1], []))
          |> snd |> pol_of_list) in
  (* List.iteri (fun i l -> Format.printf "l%i = %a@." i Poly.pp l) l; *)
  let p = { name = p_id;
            nb_vars = nb_vars;
            degree = deg;
            homogeneous = true } in
  let pe1 = Sub (Sub (Const (cp 1.),
                      Compose (Var p, List.map (fun p -> Const p) l)),
                 Mult (Const (cp lambda),
                       Sub (Const (cp 1.), Var p))) in
  let pe1 =
    let rec loop n pe1 =
      if n >= nb_inputs then
        pe1
      else
        let pe1 =
          Sub (pe1,
               Mult_scalar (Osdp.Ident.create "tau",
                            Sub (Const (cp 1.),
                                 Const (vp 1. (nb_vars + n))))) in
        loop (n + 1) pe1 in
    loop 0 pe1 in
  (* Format.printf "pe1 = %a@." (pp ~names) pe1; *)
  let pe2 =
    let p_norm =
      let rec loop n =
        if n >= nb_vars then Poly.zero
        else Poly.add (vp 1.(*0.0001*) n) (loop (n + 1)) in
      loop 0 in
    Sub (Var p, Mult_scalar (lambda'_id, Const p_norm)) in
  (* Format.printf "pe2 = %a@." (pp ~names) pe2; *)
  let _, _, vars = solve ~solver (Maximize lambda'_id) [pe1; pe2] in
  let p =
    try
      begin
        match Osdp.Ident.Map.find p.name vars with Poly p -> p | _ -> Poly.zero
      end
    with Not_found -> Poly.zero in
  (* Format.printf "p = %a@." Poly.pp p; *)
  p
  (* let normalize p = *)
  (*   let p = Poly.to_list p in *)
  (*   let mp = *)
  (*     let s, nb = *)
  (*       List.fold_left *)
  (*         (fun (m, nb) (_, c) -> m +. (abs_float c), nb + 1) *)
  (*         (0., 0) p in *)
  (*     s /. float_of_int nb in *)
  (*   let p = List.map (fun (m, c) -> m, c /. mp) p in *)
  (*   mp, Poly.of_list p in *)
  (* let norm_coeff, p = normalize p in *)

(********)
(* Test *)
(********)

let a_1 = [[1.5; -0.7]; [1.; 0.]]
let b_1 = [[1.6]; [0.]]

let a_2 = [[1.5; -0.7; -0.7; 0.4];
           [1.; 0.; 0.; 0.];
           [0.; 0.; 0.; 0.];
           [0.; 0.; 1.; 0.]]
let b_2 = [[0.5]; [0.]; [1.]; [0.]]

let a_3 = [[0.499; -0.05];[0.01; 1.]]
let b_3 = [[1.]; [0.]]

let a_4 = [[0.9379; -0.0381; -0.0414];
           [-0.0404; 0.968; -0.0179];
           [0.0142; -0.0197; 0.9823]]
let b_4 = [[0.0237]; [0.0143]; [0.0077]]

let a_5 = [[0.6227; 0.3871; -0.113; 0.0102];
           [-0.3407; 0.9103; -0.3388; 0.0649];
           [0.0918; -0.0265; -0.7319; 0.2669];
           [0.2643; -0.1298; -0.9903; 0.3331]]
let b_5 = [[0.3064; 0.1826];
           [-0.0054; 0.6731];
           [0.0494; 1.6138];
           [-0.0531; 0.4012]]

let a_6 = [[0.425; 0.; 0.; 0.; 0.];
           [0.3167; 0.1016; -0.4444; 0.; 0.];
           [0.1278; 0.4444; 0.8207; 0.; 0.];
           [0.0365; 0.127; 0.5202; 0.4163; -0.5714];
           [0.0147; 0.0512; 0.2099; 0.5714; 0.7694]]
let b_6 =  [[0.8131];
            [0.1807];
            [0.0729];
            [0.0208];
            [0.0084]]

let examples = [a_1, b_1; a_2, b_2; a_3, b_3; a_4, b_4; a_5, b_5; a_6, b_6]

let solver = Osdp.Sdp.Csdp
(* let solver = Osdp.Sdp.Mosek *)

let test_pi a b deg =
  let p = test_SOS ~solver a b deg in
  (* Format.printf "p = %a@." Poly.pp p; *)
  let pol_of_list l =
    Poly.of_list (List.map (fun (s, m) -> Osdp.Monomial.of_list m, s) l) in
  let vp f n =  (* f x_n ^ deg *)
    pol_of_list [f, Array.to_list (Array.make n 0) @ [deg]] in
  let nb_vars = List.length a in
  let templates = p :: List.mapi (fun i _ -> vp 1. i) a in
  Format.printf
    "templates = @[<v>%a@]@."
    (Osdp.Utils.fprintf_list ~sep:",@ " Poly.pp) templates;
  let l =
    List.combine a b
    |> List.map
         (fun (a, b) ->
          (a @ b)
          |> (List.fold_left (fun (m, l) c -> 0 :: m, (c, m) :: l) ([1], []))
          |> snd |> pol_of_list) in
  let cstrs =
    let cstrs_templates = List.mapi (fun i p -> LeVar (p, i)) templates in
    let cstrs_inputs =
      List.mapi (fun i _ -> LeConst (vp 1. (nb_vars + i), 1.)) (List.hd b) in
    cstrs_templates @ cstrs_inputs in
  let eqs = List.map (fun p -> [Poly.compose p l, cstrs]) templates in
  let res = solve_pi ~solver eqs in
  (* Format.printf *)
  (*   "res = @[%a@]@." *)
  (*   (Osdp.Utils.fprintf_list ~sep:",@ " (fun fmt -> Format.fprintf fmt "%.2f")) *)
  (*   res; *)
  let bounds = List.map (fun f -> exp (log f /. float_of_int deg)) res in
  Format.printf
    "bounds = @[%a@]@."
    (Osdp.Utils.fprintf_list ~sep:",@ " (fun fmt -> Format.fprintf fmt "%.2f"))
    (List.tl bounds)

let _ =
  let usage () =
    Format.printf "Usage: %s <n> <deg (even)>@." Sys.argv.(0);
    exit 0 in
  if Array.length Sys.argv <> 3 then usage ();
  let n = int_of_string Sys.argv.(1) in
  let a, b = List.nth examples (n - 1) in
  Format.printf "Analyzed system x_k+1 = A x_k + B u, ||u||_oo <= 1,@.";
  Format.printf
    "A = @[<v>[%a]@]@."
    (Osdp.Utils.fprintf_list
       ~sep:"]@,["
       (fun fmt l ->
        Format.fprintf
          fmt "@[%a@]"
          (Osdp.Utils.fprintf_list ~sep:",@ " Format.pp_print_float)
          l))
    a;
  Format.printf
    "B = @[<v>[%a]@]@."
    (Osdp.Utils.fprintf_list
       ~sep:"]@,["
       (fun fmt l ->
        Format.fprintf
          fmt "@[%a@]"
          (Osdp.Utils.fprintf_list ~sep:",@ " Format.pp_print_float)
          l))
    b;
  let deg = int_of_string Sys.argv.(2) in
  (* Format.printf "deg = %i@." deg; *)
  test_pi a b deg
