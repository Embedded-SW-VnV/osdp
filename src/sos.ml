(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module type S = sig
  module Poly : Polynomial.S
  type var
  type polynomial_expr =
    | Const of Poly.t
    | Var of var
    | Mult_scalar of Poly.Coeff.t * polynomial_expr
    | Add of polynomial_expr * polynomial_expr
    | Sub of polynomial_expr * polynomial_expr
    | Mult of polynomial_expr * polynomial_expr
    | Power of polynomial_expr * int
    | Compose of polynomial_expr * polynomial_expr list
    | Derive of polynomial_expr * int
  val var : string -> polynomial_expr
  val var_poly : string -> int -> ?homogen:bool -> int ->
                 polynomial_expr * (Monomial.t * polynomial_expr) list
  val const : Poly.t -> polynomial_expr
  val scalar : Poly.Coeff.t -> polynomial_expr
  val monomial : Monomial.t -> polynomial_expr
  val mult_scalar : Poly.Coeff.t -> polynomial_expr -> polynomial_expr
  val add : polynomial_expr -> polynomial_expr -> polynomial_expr
  val sub : polynomial_expr -> polynomial_expr -> polynomial_expr
  val mult : polynomial_expr -> polynomial_expr -> polynomial_expr
  val power : polynomial_expr -> int -> polynomial_expr
  val compose : polynomial_expr -> polynomial_expr list -> polynomial_expr
  val derive : polynomial_expr -> int -> polynomial_expr
  val nb_vars : polynomial_expr -> int
  val degree : polynomial_expr -> int
  val is_homogeneous : polynomial_expr -> bool
  val ( !! ) : Poly.t -> polynomial_expr
  val ( ?? ) : int -> polynomial_expr
  val ( ! ) : Poly.Coeff.t -> polynomial_expr
  val ( *. ) : Poly.Coeff.t -> polynomial_expr -> polynomial_expr
  val ( ~- ) : polynomial_expr -> polynomial_expr
  val ( + ) : polynomial_expr -> polynomial_expr -> polynomial_expr
  val ( - ) : polynomial_expr -> polynomial_expr -> polynomial_expr
  val ( * ) : polynomial_expr -> polynomial_expr -> polynomial_expr
  val ( / ) : polynomial_expr -> Poly.Coeff.t -> polynomial_expr
  val ( /. ) : Poly.Coeff.t -> Poly.Coeff.t -> polynomial_expr
  val ( ** ) : polynomial_expr -> int -> polynomial_expr
  val ( >= ) : polynomial_expr -> polynomial_expr -> polynomial_expr
  val ( <= ) : polynomial_expr -> polynomial_expr -> polynomial_expr
  type options = {
    sdp : Sdp.options;
    verbose : int;
    scale : bool;
    pad : float
  }
  val default : options
  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas
  type values
  type witness = Monomial.t array * float array array
  exception Dimension_error
  exception Not_linear
  val solve : ?options:options -> ?solver:Sdp.solver ->
              obj -> polynomial_expr list ->
              SdpRet.t * (float * float) * values * witness list
  val value : polynomial_expr -> values -> Poly.Coeff.t
  val value_poly : polynomial_expr -> values -> Poly.t
  val check : ?options:options -> ?values:values -> polynomial_expr ->
              witness -> bool
  val pp : Format.formatter -> polynomial_expr -> unit
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P = struct
  module Poly = P

  type polynomial_var = {
    name : Ident.t;
    poly : (Monomial.t * Ident.t) list
  }

  type var = Vscalar of Ident.t | Vpoly of polynomial_var

  type polynomial_expr =
    | Const of Poly.t
    | Var of var
    | Mult_scalar of Poly.Coeff.t * polynomial_expr
    | Add of polynomial_expr * polynomial_expr
    | Sub of polynomial_expr * polynomial_expr
    | Mult of polynomial_expr * polynomial_expr
    | Power of polynomial_expr * int
    | Compose of polynomial_expr * polynomial_expr list
    | Derive of polynomial_expr * int

  let var s = Var (Vscalar (Ident.create s))

  let var_poly s n ?homogen d =
    let homogen = match homogen with Some h -> h | None -> false in
    let name = Ident.create s in
    let l =
      let mons = (if homogen then Monomial.list_eq else Monomial.list_le) n d in
      let s = (*"__SOS__" ^*) Format.asprintf "%a" Ident.pp name ^ "_" in
      let l, _ = 
        List.fold_left
          (fun (l, i) m -> (m, Ident.create (s ^ string_of_int i)) :: l, i + 1)
          ([], 0) mons in
      List.rev l in
    let e = Var (Vpoly { name = name; poly = l }) in
    let le = Utils.map (fun (m, id) -> m, Var (Vscalar id)) l in
    e, le

  let const p = Const p
  let scalar c = Const (Poly.const c)
  let monomial m = Const (Poly.monomial m)
  let mult_scalar c e = Mult_scalar (c, e)
  let add e1 e2 = Add (e1, e2)
  let sub e1 e2 = Sub (e1, e2)
  let mult e1 e2 = Mult (e1, e2)
  let power e d = Power (e, d)
  let compose e l = Compose (e, l)
  let derive e i = Derive (e, i)
        
  let pp_names names fmt e =
    let rec pp_prior prior fmt = function
      | Const p ->
         let par =
           2 < prior || 0 < prior && List.length (Poly.to_list p) >= 2 in
         Format.fprintf fmt (if par then "(%a)" else "%a")
                        (Poly.pp_names names) p
      | Var (Vscalar id) -> Ident.pp fmt id
      | Var (Vpoly p) -> Ident.pp fmt p.name
      | Mult_scalar (n, e) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         Poly.Coeff.pp n (pp_prior 1) e
      | Add (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ + %a@])" else "@[%a@ + %a@]")
         (pp_prior 0) e1 (pp_prior 0) e2
      | Sub (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ - %a@])" else "@[%a@ - %a@]")
         (pp_prior 0) e1 (pp_prior 1) e2
      | Mult (e1, e2) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         (pp_prior 1) e1 (pp_prior 1) e2
      | Power (e, d) -> Format.fprintf fmt "%a^%d" (pp_prior 3) e d
      | Compose (e, el) ->
         Format.fprintf fmt "%a(@[%a@])" (pp_prior 2) e
                        (Utils.pp_list ~sep:",@ " (pp_prior 0)) el
      | Derive (e, i) ->
         let m = Array.to_list (Array.make i 0) @ [1] in
         Format.fprintf fmt "d/d%a(%a)"
                        (Monomial.pp_names names) (Monomial.of_list m)
                        (pp_prior 0) e in
    pp_prior 0 fmt e

  let pp = pp_names []

  (*************)
  (* Scalarize *)
  (*************)

  module LinExprSC = LinExpr.Make (Poly.Coeff)

  (* polynomials whose coefficients are linear expressions *)
  module LEPoly = Polynomial.Make (LinExpr.MakeScalar (LinExprSC))

  exception Dimension_error
  exception Not_linear

  (* Compile each polynomial_expr as a LEPoly.t. *)
  let scalarize (e : polynomial_expr) : LEPoly.t =

    let rec scalarize = function
      | Const p ->
         Poly.to_list p
         |> List.map (fun (m, c) -> m, LinExprSC.const c)
         |> LEPoly.of_list
      | Var (Vscalar id) -> LEPoly.mult_scalar (LinExprSC.var id) LEPoly.one
      | Var (Vpoly p) ->
         List.rev_map (fun (m, id) -> m, LinExprSC.var id) p.poly
         |> LEPoly.of_list
      | Mult_scalar (n, e) ->
         LEPoly.mult_scalar (LinExprSC.const n) (scalarize e)
      | Add (e1, e2) -> LEPoly.add (scalarize e1) (scalarize e2)
      | Sub (e1, e2) -> LEPoly.sub (scalarize e1) (scalarize e2)
      | Mult (e1, e2) -> LEPoly.mult (scalarize e1) (scalarize e2)
      | Power (e, d) -> LEPoly.power (scalarize e) d
      | Compose (e, el) -> LEPoly.compose (scalarize e) (List.map scalarize el)
      | Derive (e, i) -> LEPoly.derive (scalarize e) i in

    try scalarize e
    with
    | LEPoly.Dimension_error -> raise Dimension_error
    | LinExpr.Not_linear -> raise Not_linear

  (* various operations that need scalarize *)

  let nb_vars e = scalarize e |> LEPoly.nb_vars
  let degree e = scalarize e |> LEPoly.degree
  let is_homogeneous e = scalarize e |> LEPoly.is_homogeneous

  (*********)
  (* Solve *)
  (*********)

  type options = {
    sdp : Sdp.options;
    verbose : int;
    scale : bool;
    pad : float
  }

  let default = { sdp = Sdp.default; verbose = 0; scale = true; pad = 2. }

  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas

  type values = Poly.Coeff.t Ident.Map.t

  type witness = Monomial.t array * float array array

  let solve ?options ?solver obj el =
    let options, sdp_options =
      match options with None -> default, None | Some o -> o, Some o.sdp in

    let obj, obj_sign = match obj with
      | Minimize obj -> Mult_scalar (Poly.Coeff.minus_one, obj), -1.
      | Maximize obj -> obj, 1.
      | Purefeas -> Const (Poly.zero), 0. in

    (* associate an index to each (scalar) variable *)
    let var_idx, _ =
      let rec collect env = function
        | Const p -> env
        | Var (Vscalar id) -> Ident.Set.add id env
        | Var (Vpoly { name = _; poly = l }) ->
           List.fold_left (fun env (_, id) -> Ident.Set.add id env) env l
        | Mult_scalar (_, e) | Power (e, _) -> collect env e
        | Add (e1, e2) | Sub (e1, e2) | Mult (e1, e2) ->
           collect (collect env e1) e2
        | Compose (e, el) -> List.fold_left collect (collect env e) el
        | Derive (e, _) -> collect env e in
      let env = List.fold_left collect Ident.Set.empty (obj :: el) in
      Ident.Set.fold
        (fun id (m, i) -> Ident.Map.add id i m, i + 1)
        env (Ident.Map.empty, 0) in

    let (obj, scalarized), tscalarize =
      Utils.profile (fun () ->
                     let obj = scalarize obj in
                     let scalarized = List.map scalarize el in
                     obj, scalarized) in
    if options.verbose > 2 then
      Format.printf "time for scalarize: %.3fs@." tscalarize;

    (* scaling *)
    let scaling_factors =
      let sqrt_norm e =
        let coeffs_e =
          LEPoly.to_list e
          |> List.map
               (fun (_, l) ->
                let l, c = LinExprSC.to_list l in c :: List.map snd l)
          |> List.flatten in
        let sum =
          List.fold_left
            (fun s c -> s +. Poly.Coeff.to_float c ** 2.)
            0. coeffs_e in
        sqrt (sqrt sum) in
      if options.scale then List.map sqrt_norm scalarized
      else List.map (fun _ -> 1.) scalarized in
    let scalarized =
      let scale s e =
        let s = 1. /. s |> Poly.Coeff.of_float |> LinExprSC.const in
        LEPoly.mult_scalar s e in
      List.map2 scale scaling_factors scalarized in

    (* build the objective (see sdp.mli) *)
    let obj, obj_cst = match LEPoly.to_list obj with
      | [] -> ([], []), 0.
      | [m, c] when Monomial.to_list m = [] ->
         let le, c = LinExprSC.to_list c in
         let v = List.map (fun (id, c) -> Ident.Map.find id var_idx, c) le in
         (v, []), Poly.Coeff.to_float c
      | _ -> raise Not_linear in

    (* associate a monomial basis to each SOS constraint *)
    let monoms_scalarized =
      let build_monoms e =
        let monoms_e = List.map fst (LEPoly.to_list e) in
        let l =
          let h = LEPoly.is_homogeneous e in
          let n = LEPoly.nb_vars e in
          let d = (LEPoly.degree e + 1) / 2 in
          (if h then Monomial.list_eq else Monomial.list_le) n d in
        let res = Monomial.filter_newton_polytope l monoms_e in
        (* Format.printf *)
        (*   "@[Monoms for expr %a: @ @[%a@]@]@." *)
        (*   LEPoly.pp e *)
        (*   (Utils.pp_list ~sep:",@ " Monomial.pp) res; *)
        res in
      List.map (fun e -> Array.of_list (build_monoms e), e) scalarized in

    (* build the a_i, A_i and b_i (see sdp.mli) *)
    let build_cstr ei (monoms, e) =
      (* for monoms = [m_0;...; m_n], square_monoms associates to each
         monom m the list of all i >= j such that m = m_i *
         m_j. square_monoms is sorted by Monomial.compare. *)
      let square_monoms =
        let sz = Array.length monoms in
        let m = ref Monomial.Map.empty in
        for i = 0 to sz - 1 do
          for j = 0 to i do
            let mij = Monomial.mult monoms.(i) monoms.(j) in
            let lm = try Monomial.Map.find mij !m with Not_found -> [] in
            m := Monomial.Map.add mij ((i, j) :: lm) !m
          done
        done;
        Monomial.Map.bindings !m in

      (* collect the constraints by equating coefficients (linear
         expressions) of polynomials corresponding to the same
         monomial *)
      let constraints =
        let le_zero = LinExprSC.const Poly.Coeff.zero in
        let rec match_polys l p1 p2 = match p1, p2 with
          | [], [] -> l
          | [], (_, c2) :: t2 -> match_polys ((le_zero, c2) :: l) [] t2
          | (_, c1) :: t1, [] -> match_polys ((c1, []) :: l) t1 []
          | (m1, c1) :: t1, (m2, c2) :: t2 ->
             let cmp =  Monomial.compare m1 m2 in
             if cmp = 0 then match_polys ((c1, c2) :: l) t1 t2
             else if cmp > 0 then match_polys ((le_zero, c2) :: l) p1 t2
             else (* cmp < 0 *) match_polys ((c1, []) :: l) t1 p2 in
        match_polys [] (LEPoly.to_list e) square_monoms in

      (* encode the constraints for solve_ext (c.f., sdp.mli) *)
      let constraints =
        List.rev_map
          (fun (le, lij) ->
           let le, b = LinExprSC.to_list le in
           let vect =
             List.map
               (fun (id, c) ->
                Ident.Map.find id var_idx, Poly.Coeff.neg c)
               le in
           let mat = [ei, List.map (fun (i, j) -> i, j, 1.) lij] in
           (vect, mat), b)
          constraints in
      monoms, constraints in

    let monoms_cstrs = List.mapi build_cstr monoms_scalarized in

    (* pad constraints *)
    (* The solver will return X >= 0 such that tr(A_i X) = b_i + perr
       where perr is bounded by the primal feasibility error stop
       criteria of the solver. In the check function below (c.f. its
       spec in the mli), we will need \lambda_min(X) >= n perr. That's
       why we perform the change of variable X' := X - n perr I. *)
    let paddings, cstrs =
      let perr = if false then 0. else
        let bl = List.map (fun (_, c) -> List.map snd c) monoms_cstrs
                 |> List.flatten |> List.map Poly.Coeff.to_float in
        options.pad *. Sdp.pfeas_stop_crit ?options:sdp_options ?solver bl in
      if options.verbose > 0 then Format.printf "perr = %g@." perr;
      let pad_cstrs (monoms, constraints) =
        let pad = float_of_int (Array.length monoms) *. perr in
        if options.verbose > 1 then Format.printf "pad = %g@." pad;
        let has_diag mat =
          let diag (i, j, _) = i = j in
          List.exists (fun (_, m) -> List.exists diag m) mat in
        pad,
        List.map
          (fun ((vect, mat), b) ->
           (* there is at most one diagonal coefficient in mat *)
           let b = if has_diag mat then Poly.Coeff.(b - of_float pad) else b in
           vect, mat, b, b)
          constraints in
      List.split (List.map pad_cstrs monoms_cstrs) in
    let cstrs = List.flatten cstrs in

    (* Format.printf "SDP solved <@."; *)
    (* Format.printf "%a@." Sdp.pp_ext_sparse (obj, cstrs, []); *)
    (* Format.printf ">@."; *)

    (* call SDP solver *)
    let module PreSdp = PreSdp.Make (Poly.Coeff) in
    let (ret, (pobj, dobj), (res_x, res_X, _, _)), tsolver =
      (* let obj = List.map (fun (i, s) -> i, Poly.Coeff.to_float s) (fst obj), snd obj in *)
      (* let cstrs = List.map (fun (v, m, a, b) -> List.map (fun (i, s) -> i, Poly.Coeff.to_float s) v, m, Poly.Coeff.to_float a, Poly.Coeff.to_float b) cstrs in *)
      Utils.profile (fun () ->
                     PreSdp.solve_ext_sparse ?options:sdp_options ?solver obj cstrs []
                    ) in
    (* let res_x = List.map (fun (i, s) -> i, Poly.Coeff.of_float s) res_x in *)
    if options.verbose > 2 then
      Format.printf "time for solver: %.3fs@." tsolver;

    let obj = let f o = obj_sign *. (o +. obj_cst) in f pobj, f dobj in

    (* rebuild variables *)
    if not (SdpRet.is_success ret) then ret, obj, Ident.Map.empty, [] else
      let vars =
        let a = Array.of_list res_x in
        Ident.Map.map (fun i -> snd a.(i)) var_idx in
      let witnesses =
        List.combine (List.map fst monoms_cstrs) (List.map snd res_X) in
      (* unpad result *)
      List.iter2
        (fun pad (_, q) ->
         let sz = Array.length q in
         for i = 0 to sz - 1 do q.(i).(i) <- q.(i).(i) +. pad done)
        paddings witnesses;
      (* unscale result *)
      List.iter2
        (fun s (_, q) ->
         let sz = Array.length q in
         for i = 0 to sz - 1 do
           for j = 0 to sz - 1 do
             q.(i).(j) <- s *. q.(i).(j)
           done
         done)
        scaling_factors witnesses;
      ret, obj, vars, witnesses

  (* Utils.float_of_q may fail, raising Z.Overflow. *)
  let solve ?options ?solver obj el =
    try solve ?options ?solver obj el
    with Z.Overflow -> SdpRet.Unknown, (0., 0.), Ident.Map.empty, []

  let value_poly e m =
    let rec aux = function
      | Const p -> p
      | Var (Vscalar id) -> Poly.const (Ident.Map.find id m)
      | Var (Vpoly p) ->
         List.map (fun (mon, id) -> mon, Ident.Map.find id m) p.poly
         |> Poly.of_list
      | Mult_scalar (c, e) -> Poly.mult_scalar c (aux e)
      | Add (e1, e2) -> Poly.add (aux e1) (aux e2)
      | Sub (e1, e2) -> Poly.sub (aux e1) (aux e2)
      | Mult (e1, e2) -> Poly.mult (aux e1) (aux e2)
      | Power (e, d) -> Poly.power (aux e) d
      | Compose (e, el) -> Poly.compose (aux e) (List.map aux el)
      | Derive (e, i) -> Poly.derive (aux e) i in
    aux e

  let value e m =
    match Poly.is_const (value_poly e m) with
    | None -> raise Dimension_error
    | Some c -> c

  let check ?options:options ?values:values e (v, q) =
    let options = match options with Some o -> o | None -> default in
    let values = match values with Some v -> v | None -> Ident.Map.empty in
    let module PQ = Polynomial.Q in let module M = Monomial in
    (* first compute (exactly, using Q) a polynomial p from
       polynomial_expr e *)
    let rec scalarize = function
      | Const p ->
         Poly.to_list p
         |> List.map (fun (m, c) -> m, Poly.Coeff.to_q c)
         |> PQ.of_list
      | Var (Vscalar id) ->
         PQ.const (Poly.Coeff.to_q (Ident.Map.find id values))
      | Var (Vpoly p) ->
         List.map (fun (m, id) -> m, Ident.Map.find id values) p.poly
         |> List.map (fun (m, c) -> m, Poly.Coeff.to_q c)
         |> PQ.of_list
      | Mult_scalar (c, e) ->
         PQ.mult_scalar (Poly.Coeff.to_q c) (scalarize e)
      | Add (e1, e2) -> PQ.add (scalarize e1) (scalarize e2)
      | Sub (e1, e2) -> PQ.sub (scalarize e1) (scalarize e2)
      | Mult (e1, e2) -> PQ.mult (scalarize e1) (scalarize e2)
      | Power (e, d) -> PQ.power (scalarize e) d
      | Compose (e, el) ->
         PQ.compose (scalarize e) (List.map scalarize el)
      | Derive (e, i) -> PQ.derive (scalarize e) i in
    let (b, r), time = Utils.profile (fun () ->
    let p = scalarize e in
    (* then check that p can be expressed in monomial base v *)
    let check_base =
      let s = ref M.Set.empty in
      let sz = Array.length v in
      for i = 0 to sz - 1 do
        for j = 0 to i do
          s := M.Set.add (M.mult v.(i) v.(j)) !s
        done
      done;
      List.for_all (fun (m, _) -> M.Set.mem m !s) (PQ.to_list p) in
    if not check_base then false, Q.zero else
      (* compute polynomial v^T q v *)
      let p' =
        let p' = ref [] in
        let sz = Array.length v in
        for i = 0 to sz - 1 do
          for j = 0 to sz - 1 do
            p' := (M.mult v.(i) v.(j), Scalar.Q.of_float q.(i).(j)) :: !p'
          done
        done;
        PQ.of_list !p' in
      (* compute the maximum difference between corresponding
         coefficients *)
      let r =
        let p'' =
          PQ.merge
            (fun _ c c' ->
             match c, c' with
             | None, None -> None
             | Some c, None | None, Some c -> Some (Q.abs c)
             | Some c, Some c' -> Some (Q.(abs (sub c c')))) p p' in
        PQ.fold (fun _ c m -> Q.max c m) p'' Q.zero in
      (* let rec cpt_diff p p' = match p, p' with *)
      (*   | [], [] -> Scalar.Q.zero *)
      (*   | [], (_, c) :: l | (_, c) :: l, [] -> Q.max (Q.abs c) (cpt_diff [] l) *)
      (*   | (m, c) :: l, (m', c') :: l' -> *)
      (*      let cmp = M.compare m m' in *)
      (*      if cmp = 0 then Q.max (Q.abs (Q.sub c' c)) (cpt_diff l l') *)
      (*      else if cmp < 0 then Q.max (Q.abs c) (cpt_diff l p') *)
      (*      else (\* cmp > 0 *\) Q.max (Q.abs c') (cpt_diff p l') in *)
      (* let r = cpt_diff (PQ.to_list p) (PQ.to_list p') in *)
      true, r) in
    (* Format.printf "time for scalarize+cpt_diff: %.3fs@." time; *)
    if not b then false else
      let () = if options.verbose > 0 then Format.printf "r = %g@." (Utils.float_of_q r) in
      (* Format.printf "Q = %a@." Matrix.Float.pp (Matrix.Float.of_array_array q); *)
      (* form the interval matrix q +/- r *)
      let qpmr, time = Utils.profile (fun () ->
        let itv f =
          let q = Q.of_float f in
          let l, _ = Utils.itv_float_of_q (Q.sub q r) in
          let _, u = Utils.itv_float_of_q (Q.add q r) in
          l, u in
        Array.map (Array.map itv) q) in
      (* Format.printf "time for qpmr: %.3fs@." time; *)
      (* and check its positive definiteness *)
      let res, time = Utils.profile (fun () -> Posdef.check_itv qpmr) in
      (* Format.printf "time for posdef_check: %.3fs@." time; *)
      (* let qmnr = *)
      (*   let nr = Q.((of_int (Array.length q)) * r) in *)
      (*   let mdiag i j c = *)
      (*     let c = Scalar.Q.of_float c in *)
      (*     if i = j then Q.(c - nr) else c in *)
      (*   Array.mapi (fun i -> (Array.mapi (fun j -> mdiag i j))) q in *)
      (* let res = Posdef.check_complete qmnr in *)
      (* Format.printf "res = %B@." res; *)
      res

  (* function solve including a posteriori checking with check just
     above *)
  let solve ?options ?solver obj el =
    let (ret, obj, vals, wits), tsolve =
      Utils.profile (fun () -> solve ?options ?solver obj el) in
    let options = match options with Some o -> o | None -> default in
    if options.verbose > 2 then
      Format.printf "time for solve: %.3fs@." tsolve;
    if not (SdpRet.is_success ret) then ret, obj, vals, wits else
      let res, tcheck =
        Utils.profile (fun () ->
      let check_repl e wit = check ~options ~values:vals e wit in
      if List.for_all2 check_repl el wits then SdpRet.Success, obj, vals, wits
      else SdpRet.PartialSuccess, obj, vals, wits
                      ) in
      if options.verbose > 2 then
        Format.printf "time for check: %.3fs@." tcheck;
      res

  let ( !! ) = const
  let ( ?? ) i = const (Poly.( ?? ) i)
  let ( ! ) = scalar
  let ( *. ) = mult_scalar
  let ( ~- ) = sub (const Poly.zero)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( / ) e c = Mult_scalar (Poly.Coeff.inv c, e)
  let ( /. ) c1 c2 = const (Poly.( /. ) c1 c2)
  let ( ** ) = power
  let ( >= ) e1 e2 = e1 - e2
  let ( <= ) e1 e2 = e2 - e1
end

module Q = Make (Polynomial.Q)

module Float = Make (Polynomial.Float)
