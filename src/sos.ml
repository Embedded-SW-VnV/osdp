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
  val make : ?n:int -> ?d:int -> ?homogen:bool -> string -> polynomial_expr
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
  val of_list : (Monomial.t * polynomial_expr) list -> polynomial_expr
  exception Dimension_error
  val to_list : polynomial_expr -> (Monomial.t * polynomial_expr) list
  val nb_vars : polynomial_expr -> int
  val degree : polynomial_expr -> int
  val is_homogeneous : polynomial_expr -> bool
  val param_vars : polynomial_expr -> var list
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
  val pp : Format.formatter -> polynomial_expr -> unit
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit
  type options = {
    sdp : Sdp.options;
    verbose : int;
    scale : bool;
    trace_obj : bool;
    dualize : bool;
    monoms : Monomial.t list list;
    pad : float;
    pad_list : float list
  }
  val default : options
  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas
  type values
  type 'a witness = Monomial.t array * 'a array array
  exception Not_linear
  val solve : ?options:options -> ?solver:Sdp.solver ->
              obj -> polynomial_expr list ->
              SdpRet.t * (float * float) * values * float witness list
  val value : polynomial_expr -> values -> Poly.Coeff.t
  val value_poly : polynomial_expr -> values -> Poly.t
  val check : ?options:options -> ?values:values -> polynomial_expr ->
              float witness -> bool
  val check_round : ?options:options -> ?values:values ->
                    polynomial_expr list -> float witness list ->
                    (values * Scalar.Q.t witness list) option
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

  let of_list l =
    let l = List.rev l in
    List.fold_left
      (fun e (m, e') -> add (mult e' (monomial m)) e)
      (const Poly.zero) l

  (* polynomial_expr viewed as polynomials *)
  module PEPoly =
    Polynomial.Make
      (Scalar.Make
         (struct
           type t = polynomial_expr
           let compare = Stdlib.compare
           let zero = const (Poly.zero)
           let one = const (Poly.one)
           let of_float _ = assert false
           let to_float _ = assert false
           let of_q _ = assert false
           let to_q _ = assert false
           let add e1 e2 = match e1, e2 with
             | Const p1, Const p2 -> Const (Poly.add p1 p2)
             | _ -> Add (e1, e2)
           let sub e1 e2 = match e1, e2 with
             | Const p1, Const p2 -> Const (Poly.sub p1 p2)
             | _ -> Sub (e1, e2)
           let mult e1 e2 = match e1, e2 with
             | Const p1, Const p2 -> Const (Poly.mult p1 p2)
             | _ -> Mult (e1, e2)
           let div _ _ = assert false
           let pp = pp
         end))

  exception Dimension_error

  let to_list e =
    let rec aux e = match e with
      | Const p ->
         Poly.to_list p
         |> List.map (fun (m, c) -> m, const (Poly.const c))
         |> PEPoly.of_list
      | Var (Vscalar _) -> PEPoly.const e
      | Var (Vpoly p) ->
         List.rev_map (fun (m, id) -> m, Var (Vscalar id)) p.poly
         |> PEPoly.of_list
      | Mult_scalar (c, e) -> PEPoly.mult_scalar (const (Poly.const c)) (aux e)
      | Add (e1, e2) -> PEPoly.add (aux e1) (aux e2)
      | Sub (e1, e2) -> PEPoly.sub (aux e1) (aux e2)
      | Mult (e1, e2) -> PEPoly.mult (aux e1) (aux e2)
      | Power (e, n) -> PEPoly.power (aux e) n
      | Compose (e, el) -> PEPoly.compose (aux e) (List.map aux el)
      | Derive (e, n) -> PEPoly.derive (aux e) n in
    try PEPoly.to_list (aux e)
    with PEPoly.Dimension_error -> raise Dimension_error

  let make ?n ?d ?homogen s =
    let n = match n with Some n -> n | None -> 1 in
    let d = match d with Some d -> d | None -> 1 in
    let homogen = match homogen with Some h -> h | None -> false in
    if n <= 1 && d <= 1 then Var (Vscalar (Ident.create s)) else
      let name = Ident.create s in
      let l =
        let mons =
          (if homogen then Monomial.list_eq else Monomial.list_le) n d in
        let s = (*"__SOS__" ^*) Format.asprintf "%a" Ident.pp name ^ "_" in
        let l, _ = 
          List.fold_left
            (fun (l, i) m ->
              (m, Ident.create (s ^ string_of_int i)) :: l, i + 1)
            ([], 0) mons in
        List.rev l in
      Var (Vpoly { name = name; poly = l })

  let nb_vars e = to_list e |> PEPoly.of_list |> PEPoly.nb_vars
  let degree e = to_list e |> PEPoly.of_list |> PEPoly.degree
  let is_homogeneous e = to_list e |> PEPoly.of_list |> PEPoly.is_homogeneous

  let param_vars e =
    let rec aux env = function
      | Const _p -> env
      | Var ((Vscalar id) as v) | Var ((Vpoly { name = id; poly = _ }) as v) ->
         Ident.Map.add id v env
      | Mult_scalar (_, e) | Power (e, _) -> aux env e
      | Add (e1, e2) | Sub (e1, e2) | Mult (e1, e2) -> aux (aux env e1) e2
      | Compose (e, el) -> List.fold_left aux (aux env e) el
      | Derive (e, _) -> aux env e in
    aux Ident.Map.empty e |> Ident.Map.bindings |> List.map snd

  (*************)
  (* Scalarize *)
  (*************)

  module LinExprSC = LinExpr.Make (Poly.Coeff)

  (* polynomials whose coefficients are linear expressions *)
  module LEPoly = Polynomial.Make (LinExpr.MakeScalar (LinExprSC))

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

  (*********)
  (* Solve *)
  (*********)

  type options = {
    sdp : Sdp.options;
    verbose : int;
    scale : bool;
    trace_obj : bool;
    dualize : bool;
    monoms : Monomial.t list list;
    pad : float;
    pad_list : float list
  }

  let default = {
    sdp = Sdp.default;
    verbose = 0;
    scale = true;
    trace_obj = false;
    dualize = false;
    monoms = [];
    pad = 2.;
    pad_list = []
  }

  type obj =
      Minimize of polynomial_expr | Maximize of polynomial_expr | Purefeas

  module Dualize = Dualize.Make (Poly.Coeff)

  type dualize_details = float Dualize.details_val Ident.Map.t
                         * (polynomial_expr * Ident.t array array) list

  type values = Poly.Coeff.t Ident.Map.t * dualize_details option

  type 'a witness = Monomial.t array * 'a array array

  let solve ?options ?solver obj el =
    let options, sdp_options =
      match options with None -> default, None | Some o -> o, Some o.sdp in

    let obj, obj_sign = match obj with
      | Minimize obj -> Mult_scalar (Poly.Coeff.minus_one, obj), -1.
      | Maximize obj -> obj, 1.
      | Purefeas -> Const (Poly.zero), 0. in

    (* associate an index to each (scalar) variable *)
    let var_idx, _ =
      let env =
        let e = List.fold_left add (const Poly.zero) (obj :: el) in
        List.fold_left
          (fun env v ->
            match v with
            | Vscalar id -> Ident.Set.add id env
            | Vpoly { name = _; poly = l } ->
               List.fold_left (fun env (_, id) -> Ident.Set.add id env) env l)
          Ident.Set.empty (param_vars e) in
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
      | [m, c] when Monomial.(compare m one) = 0 ->
         let le, c = LinExprSC.to_list c in
         let v = List.map (fun (id, c) -> Ident.Map.find id var_idx, c) le in
         (v, []), Poly.Coeff.to_float c
      | _ -> raise Not_linear in

    (* associate a monomial basis to each SOS constraint *)
    let monoms_scalarized =
      let rec build_monoms ml el = match ml, el with
        | _, [] -> []
        | m :: ml, e :: el -> (Array.of_list m, e) :: build_monoms ml el
        | [], e :: el ->
          let h = LEPoly.is_homogeneous e in
          let n = LEPoly.nb_vars e in
          let d = (LEPoly.degree e + 1) / 2 in
          let m = (if h then Monomial.list_eq else Monomial.list_le) n d in
          (Array.of_list m, e) :: build_monoms [] el in
      build_monoms options.monoms scalarized in

    (* for monoms = [m_0;...; m_n], square_monoms associates to each
       monom m the list of all i >= j such that m = m_i *
       m_j. square_monoms is sorted by Monomial.compare. *)
    let square_monoms monoms =
      let sz = Array.length monoms in
      let m = ref Monomial.Map.empty in
      for i = 0 to sz - 1 do
        for j = 0 to i do
          let mij = Monomial.mult monoms.(i) monoms.(j) in
          let lm = try Monomial.Map.find mij !m with Not_found -> [] in
          m := Monomial.Map.add mij ((i, j) :: lm) !m
        done
      done;
      !m in

    (* Format.printf *)
    (*   "@[<v2>before refines:@ %a@]@." *)
    (*   (Utils.pp_list *)
    (*      ~sep:"@ " *)
    (*      (fun fmt (m, e) -> *)
    (*        Format.fprintf *)
    (*          fmt "@[<v2>@[%a@]:@ @[%a@]@]" LEPoly.pp e *)
    (*          (Utils.pp_array ~sep:",@ " Monomial.pp) m)) *)
    (*   monoms_scalarized; *)

    (* let cpt_refines = ref 0 in *)
    (* let cpt_newton = ref 0. in *)
    (* let cpt_simpl = ref 0. in *)
    
    (* refine the monomial basis *)
    let rec refines monoms_e =
      (* let () = incr cpt_refines; Format.printf "<%d refines>@." !cpt_refines in *)
      (* first use Newton polytope *)
      let monoms_e'(* , t_newton *) = (* Utils.profile (fun () -> *)
        List.map
          (fun (m, e) ->
            let l = Array.to_list m in
            let m_e = List.map fst (LEPoly.to_list e) in
            Array.of_list (NewtonPolytope.filter l m_e), e)
          monoms_e(* ) *) in
      (* cpt_newton := !cpt_newton +. t_newton; *)
      let eq_lengths (m, _) (m', _) = Array.length m = Array.length m' in
      if List.for_all2 eq_lengths monoms_e monoms_e' then monoms_e else
        (* then remove from e monomials not present in monoms basis *)
        let collect_zeros zeros (monoms, e) =
          let sq_monoms = square_monoms monoms in
          let e = LEPoly.to_list e in
          List.fold_left
            (fun zeros (m, c) ->
              if Monomial.Map.mem m sq_monoms then zeros else
                match LinExprSC.is_var c with
                | None -> zeros
                | Some (id, _) -> Ident.Set.add id zeros)
            zeros e in
        let zeros(* , t_collect *) = (* Utils.profile (fun () -> *) List.fold_left collect_zeros Ident.Set.empty monoms_e'(* ) *) in
        (* cpt_simpl := !cpt_simpl +. t_collect; *)
        (* try to iterate as long as progress is made *)
        if Ident.Set.is_empty zeros then monoms_e' else
          let set_zeros e =
            let set_zeros_le le =
              let l, c = LinExprSC.to_list le in
              let l =
                List.filter (fun (id, _) -> not (Ident.Set.mem id zeros)) l in
              LinExprSC.of_list l c in
            LEPoly.to_list e
            |> List.map (fun (m, e) -> m, set_zeros_le e)
            |> LEPoly.of_list in
          let tmp(* , t_tmp *) = (* Utils.profile (fun () -> *) List.map (fun (m, e) -> m, set_zeros e) monoms_e'(* ) *) in
          (* cpt_simpl := !cpt_simpl +. t_tmp; *)
          refines tmp in
    let monoms_scalarized(* , trefines *) = (* Utils.profile (fun () -> *) refines monoms_scalarized(* ) *) in
    (* Format.printf "time for refining monomials: %.3fs@." trefines; *)
    (* Format.printf "time for newton_polytope: %.3fs@." !cpt_newton; *)
    (* Format.printf "time for setting zeros: %.3fs@." !cpt_simpl; *)

    (* Format.printf *)
    (*   "@[<v 2>after refines:@ %a@]@." *)
    (*   (Utils.pp_list *)
    (*      ~sep:"@ " *)
    (*      (fun fmt (m, e) -> *)
    (*        Format.fprintf *)
    (*          fmt "@[<v2>@[%a@]:@ @[%a@]@]" LEPoly.pp e *)
    (*          (Utils.pp_array ~sep:",@ " Monomial.pp) m)) *)
    (*   monoms_scalarized; *)

    (* build the a_i, A_i and b_i (see sdp.mli) *)
    let build_cstr ei (monoms, e) =
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
        let sq_monoms = Monomial.Map.bindings (square_monoms monoms) in
        match_polys [] (LEPoly.to_list e) sq_monoms in

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

    (* build objective for trace_obj option *)
    let obj =
      if not (options.trace_obj && obj = ([], [])) then obj else
        let neg_tr ei (monoms, _) =
          ei, List.mapi (fun i _ -> i, i, -1.) (Array.to_list monoms) in
        [], List.mapi neg_tr monoms_cstrs in

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
        Sdp.pfeas_stop_crit ?options:sdp_options ?solver bl in
      if options.verbose > 0 then Format.printf "perr = %g@." perr;
      let monoms_cstrs_pad =
        let rec aux mc p = match mc, p with
          | [], _ -> []
          | mc :: mcl, [] -> (mc, options.pad) :: aux mcl []
          | mc :: mcl, p :: pl -> (mc, p) :: aux mcl pl in
        aux monoms_cstrs options.pad_list in
      let pad_cstrs ((monoms, constraints), pad) =
        let pad = float_of_int (Array.length monoms) *. (pad *. perr) in
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
      List.split (List.map pad_cstrs monoms_cstrs_pad) in
    let cstrs = List.flatten cstrs in

    let module PreSdp = PreSdp.Make (Poly.Coeff) in

    (* Format.printf "SDP solved <@."; *)
    (* Format.printf "%a@." PreSdp.pp_ext_sparse (obj, cstrs, []); *)
    (* Format.printf ">@."; *)

    (* call SDP solver *)
    let ret, (pobj, dobj), (res_x, res_X), dualize_details, tsolver =
      if options.dualize then
        let (ret, (pobj, dobj), (res_x, res_X), details), tsolver =
          Utils.profile (fun () ->
              Dualize.solve_ext_sparse_details ?options:sdp_options ?solver obj cstrs []
            ) in
        ret, (pobj, dobj), (res_x, res_X), Some details, tsolver
      else
        let (ret, (pobj, dobj), (res_x, res_X, _, _)), tsolver =
          Utils.profile (fun () ->
              PreSdp.solve_ext_sparse ?options:sdp_options ?solver obj cstrs []
            ) in
        ret, (pobj, dobj), (res_x, res_X), None, tsolver in
    if options.verbose > 2 then
      Format.printf "time for solver: %.3fs@." tsolver;

    let obj = let f o = obj_sign *. (o +. obj_cst) in f pobj, f dobj in

    (* rebuild variables *)
    if not (SdpRet.is_success ret) then
      ret, obj, (Ident.Map.empty, None), []
    else
      let module IntMap =
        Map.Make (struct type t = int let compare = compare end) in
      let vars =
        let res_x =
          IntMap.(List.fold_left (fun m (i, c) -> add i c m) empty res_x) in
        Ident.Map.map
          (fun i -> try IntMap.find i res_x with Not_found -> P.Coeff.zero)
          var_idx in
      let details = match dualize_details with
        | None -> None
        | Some (v, m, dv) ->
          let tr =
            let v =
              IntMap.(List.fold_left (fun m (i, id) -> add i id m) empty v) in
            Ident.Map.fold
              (fun id i m ->
                 try Ident.Map.add (IntMap.find i v) id m with Not_found -> m)
              var_idx Ident.Map.empty in
          let m =
            let m =
              IntMap.(List.fold_left (fun m (i, b) -> add i b m) empty m) in
            let dummy = Ident.create "_dummy_" in
            List.mapi
              (fun i e ->
                 let b =
                   try IntMap.find i m
                   with Not_found -> Array.make_matrix 0 0 dummy in
                 let sz = Array.length b in
                 for i = 0 to sz - 1 do
                   for j = 0 to i do
                     try b.(i).(j) <- Ident.Map.find b.(i).(j) tr
                     with Not_found -> ()
                   done
                 done;
                 e, b)
              el in
          let repl = Ident.Map.(bindings (map Dualize.ScalarLinExpr.var tr)) in
          let tr_dv d = match d with
            | Dualize.DV _ -> d
            | Dualize.DVexpr le ->
              Dualize.(DVexpr (ScalarLinExpr.replace le repl)) in
          let dv =
            Ident.Map.fold
              (fun i d m ->
                 let i = try Ident.Map.find i tr with Not_found -> i in
                 Ident.Map.add i (tr_dv d) m)
              dv Ident.Map.empty in
          Some (dv, m) in
      let witnesses =
        let rec combine monoms res_X = match monoms, res_X with
          | [], [] -> []
          | [], _ -> assert false
          | m :: q, _ when Array.length m = 0 ->
             (m, Array.make_matrix 0 0 0.) :: combine q res_X
          | m :: q, m' :: q' -> (m, m') :: combine q q'
          | _ -> assert false in
        combine (List.map fst monoms_cstrs) (List.map snd res_X) in
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
      ret, obj, (vars, details), witnesses

  let value_poly e (m, _) =
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
    let values = match values with Some (v, _) -> v | None -> Ident.Map.empty in
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
    let (b, r), _time = Utils.profile (fun () ->
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
      let qpmr, _time = Utils.profile (fun () ->
        let itv f =
          let q = Q.of_float f in
          let l, _ = Utils.itv_float_of_q (Q.sub q r) in
          let _, u = Utils.itv_float_of_q (Q.add q r) in
          l, u in
        Array.map (Array.map itv) q) in
      (* Format.printf "time for qpmr: %.3fs@." time; *)
      (* and check its positive definiteness *)
      let res, _time = Utils.profile (fun () -> Posdef.check_itv qpmr) in
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

  let check_round ?options:options ?values:values el wl =
    let _options = options in  (* silence warning about unused var options *)
    (* let options = match options with Some o -> o | None -> default in *)
    let orig_vals, values =
      match values with Some (ov, v) -> ov, v | None -> Ident.Map.empty, None in
    let values = match values with
      | Some (_d, l) when el = List.map fst l -> values
      | _ -> None in
    match values with
    | None -> None
    | Some (dv, el) ->
      let module PQ = Polynomial.Q in let module M = Monomial in
      let try_rounding den =
        let denf = PQ.Coeff.to_float den in
        let round f =
          let s, af = if f >= 0. then 1, f else -1, -.f in
          let i = s * int_of_float (denf *. af +. 0.5) in
          PQ.Coeff.(of_int i / den) in
        (* first round all floats in dv to rationals with den denominator *)
        let dv =
          Ident.Map.map
            (fun d -> match d with
               | Dualize.DVexpr le -> Dualize.DVexpr le
               | Dualize.DV f -> Dualize.DV (round f))
            dv in
        (* then compute all other values *)
        let get id = match Ident.Map.find id dv with
          | Dualize.DV q -> q | Dualize.DVexpr _ -> assert false in
        let values =
          Ident.Map.map
            (function
              | Dualize.DV q -> q
              | Dualize.DVexpr le ->
                let l, c = Dualize.ScalarLinExpr.to_list le in
                let l = List.map (fun (id, c) -> id, Dualize.Scalar.to_q c) l in
                let c = Dualize.Scalar.to_q c in
                List.fold_left (fun r (id, c) -> PQ.Coeff.(r + c * get id)) c l)
            dv in
        (* check that matrices are PSD *)
        let wl =
          List.rev_map2
            (fun (_, m) (z, _) ->
               let sz = Array.length m in
               let a = Array.make_matrix sz sz PQ.Coeff.zero in
               for i = 0 to sz - 1 do
                 for j = 0 to i do
                   let q =
                     try Ident.Map.find m.(i).(j) values
                     with Not_found -> PQ.Coeff.zero in
                   a.(i).(j) <- q; a.(j).(i) <- q
                 done
               done;
               (* Format.printf *)
               (*   "  z = @[%a@], q = [@[%a@]]@." *)
               (*   (Utils.pp_array ~sep:",@ " Monomial.pp) z *)
               (*   (Utils.pp_matrix ~begl:"@[" ~endl:"@]" ~sepl:";@ " ~sepc:",@ " PQ.Coeff.pp) a; *)
               if not (Posdef.check_PSD a) then raise Exit;
               z, a)
            (List.rev el) (List.rev wl) in
        values, wl in
      let rounding =
        let dens =
          let rec range n m = if n > m then [] else n :: range (n + 1) m in
          List.map Q.of_int (range 1 31)
          @ List.map Q.(mul_2exp one) (range 5 66) in
        let rec find_rounding = function
          | [] -> None
          | den :: l ->
            (* Format.printf "  Try rounding %a@." PQ.Coeff.pp den; *)
            try Some (try_rounding den)
            with Exit -> find_rounding l in
        find_rounding dens in
      (* check that equalities indeed hold (sanity check) *)
      let check_eq values e (z, q) =
        let get id =
          try Ident.Map.find id values
          with Not_found -> Poly.Coeff.to_q (Ident.Map.find id orig_vals) in
        let rec cpt = function
          | Const p ->
            Poly.to_list p
            |> List.map (fun (m, c) -> m, Poly.Coeff.to_q c)
            |> PQ.of_list
          | Var (Vscalar id) -> PQ.const (get id)
          | Var (Vpoly p) ->
            List.map (fun (m, id) -> m, get id) p.poly
            |> PQ.of_list
          | Mult_scalar (c, e) ->
            PQ.mult_scalar (Poly.Coeff.to_q c) (cpt e)
          | Add (e1, e2) -> PQ.add (cpt e1) (cpt e2)
          | Sub (e1, e2) -> PQ.sub (cpt e1) (cpt e2)
          | Mult (e1, e2) -> PQ.mult (cpt e1) (cpt e2)
          | Power (e, d) -> PQ.power (cpt e) d
          | Compose (e, el) ->
            PQ.compose (cpt e) (List.map cpt el)
          | Derive (e, i) -> PQ.derive (cpt e) i in
        let ztqz z q =
          let sz = Array.length q in
          let p = ref PQ.zero in
          for i = 0 to sz - 1 do
            for j = 0 to sz - 1 do
              let m = PQ.(monomial z.(i) * const q.(i).(j) * monomial z.(j)) in
              p := PQ.add !p m
            done
          done;
          !p in
        let p = cpt e in
        let p' = ztqz z q in
        PQ.compare p p' = 0 in
      match rounding with
      | None -> None
      | Some (values, wl) ->
        if List.for_all2 (fun (e, _) w -> check_eq values e w) el wl then
          let values =
            Ident.Map.fold
              (fun id c m -> Ident.Map.add id (Poly.Coeff.of_q c) m)
              values orig_vals in
          Some ((values, None), wl)
        else
          None

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
