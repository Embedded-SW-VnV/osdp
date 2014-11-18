(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module type S = sig
  module Poly : Polynomial.S
  type polynomial_var = {
    name : Ident.t;
    nb_vars : int;
    degree : int;
    homogeneous : bool }
  type polynomial_expr =
    | Const of Poly.t
    | Var of polynomial_var
    | Mult_scalar of Ident.t * polynomial_expr
    | Add of polynomial_expr * polynomial_expr
    | Sub of polynomial_expr * polynomial_expr
    | Mult of polynomial_expr * polynomial_expr
    | Power of polynomial_expr * int
    | Compose of polynomial_expr * polynomial_expr list
  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas
  type ('a, 'b) value_t = Scalar of 'a | Poly of 'b
  exception Type_error of string
  val solve : ?solver:Sdp.solver -> obj_t -> polynomial_expr list ->
              SdpRet.t * (float * float)
              * (Poly.Coeff.t, Poly.t) value_t Ident.Map.t
  val pp : Format.formatter -> polynomial_expr -> unit
  val pp_names : string list -> Format.formatter -> polynomial_expr -> unit
end

module Make (P : Polynomial.S) : S with module Poly = P = struct
  module Poly = P

  type polynomial_var = {
    name : Ident.t;
    nb_vars : int;
    degree : int;
    homogeneous : bool }

  type polynomial_expr =
    | Const of Poly.t
    | Var of polynomial_var
    | Mult_scalar of Ident.t * polynomial_expr
    | Add of polynomial_expr * polynomial_expr
    | Sub of polynomial_expr * polynomial_expr
    | Mult of polynomial_expr * polynomial_expr
    | Power of polynomial_expr * int
    | Compose of polynomial_expr * polynomial_expr list

  let pp_names names fmt e =
    let rec pp_prior prior fmt = function
      | Const p ->
         let par =
           2 < prior || 0 < prior && List.length (Poly.to_list p) >= 2 in
         Format.fprintf fmt (if par then "(%a)" else "%a")
                        (Poly.pp_names names) p
      | Var v -> Ident.pp fmt v.name
      | Mult_scalar (i, e) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         Ident.pp i (pp_prior 1) e
      | Add (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ + %a@])" else "@[%a@ + %a@]")
         (pp_prior 0) e1 (pp_prior 0) e2
      | Sub (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ - %a@])" else "@[%a@ - %a@]")
         (pp_prior 0) e1 (pp_prior 1) e2
      | Mult (e1, e2) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         (pp_prior 1) e1 (pp_prior 1) e2
      | Power (e, d) -> Format.fprintf fmt "%a^%i" (pp_prior 3) e d
      | Compose (e, el) ->
         Format.fprintf fmt "%a(@[%a@])" (pp_prior 2) e
                        (Utils.fprintf_list ~sep:",@ " (pp_prior 0)) el in
    pp_prior 0 fmt e

  let pp = pp_names []

  (*****************)
  (* Type checking *)
  (*****************)

  (* Type of variables. *)
  type ty =
    | TYscal  (* scalar variable *)
    | TYpoly of polynomial_var  (* polynomial variable *)

  exception Type_error of string

  let type_error id s =
    let s = match id with
      | None -> s
      | Some id -> "'" ^ Format.asprintf "%a" Ident.pp id ^ "' " ^ s in
    raise (Type_error s)

  let type_check (el : polynomial_expr list) : ty Ident.Map.t =
    let constrain i t env =
      let t =
        try
          if t = (Ident.Map.find i env) then t
          else type_error (Some i) "has inconsistent types."
        with Not_found -> t in
      Ident.Map.add i t env in
    let rec type_check env = function
      | Const p -> env
      | Var v -> constrain v.name (TYpoly v) env
      | Mult_scalar (i, e) -> type_check (constrain i TYscal env) e
      | Power (e, _) -> type_check env e
      | Add (e1, e2) | Sub (e1, e2) | Mult (e1, e2) ->
         type_check (type_check env e1) e2
      | Compose (e, el) -> List.fold_left type_check (type_check env e) el in
    List.fold_left type_check Ident.Map.empty el

  (*************)
  (* Scalarize *)
  (*************)

  module LinExprSC = LinExpr.Make (Poly.Coeff)

  (* polynomials whose coefficients are linear expressions *)
  module LEPoly = Polynomial.Make (LinExpr.MakeScalar (LinExprSC))

  (* Returns a vector base (c.f. identspoly_of_params below) trying to
     keep it as small as possible such that base' * M * base contains
     all monomials in [lm]. *)
  let base_of_params (p : polynomial_var) (lm : Monomial.t list option) :
        Monomial.t array =
    let l =
      ((if p.homogeneous then Monomial.list_eq else Monomial.list_le)
         p.nb_vars ((p.degree + 1) / 2)) in
    let l = match lm with
      | None -> l
      | Some lm -> Monomial.filter_newton_polytope l lm in
    Array.of_list l

  (* Returns the polynomial base' * M * base with base the vector of
     monomials obtained from [Monomial.list_le] or [Monomial.list_eq]
     according to parameters in [p] and M a symmetric matrix of newly
     created scalar variables. Also returns (upper triangular part of)
     the newly created variables. *)
  let identspoly_of_params (p : polynomial_var) (lm : Monomial.t list option) :
        LEPoly.t * ((Ident.t * (int * int)) * Ident.t) list =
    let poly = ref [] in
    let new_ids = ref [] in
    let new_id i j =
      let s = Format.asprintf "%a" Ident.pp p.name in
      let new_id = Ident.create ((*"__SOS__" ^*) s ^ "_"
                                 ^ string_of_int i ^ "_" ^ string_of_int j) in
      new_ids := ((p.name, (i, j)), new_id) :: !new_ids; new_id in
    let base = base_of_params p lm in
    let size = Array.length base in
    let two = Poly.Coeff.of_float 2. in
    for i = 0 to size - 1 do
      poly := (Monomial.mult base.(i) base.(i),
               LinExprSC.var (new_id i i)) :: !poly;
      for j = i + 1 to size - 1 do
        poly := (Monomial.mult base.(i) base.(j),
                 LinExprSC.of_list [new_id i j, two] Poly.Coeff.zero) :: !poly
      done
    done;
    LEPoly.of_list !poly, !new_ids

  let scalarize (el : polynomial_expr list) :
        LEPoly.t list * (Ident.t * (int * int)) Ident.Map.t =
    (* mapping each polynomial variable and indices (only upper
       diagonal since matrix variables are symmetric) to the newly
       created scalar variable *)
    let new_ids : ((Ident.t * (int * int)) * Ident.t) list ref = ref [] in

    let scalarize_poly_var =
      (* mapping each polynomial variable to the matrix of its newly
         created scalar variables *)
      let htbl : (Ident.t, LEPoly.t) Hashtbl.t = Hashtbl.create 31 in
      fun params ->
        try Hashtbl.find htbl params.name
        with Not_found ->
          let poly, new_ids' = identspoly_of_params params None in
          new_ids := new_ids' @ !new_ids;
          Hashtbl.add htbl params.name poly; poly in

    let rec scalarize = function
      | Const p ->
         let l = Poly.to_list p in
         let l = List.map (fun (m, c) -> m, LinExprSC.const c) l in
         LEPoly.of_list l
      | Var v -> scalarize_poly_var v
      | Mult_scalar (i, e) ->
         let le = LinExprSC.var i in
         LEPoly.mult_scalar le (scalarize e)
      | Power (e, d) -> LEPoly.power (scalarize e) d
      | Add (e1, e2) -> LEPoly.add (scalarize e1) (scalarize e2)
      | Sub (e1, e2) -> LEPoly.sub (scalarize e1) (scalarize e2)
      | Mult (e1, e2) -> LEPoly.mult (scalarize e1) (scalarize e2)
      | Compose (e, el) ->
         LEPoly.compose (scalarize e) (List.map scalarize el) in

    (* scalarize *)
    let el = try List.map scalarize el
             with LEPoly.Dimension_error -> type_error None "dimension error" in
    (* and collect bindings *)
    let b = List.fold_left (fun m (coord, id) -> Ident.Map.add id coord m)
                           Ident.Map.empty !new_ids in
    el, b

  (*********)
  (* Solve *)
  (*********)

  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type ('a, 'b) value_t = Scalar of 'a | Poly of 'b

  let solve ?solver obj el =
    let env = type_check el in

    let scalarized, binding = scalarize el in

    (* count variables and associate an index to each one *)
    let var_total, var_idx =
      Ident.Map.fold
        (fun id _ (i, var_idx) -> i + 1, Ident.Map.add id i var_idx)
        env (0, Ident.Map.empty) in

    (* associate its base to each variable index *)
    let idx_base =
      let a = Array.make var_total [||] in
      Ident.Map.iter
        (fun id ty ->
           let b = match ty with
             | TYscal -> [|Monomial.of_list []|]
             | TYpoly p -> base_of_params p None in
           a.(Ident.Map.find id var_idx) <- b) env; a in

    let le_zero = LinExprSC.const Poly.Coeff.zero in

    (* build the A_i and a_i (see sdp.mli) *)
    let build_cstr ei e =
      let eq_params = { name = Ident.create ("eq" ^ string_of_int ei);
                        nb_vars = LEPoly.nb_vars e;
                        degree = LEPoly.degree e;
                        homogeneous = LEPoly.is_homogeneous e } in
      let eq_poly, eq_binding =
        let lm = List.map fst (LEPoly.to_list e) in
        identspoly_of_params eq_params (Some lm) in
      let eq_binding = List.fold_left
                         (fun m ((_, ij), id) -> Ident.Map.add id ij m)
                         Ident.Map.empty eq_binding in
      (* Encode equality le_vars = le_eq (i.e., l_vars - l_eq = -c_var
         with l_vars and l_eq the linear part of le_vars and le_eq and
         c_vars the constant part of le_vars (constant part of le_eq
         is 0)). Eventually, A_i (var_blks and eq_blk) encodes l_vars
         - l_eq and a_i encodes -c_var. *)
      let equate le_vars le_eq =
        let var_blks = Array.make var_total [] in
        let l_vars, c_vars = LinExprSC.to_list le_vars in
        List.iter
          (fun (id, c) ->
           let id, (i, j) = try Ident.Map.find id binding
                            with Not_found -> id, (0, 0) in
           let idx = Ident.Map.find id var_idx in
           if i = j then
             var_blks.(idx) <- (i, i, Poly.Coeff.to_float c) :: var_blks.(idx)
           else
             let c = Poly.Coeff.to_float c /. 2. in
             var_blks.(idx) <- (j, i, c) :: var_blks.(idx)) l_vars;
        let eq_blk = ref [] in
        let l_eq, _ = LinExprSC.to_list le_eq in
        List.iter
          (fun (id, c) ->
           let i, j = Ident.Map.find id eq_binding in
           if i = j then
             eq_blk := (i, i, -. (Poly.Coeff.to_float c)) :: !eq_blk
           else
             let c = Poly.Coeff.to_float c /. 2. in
             eq_blk := (j, i, -. c) :: !eq_blk) l_eq;
        let var_blks = Array.to_list (Array.mapi (fun i b -> i, b) var_blks) in
        (* A_i, a_i *)
        Sdp.Eq ((var_total + ei, !eq_blk) :: var_blks,
                -. Poly.Coeff.to_float c_vars) in
      (* equate coefficients (linear expressions) of polynomials
         corresponding to the same monomial *)
      let rec match_polys l p1 p2 = match p1, p2 with
        | [], [] -> l
        | [], (_, c2) :: t2 -> match_polys (equate le_zero c2 :: l) [] t2
        | _ :: _, [] -> assert false
        | (m1, c1) :: t1, (m2, c2) :: t2 ->
           let cmp =  Monomial.compare m1 m2 in
           if cmp = 0 then match_polys (equate c1 c2 :: l) t1 t2
           else if cmp > 0 then match_polys (equate le_zero c2 :: l) p1 t2
           else (* cmp < 0 *) assert false in
      match_polys [] (LEPoly.to_list e) (LEPoly.to_list eq_poly) in
    let cstrs = List.flatten (List.mapi build_cstr scalarized) in

    (* build the objective C (see sdp.mli) *)
    let obj = match obj with
      | Minimize id ->
         begin
           try [Ident.Map.find id var_idx, [0, 0, -1.]] with Not_found -> []
         end
      | Maximize id ->
         begin
           try [Ident.Map.find id var_idx, [0, 0, 1.]] with Not_found -> []
         end
      | Purefeas -> [] in

    (* call SDP solver *)
    let ret, res, (primal_sol, dual_sol) = Sdp.solve_sparse ?solver obj cstrs in

    (* rebuild polynomial variables *)
    if not (SdpRet.is_success ret) then
      ret, res, Ident.Map.empty
    else
      let vars =
        let a = Array.of_list primal_sol in
        Ident.Map.map (fun i -> snd a.(i), idx_base.(i)) var_idx in
      let vars =
        Ident.Map.mapi
          (fun id (var, base) ->
           match Ident.Map.find id env with
           | TYscal -> Scalar (Poly.Coeff.of_float var.(0).(0))
           | TYpoly _ ->
              let p = ref Poly.zero in
              let sz = Array.length base in
              for i = 0 to sz - 1 do
                for j = 0 to sz - 1 do
                  p := Poly.add
                         !p (Poly.of_list [Monomial.mult base.(i) base.(j),
                                           Poly.Coeff.of_float var.(i).(j)]);
                done
              done; Poly !p) vars in
      ret, res, vars
end

module Float = Make (Polynomial.Float)
