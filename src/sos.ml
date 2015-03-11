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

  (* Compile each polynomial_expr as a LEPoly.t, also returns a
     mapping of each polynomial variable to a polynomial with each
     coefficient a fresh scalar variable. *)
  let scalarize (env : ty Ident.Map.t) (el : polynomial_expr list) :
        LEPoly.t list * ((Monomial.t * Ident.t) list) Ident.Map.t =

    (* Gives a polynomial of corresponding degree and number of
       variables with each coefficient a new scalar variable
       (Ident.t). *)
    let expand_poly_var (p : polynomial_var) : (Monomial.t * Ident.t) list =
      let monoms =
        (if p.homogeneous then Monomial.list_eq else Monomial.list_le)
          p.nb_vars p.degree in
      let s = (*"__SOS__" ^*) Format.asprintf "%a" Ident.pp p.name ^ "_" in
      List.mapi (fun i m -> m, Ident.create (s ^ string_of_int i)) monoms in

    let env =
      Ident.Map.fold
        (fun id ty m ->
         match ty with
         | TYscal -> m
         | TYpoly p -> Ident.Map.add id (expand_poly_var p) m)
        env Ident.Map.empty in

    let rec scalarize = function
      | Const p ->
         Poly.to_list p
         |> List.map (fun (m, c) -> m, LinExprSC.const c)
         |> LEPoly.of_list
      | Var v ->
         Ident.Map.find (v.name) env
         |> List.map (fun (m, id) -> m, LinExprSC.var id)
         |> LEPoly.of_list
      | Mult_scalar (i, e) ->
         let le = LinExprSC.var i in
         LEPoly.mult_scalar le (scalarize e)
      | Power (e, d) -> LEPoly.power (scalarize e) d
      | Add (e1, e2) -> LEPoly.add (scalarize e1) (scalarize e2)
      | Sub (e1, e2) -> LEPoly.sub (scalarize e1) (scalarize e2)
      | Mult (e1, e2) -> LEPoly.mult (scalarize e1) (scalarize e2)
      | Compose (e, el) ->
         LEPoly.compose (scalarize e) (List.map scalarize el) in

    let el =
      try List.map scalarize el
      with LEPoly.Dimension_error -> type_error None "dimension error" in
    el, env

  (*********)
  (* Solve *)
  (*********)

  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type ('a, 'b) value_t = Scalar of 'a | Poly of 'b

  let solve ?solver obj el =
    let env = type_check el in

    let scalarized, binding = scalarize env el in

    (* associate an index to each (scalar) variable *)
    let var_idx, _ =
      Ident.Map.fold
        (fun id ty (m, i) ->
         match ty with
         | TYscal -> Ident.Map.add id i m, i + 1
         | TYpoly p ->
            List.fold_left
              (fun (m, i) (_, id) -> Ident.Map.add id i m, i + 1)
              (m, i) (Ident.Map.find p.name binding))
        env (Ident.Map.empty, 0) in

    (* build the a_i, A_i and b_i (see sdp.mli) *)
    let build_cstr ei e =
      (* monomial base for this polynomial_expr *)
      let monoms =
        let monoms_e = List.map fst (LEPoly.to_list e) in
        let l =
          let h = LEPoly.is_homogeneous e in
          let n = LEPoly.nb_vars e in
          let d = (LEPoly.degree e + 1) / 2 in
          (if h then Monomial.list_eq else Monomial.list_le) n d in
        Monomial.filter_newton_polytope l monoms_e in

      (* for monoms = [m_0;...; m_n], square_monoms associates to each
         monom m the list of all i >= j such that m = m_i *
         m_j. square_monoms is sorted by Monomial.compare. *)
      let square_monoms =
        let m = Array.of_list monoms in
        let sz = Array.length m in
        let l = ref [] in
        for i = 0 to sz - 1 do
          for j = 0 to i do
            l := (Monomial.mult m.(i) m.(j), (i, j)) :: !l
          done
        done;
        let l = List.sort (fun (m, _) (m', _) -> Monomial.compare m m') !l in
        let rec merge = function
          | [] -> []
          | (m, ij) :: l ->
             match merge l with
             | [] -> [m, [ij]]
             | ((m', lij) :: l') as l ->
                if Monomial.compare m m' = 0 then (m, ij :: lij) :: l'
                else (m, [ij]) :: l in
        merge l in

      (* collect the constraints by equating coefficients (linear
         expressions) of polynomials corresponding to the same
         monomial *)
      let constraints =
        let le_zero = LinExprSC.const Poly.Coeff.zero in
        let rec match_polys l p1 p2 = match p1, p2 with
          | [], [] -> l
          | [], (_, c2) :: t2 -> match_polys ((le_zero, c2) :: l) [] t2
          | _ :: _, [] -> assert false
          | (m1, c1) :: t1, (m2, c2) :: t2 ->
             let cmp =  Monomial.compare m1 m2 in
             if cmp = 0 then match_polys ((c1, c2) :: l) t1 t2
             else if cmp > 0 then match_polys ((le_zero, c2) :: l) p1 t2
             else (* cmp < 0 *) assert false in
        match_polys [] (LEPoly.to_list e) square_monoms in

      (* encode the constraints for solve_ext (c.f., sdp.mli) *)
      List.rev_map
        (fun (le, lij) ->
         let le, b = LinExprSC.to_list le in
         let vect =
           List.map
             (fun (id, c) ->
              Ident.Map.find id var_idx, -. LinExprSC.Coeff.to_float c)
             le in
         let mat = [ei, List.map (fun (i, j) -> i, j, 1.) lij] in
         let b = LinExprSC.Coeff.to_float b in
         vect, mat, b, b)
        constraints in

    let cstrs = List.flatten (List.mapi build_cstr scalarized) in

    (* build the objective (see sdp.mli) *)
    let obj = match obj with
      | Minimize id ->
         begin
           try [Ident.Map.find id var_idx, -1.], [] with Not_found -> [], []
         end
      | Maximize id ->
         begin
           try [Ident.Map.find id var_idx, 1.], [] with Not_found -> [], []
         end
      | Purefeas -> [], [] in

    Format.printf "SDP solved <@.";
    Format.printf "%a@." Sdp.pp_ext_sparse (obj, cstrs, []);
    Format.printf ">@.";

    (* call SDP solver *)
    let ret, obj, (res_x, _, _) =
      Sdp.solve_ext_sparse ?solver obj cstrs [] in

    (* rebuild polynomial variables *)
    if not (SdpRet.is_success ret) then
      ret, obj, Ident.Map.empty
    else
      let vars =
        let a = Array.of_list res_x in
        Ident.Map.map (fun i -> snd a.(i)) var_idx in
      let vars =
        let get_var id = Poly.Coeff.of_float (Ident.Map.find id vars) in
        Ident.Map.mapi
          (fun id ->
           function
           | TYscal -> Scalar (get_var id)
           | TYpoly _ ->
              let p =
                Ident.Map.find id binding
                |> List.map (fun (m, id) -> m, get_var id)
                |> Poly.of_list in
              Poly p)
          env in
      ret, obj, vars
end

module Float = Make (Polynomial.Float)
