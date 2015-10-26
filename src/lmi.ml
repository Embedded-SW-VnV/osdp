(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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
  module Mat : Matrix.S
  type var
  type matrix_expr =
    | Const of Mat.t
    | Var of var
    | Zeros of int * int
    | Eye of int
    | Kron of int * int * int
    | Kron_sym of int * int * int
    | Block of matrix_expr array array
    | Lift_block of matrix_expr * int * int * int * int
    | Transpose of matrix_expr
    | Minus of matrix_expr
    | Add of matrix_expr * matrix_expr
    | Sub of matrix_expr * matrix_expr
    | Mult of matrix_expr * matrix_expr
  val var : string -> int -> matrix_expr
  val scalar : Mat.Coeff.t -> matrix_expr
  val nb_lines : matrix_expr -> int
  val nb_cols : matrix_expr -> int
  val is_symmetric : matrix_expr -> bool
  type options = { 
    sdp : Sdp.options;
    verbose : int
  }
  val default : options
  type obj = Minimize of matrix_expr | Maximize of matrix_expr | Purefeas
  type values
  exception Dimension_error of string
  exception Not_linear
  exception Not_symmetric
  val solve : ?options:options -> ?solver:Sdp.solver ->
              obj -> matrix_expr list ->
              SdpRet.t * (float * float) * values
  val value : matrix_expr -> values -> Mat.Coeff.t
  val value_mat : matrix_expr -> values -> Mat.t
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) : S with module Mat = M = struct
  module Mat = M

  type var = { name : Ident.t; mat : Ident.t array array }

  type matrix_expr =
    | Const of Mat.t
    | Var of var
    | Zeros of int * int
    | Eye of int
    | Kron of int * int * int
    | Kron_sym of int * int * int
    | Block of matrix_expr array array
    | Lift_block of matrix_expr * int * int * int * int
    | Transpose of matrix_expr
    | Minus of matrix_expr
    | Add of matrix_expr * matrix_expr
    | Sub of matrix_expr * matrix_expr
    | Mult of matrix_expr * matrix_expr

  let var s dim =
    let name = Ident.create s in
    let a =
      let new_id i j =
        let s = (*"__LMI__" ^*) Format.asprintf "%a" Ident.pp name ^ "_" in
        Ident.create (s ^ string_of_int i ^ "_" ^ string_of_int j) in
      let a = Array.make_matrix dim dim name in
      for i = 0 to dim - 1 do
        for j = i to dim - 1 do
          a.(i).(j) <- new_id i j; a.(j).(i) <- a.(i).(j)
        done
      done;
      a in
    Var { name = name; mat = a }
                              
  let scalar s = Const (Mat.of_list_list [[s]])

  let pp fmt e =
    let rec pp_prior prior fmt = function
      | Const m when Mat.nb_lines m = 1 && Mat.nb_cols m = 1 ->
         begin
           match Mat.to_list_list m with
           | [[e]] -> Mat.Coeff.pp fmt e
           | _ -> assert false
         end
      | Const m -> Mat.pp fmt m
      | Var v -> Ident.pp fmt v.name
      | Zeros (n, m) -> Format.fprintf fmt "zeros(%i, %i)" n m
      | Eye n -> Format.fprintf fmt "eye(%i, %i)" n n
      | Kron (n, i, j) -> Mat.pp fmt (Mat.kron n i j)
      | Kron_sym (n, i, j) -> Mat.pp fmt (Mat.kron_sym n i j)
      | Block a ->
         Format.fprintf fmt "[@[%a@]]"
                        (Utils.pp_array ~sep:";@ "
                           (fun fmt -> Format.fprintf fmt "@[%a@]"
                              (Utils.pp_array ~sep:",@ " (pp_prior 0)))) a
      | Lift_block (m, i, j, k, l) ->
         Format.fprintf fmt "lift_block(@[%a,@ %i, %i, %i, %i@])"
                        (pp_prior 0) m i j k l
      | Transpose m -> Format.fprintf fmt "%a'" (pp_prior 2) m
      | Minus m -> Format.fprintf fmt "-%a" (pp_prior (max 1 prior)) m
      | Add (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ + %a@])" else "@[%a@ + %a@]")
         (pp_prior 0) e1 (pp_prior 0) e2
      | Sub (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ - %a@])" else "@[%a@ - %a@]")
         (pp_prior 0) e1 (pp_prior 1) e2
      | Mult (e1, e2) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         (pp_prior 1) e1 (pp_prior 1) e2 in
    pp_prior 0 fmt e

  (*************)
  (* Scalarize *)
  (*************)

  module LinExprSC = LinExpr.Make (Mat.Coeff)

  (* matrices whose coefficients are linear expressions *)
  module LEMat = Matrix.Make (LinExpr.MakeScalar (LinExprSC))

  exception Dimension_error of string

  exception Not_linear

  (* Decomposes all matrix variables into a matrix of new scalar
     variables and returns a matrix of linear expressions in those
     scalar variables.

     @raise Dimension_error in case [el] contains an inconsistent
     operation.

     @raise LinExpr.Not_linear if one of the input matrix expressions
     is non linear. *)
  let scalarize (e : matrix_expr) : LinExprSC.t array array =

    let rec scalarize = function
      | Const m ->
         Mat.to_list_list m
         |> List.map (List.map LinExprSC.const)
         |> LEMat.of_list_list
      | Var v ->
         Array.map (Array.map LinExprSC.var) v.mat
         |> LEMat.of_array_array
      | Zeros (n, m) -> LEMat.zeros n m
      | Eye n -> LEMat.eye n
      | Kron (n, i, j) -> LEMat.kron n i j
      | Kron_sym (n, i, j) -> LEMat.kron_sym n i j
      | Block a -> LEMat.block (Array.map (Array.map scalarize) a)
      | Lift_block (m, i, j, k, l) -> LEMat.lift_block (scalarize m) i j k l
      | Transpose m -> LEMat.transpose (scalarize m)
      | Minus m -> LEMat.minus (scalarize m)
      | Add (e1, e2) -> LEMat.add (scalarize e1) (scalarize e2)
      | Sub (e1, e2) -> LEMat.sub (scalarize e1) (scalarize e2)
      | Mult (e1, e2) ->
         let e1 = scalarize e1 in
         let e2 = scalarize e2 in
         if LEMat.nb_lines e1 = 1 && LEMat.nb_cols e1 = 1 then
           match LEMat.to_list_list e1 with
           | [[e1]] -> LEMat.mult_scalar e1 e2
           | _ -> assert false
         else
           LEMat.mult e1 e2 in

    (* scalarize *)
    let e =
      try scalarize e
      with
      | LEMat.Dimension_error s -> raise (Dimension_error s)
      | LinExpr.Not_linear -> raise Not_linear in
    LEMat.to_array_array e

  (* various operations that need scalarize *)

  let nb_lines e = scalarize e |> LEMat.of_array_array |> LEMat.nb_lines
  let nb_cols e = scalarize e |> LEMat.of_array_array |> LEMat.nb_cols

  let is_symmetric e = scalarize e |> LEMat.of_array_array |> LEMat.is_symmetric

  (*********)
  (* Solve *)
  (*********)

  type options = {
    sdp : Sdp.options;
    verbose : int
  }

  let default = { sdp = Sdp.default; verbose = 0 }

  type obj = Minimize of matrix_expr | Maximize of matrix_expr | Purefeas

  type values = Mat.Coeff.t Ident.Map.t

  exception Not_symmetric

  let solve ?options ?solver obj el =
    let options, sdp_options =
      match options with None -> default, None | Some o -> o, Some o.sdp in
    
    let obj, obj_sign = match obj with
      | Minimize obj -> obj, 1.
      | Maximize obj -> Minus obj, -1.
      | Purefeas -> Const (Mat.zeros 1 1), 0. in

    let obj = scalarize obj in
    let scalarized = List.map scalarize el in

    let obj, obj_cst = match obj with
      | [|[|obj|]|] ->
         let le, c = LinExprSC.to_list obj in
         let tf = LinExprSC.Coeff.to_float in
         let le =
           List.fold_left
             (fun m (id, c) -> Ident.Map.add id (tf c) m)
             Ident.Map.empty le in
         le, tf c
      | _ -> raise Not_linear in

    (* check symmetry *)
    List.iter
      (fun a ->
       if not (LEMat.(is_symmetric (of_array_array a))) then
         raise Not_symmetric)
      scalarized;

    (* building block matrices A_i and C (see sdp.mli) *)
    let blks_A : (Ident.t, (int * float array array) list) Hashtbl.t = Hashtbl.create 31 in
    let blks_C = ref [] in

    (* build ith diagonal block, corresponding to scalarized matrix_expr me *)
    let build_blk i me =
      let sz = Array.length me in
      let get_blk_A id =
        let l = try Hashtbl.find blks_A id with Not_found -> [] in
        match l with
        | (i', a) :: _ when i' = i -> a
        | _ ->
           let a = Array.make_matrix sz sz 0. in
           Hashtbl.replace blks_A id ((i, a) :: l);
           a in
      let blk_C = Array.make_matrix sz sz 0. in
      for i = 0 to sz - 1 do
        for j = i to sz - 1 do
          let lin, const = LinExprSC.to_list me.(i).(j) in
          List.iter
            (fun (id, c) ->
               let blk_A = get_blk_A id in
               blk_A.(i).(j) <- LinExprSC.Coeff.to_float c;
               blk_A.(j).(i) <- blk_A.(i).(j)) lin;
          (* C is the opposite of the constant part *)
          blk_C.(i).(j) <- -. LinExprSC.Coeff.to_float const;
          blk_C.(j).(i) <- blk_C.(i).(j)
        done
      done;
      blks_C := (i, blk_C) :: !blks_C in
    (* build all blocks *)
    List.iteri build_blk scalarized;

    (* add the scalars b_i (see sdp.mli) *)
    let vars, constraints = Hashtbl.fold
      (fun id blks (lv, lc) ->
       let b_i = try Ident.Map.find id obj with Not_found -> 0. in
       id :: lv, Sdp.Eq (blks, b_i) :: lc) blks_A ([], []) in

    (* Format.printf "SDP solved <@."; *)
    (* Format.printf "%a@." Sdp.pp (!blks_C, constraints); *)
    (* Format.printf ">@."; *)

    (* call SDP solver *)
    let ret, (pres, dres), (_, dual_sol, _) =
      Sdp.solve ?options:sdp_options ?solver !blks_C constraints in

    let res = let f o = obj_sign *. (o +. obj_cst) in f pres, f dres in

    (* rebuild variables *)
    if not (SdpRet.is_success ret) then
      ret, res, Ident.Map.empty
    else
      let vars =
        List.mapi (fun i id -> id, Mat.Coeff.of_float (dual_sol.(i))) vars
        |> Ident.Map.(List.fold_left (fun m (id, v) -> add id v m) empty) in
      ret, res, vars

  let rec value_mat e m =
    let rec aux = function
      | Const mat -> mat
      | Var v ->
         Array.map (Array.map (fun id -> Ident.Map.find id m)) v.mat
         |> Mat.of_array_array
      | Zeros (n, m) -> Mat.zeros n m
      | Eye n -> Mat.eye n
      | Kron (n, i, j) -> Mat.kron n i j
      | Kron_sym (n, i, j) -> Mat.kron_sym n i j
      | Block a -> Mat.block (Array.map (Array.map aux) a)
      | Lift_block (m, i, j, k, l) -> Mat.lift_block (aux m) i j k l
      | Transpose m -> Mat.transpose (aux m)
      | Minus m -> Mat.minus (aux m)
      | Add (e1, e2) -> Mat.add (aux e1) (aux e2)
      | Sub (e1, e2) -> Mat.sub (aux e1) (aux e2)
      | Mult (e1, e2) -> Mat.mult (aux e1) (aux e2) in
    aux e

  let value e m =
    match Mat.to_list_list (value_mat e m) with
    | [[s]] -> s
    | _ -> raise (Dimension_error "value (scalar expected)")
end

module Q = Make (Matrix.Q)

module Float = Make (Matrix.Float)
