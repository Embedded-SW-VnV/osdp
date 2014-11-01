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
  module Mat : Matrix.S
  type matrix_expr =
    | Const of Mat.t
    | Var of Ident.t
    | Zeros of int * int
    | Eye of int
    | Kronecker_sym of int * int * int
    | Block of matrix_expr array array
    | Lift_block of matrix_expr * int * int * int * int
    | Transpose of matrix_expr
    | Minus of matrix_expr
    | Scale_const of Mat.Coeff.t * matrix_expr
    | Scale_var of Ident.t * matrix_expr
    | Add of matrix_expr * matrix_expr
    | Sub of matrix_expr * matrix_expr
    | Mult of matrix_expr * matrix_expr
  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas
  type ('a, 'b) value_t = Scalar of 'a | Mat of 'b
  exception Type_error of string
  exception Not_symmetric
  val solve : ?solver:Sdp.solver -> obj_t -> matrix_expr list ->
    SdpRet.t * (float * float) * (Mat.Coeff.t, Mat.t) value_t Ident.Map.t
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) : S with module Mat = M = struct
  module Mat = M

  type matrix_expr =
    | Const of Mat.t
    | Var of Ident.t
    | Zeros of int * int
    | Eye of int
    | Kronecker_sym of int * int * int
    | Block of matrix_expr array array
    | Lift_block of matrix_expr * int * int * int * int
    | Transpose of matrix_expr
    | Minus of matrix_expr
    | Scale_const of Mat.Coeff.t * matrix_expr
    | Scale_var of Ident.t * matrix_expr
    | Add of matrix_expr * matrix_expr
    | Sub of matrix_expr * matrix_expr
    | Mult of matrix_expr * matrix_expr

  let pp fmt e =
    let rec pp_prior prior fmt = function
      | Const m -> Mat.pp fmt m
      | Var i -> Ident.pp fmt i
      | Zeros (n, m) -> Format.fprintf fmt "zeros(%i, %i)" n m
      | Eye n -> Format.fprintf fmt "eye(%i, %i)" n n
      | Kronecker_sym (n, i, j) -> Mat.pp fmt (Mat.kronecker_sym n i j)
      | Block a ->
         Format.fprintf fmt "[@[%a@]]"
                        (Utils.fprintf_array ~sep:";@ "
                           (fun fmt -> Format.fprintf fmt "@[%a@]"
                              (Utils.fprintf_array ~sep:",@ " (pp_prior 0)))) a
      | Lift_block (m, i, j, k, l) ->
         Format.fprintf fmt "lift_block(@[%a,@ %i, %i, %i, %i@])"
                        (pp_prior 0) m i j k l
      | Transpose m -> Format.fprintf fmt "%a'" (pp_prior 2) m
      | Minus m -> Format.fprintf fmt "-%a" (pp_prior (max 1 prior)) m
      | Scale_const (e, m) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         Mat.Coeff.pp e (pp_prior 1) m
      | Scale_var (i, e) -> Format.fprintf fmt
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
         (pp_prior 1) e1 (pp_prior 1) e2 in
    pp_prior 0 fmt e

  (*****************)
  (* Type checking *)
  (*****************)

  (* Type of variables. *)
  type ty =
    | TYscal  (* scalar variable *)
    | TYmat of int option  (* matrix variable and size (if known) *)

  type uf =
    | UFr of ty  (* variable is of type ty *)
    | UFl of Ident.t  (* variable is of the same type than another *)

  exception Type_error of string

  let type_error id s =
    let s = match id with
      | None -> s
      | Some i -> "'" ^ Format.asprintf "%a" Ident.pp i ^ "' " ^ s in
    raise (Type_error s)

  (* Infers types of variables (i.e., whether they are scalars or
     matrices and, in the latter case, size of the matrices).

     @raise Type_error with an explanatory message in case something
     inconsistent is found. *)
  let type_check (el : matrix_expr list) : ty Ident.Map.t =
    let htbl : (Ident.t, uf) Hashtbl.t = Hashtbl.create 31 in

    (* Returns representant of identifier i. *)
    let rec find_repr i =
      try
        match Hashtbl.find htbl i with
        | UFr _ -> i
        | UFl i' ->
           let i'' = find_repr i' in
           Hashtbl.add htbl i (UFl i'');  (* path compression *)
           i''
      with Not_found -> i in

    (* Returns what we know so far about i. *)
    let find i =
      try
        match Hashtbl.find htbl (find_repr i) with
        | UFl _ -> assert false  (* result of find_repr can't link to UFl *)
        | UFr t -> Some t
      with Not_found -> None in

    let set i t = Hashtbl.replace htbl (find_repr i) t in

    (* Combines types t1 and t2, raises Type_error if they are inconsistent. *)
    let meet i t1 t2 =
      match t1, t2 with
      | TYscal, TYscal -> TYscal
      | TYscal, TYmat _  | TYmat _, TYscal ->
         type_error i "cannot be a scalar and a matrix at the same time."
      | TYmat None, _ -> t2
      | _, TYmat None -> t1
      | TYmat (Some n1), TYmat (Some n2) ->
         if n1 = n2 then t1 else
           type_error i ("cannot be of size " ^ string_of_int n1 ^ " and "
                         ^ string_of_int n2 ^ " at the same time.") in

    (* constrain i to have type ti *)
    let constrain i ti =
      let ti =
        match find i with None -> ti | Some ti' -> meet (Some i) ti ti' in
      set i (UFr ti) in

    (* constrain i and i' to have same type *)
    let equate i i' =
      if Ident.compare (find_repr i) (find_repr i') <> 0 then
        match find i, find i' with
        | None, _ -> set i (UFl i')
        | _, None -> set i' (UFl i)
        | Some ti, Some ti' ->
           let t = meet (Some i) ti ti' in
           set i' (UFr t); set i (UFl i') in

    let rec type_check sline scol = function
      | Const m ->
         meet None sline (TYmat (Some (Mat.nb_lines m))),
         meet None scol (TYmat (Some (Mat.nb_cols m))),
         None
      | Var i ->
         let t = TYmat None in  (* i is a square matrix... *)
         let t = meet (Some i) t sline in  (* ...of size sline... *)
         let t = meet (Some i) t scol in  (* ...and scol *)
         constrain i t;
         t, t, Some i
      | Zeros (n, m) ->
         meet None sline (TYmat (Some n)), meet None scol (TYmat (Some m)), None
      | Eye n | Kronecker_sym (n, _, _) ->
         let t = meet None (TYmat (Some n)) sline in
         let t = meet None t scol in
         t, t, None
      | Block a ->
         if Array.length a <= 0 || Array.length a.(0) <= 0 then
           type_error None "Block matrix dimension error.";
         let nl = Array.length a in
         let nc = Array.length a.(0) in
         let slines = Array.make nl (TYmat None) in
         let scols = Array.make nc (TYmat None) in
         let eqs = Array.make_matrix nl nc None in
         for i = 0 to nl - 1 do
           for j = 0 to nc - 1 do
             let sl, sc, eq = type_check slines.(i) scols.(j) a.(i).(j) in
             slines.(i) <- sl; scols.(j) <- sc; eqs.(i).(j) <- eq
           done
         done;
         let eqc = Array.make nc None in
         for i = 0 to nl - 1 do
           let eql = ref None in
           for j = 0 to nc - 1 do
             match eqs.(i).(j) with None -> () | Some id ->
               constrain id slines.(i); constrain id scols.(j);
               begin match !eql with None -> eql := Some id
                                   | Some id' -> equate id id' end;
               begin match eqc.(j) with None -> eqc.(j) <- Some id
                                      | Some id' -> equate id id' end
           done
         done;
         let sum a =
           Array.fold_left
             (fun t1 t2 ->
                match t1, t2 with
                | TYmat (Some n1), TYmat (Some n2) -> TYmat (Some (n1 + n2))
                | _ -> TYmat None)
             (TYmat (Some 0)) a in
         sum slines, sum scols, None
      | Lift_block (m, i, j, k, l) ->
         let sline = meet None sline (TYmat (Some i)) in
         let scol = meet None scol (TYmat (Some j)) in
         sline, scol, None
      | Transpose m ->
         let sline, scol, eq = type_check scol sline m in scol, sline, eq
      | Minus m | Scale_const (_, m) -> type_check sline scol m
      | Scale_var (i, e) ->
         constrain i TYscal; type_check sline scol e
      | Add (e1, e2) | Sub (e1, e2) ->
         let sline, scol, eq1 = type_check sline scol e1 in
         let sline, scol, eq2 = type_check sline scol e2 in
         let eq = match eq1, eq2 with
           | None, None -> None
           | Some i, None
           | None, Some i -> constrain i scol; Some i
           | Some i, Some j ->
              constrain i scol; equate i j; Some i in
         sline, scol, eq
      | Mult (e1, e2) ->
         let sline, smiddle, eq1 = type_check sline (TYmat None) e1 in
         let smiddle, scol, eq2 = type_check smiddle scol e2 in
         let eq = match eq1, eq2 with
           | None, None -> None
           | Some i, None -> constrain i smiddle; None
           | None, Some i -> constrain i scol; None
           | Some i, Some j ->
              constrain i scol; equate i j; Some i in
         sline, scol, eq in

    List.iter
      (fun e -> let _ = type_check (TYmat None) (TYmat None) e in ()) el;
    Hashtbl.fold
      (fun i _ m -> match find i with None -> m | Some t -> Ident.Map.add i t m)
      htbl Ident.Map.empty

  (*************)
  (* Scalarize *)
  (*************)

  module LinExprSC = LinExpr.Make (Mat.Coeff)

  (* matrices whose coefficients are linear expressions *)
  module LEMat = Matrix.Make (LinExpr.MakeScalar (LinExprSC))

  (* Decomposes all matrix variables into a matrix of new scalar
     variables and returns a matrix of linear expressions in those
     scalar variables. Also returns a mapping [m]. All new variables
     [sv] map to [v, (i, j)] in [m] where [v] is the matrix variable
     they are part of and [i] and [j] their indices (starting from 0)
     in [v]. Only upper triangular coeffs are provided since all
     matrix variables [v] are symmetric. The second argument should be
     the result of {!type_check} on the first argument.

     @raise Type_error in case the type of a variable is unknown.

     @raise LinExpr.Not_linear if one of the input matrix expressions
     is non linear. *)
  let scalarize (el : matrix_expr list) (env : ty Ident.Map.t) :
        LinExprSC.t array array list * (Ident.t * (int * int)) Ident.Map.t =
    (* mapping each matrix variable and indices (only upper diagonal
       since matrix variables are symmetric) to the newly created
       scalar variable *)
    let new_ids : ((Ident.t * (int * int)) * Ident.t) list ref = ref [] in

    let scalarize_mat_var =
      (* mapping each matrix variable to the matrix of its newly
         created scalar variables *)
      let htbl : (Ident.t, LEMat.t) Hashtbl.t = Hashtbl.create 31 in
      let get_size id =
        let err id =
          type_error (Some id) ("Unable to infer type of the variable. Can "
                                ^ "be fixed by adding zeros(n, n) to it.") in
        let ty = try Ident.Map.find id env with Not_found -> err id in
        match ty with TYmat (Some n) -> n | _ -> err id in
      let new_id id i j =
        let s = Format.asprintf "%a" Ident.pp id in
        let new_id = Ident.create ("__LMI__" ^  s ^ "_"
                                   ^ string_of_int i ^ "_" ^ string_of_int j) in
        new_ids := ((id, (i, j)), new_id) :: !new_ids; new_id in
      fun id ->
        try Hashtbl.find htbl id
        with Not_found ->
          let size = get_size id in
          let a = Array.make_matrix size size LEMat.Coeff.zero in
          for i = 0 to size - 1 do
            for j = i to size - 1 do
              a.(i).(j) <- LinExprSC.var (new_id id i j); a.(j).(i) <- a.(i).(j)
            done
          done;
          let m = LEMat.of_array_array a in Hashtbl.add htbl id m; m in

    let rec scalarize = function
      | Const m ->
         let l = Mat.to_list_list m in
         let l = List.map (List.map LinExprSC.const) l in
         LEMat.of_list_list l
      | Var i -> scalarize_mat_var i
      | Zeros (n, m) -> LEMat.zeros n m
      | Eye n -> LEMat.eye n
      | Kronecker_sym (n, i, j) -> LEMat.kronecker_sym n i j
      | Block a -> LEMat.block (Array.map (Array.map scalarize) a)
      | Lift_block (m, i, j, k, l) -> LEMat.lift_block (scalarize m) i j k l
      | Transpose m -> LEMat.transpose (scalarize m)
      | Minus m -> LEMat.minus (scalarize m)
      | Scale_const (e, m) ->
         let le = LinExprSC.const e in
         LEMat.mult_scalar le (scalarize m)
      | Scale_var (i, e) ->
         let le = LinExprSC.var i in
         LEMat.mult_scalar le (scalarize e)
      | Add (e1, e2) -> LEMat.add (scalarize e1) (scalarize e2)
      | Sub (e1, e2) -> LEMat.sub (scalarize e1) (scalarize e2)
      | Mult (e1, e2) -> LEMat.mult (scalarize e1) (scalarize e2) in

    (* scalarize *)
    let el = try List.map scalarize el
             with LEMat.Dimension_error -> type_error None "dimension error" in
    let el = List.map LEMat.to_array_array el in
    (* and collect bindings *)
    let b = List.fold_left (fun m (coord, id) -> Ident.Map.add id coord m)
                           Ident.Map.empty !new_ids in
    el, b

  (*********)
  (* Solve *)
  (*********)

  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

  exception Not_symmetric

  let solve ?solver obj el = 
    let env = type_check el in

    let scalarized, binding = scalarize el env in

    (* check symmetry *)
    List.iter
      (fun a ->
         let sz = Array.length a in
         if sz > 0 && Array.length a.(0) > 0 then begin
           if sz <> Array.length a.(0) then
             raise Not_symmetric; 
           for i = 0 to sz - 1 do
             for j = i + 1 to sz - 1 do
               if not (LinExprSC.eq a.(i).(j) a.(j).(i)) then
                 raise Not_symmetric
             done
           done
         end) scalarized;

    (* building block matrices A_i and C (see csdp.mli) *)
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

    (* add the scalars a_i (see csdp.mli) *)
    let vars, constraints = Hashtbl.fold
      (fun id blks (lv, lc) ->
       let a_i = match obj with
         | Minimize id' when Ident.compare id id' = 0 -> 1.
         | Maximize id' when Ident.compare id id' = 0 -> -1.
         | _ -> 0. in
       id :: lv, (blks, a_i) :: lc) blks_A ([], []) in

    (* call SDP solver *)
    let ret, (pres, dres), (primal_sol, dual_sol) =
      Sdp.solve ?solver !blks_C constraints in

    (* rebuild matrix variables *)
    if not (SdpRet.is_success ret) then
      ret, (-. dres, -. pres), Ident.Map.empty
    else
      let vars = List.mapi (fun i id -> id, dual_sol.(i)) vars in
      let scalars, matrices =
        List.fold_left
          (fun (scalars, matrices) (id, f) ->
           try
             let mid, (i, j) = Ident.Map.find id binding in
             (* coefficient of matrix variable *)
             let mat =
               try Ident.Map.find mid matrices
               with Not_found ->
                 let sz =
                   try
                     match Ident.Map.find mid env with
                     | TYmat (Some sz) -> sz
                     | _ -> assert false  (* should never happen *)
                   with Not_found -> assert false in  (* should never happen *)
                 Array.make_matrix sz sz Mat.Coeff.zero in
             mat.(i).(j) <- Mat.Coeff.of_float f; mat.(j).(i) <- mat.(i).(j);
             scalars, Ident.Map.add mid mat matrices
           with Not_found ->
             (* scalar variable *)
             Ident.Map.add id (Mat.Coeff.of_float f) scalars, matrices)
          (Ident.Map.empty, Ident.Map.empty) vars in
      let vars = Ident.Map.map (fun e -> Scalar e) scalars in
      let vars =
        Ident.Map.fold
          (fun id m vars -> Ident.Map.add id (Mat (Mat.of_array_array m)) vars)
          matrices vars in
       ret, (-. dres, -. pres), vars
end

module Float = Make (Matrix.Float)
