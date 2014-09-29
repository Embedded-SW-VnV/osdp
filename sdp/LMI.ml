module type S = sig
  module Mat : Matrix.S
  module LinExpr : LinExpr.S
  type matrix_expr =
    | MEconst of Mat.t
    | MEvar of Ident.t
    | MEzeros of int * int
    | MEeye of int
    | MEkronecker_sym of int * int * int
    | MEblock of matrix_expr array array
    | MElift_block of matrix_expr * int * int * int * int
    | MEtranspose of matrix_expr
    | MEminus of matrix_expr
    | MEmult_const of Mat.Elem.t * matrix_expr
    | MEmult_scalar of Ident.t * matrix_expr
    | MEadd of matrix_expr * matrix_expr
    | MEsub of matrix_expr * matrix_expr
    | MEmult of matrix_expr * matrix_expr
  type ty =
    | TIscal
    | TImat of int option
  exception Type_error of string
  val type_check : matrix_expr list -> ty Ident.Map.t
  exception Not_linear
  val scalarize : matrix_expr list -> ty Ident.Map.t ->
                  LinExpr.t array array list * (Ident.t * (int * int)) Ident.Map.t
  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas
  type ('a, 'b) value_t = Scalar of 'a | Mat of 'b
  exception Not_symmetric
  val solve : obj_t -> matrix_expr list ->
    float * (Mat.Elem.t, Mat.t) value_t Ident.Map.t
  val pp : Format.formatter -> matrix_expr -> unit
end

module Make (M : Matrix.S) (LE : LinExpr.S with module Coeff = M.Elem) :
  S with module Mat = M and module LinExpr = LE =
struct
  module Mat = M
  module LinExpr = LE

  type matrix_expr =
    | MEconst of Mat.t
    | MEvar of Ident.t
    | MEzeros of int * int
    | MEeye of int
    | MEkronecker_sym of int * int * int
    | MEblock of matrix_expr array array
    | MElift_block of matrix_expr * int * int * int * int
    | MEtranspose of matrix_expr
    | MEminus of matrix_expr
    | MEmult_const of Mat.Elem.t * matrix_expr
    | MEmult_scalar of Ident.t * matrix_expr
    | MEadd of matrix_expr * matrix_expr
    | MEsub of matrix_expr * matrix_expr
    | MEmult of matrix_expr * matrix_expr

  let pp fmt e =
    let rec pp_prior prior fmt = function
      | MEconst m -> Mat.pp fmt m
      | MEvar i -> Ident.pp fmt i
      | MEzeros (n, m) -> Format.fprintf fmt "zeros(%i, %i)" n m
      | MEeye n -> Format.fprintf fmt "eye(%i, %i)" n n
      | MEkronecker_sym (n, i, j) -> Mat.pp fmt (Mat.kronecker_sym n i j)
      | MEblock a ->
         Format.fprintf fmt "[@[%a@]]"
                        (Utils.fprintf_array ~sep:";@ "
                           (fun fmt -> Format.fprintf fmt "@[%a@]"
                              (Utils.fprintf_array ~sep:",@ " (pp_prior 0)))) a
      | MElift_block (m, i, j, k, l) ->
         Format.fprintf fmt "lift_block(@[%a,@ %i, %i, %i, %i@])"
                        (pp_prior 0) m i j k l
      | MEtranspose m -> Format.fprintf fmt "%a'" (pp_prior 2) m
      | MEminus m -> Format.fprintf fmt "-%a" (pp_prior (max 1 prior)) m
      | MEmult_const (e, m) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         Mat.Elem.pp e (pp_prior 1) m
      | MEmult_scalar (i, e) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         Ident.pp i (pp_prior 1) e
      | MEadd (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ + %a@])" else "@[%a@ + %a@]")
         (pp_prior 0) e1 (pp_prior 0) e2
      | MEsub (e1, e2) -> Format.fprintf fmt
         (if 0 < prior then "(@[%a@ - %a@])" else "@[%a@ - %a@]")
         (pp_prior 0) e1 (pp_prior 1) e2
      | MEmult (e1, e2) -> Format.fprintf fmt
         (if 1 < prior then "(@[%a@ * %a@])" else "@[%a@ * %a@]")
         (pp_prior 1) e1 (pp_prior 1) e2 in
    pp_prior 0 fmt e

  (*****************)
  (* Type checking *)
  (*****************)

  type ty =
    | TIscal  (* scalar variable *)
    | TImat of int option  (* matrix variable and size (if known) *)

  type uf =
    | UFr of ty  (* variable is of type ty *)
    | UFl of Ident.t  (* variable is of the same type than another *)

  exception Type_error of string

  let type_error id s =
    let s = match id with
      | None -> s
      | Some i -> "'" ^ Format.asprintf "%a" Ident.pp i ^ "' " ^ s in
    raise (Type_error s)

  let type_check el =
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
      | TIscal, TIscal -> TIscal
      | TIscal, TImat _  | TImat _, TIscal ->
         type_error i "cannot be a scalar and a matrix at the same time."
      | TImat None, _ -> t2
      | _, TImat None -> t1
      | TImat (Some n1), TImat (Some n2) ->
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
      | MEconst m ->
         meet None sline (TImat (Some (Mat.nb_lines m))),
         meet None scol (TImat (Some (Mat.nb_cols m))),
         None
      | MEvar i ->
         let t = TImat None in  (* i is a square matrix... *)
         let t = meet (Some i) t sline in  (* ...of size sline... *)
         let t = meet (Some i) t scol in  (* ...and scol *)
         constrain i t;
         t, t, Some i
      | MEzeros (n, m) ->
         meet None sline (TImat (Some n)), meet None scol (TImat (Some m)), None
      | MEeye n | MEkronecker_sym (n, _, _) ->
         let t = meet None (TImat (Some n)) sline in
         let t = meet None t scol in
         t, t, None
      | MEblock a ->
         if Array.length a <= 0 || Array.length a.(0) <= 0 then
           type_error None "Block matrix dimension error.";
         let nl = Array.length a in
         let nc = Array.length a.(0) in
         let slines = Array.make nl (TImat None) in
         let scols = Array.make nc (TImat None) in
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
                | TImat (Some n1), TImat (Some n2) -> TImat (Some (n1 + n2))
                | _ -> TImat None)
             (TImat (Some 0)) a in
         sum slines, sum scols, None
      | MElift_block (m, i, j, k, l) ->
         let sline = meet None sline (TImat (Some i)) in
         let scol = meet None scol (TImat (Some j)) in
         sline, scol, None
      | MEtranspose m ->
         let sline, scol, eq = type_check scol sline m in scol, sline, eq
      | MEminus m | MEmult_const (_, m) -> type_check sline scol m
      | MEmult_scalar (i, e) ->
         constrain i TIscal; type_check sline scol e
      | MEadd (e1, e2) | MEsub (e1, e2) ->
         let sline, scol, eq1 = type_check sline scol e1 in
         let sline, scol, eq2 = type_check sline scol e2 in
         let eq = match eq1, eq2 with
           | None, None -> None
           | Some i, None
           | None, Some i -> constrain i scol; Some i
           | Some i, Some j ->
              constrain i scol; equate i j; Some i in
         sline, scol, eq
      | MEmult (e1, e2) ->
         let sline, smiddle, eq1 = type_check sline (TImat None) e1 in
         let smiddle, scol, eq2 = type_check smiddle scol e2 in
         let eq = match eq1, eq2 with
           | None, None -> None
           | Some i, None -> constrain i smiddle; None
           | None, Some i -> constrain i scol; None
           | Some i, Some j ->
              constrain i scol; equate i j; Some i in
         sline, scol, eq in

    List.iter
      (fun e -> let _ = type_check (TImat None) (TImat None) e in ()) el;
    Hashtbl.fold
      (fun i _ m -> match find i with None -> m | Some t -> Ident.Map.add i t m)
      htbl Ident.Map.empty

  (*************)
  (* Scalarize *)
  (*************)

  exception Not_linear

  (* matrices whose coefficients are linear expressions *)
  module LEMat = Matrix.Make (struct
    type t = LinExpr.t
    let zero = LinExpr.const LinExpr.Coeff.zero
    let one = LinExpr.const LinExpr.Coeff.one
    let is_zero e =
      LinExpr.is_const e
      && let _, c = LinExpr.to_list e in LinExpr.Coeff.is_zero c
    let of_float _ = assert false  (* should never happen *)
    let to_float _ = assert false  (* should never happen *)
    let add = LinExpr.add
    let sub = LinExpr.sub
    let mult e1 e2 =
      match LinExpr.is_const e1, LinExpr.is_const e2 with
      | false, false -> raise Not_linear
      | true, _ ->
         let _, s = LinExpr.to_list e1 in
         LinExpr.mult_scalar s e2
      | false, true ->
         let _, s = LinExpr.to_list e2 in
         LinExpr.mult_scalar s e1
    let div _ _ = assert false  (* should never happen *)
    let pp = LinExpr.pp
  end)

  let scalarize el env =
    let htbl : ((Ident.t * (int * int)), Ident.t) Hashtbl.t = Hashtbl.create 31 in

    let scalarize_mat_var id =
      let size =
        let ty =
          try Ident.Map.find id env
          with Not_found ->
            type_error (Some id) ("Unable to infer type of the variable. Can "
                                  ^ "be fixed by adding zeros(n, n) to it.") in
        match ty with TImat (Some n) -> n | _ -> type_error None "" in
      let a = Array.make_matrix size size LEMat.Elem.zero in
      for i = 0 to size - 1 do
        for j = i to size - 1 do
          let sid =
            try Hashtbl.find htbl (id, (i, j))
            with Not_found ->
              let new_id =
                let s = Format.asprintf "%a" Ident.pp id in
                Ident.create ((* "__LMI__" ^ *) s ^ "_"
                              ^ string_of_int i ^ "_" ^ string_of_int j) in
              Hashtbl.add htbl (id, (i, j)) new_id;
              new_id in
          a.(i).(j) <- LinExpr.var sid; a.(j).(i) <- a.(i).(j)
        done
      done;
      LEMat.of_array_array a in

    let rec scalarize = function
      | MEconst m ->
         let l = Mat.to_list_list m in
         let l = List.map (List.map LinExpr.const) l in
         LEMat.of_list_list l
      | MEvar i -> scalarize_mat_var i
      | MEzeros (n, m) -> LEMat.zeros n m
      | MEeye n -> LEMat.eye n
      | MEkronecker_sym (n, i, j) -> LEMat.kronecker_sym n i j
      | MEblock a -> LEMat.block (Array.map (Array.map scalarize) a)
      | MElift_block (m, i, j, k, l) -> LEMat.lift_block (scalarize m) i j k l
      | MEtranspose m -> LEMat.transpose (scalarize m)
      | MEminus m -> LEMat.minus (scalarize m)
      | MEmult_const (e, m) ->
         let le = LinExpr.const e in
         LEMat.mult_scalar le (scalarize m)
      | MEmult_scalar (i, e) ->
         let le = LinExpr.var i in
         LEMat.mult_scalar le (scalarize e)
      | MEadd (e1, e2) -> LEMat.add (scalarize e1) (scalarize e2)
      | MEsub (e1, e2) -> LEMat.sub (scalarize e1) (scalarize e2)
      | MEmult (e1, e2) -> LEMat.mult (scalarize e1) (scalarize e2) in

    (* scalarize *)
    let el = List.map scalarize el in
    let el = List.map LEMat.to_array_array el in
    (* and collect bindings *)
    let b = Hashtbl.fold
              (fun coord id -> Ident.Map.add id coord)
              htbl Ident.Map.empty in
    el, b

  (*********)
  (* Solve *)
  (*********)

  type obj_t = Minimize of Ident.t | Maximize of Ident.t | Purefeas

  type ('a, 'b) value_t = Scalar of 'a | Mat of 'b

  exception Not_symmetric

  let solve obj el = 
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
               if not (LinExpr.eq a.(i).(j) a.(j).(i)) then
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
          let lin, const = LinExpr.to_list me.(i).(j) in
          List.iter
            (fun (id, c) ->
               let blk_A = get_blk_A id in
               blk_A.(i).(j) <- LinExpr.Coeff.to_float c;
               blk_A.(j).(i) <- blk_A.(i).(j)) lin;
          (* C is the opposite of the constant part *)
          blk_C.(i).(j) <- -. LinExpr.Coeff.to_float const;
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

    List.iter2
      (fun id (blks, a_i) ->
    Format.printf
      "A_%a: %f, @[%a@]@." Ident.pp id a_i
      (Utils.fprintf_list ~sep:",@ "
         (fun fmt (i, b) ->
            Format.fprintf
              fmt "(%i, [@[%a@]])" i
              (Utils.fprintf_array
                 ~sep:";@ "
                 (fun fmt l ->
                    Format.fprintf
                      fmt "@[%a@]"
                      (Utils.fprintf_array
                         ~sep:",@ "
                         (fun fmt -> Format.fprintf fmt "%f")) l)) b)) blks)
      vars constraints;

    Format.printf
      "C: @[%a@]@."
      (Utils.fprintf_list ~sep:",@ "
         (fun fmt (i, b) ->
            Format.fprintf
              fmt "(%i, [@[%a@]])" i
              (Utils.fprintf_array
                 ~sep:";@ "
                 (fun fmt l ->
                    Format.fprintf
                      fmt "@[%a@]"
                      (Utils.fprintf_array
                         ~sep:",@ "
                         (fun fmt -> Format.fprintf fmt "%f")) l)) b)) !blks_C;

    (* call SDP solver *)
    let res, (primal_sol, dual_sol) = Sdp.solve !blks_C constraints in

    (* rebuild matrix variables *)
    if res = infinity || res = neg_infinity then
      res, Ident.Map.empty
    else
      let vars = List.mapi (fun i id -> id, dual_sol.(i)) vars in
      let scalars, matrices = List.fold_left
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
                     | TImat (Some sz) -> sz
                     | _ -> assert false  (* should never happen *)
                   with Not_found -> assert false in  (* should never happen *)
                 Array.make_matrix sz sz Mat.Elem.zero in
             mat.(i).(j) <- Mat.Elem.of_float f; mat.(j).(i) <- mat.(i).(j);
             scalars, Ident.Map.add mid mat matrices
           with Not_found ->
             (* scalar variable *)
             Ident.Map.add id (Mat.Elem.of_float f) scalars, matrices)
        (Ident.Map.empty, Ident.Map.empty) vars in
      let vars = Ident.Map.map (fun e -> Scalar e) scalars in
      let vars = Ident.Map.fold
        (fun id m vars -> Ident.Map.add id (Mat (Mat.of_array_array m)) vars)
        matrices vars in
      res, vars
end

module NumLMI = Make (Matrix.NumMat) (LinExpr.Num)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
