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

type sparse_matrix = (int * int * float) list

type matrix = float array array

type 'a block_diag = (int * 'a) list

let matrix_of_sparse m =
  let sz = List.fold_left (fun d (i, j, _) -> max d (max i j + 1)) 0 m in
  let a = Array.make_matrix sz sz 0. in
  List.iter (fun (i, j, f) -> a.(i).(j) <- f; a.(j).(i) <- f) m;
  a

let matrix_to_sparse m =
  let sz = Array.length m in
  let l = ref [] in
  for j = 0 to sz - 1 do
    for i = j to sz - 1 do
      if m.(i).(j) <> 0. then l := (i, j, m.(i).(j)) :: !l
    done
  done;
  !l

let block_diag_of_sparse = List.map (fun (i, m) -> i, matrix_of_sparse m)

let block_diag_to_sparse = List.map (fun (i, m) -> i, matrix_to_sparse m)

(********)
(* SDP. *)
(********)

type solver = Csdp | Mosek | Sdpa | SdpaGmp | SdpaDd

type options = {
  solver : solver;
  verbose : int;
  max_iteration : int;
  stop_criterion : float;
  initial : float;
  precision : int
}

let default = {
  solver = Csdp;
  verbose = 0;
  max_iteration = 100;
  stop_criterion = 1.0E-7;
  initial = 1.0E2;
  precision = 200
}

(* define default solver *)
let get_solver options = function
  | Some s -> s
  | None -> match options with Some o -> o.solver | None -> default.solver

type 'a obj = 'a block_diag

type 'a constr =
  | Eq of 'a block_diag * float
  | Le of 'a block_diag * float
  | Ge of 'a block_diag * float

(* check symmetry *)
let sym_err () = raise (Invalid_argument "non symmetric matrix")

let check_sym m =
  let sz = Array.length m in
  for i = 1 to sz - 1 do
    for j = 0 to i - 1 do
      if m.(i).(j) <> m.(j).(i) then sym_err ()
    done
  done

let check_sparse = List.iter (fun (i, j, _) -> if j > i then sym_err ())

let check_prog f obj constraints =
  let check_block f = List.iter (fun (_, m) -> f m) in
  check_block f obj;
  List.iter
    (function Eq (m, _) | Le (m, _) | Ge (m, _) -> check_block f m)
    constraints

let solve_sparse ?options ?solver ?init obj constraints =
  check_prog check_sparse obj constraints;
  let solver = get_solver options solver in
  let options = match options with Some o -> o | None -> default in
  let max_idx =
    let f = List.fold_left (fun m (i, _) -> max m i) in
    List.fold_left
      (fun m -> function Eq (bd, _) | Le (bd, _) | Ge (bd, _) -> f m bd)
      (f min_int obj) constraints in
  let constraints, _ =  (* add slack variables *)
    List.fold_left
      (fun (cstrs, i) ->
       function
       | Eq (bd, f) -> (bd, f) :: cstrs, i
       | Le (bd, f) -> ((i, [0, 0, 1.]) :: bd, f) :: cstrs, i + 1
       | Ge (bd, f) -> ((i, [0, 0, -1.]) :: bd, f) :: cstrs, i + 1)
      ([], max_idx + 1) (List.rev constraints) in
  let ret, res, (res_X, res_y, res_Z) = match solver with
    | Csdp ->
       let options = { Csdp.verbose = options.verbose } in
       Csdp.solve ~options obj constraints
    | Mosek ->
       let options = { Moseksdp.verbose = options.verbose } in
       Moseksdp.solve ~options obj constraints
    | (Sdpa | SdpaGmp | SdpaDd) as s ->
       let solver =
         match s with
         | Sdpa -> Sdpa.Sdpa | SdpaGmp -> Sdpa.SdpaGmp | SdpaDd -> Sdpa.SdpaDd
         | _ -> assert false in
       let options = { Sdpa.solver = solver;
                       Sdpa.verbose = options.verbose;
                       Sdpa.max_iteration = options.max_iteration;
                       Sdpa.stop_criterion = options.stop_criterion;
                       Sdpa.initial = options.initial;
                       Sdpa.precision = options.precision } in
       Sdpa.solve ~options ?init obj constraints in
  let filter = List.filter (fun (i, _) -> i <= max_idx) in
  ret, res, (filter res_X, res_y, filter res_Z)

let solve ?options ?solver ?init obj constraints =
  check_prog check_sym obj constraints;
  let obj = block_diag_to_sparse obj in
  let constraints =
    List.map
      (function
        | Eq (c, b) -> Eq (block_diag_to_sparse c, b)
        | Le (c, b) -> Le (block_diag_to_sparse c, b)
        | Ge (c, b) -> Ge (block_diag_to_sparse c, b))
      constraints in
  solve_sparse ?options ?solver ?init obj constraints

(*************************)
(* Extended formulation. *)
(*************************)
  
type vector = (int * float) list

type 'a obj_ext = vector * 'a block_diag

type 'a constr_ext = vector * 'a block_diag * float * float

type bounds = (int * float * float) list

module IntMap = Map.Make (struct type t = int let compare = compare end)
                                    
let to_simple obj constraints bounds =
  let max_idx =
    let f = List.fold_left (fun m (i, _) -> max m i) in
    List.fold_left
      (fun m (_, bd, _, _) -> f m bd)
      (f min_int (snd obj)) constraints in
  let trans, add_cstrs =
    let bounds =
      let vars =
        let collect =
          List.fold_left
            (fun m (v, _) -> IntMap.add v (neg_infinity, infinity) m) in
        List.fold_left
          (fun m (v, _, _, _) -> collect m v)
          (collect IntMap.empty (fst obj)) constraints in
      List.fold_left
        (fun m (v, lb, ub) ->
         if IntMap.mem v m then IntMap.add v (lb, ub) m else m)
        vars bounds in
    let trans, add_cstrs, _ =
      IntMap.fold
        (fun v (lb, ub) (m, ac, i) ->
         if lb = neg_infinity && ub = infinity then
           IntMap.add v (Some i, Some (i + 1), 0.) m, ac, i + 2
         else if ub = infinity then
           IntMap.add v (Some i, None, lb) m, ac, i + 1
         else if lb = neg_infinity then
           IntMap.add v (None, Some i, ub) m, ac, i + 1
         else
           let c = Le ([i, [0, 0, 1.]], ub -. lb) in
           IntMap.add v (Some i, None, lb) m, c :: ac, i + 1)
        bounds (IntMap.empty, [], max_idx + 1) in
    trans, add_cstrs in
  let translate vect mat =
    List.fold_left
      (fun (mat, offset) (v, c) ->
       let vp, vn, vb = IntMap.find v trans in
       let mat = match vn with
         | None -> mat
         | Some vn -> (vn, [0, 0, -.c]) :: mat in
       let mat = match vp with
         | None -> mat
         | Some vp -> (vp, [0, 0, c]) :: mat in
       mat, offset +. c *. vb)
      (mat, 0.) (List.rev vect) in
  let obj, offset = translate (fst obj) (snd obj) in
  let constraints =
    List.fold_left
      (fun cstrs (vect, mat, lb, ub) ->
       let mat, off = translate vect mat in
       if lb = ub then Eq (mat, ub -. off) :: cstrs
       else
         let cstrs =
           if ub = infinity then cstrs
           else Le (mat, ub -. off) :: cstrs in
         if lb = neg_infinity then cstrs
         else Ge (mat, lb -. off) :: cstrs)
      [] (List.rev constraints) in
  obj, offset, constraints @ add_cstrs, (trans, max_idx)

let of_simple_res (trans, max_idx) (res_X, res_y, res_Z) =
  let res_X, vars = List.partition (fun (i, _) -> i <= max_idx) res_X in
  let vars =
    List.fold_left
      (fun m (i, a) -> IntMap.add i a.(0).(0) m)
      IntMap.empty vars in
  let res_x =
    IntMap.fold
      (fun v (vp, vn, vb) l ->
       let vb = match vp with
         | None -> vb
         | Some vp -> vb +. IntMap.find vp vars in
       let vb = match vn with
         | None -> vb
         | Some vn -> vb -. IntMap.find vn vars in
       (v, vb) :: l)
      trans [] in
  let res_Z = List.filter (fun (i, _) -> i <= max_idx) res_Z in
  List.rev res_x, res_X, res_y, res_Z
                                          
let check_prog_ext f obj constraints =
  let check_block f = List.iter (fun (_, m) -> f m) in
  check_block f (snd obj);
  List.iter (fun (_, m, _, _) -> check_block f m) constraints

let solve_ext_sparse ?options ?solver obj constraints bounds =
  check_prog_ext check_sparse obj constraints;
  match get_solver options solver with
  | Csdp | Sdpa | SdpaGmp | SdpaDd ->
     let obj, offset, constraints, trans = to_simple obj constraints bounds in
     let ret, (pobj, dobj), res =
       solve_sparse ?options ?solver obj constraints in
     ret, (pobj +. offset, dobj +. offset), of_simple_res trans res
  | Mosek ->
     let options = match options with Some o -> o | None -> default in
     let options = { Moseksdp.verbose = options.verbose } in
     let ret, res, (res_x, res_X, res_y, res_Z) =
       Moseksdp.solve_ext ~options obj constraints bounds in
     ret, res, (res_x, res_X, res_y, res_Z)

let solve_ext ?options ?solver obj constraints bounds =
  check_prog_ext check_sym obj constraints;
  let obj = fst obj, block_diag_to_sparse (snd obj) in
  let constraints =
    List.map
      (fun (v, m, lb, ub) -> v, block_diag_to_sparse m, lb, ub)
      constraints in
  solve_ext_sparse ?solver obj constraints bounds

(***********************)
(* Printing functions. *)
(***********************)
  
let pp_sparse_matrix fmt m =
  let pp_e fmt (i, j, f) = Format.fprintf fmt "(%d, %d, %g)" i j f in
  Format.fprintf fmt "[@[%a@]]" (Utils.pp_list ~sep:",@ " pp_e) m

let pp_matrix fmt m = Matrix.Float.pp fmt (Matrix.Float.of_array_array m)

let pp_block_diag f fmt bd =
  let pp_e fmt (i, m) = Format.fprintf fmt "(%d, %a)" i f m in
  Format.fprintf fmt "[@[%a@]]" (Utils.pp_list ~sep:",@ " pp_e) bd

let pp_obj f fmt bd =
  let pp_e fmt (i, m) = Format.fprintf fmt "tr(%a X_%d)" f m i in
  match bd with
  | [] -> Format.fprintf fmt "0"
  | _ -> Format.fprintf fmt "@[%a@]" (Utils.pp_list ~sep:"@ + " pp_e) bd

let pp_constr f fmt c =
  let m, b = match c with Eq (m, b) | Le (m, b) | Ge (m, b) -> m, b in
  let cmp = match c with Eq _ -> "=" | Le _ -> "<=" | Ge _ -> ">=" in
  Format.fprintf fmt "%a %s %g" (pp_obj f) m cmp b

let pp f fmt (obj, cstrs) =
  Format.fprintf
    fmt "@[<v>maximize   %a@ subject to @[<v>%a@],@            X psd@]"
    (pp_obj f) obj (Utils.pp_list ~sep:",@ " (pp_constr f)) cstrs

let pp_sparse = pp pp_sparse_matrix

let pp = pp pp_matrix

let pp_vector fmt v =
  let pp_e fmt (i, f) = Format.fprintf fmt "(%d, %g)" i f in
  Format.fprintf fmt "[@[%a@]]" (Utils.pp_list ~sep:",@ " pp_e) v

let pp_obj_ext f fmt (v, m) =
  let pp_e_v fmt (i, f) = Format.fprintf fmt "%g x_%d" f i in
  let pp_e_m fmt (i, m) = Format.fprintf fmt "tr(%a X_%d)" f m i in
  match v, m with
  | [], [] -> Format.printf "0"
  | [], _ ->
     Format.fprintf fmt "@[%a@]" (Utils.pp_list ~sep:"@ + " pp_e_m) m
  | _, [] ->
     Format.fprintf fmt "@[%a@]" (Utils.pp_list ~sep:"@ + " pp_e_v) v
  | _ ->
     Format.fprintf
       fmt "@[%a@ + %a@]"
       (Utils.pp_list ~sep:"@ + " pp_e_v) v
       (Utils.pp_list ~sep:"@ + " pp_e_m) m

let pp_constr_ext f fmt (v, m, lb, ub) =
  if lb = ub then
    Format.fprintf fmt "%a = %g" (pp_obj_ext f) (v, m) lb
  else if lb = neg_infinity then
    Format.fprintf fmt "%a <= %g" (pp_obj_ext f) (v, m) ub
  else if ub = infinity then
    Format.fprintf fmt "%a >= %g" (pp_obj_ext f) (v, m) lb
  else
    Format.fprintf fmt "%g <= %a <= %g" lb (pp_obj_ext f) (v, m) ub

let pp_bounds fmt v =
  let pp_e fmt (i, lb, ub) =
    if lb = ub then Format.fprintf fmt "x_%d = %g" i lb
    else if lb = neg_infinity then Format.fprintf fmt "x_%d <= %g" i ub
    else if ub = infinity then Format.fprintf fmt "x_%d >= %g" i lb
    else Format.fprintf fmt "%g <= x_%d <= %g" lb i ub in
  Format.fprintf fmt "@[%a@]" (Utils.pp_list ~sep:",@ " pp_e) v

let pp_ext f fmt (obj, cstrs, bounds) =
  match bounds with
  | [] ->
     Format.fprintf
       fmt "@[<v>maximize   %a@ subject to @[<v>%a@],@            X psd@]"
       (pp_obj_ext f) obj
       (Utils.pp_list ~sep:",@ " (pp_constr_ext f)) cstrs
  | _ ->
     Format.fprintf
       fmt "@[<v>maximize   %a@ \
            subject to @[<v>%a@],@            %a,@            X psd@]"
       (pp_obj_ext f) obj
       (Utils.pp_list ~sep:",@ " (pp_constr_ext f)) cstrs
       pp_bounds bounds

let pp_ext_sparse = pp_ext pp_sparse_matrix

let pp_ext = pp_ext pp_matrix

let pp_ext_sparse_sedumi fmt (obj, cstrs, bounds) =
  let () = if bounds <> [] then assert false  (* not implemented *) in
  let v, m =
    let vm = obj :: List.map (fun (v, m, _, _) -> v, m) cstrs in
    List.split vm in
  let module S = Set.Make (struct type t = int let compare = compare end) in
  let module M = Map.Make (struct type t = int let compare = compare end) in
  let size_v, before_v =
    List.flatten v
    |> List.fold_left (fun sv (i, _) -> S.add i sv) S.empty
    |> S.elements |> List.sort compare
    |> List.fold_left (fun (s, b) i -> s + 1, M.add i s b) (0, M.empty) in
  let blocks_m =
    List.fold_left
      (fun mm (i, m) ->
       let sz =
         List.fold_left (fun d (i, j, _) -> max d (max i j + 1)) 0 m in
       let szi = try M.find i mm with Not_found -> 0 in
       M.add i (max sz szi) mm)
      M.empty (List.flatten m) in
  let size_m, before_m =
    M.bindings blocks_m |> List.sort (fun (i, _) (j, _) -> compare i j)
    |> List.fold_left
         (fun (s, b) (i, si) -> s + si * si, M.add i s b)
         (0, M.empty) in
  let tr_v_m v m =
    let tr_v v = List.map (fun (i, f) -> M.find i before_v + 1, f) v in
    let tr_m m =
      let tr_block (sz, m) =
        let m =
          List.fold_left
            (fun m (i, j, f) ->
             if i = j then (i, j, f) :: m
             else (i, j, f) :: (j, i, f) :: m)
            [] m in
        List.map (fun (i, j, f) -> i * sz + j + 1, f) m in
      List.map
        (fun (i, m) ->
         let sz = M.find i blocks_m in
         let m = tr_block (sz, m) in
         let offset = M.find i before_m in
         List.map (fun (i, f) -> i + offset, f) m)
        m
      |> List.flatten in
    List.fold_left (fun l (i, f) -> (i + size_v, f) :: l) (tr_v v) (tr_m m) in
  let mA =
    List.mapi
      (fun i (v, m, _, _) -> List.map (fun (j, f) -> i + 1, j, f) (tr_v_m v m))
      cstrs
    |> List.flatten in
  let mc = List.map (fun (j, f) -> j, 1, f) (tr_v_m (fst obj) (snd obj)) in
  let mb =
    List.mapi
      (fun i (_, _, b, b') ->
       let () = if b' <> b' then assert false  (* not implemented *) in
       i + 1, 1, b)
      cstrs in
  let pp_sparse fmt m =
    let vi = List.map (fun (i, _, _) -> i) m in
    let vj = List.map (fun (_, j, _) -> j) m in
    let vv = List.map (fun (_, _, v) -> v) m in
    Format.fprintf
      fmt "@[<v>i = [@[%a@]];@ \
           j = [@[%a@]];@ \
           v = [@[%a@]];@]"
      (Utils.pp_list ~sep:",@ " Format.pp_print_int) vi
      (Utils.pp_list ~sep:",@ " Format.pp_print_int) vj
      (Utils.pp_list ~sep:",@ " (fun fmt -> Format.fprintf fmt "%.17e")) vv in
  let old_fmt = Format.pp_get_formatter_out_functions fmt () in
  let () =
    let out_newline () = old_fmt.Format.out_string "...\n" 0 4 in
    Format.pp_set_formatter_out_functions
      fmt { old_fmt with Format.out_newline = out_newline } in
  let () =
    let nb_cstrs = List.length cstrs in
    Format.fprintf
      fmt "@[<v>%a@ \
           A = sparse(i, j, v, %d, %d);@ \
           %a@ \
           c = sparse(i, j, v, %d, 1);@ \
           %a@ \
           b = sparse(i, j, v, %d, 1);@ \
           K = struct('f', %d, 's', [@[%a@]]);@]"
      pp_sparse mA nb_cstrs (size_v + size_m)
      pp_sparse mc (size_v + size_m)
      pp_sparse mb nb_cstrs
      size_v
      (Utils.pp_list ~sep:";@ " Format.pp_print_int)
      (List.map snd (M.bindings blocks_m)) in
  Format.pp_set_formatter_out_functions fmt old_fmt

let pp_ext_sedumi fmt (obj, cstrs, bounds) =
  let obj = fst obj, block_diag_to_sparse (snd obj) in
  let cstrs =
    List.map
      (fun (v, m, lb, ub) -> v, block_diag_to_sparse m, lb, ub)
      cstrs in
  pp_ext_sparse_sedumi fmt (obj, cstrs, bounds)

(****************************)
(* Miscellaneous functions. *)
(****************************)
  
let pfeas_stop_crit ?options ?solver bl =
  match get_solver options solver with
  | Csdp ->
     0.00000001 *. (1. +. sqrt (List.fold_left (fun x y -> x +. y *. y) 0. bl))
  | Mosek ->
     0.00000001 *. (1. +. List.fold_left (fun x y -> max x (abs_float y)) 0. bl)
  | Sdpa
  | SdpaGmp
  | SdpaDd ->
     match options with
     | None -> default.stop_criterion
     | Some opt -> opt.stop_criterion
