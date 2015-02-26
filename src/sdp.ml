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

type matrix = float array array

type sparse_matrix = (int * int * float) list

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
      l := (i, j, m.(i).(j)) :: !l
    done
  done;
  !l

let block_diag_of_sparse = List.map (fun (i, m) -> i, matrix_of_sparse m)

let block_diag_to_sparse = List.map (fun (i, m) -> i, matrix_to_sparse m)

type solver = Csdp | Mosek

(* define default solver *)
let get_solver = function None -> Csdp | Some s -> s

type 'a obj = 'a block_diag

type 'a constr =
  | Eq of 'a block_diag * float
  | Le of 'a block_diag * float
  | Ge of 'a block_diag * float

let add_slacks m1 mm1 obj constraints =
  let max_idx =
    let f = List.fold_left (fun m (i, _) -> max m i) in
    List.fold_left
      (fun m -> function Eq (bd, _) | Le (bd, _) | Ge (bd, _) -> f m bd)
      (f min_int obj) constraints in
  let constraints, _ =
    List.fold_left
      (fun (cstrs, i) ->
       function
       | Eq (bd, f) -> (bd, f) :: cstrs, i
       | Le (bd, f) -> ((i, m1) :: bd, f) :: cstrs, i + 1
       | Ge (bd, f) -> ((i, mm1) :: bd, f) :: cstrs, i + 1)
      ([], max_idx + 1) constraints in
  List.rev constraints

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

let solve ?solver obj constraints =
  check_prog check_sym obj constraints;
  let constraints = add_slacks [|[|1.|]|] [|[|-1.|]|] obj constraints in
  match get_solver solver with
  | Csdp -> Csdp.solve obj constraints
  | Mosek ->
     let obj = block_diag_to_sparse obj in
     let constraints =
       List.map (fun (c, b) -> block_diag_to_sparse c, b) constraints in
     Moseksdp.solve obj constraints
  
let solve_sparse ?solver obj constraints =
  check_prog check_sparse obj constraints;
  let constraints = add_slacks [0, 0, 1.] [0, 0, -1.] obj constraints in
  match get_solver solver with
  | Csdp ->
     let obj = block_diag_of_sparse obj in
     let constraints =
       List.map (fun (c, b) -> block_diag_of_sparse c, b) constraints in
     Csdp.solve obj constraints
  | Mosek ->
     Moseksdp.solve obj constraints
