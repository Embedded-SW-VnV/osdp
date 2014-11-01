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

type solver = Csdp | Mosek

type matrix = float array array

type sparse_matrix = (int * int * float) list

type 'a block_diag = (int * 'a) list

(* define default solver *)
let get_solver = function None -> Csdp | Some s -> s

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

module IntMap = Map.Make (struct type t = int let compare = compare end)

let solve_csdp obj constraints =

  let build_block_matrices imll =
    let zero n = Array.make_matrix n n 0. in
    let resize n m =
      if Array.length m = n then m
      else
        begin
          let a = zero n in
          for i = 0 to Array.length m - 1 do
            for j = 0 to Array.length m - 1 do
              a.(i).(j) <- m.(i).(j)
            done
          done;
          a
        end in
    let imll = List.map (List.sort (fun (i, _) (j, _) -> compare i j)) imll in
    let sizes =
      List.fold_left
        (List.fold_left
           (fun sizes (i, m) ->
            let sz = Array.length m in
            try
              if sz <= IntMap.find i sizes then sizes
              else IntMap.add i sz sizes
            with Not_found -> IntMap.add i sz sizes))
        IntMap.empty
        imll in
    List.map
      (fun iml ->
       let bm, _ =
         IntMap.fold
           (fun i sz (bm, iml) ->
            match iml with
            | [] -> (zero sz :: bm), []
            | (i', _)::_ when i' <> i -> (zero sz :: bm), iml
            | (_, m)::iml -> (resize sz m :: bm), iml)
           sizes
           ([], iml) in
       List.rev bm)
      imll, sizes in

  let read_block_matrix sizes blockmatrix =
    let bm, _ =
      IntMap.fold
        (fun i _ (obm, ibm) ->
         match ibm with
         | [] -> assert false  (* should never happen *)
         | h :: t -> ((i, h) :: obm), t)
        sizes
        ([], blockmatrix) in
    List.rev bm in

  let obj, constraints, sizes =
    let cstrs, bounds = List.split constraints in
    let obj, cstrs, sizes = match build_block_matrices (obj :: cstrs) with
      | [], _ ->
        assert false  (* Impossible: build_block_matrices preserves size
                       * of a list of size at least one (thanks to obj). *)
      | obj :: cstrs, sizes -> obj, cstrs, sizes in
    obj, List.combine cstrs bounds, sizes in
  let ret, res, (res_X, res_y) = Csdp.solve obj constraints in
  let res_X = read_block_matrix sizes res_X in
  ret, res, (res_X, res_y)

let solve_mosek obj constraints =
  let ret, res, (res_X, res_y) = Moseksdp.solve obj constraints in
  let min_idx_block_diag = List.fold_left (fun o (i, _) -> min o i) max_int in
  let offset =
    List.fold_left (fun o (m, _) -> min o (min_idx_block_diag m))
    (min_idx_block_diag obj) constraints in
  let res_X = List.mapi (fun i m -> i + offset, m) res_X in
  ret, res, (res_X, res_y)

let solve ?solver obj constraints =
  match get_solver solver with
  | Csdp -> solve_csdp obj constraints
  | Mosek ->
     let obj = block_diag_to_sparse obj in
     let constraints =
       List.map (fun (c, b) -> block_diag_to_sparse c, b) constraints in
     solve_mosek obj constraints
  
let solve_sparse ?solver obj constraints =
  match get_solver solver with
  | Csdp ->
     let obj = block_diag_of_sparse obj in
     let constraints =
       List.map (fun (c, b) -> block_diag_of_sparse c, b) constraints in
     solve_csdp obj constraints
  | Mosek ->
     solve_mosek obj constraints
