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

type matrix = (int * int * float) list
type block_diag_matrix = (int * matrix) list

external solve : block_diag_matrix -> (block_diag_matrix * float) list ->
                 SdpRet.t
                 * (float * float) * (float array array list * float array) =
  "csdp_solve"

(* The C stub above returns a block diagonal matrix with (M-m+1)
   blocks where m and M are respectively the min and max diagonal
   block index appearing in the input (obj and constraints). We have
   to clean that to keep only indices actually appearing in the
   input. *)
let solve obj constraints =
  let ret, res, (res_X, res_y) = solve obj constraints in
  let res_X =
    let min_idx, max_idx =
      let range_idx_block_diag =
        List.fold_left (fun (mi, ma) (i, _) -> min mi i, max ma i) in
      let range = range_idx_block_diag (max_int, min_int) obj in
      List.fold_left
        (fun range (m, _) -> range_idx_block_diag range m)
        range constraints in
    if min_idx > max_idx then []
    else
      let appear =
        let a = Array.make (max_idx - min_idx + 1) false in
        let mark = List.iter (fun (i, _) -> a.(i - min_idx) <- true) in
        mark obj; List.iter (fun (m, _) -> mark m) constraints; a in
      List.mapi (fun i m -> i + min_idx, m) res_X
      |> List.filter (fun (i, _) -> appear.(i - min_idx)) in
  ret, res, (res_X, res_y)
