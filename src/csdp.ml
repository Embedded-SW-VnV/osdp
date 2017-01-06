(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
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

type matrix = (int * int * float) list
type block_diag_matrix = (int * matrix) list

type options = {
  verbose : int;
}

let default = {
  verbose = 0;
}

external solve : int -> block_diag_matrix -> (block_diag_matrix * float) list ->
                 SdpRet.t
                 * (float * float) * (float array array list
                                      * float array * float array array list) =
  "csdp_solve"

module IntSet = Set.Make (struct type t = int let compare = compare end)
module IntMap = Map.Make (struct type t = int let compare = compare end)

(* The C stub above requires matrix blocks to be numbered sequentially
   (without holes) and returns a block diagonal matrix with (M-m+1)
   blocks where m and M are respectively the min and max diagonal
   block index appearing in the input (obj and constraints). This
   wrapper enforces that point. *)
let solve ?options obj constraints =
  let verbose = match options with None -> 0 | Some o -> o.verbose in
  let appear =
    let collect_mx = List.fold_left (fun s (i, _) -> IntSet.add i s) in
    List.fold_left
      (fun s (m, _) -> collect_mx s m)
      (collect_mx IntSet.empty obj) constraints in
  let trans, rev_trans, _ =
    IntSet.fold
      (fun i (trans, rev_trans, j) ->
        IntMap.add i j trans, IntMap.add j i rev_trans, j + 1)
      appear (IntMap.empty, IntMap.empty, 0) in
  let obj, constraints =
    let tr_mx = List.map (fun (i, b) -> IntMap.find i trans, b) in
    tr_mx obj, List.map (fun (m, f) -> tr_mx m, f) constraints in
  let ret, res, (res_X, res_y, res_Z) = solve verbose obj constraints in
  let res_X, res_Z =
    let tr_mx = List.mapi (fun i b -> IntMap.find i rev_trans, b) in
    tr_mx res_X, tr_mx res_Z in
  ret, res, (res_X, res_y, res_Z)
