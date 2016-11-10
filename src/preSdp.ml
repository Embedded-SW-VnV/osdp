(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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

module type S = sig
  module Scalar : Scalar.S
  type vector = (int * Scalar.t) list
  type 'a obj_ext = vector * 'a Sdp.block_diag
  type 'a constr_ext = vector * 'a Sdp.block_diag * Scalar.t * Scalar.t
  val solve_ext_sparse : ?options:Sdp.options -> ?solver:Sdp.solver ->
                         Sdp.sparse_matrix obj_ext ->
                         Sdp.sparse_matrix constr_ext list -> Sdp.bounds ->
                         SdpRet.t * (float * float)
                         * (vector * Sdp.matrix Sdp.block_diag
                            * float array * Sdp.matrix Sdp.block_diag)
end

module Make (S : Scalar.S) : S with module Scalar = S = struct
  module Scalar = S

  type vector = (int * Scalar.t) list

  type 'a obj_ext = vector * 'a Sdp.block_diag

  type 'a constr_ext = vector * 'a Sdp.block_diag * Scalar.t * Scalar.t

  module IntMap = Map.Make (struct type t = int let compare = compare end)
                                    
  let solve_ext_sparse ?options ?solver obj constraints bounds =
    let obj = List.sort (fun (i, _) (j, _) -> compare i j) (fst obj), snd obj in
    let constraints =
      List.map
        (fun (v, m, bl, bu) ->
         List.sort (fun (i, _) (j, _) -> compare i j) v, m, bl, bu)
        constraints in

    let replace i (bi, vi) (v, m, bl, bu) =
      try
        let c, v = List.assoc i v, List.remove_assoc i v in
        let bi, vi = S.(c * bi), List.map (fun (j, c') -> j, S.(c * c')) vi in
        let bl, bu = S.(bl - bi, bu - bi) in
        let rec merge vi v = match vi, v with
          | [], _ -> v
          | _, [] -> vi
          | (hi, hci) :: vi', (h, hc) :: v' ->
             if hi < h then (hi, hci) :: merge vi' v
             else if hi > h then (h, hc) :: merge vi v'
             else (* hi = h *) (hi, S.(hci + hc)) :: merge vi' v' in
        merge vi v, m, bl, bu
      with Not_found -> v, m, bl, bu in
    let replace_repl i bvi (b, v) =
      let v, _, mbi, _ = replace i bvi (v, [], S.zero, S.zero) in
      S.(b - mbi), v in

    let rec find_repl = function
      | [] -> None
      | ((i, c) :: v, ([] | [_, []]), bl, bu) :: cstrs
           when S.(bu = bl) && S.(c <> zero)
                && List.for_all (fun (j, _, _) -> j <> i) bounds ->
         let v =
           let c = S.neg c in
           List.map (fun (j, c') -> j, S.(c' / c)) v in
         Some ((i, (S.(bl / c), v)), cstrs)
      | c :: cstrs ->
         match find_repl cstrs with
         | None -> None
         | Some (ibv, cstrs) -> Some (ibv, c :: cstrs) in
    let rec find_all_repl to_replace constraints =
      match find_repl constraints with
      | None -> to_replace, constraints
      | Some ((i, bv), constraints) ->
         let to_replace = IntMap.map (replace_repl i bv) to_replace in
         let constraints = List.map (replace i bv) constraints in
         find_all_repl (IntMap.add i bv to_replace) constraints in

    (* Format.printf "constraints:@."; *)
    (* List.iter *)
    (*   (fun (v, _, bl, bu) -> *)
    (*    Format.printf *)
    (*      "@[%a@], m, %a, %a@." *)
    (*      (Utils.pp_list ~sep:",@ " (fun fmt (i, c) -> Format.fprintf fmt "(%d, %a)" i S.pp c)) v *)
    (*      S.pp bl S.pp bu *)
    (*   ) *)
    (*   constraints; *)

    let to_replace, constraints = find_all_repl IntMap.empty constraints in
    (* IntMap.iter *)
    (*   (fun i (b, v) -> *)
    (*    Format.printf *)
    (*      "to_replace %d = %a, @[%a@]@." *)
    (*      i S.pp b *)
    (*      (Utils.pp_list ~sep:",@ " (fun fmt (i, c) -> Format.fprintf fmt "(%d, %a)" i S.pp c)) *)
    (*      v) *)
    (*   to_replace; *)

    let constraints =
      List.map
        (fun (v, m, bl, bu) ->
         let v = List.map (fun (i, s) -> i, S.to_float s) v in
         v, m, S.to_float bl, S.to_float bu)
        constraints in
    let offset, obj =
      IntMap.fold
        (fun i bvi (offset, (v, m)) ->
         let offset, v = replace_repl i bvi (offset, v) in (offset, (v, m)))
        to_replace (S.zero, obj) in
    let obj = List.map (fun (i, s) -> i, S.to_float s) (fst obj), snd obj in

    (* Format.printf "new constraints:@."; *)
    (* List.iter *)
    (*   (fun (v, _, bl, bu) -> *)
    (*    Format.printf *)
    (*      "@[%a@], m, %g, %g@." *)
    (*      (Utils.pp_list ~sep:",@ " (fun fmt (i, c) -> Format.fprintf fmt "(%d, %g)" i c)) v *)
    (*      bl bu *)
    (*   ) *)
    (*   constraints; *)

    let ret, obj, (res_x, res_X, res_y, res_Z) =
      Sdp.solve_ext_sparse ?options ?solver obj constraints bounds in

    let res_x =
      let res_x =
        List.fold_left
          (fun m (i, f) -> IntMap.add i (S.of_float f) m)
          IntMap.empty res_x in
      let res_x' =
        IntMap.map
          (fun (b, v) ->
           List.fold_left
             (fun b (i, c) -> S.(b + c * (IntMap.find i res_x)))
             b v)
          to_replace in
      IntMap.bindings
        (IntMap.merge
           (fun _ x x' ->
            match x, x' with
            | None, None | Some _, Some _ -> assert false
            | Some x, _ | _, Some x -> Some x)
           res_x res_x') in
    let obj =
      let offset = S.to_float offset in
      fst obj +. offset, snd obj +. offset in
    ret, obj, (res_x, res_X, res_y, res_Z)
end

module Q = Make (Scalar.Q)

module Float = Make (Scalar.Float)
