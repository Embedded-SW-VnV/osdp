(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2017  P. Roux
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
                         * (vector * Sdp.matrix Sdp.block_diag)
  module ScalarLinExpr : LinExpr.S with module Coeff = Scalar
  type 'a details_val = DV of 'a | DVexpr of ScalarLinExpr.t
  type details =
    (int * Ident.t) list * Ident.t array array Sdp.block_diag
    * float details_val Ident.Map.t
  val solve_ext_sparse_details :
    ?options:Sdp.options -> ?solver:Sdp.solver ->
    Sdp.sparse_matrix obj_ext ->
    Sdp.sparse_matrix constr_ext list -> Sdp.bounds ->
    SdpRet.t * (float * float)
    * (vector * Sdp.matrix Sdp.block_diag)
    * details
  val pp_obj_ext : (Format.formatter -> 'a -> unit) ->
                   Format.formatter -> 'a obj_ext -> unit
  val pp_constr_ext : (Format.formatter -> 'a -> unit) ->
                      Format.formatter -> 'a constr_ext -> unit
  val pp_ext_sparse : Format.formatter ->
                      (Sdp.sparse_matrix obj_ext *
                       Sdp.sparse_matrix constr_ext list * Sdp.bounds) -> unit
end

module Make (S : Scalar.S) : S with module Scalar = S = struct
  module Scalar = S

  type vector = (int * Scalar.t) list

  type 'a obj_ext = vector * 'a Sdp.block_diag

  type 'a constr_ext = vector * 'a Sdp.block_diag * Scalar.t * Scalar.t

  module ScalarLinExpr = LinExpr.Make (Scalar)

  type 'a details_val = DV of 'a | DVexpr of ScalarLinExpr.t

  type details =
    (int * Ident.t) list * Ident.t array array Sdp.block_diag
    * float details_val Ident.Map.t

  module IMap = Map.Make (struct type t = int let compare = compare end)
  module IIIMap =
    Map.Make (struct type t = int * int * int let compare = compare end)
                                    
  let solve_ext_sparse_details ?options ?solver obj constraints bounds =
    if bounds <> [] then assert false;  (* not implemented *)
    let constraints =
      List.map
        (fun (v, m, lb, ub) ->
           if not (Scalar.equal lb ub) then assert false;  (* not implemented *)
           v, m, lb)
        constraints in

    (* associate an Ident.t to each primal vector variable x *)
    let vids =
      List.fold_left
        (fun m (i, _) ->
           if IMap.mem i m then m
           else IMap.add i (Ident.create (Format.sprintf "_v%d_" i)) m)
        IMap.empty
        (List.flatten (fst obj :: List.map (fun (x, _, _) -> x) constraints)) in
    (* associate an Ident.t to each primal matrix variable X *)
    let mids =
      List.fold_left
        (fun m (bi, b) ->
           List.fold_left
             (fun m (i, j, _) ->
                if IIIMap.mem (bi, i, j) m then m else
                  let s = Format.sprintf "_m%d_%d_%d_" bi i j in
                  IIIMap.add (bi, i, j) (Ident.create s) m)
             m b)
        IIIMap.empty
        (List.flatten (snd obj :: List.map (fun (_, x, _) -> x) constraints)) in

    (* translate all constraints with the new identifiers *)
    let module LE = ScalarLinExpr in
    let l_of_vec v =
      List.fold_left (fun l (i, c) -> (IMap.find i vids, c) :: l) [] v in
    let l_of_mat m =
      List.fold_left
        (fun l (bi, b) ->
           List.fold_left
             (fun l (i, j, c) ->
                let c = if i = j then S.of_float c else S.of_float (2. *. c) in
                (IIIMap.find (bi, i, j) mids, c) :: l)
             l b)
        [] m in
    let eqs =
      List.map
        (fun (v, m, b) -> LE.of_list (l_of_vec v @ l_of_mat m) S.(~- b))
        constraints in

    (* eliminate variables *)
    let assigs =
      let rec eliminate assigs eqs =
        match eqs with
        | [] -> assigs
        | eq :: eqs ->
          match LE.choose eq with
          | None ->
            begin match LE.is_const eq with
              | None -> assert false  (* LE.choose returned None *)
              | Some c ->
                if S.sign c <> 0 then raise Exit else eliminate assigs eqs end
          | Some (id, c) ->
            let eq = LE.mult_scalar S.(neg (inv c)) (LE.remove eq id) in
            let repl l = LE.replace l [id, eq] in
            let assigs = Ident.Map.map repl assigs in
            let eqs = List.map repl eqs in
            eliminate (Ident.Map.add id eq assigs) eqs in
      try Some (eliminate Ident.Map.empty eqs) with Exit -> None in

    (* begin match assigs with *)
    (* | None -> () *)
    (* | Some assigs -> *)
    (*    Format.printf "assigs =@."; *)
    (*    Ident.Map.iter *)
    (*      (fun id le -> Format.printf "  %a -> %a@." Ident.pp id LE.pp le) *)
    (*      assigs *)
    (* end; *)

    (* check that no diagonal variable is assigned a negative value *)
    let assigs = match assigs with
      | None -> None
      | Some assigs ->
        let check (_, i, j) id =
          i <> j ||
            try
              let e = Ident.Map.find id assigs in
              match LE.is_const e with None -> true | Some c -> S.sign c >= 0
            with Not_found -> true in
        if IIIMap.for_all check mids then Some assigs else None in

    match assigs with
    | None ->
       (* Format.printf "Primal infeasibility detected in dualize.@."; *)
       SdpRet.PrimalInfeasible, (0., 0.), ([], []), ([], [], Ident.Map.empty)
    | Some assigs ->

      (* build matrices: express that primal matrix variable X must be PSD *)
      let mAi, mC =  (* TODO : risque t-on d'oublier des morceaux de
                        matrice en ne considérant que les indices de mids *)
        IIIMap.fold
          (fun bij id (mAi, mC) ->
             let e = try Ident.Map.find id assigs with Not_found -> LE.var id in
             let l, c = LE.to_list e in
             let mAi =
               List.fold_left
                 (fun mAi (i, c) ->
                    let mat =
                      try Ident.Map.find i mAi with Not_found -> IIIMap.empty in
                    Ident.Map.add i (IIIMap.add bij c mat) mAi)
                 mAi l in
             let mC = if S.sign c = 0 then mC else IIIMap.add bij S.(~- c) mC in
             mAi, mC)
          mids (Ident.Map.empty, IIIMap.empty) in
      
      let obj, obj_c =
        let le = LE.of_list (l_of_vec (fst obj) @ l_of_mat (snd obj)) S.zero in
        let le = LE.mult_scalar S.(~- one) le in
        let le = LE.replace le (Ident.Map.bindings assigs) in
        let l, c = LE.to_list le in
        List.fold_left
          (fun m (id, c) -> Ident.Map.add id c m)
          Ident.Map.empty l,
        S.to_float c in

      (* build dual problem *)
      let dual_vars =
        Ident.Set.empty |> Ident.Map.fold (fun id _ -> Ident.Set.add id) mAi
        |> Ident.Map.fold (fun id _ -> Ident.Set.add id) obj in

      (* Format.printf "dual_vars:"; *)
      (* Ident.Set.iter (Format.printf " %a" Ident.pp) dual_vars; *)
      (* Format.printf "@."; *)
      (* Format.printf "mAi:@."; *)
      (* Ident.Set.iter *)
      (*   (fun id -> *)
      (*     Format.printf "  %a:" Ident.pp id; *)
      (*     let mx = Ident.Map.find id mAi in *)
      (*     IIIMap.iter *)
      (*       (fun (bi, i, j) c -> *)
      (*         Format.printf " (%d, %d, %d)=%a" bi i j S.pp c) *)
      (*       mx; *)
      (*     Format.printf "@.") *)
      (*   dual_vars; *)
      (* Format.printf "mC: "; *)
      (* IIIMap.iter *)
      (*   (fun (bi, i, j) c -> *)
      (*     Format.printf " (%d, %d, %d)=%a" bi i j S.pp c) *)
      (*   mC; *)
      (* Format.printf "@."; *)
      
      let tr_mx m =
        IIIMap.fold
          (fun (bi, i, j) c m ->
             let b = try IMap.find bi m with Not_found -> [] in
             IMap.add bi ((i, j, S.to_float c) :: b) m)
          m IMap.empty
        |> IMap.bindings in
      
      (* matrices A_i and vector b for the dual problem *)
      let dual_constraints =
        Ident.Set.fold
          (fun id l ->
             let mA = try tr_mx (Ident.Map.find id mAi) with Not_found -> [] in
             let cb = try Ident.Map.find id obj with Not_found -> S.zero in
             Sdp.Eq (mA, S.to_float cb) :: l)
          dual_vars [] in

      (* matrix C for the dual problem *)
      let dual_obj = tr_mx mC in

      (* solve the dual problem *)
      let ret, (pobj, dobj), (_, y, _) =
        Sdp.solve_sparse ?options ?solver dual_obj dual_constraints in

      let ret = match ret with
        | SdpRet.Success | SdpRet.PartialSuccess | SdpRet.Unknown -> ret
        | SdpRet.PrimalInfeasible -> SdpRet.DualInfeasible
        | SdpRet.DualInfeasible -> SdpRet.PrimalInfeasible
        | SdpRet.NearPrimalInfeasible -> SdpRet.NearDualInfeasible
        | SdpRet.NearDualInfeasible -> SdpRet.NearPrimalInfeasible in

      let obj = -. (dobj +. obj_c), -. (pobj +. obj_c) in

      if Array.length y <> Ident.Set.cardinal dual_vars then
        ret, obj, ([], []), ([], [], Ident.Map.empty)
      else
        (* read dual solution *)
        let dual_sol =
          List.fold_left2
            (fun m id f -> Ident.Map.add id f m)
            Ident.Map.empty
            (Ident.Set.elements dual_vars)
            (List.rev (Array.to_list y)) in

        let details_values =
          dual_sol |> Ident.Map.map (fun f -> DV f)
          |> Ident.Map.fold
            (fun id le dv -> Ident.Map.add id (DVexpr le) dv)
            assigs
          |> IMap.fold
            (fun _ id dv ->
               if Ident.Map.mem id dv then dv else Ident.Map.add id (DV 0.) dv)
            vids in
        
        let blocksizes =
          IIIMap.fold
            (fun (bi, i, j) _ blocksizes ->
               let sz = try IMap.find bi blocksizes with Not_found -> 0 in
               IMap.add bi (max sz (max i j + 1)) blocksizes)
            mids IMap.empty in
        
        (* build details to return a trace of the elimination of variables *)
        let details =
          let bids =
            let dummy = Ident.create "_dummy_" in
            IMap.map (fun sz -> Array.make_matrix sz sz dummy) blocksizes in
          IIIMap.iter
            (fun (bi, i, j) id -> let b = IMap.find bi bids in b.(i).(j) <- id)
            mids;
          IMap.bindings vids, IMap.bindings bids, details_values in

        (* rebuild primal solution from details *)
        let getf id = match Ident.Map.find id details_values with
          | DV f -> f | DVexpr _ -> raise Not_found in
        let v =
          let get id =
            try
              match Ident.Map.find id details_values with
              | DV f -> S.of_float f
              | DVexpr le ->
                let l, c = LE.to_list le in
                List.fold_left
                  (fun r (id, c) -> S.(r + c * of_float (getf id)))
                  c l
            with Not_found -> S.zero in
          List.map (fun (i, id) -> i, get id) ((fun (x, _, _) -> x) details) in
        let m =
          let get b =
            let getc id =
              try
                match Ident.Map.find id details_values with
                | DV f -> f
                | DVexpr le ->
                  let l, c = LE.to_list le in
                  List.fold_left
                    (fun r (id, c) -> r +. S.to_float c *. getf id)
                    (S.to_float c) l
              with Not_found -> 0. in
            let sz = Array.length b in
            let b' = Array.make_matrix sz sz 0. in
            for i = 0 to sz - 1 do
              for j = 0 to i do
                let c = getc b.(i).(j) in
                b'.(i).(j) <- c;
                b'.(j).(i) <- c;
              done
            done;
            b' in
          List.map (fun (i, b) -> i, get b) ((fun (_, x, _) -> x) details) in
        
        ret, obj, (v, m), details
    
  let solve_ext_sparse ?options ?solver obj constraints bounds =
    let ret, obj, primsol, _ =
      solve_ext_sparse_details ?options ?solver obj constraints bounds in
    ret, obj, primsol

  (***********************)
  (* Printing functions. *)
  (***********************)
  
  let pp_obj_ext f fmt (v, m) =
    let pp_e_v fmt (i, s) = Format.fprintf fmt "%a x_%d" Scalar.pp s i in
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
    if Scalar.equal lb ub then
      Format.fprintf fmt "%a = %a" (pp_obj_ext f) (v, m) Scalar.pp lb
    else if Scalar.to_float lb = neg_infinity then
      Format.fprintf fmt "%a <= %a" (pp_obj_ext f) (v, m) Scalar.pp ub
    else if Scalar.to_float ub = infinity then
      Format.fprintf fmt "%a >= %a" (pp_obj_ext f) (v, m) Scalar.pp lb
    else
      Format.fprintf
        fmt "%a <= %a <= %a" Scalar.pp lb (pp_obj_ext f) (v, m) Scalar.pp ub

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
         Sdp.pp_bounds bounds

  let pp_ext_sparse = pp_ext Sdp.pp_sparse_matrix
end

module Q = Make (Scalar.Q)

module Float = Make (Scalar.Float)
