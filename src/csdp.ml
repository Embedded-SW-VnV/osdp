type matrix = float array array
type block_diag_matrix = (int * matrix) list

(* All block diagonal matrices in obj and constraints should have the
   same shape, i.e. same number of blocks and corresponding blocks of
   same size. The resulting block diagonal matrix X will also have
   this shape. *)
external solve : matrix list -> (matrix list * float) list ->
                 SdpRet.t * (float * float) * (matrix list * float array) =
  "csdp_solve"

module IntMap = Map.Make (struct type t = int let compare = compare end)

let solve obj constraints =

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
  let ret, res, (res_X, res_y) = solve obj constraints in
  let res_X = read_block_matrix sizes res_X in
  ret, res, (res_X, res_y)
