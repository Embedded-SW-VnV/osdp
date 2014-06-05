type matrix = float array array
type block_diag_matrix = (int * matrix) list

let string_of_matrix m =
  "[" ^ String.concat "; "
    (List.map
       (fun line -> String.concat ", "
         (List.map string_of_float (Array.to_list line)))
       (Array.to_list m))
  ^ "]"

let string_of_block_matrix bm =
  "[" ^ String.concat "; " (List.map (fun (i, bm) -> "(" ^ string_of_int i
    ^ ", " ^ string_of_matrix bm ^ ")") bm) ^ "]"

module IntMap = Map.Make (struct type t = int let compare = compare end)

let build_block_matrices imll =
  let zero n = Array.make_matrix n n 0. in
  let resize n m =
    if Array.length m = n then m
    else begin
      let a = zero n in
      for i = 0 to Array.length m - 1 do
        for j = 0 to Array.length m - 1 do
          a.(i).(j) <- m.(i).(j)
        done
      done;
      a
    end in
  let imll = List.map (List.sort (fun (i, _) (j, _) -> compare i j)) imll in
  let sizes = List.fold_left
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
      let bm, _ = IntMap.fold
        (fun i sz (bm, iml) -> match iml with
          | [] -> (zero sz :: bm), []
          | (i', _)::_ when i' <> i -> (zero sz :: bm), iml
          | (_, m)::iml -> (resize sz m :: bm), iml)
        sizes
        ([], iml) in
      List.rev bm)
    imll, sizes

let read_block_matrix sizes blockmatrix =
  let bm, _ = IntMap.fold
    (fun i _ (obm, ibm) -> match ibm with
      | [] -> assert false  (* should never happen *)
      | h :: t -> ((i, h) :: obm), t)
    sizes
    ([], blockmatrix) in
  List.rev bm

let solve obj constraints =
  (* print_string ("obj: " ^ string_of_block_matrix obj ^ "\n" *)
  (*               ^ "constraints: " ^ String.concat "\n" *)
  (*                 (List.map (fun (m, b) -> string_of_block_matrix m ^ ": " *)
  (*                   ^ string_of_float b) constraints)); *)
  (* print_newline (); *)
  let obj, constraints, sizes =
    let cstrs, bounds = List.split constraints in
    let obj, cstrs, sizes = match build_block_matrices (obj :: cstrs) with
      | [], _ ->
        assert false  (* Impossible: build_block_matrices preserves size
                       * of a list of size at least one (thanks to obj). *)
      | obj :: cstrs, sizes -> obj, cstrs, sizes in
    obj, List.combine cstrs bounds, sizes in
  let res, (res_X, res_y) = Csdp.solve obj constraints in
  let res_X = read_block_matrix sizes res_X in
  (* print_string ("res_X: " ^ string_of_block_matrix res_X); *)
  (* print_newline (); *)
  res, (res_X, res_y)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
