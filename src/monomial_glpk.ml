(* let pp_names names fmt m = *)
(*   let rec name_vars i names = function *)
(*     | [] -> [] *)
(*     | h :: t -> *)
(*        let n, names = *)
(*          match names with [] -> "x" ^ string_of_int i, [] | n :: t -> n, t in *)
(*        (n, h) :: name_vars (i + 1) names t in *)
(*   let l = name_vars 0 names m in *)
(*   let l = List.filter (fun (_, e) -> e <> 0) l in *)
(*   match l with *)
(*   | [] -> Format.fprintf fmt "1" *)
(*   | _ :: _ -> *)
(*      Format.fprintf *)
(*        fmt *)
(*        "@[%a@]" *)
(*        (Utils.pp_list ~sep:"@ " (fun fmt (n, e) -> *)
(*                                  if e = 1 then *)
(*                                    Format.fprintf fmt "%s" n *)
(*                                  else *)
(*                                    Format.fprintf fmt "%s^%i" n e)) *)
(*        l *)

(* let pp = pp_names [] *)

let filter_newton_polytope s p =
  (* keep monomials s_i of s such that 2 s_i is in p *)
  let skeep, sfilter =
    let rec inter x y = match x, y with
      | [], _ | _, [] -> [], x
      | hx :: tx, hy :: ty ->
         let c = compare (List.map (( * ) 2) hx) hy in
         if c = 0 then let k, f = inter tx ty in hx :: k, f
         else if c < 0 then let k, f = inter tx y in k, hx :: f
         else (* c > 0 *) let k, f = inter x ty in k, f in
    inter (List.sort compare s) (List.sort compare p) in
  (* Format.printf *)
  (*   "@[<2>keep:@ @[%a@]@]@." (Utils.pp_list ~sep:",@ " pp) skeep; *)
  (* Format.printf *)
  (*   "@[<2>to filter:@ @[%a@]@]@." (Utils.pp_list ~sep:",@ " pp) sfilter; *)
  (* look for separating hyperplane to rule out monomials not in the
     Newton polynomial *)
  let s =
    (* let prfa = Utils.pp_array ~sep:",@ " Format.pp_print_float in *)
    let n =
      let f n l = max n (List.length l) in
      List.fold_left f (List.fold_left f 0 s) p in
    let center =
      let rec sub x y = match x, y with
        | _, [] -> List.map float_of_int x
        | [], _ -> sub [0] y
        | hx :: tx, hy :: ty -> float_of_int (hx - hy) :: sub tx ty in
      let o = match p with [] -> [] | h :: _ -> h in
      fun x ->
      let a = Array.of_list (sub x o) in
      Array.append a (Array.make (n - Array.length a) 0.) in
    let find_separating_plane si =
      let zcoeffs = center (List.map (( * ) 2) si) in
      let cstrs = Array.of_list (List.map center p) in
      let pbounds = Array.make (Array.length cstrs) (neg_infinity, 1.) in
      let xbounds = Array.make n (-1000000., 1000000.) in
      (* Format.printf "@[<v2>si = %a@," pp si; *)
      (* Format.printf "obj = @[%a@]@," prfa zcoeffs; *)
      (* Format.printf *)
      (*   "cstrs = @[<v>%a@]@." *)
      (*   (Utils.pp_array *)
      (*      ~sep:"@," *)
      (*      (fun fmt a -> *)
      (*       Format.fprintf *)
      (*         fmt "@[%a@] <= 1" prfa a)) cstrs; *)
      let lp = Glpk.make_problem Glpk.Maximize zcoeffs cstrs pbounds xbounds in
      Glpk.set_message_level lp 1;
      Glpk.use_presolver lp true;
      Glpk.simplex lp;
      if Glpk.get_obj_val lp < 1.0001 then None
      else Some (Glpk.get_col_primals lp) in
    let rec filter s = function
      | [] -> s
      | si :: sfilter ->
         match find_separating_plane si with
         | None -> filter (si :: s) sfilter
         | Some a ->
            let test sj =
              let sj = center sj in
              let obj = ref 0. in
              for i = 0 to n - 1 do
                obj := !obj +. a.(i) *. sj.(i)
              done;
              !obj < 1.0001 in
            filter s (List.filter test sfilter) in
    filter skeep sfilter
    (* List.iter *)
    (*   (fun si -> *)
    (*    match find_separating_plane si with *)
    (*    | None -> Format.printf "%a: None@." pp si *)
    (*    | Some a -> Format.printf "%a: Some [|%a|]@." pp si prfa a) *)
    (*   sfilter; *)
    (* s *)
  in
  (* Format.printf *)
  (*   "@[<2>%d monomials after filtering:@ @[%a@]@]@." *)
  (*   (List.length s) (Utils.fprintf_list ~sep:",@ " pp) s; *)
  s
