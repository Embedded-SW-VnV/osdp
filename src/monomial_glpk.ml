let pp_names names fmt m =
  let rec name_vars i names = function
    | [] -> []
    | h :: t ->
       let n, names =
         match names with [] -> "x" ^ string_of_int i, [] | n :: t -> n, t in
       (n, h) :: name_vars (i + 1) names t in
  let l = name_vars 0 names m in
  let l = List.filter (fun (_, e) -> e <> 0) l in
  match l with
  | [] -> Format.fprintf fmt "1"
  | _ :: _ ->
     Format.fprintf
       fmt
       "@[%a@]"
       (Utils.pp_list ~sep:"@ " (fun fmt (n, e) ->
                                 if e = 1 then
                                   Format.fprintf fmt "%s" n
                                 else
                                   Format.fprintf fmt "%s^%d" n e))
       l

let pp = pp_names []

module Var = struct
  type t = string
  let print = Format.pp_print_string
  let compare = String.compare
  let is_int _ = false
end

module Float = struct
  type t = float
  let add = ( +. )
  let mult = ( *. )
  let compare = compare
  let equal = ( = )
  let zero = 0.
  let one = 1.
  let m_one = -1.
  let is_zero n = n = zero
  let to_string = string_of_float
  let print fmt t = Format.fprintf fmt "%g" t
  let is_int _ = false
  let div = ( /. )
  let sub = ( -. )
  let is_one v = v = 1.
  let is_m_one v = v = -1.
  let sign x = if x > 0. then 1 else if x < 0. then -1 else 0
  let min = min
  let abs = abs_float
  let minus = ( ~-. )
end

module Ex = struct
  type t = unit
  let empty = ()
  let union _ _ = ()
  let print _ _ = ()
end

module Sim = OcplibSimplex.Basic.Make (Var) (Float) (Ex)

let filter_newton_polytope_glpk s p =
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
      Format.printf
        "@[<2>zcoeffs:@ @[%a@]@]@." (Utils.pp_array ~sep:",@ " Format.pp_print_float) zcoeffs;
      let cstrs = Array.of_list (List.map center p) in
      Format.printf
        "@[<2>cstrs:@ @[%a@]@]@." (Utils.pp_array ~sep:";@ " (Utils.pp_array ~sep:",@ " Format.pp_print_float)) cstrs;
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
  Format.printf
    "@[<2>glpk: %d monomials after filtering:@ @[%a@]@]@."
    (List.length s) (Utils.pp_list ~sep:",@ " pp) s;
  s

let filter_newton_polytope_ocplib_simplex s p =
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
  Format.printf
    "@[<2>p:@ @[%a@]@]@." (Utils.pp_list ~sep:",@ " pp) p;
  Format.printf
    "@[<2>keep:@ @[%a@]@]@." (Utils.pp_list ~sep:",@ " pp) skeep;
  Format.printf
    "@[<2>to filter:@ @[%a@]@]@." (Utils.pp_list ~sep:",@ " pp) sfilter;
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
      let p_of_list l =
        l |> List.mapi (fun n c -> "x" ^ string_of_int n, c)
        |> Sim.Core.P.from_list in
      let nb = ref 0 in
      let add_cstr sim c =
        let () = Format.printf "%a <= 1@." Sim.Core.P.print c in
        (* let () = Format.printf "c is_polynomial: %B@." (Sim.Core.P.is_polynomial c) in *)
        match Sim.Core.P.bindings c with
        | [] -> sim
        | [v, c] ->
           (* Format.printf "c = %g@." c; *)
           if c > 0. then Sim.Assert.var sim v None () (Some (1. /. c, 0.)) ()
           else Sim.Assert.var sim v (Some (1. /. c, 0.)) () None ()
        | _ ->
           let s = "s" ^ string_of_int !nb in
           incr nb;
           (* let () = Format.printf "avant@." in *)
           let sim = Sim.Assert.poly sim c s None () (Some (1., 0.)) () in
           (* let () = Format.printf "apres@." in *)
           sim in
      let center' x = Array.to_list (center x) in
      let z = si |> List.map (( * ) 2) |> center' |> p_of_list in
      let cstrs = p |> List.map center' |> List.map p_of_list in
      let sim = Sim.Core.empty ~is_int:false ~check_invs:true ~debug:0 in
      let sim = List.fold_left add_cstr sim cstrs in
      let sim, opt = Sim.Solve.maximize sim z in
      match Sim.Result.get opt sim with
      | (Sim.Core.Unknown | Sim.Core.Sat _ | Sim.Core.Unsat _) -> None
      | Sim.Core.Unbounded sol ->
         let a = Array.make n 0. in
         let { Sim.Core.main_vars; _ } = Lazy.force sol in
         List.iter
           (fun (v, c) ->
            let i = int_of_string (String.sub v 1 (String.length v - 1)) in
            a.(i) <- c)
           main_vars;
         Some a
      | Sim.Core.Max (mx, sol) ->
         let { Sim.Core.max_v; is_le; _ } = Lazy.force mx in
         if max_v < 1.0001 then None else
           let a = Array.make n 0. in
           let { Sim.Core.main_vars; _ } = Lazy.force sol in
           List.iter
             (fun (v, c) ->
              let i = int_of_string (String.sub v 1 (String.length v - 1)) in
              a.(i) <- c)
             main_vars;
           Some a in
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
  Format.printf
    "@[<2>ocplib_simplex: %d monomials after filtering:@ @[%a@]@]@."
    (List.length s) (Utils.pp_list ~sep:",@ " pp) s;
  s

module SM = Set.Make (struct
  type t = int list
  let degree = List.fold_left ( + ) 0
  let compare m1 m2 =
    let rec compare m1 m2 = match m1, m2 with
      | [], [] -> 0
      | [], _ -> compare [0] m2
      | _, [] -> compare m1 [0]
      | h1 :: t1, h2 :: t2 ->
         let c = compare t1 t2 in
         if c <> 0 then c else Pervasives.compare h1 h2 in
    let c = Pervasives.compare (degree m1) (degree m2) in
    if c <> 0 then c else (* c = 0 *) compare m1 m2
end)

let filter_newton_polytope s p =
  let r1 = filter_newton_polytope_glpk s p in
  let r2 = filter_newton_polytope_ocplib_simplex s p in
  let s1 = List.fold_right SM.add r1 SM.empty in
  let s2 = List.fold_right SM.add r2 SM.empty in
  Format.printf "glpk = ocplib_simplex: %B@." (SM.equal s1 s2);
  r1
