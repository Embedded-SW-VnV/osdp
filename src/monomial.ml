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

(* type invariant: empty list or the last element of the list is non zero *)
type t = int list

let rec of_list = function
  | [] -> []
  | h :: t ->
     match of_list t with
     | [] -> if h = 0 then [] else [h]
     | t -> h :: t

let to_list m = m

let rec compare m1 m2 = match m1, m2 with
  | [], [] -> 0
  | [], _ -> compare [0] m2
  | _, [] -> compare m1 [0]
  | h1 :: t1, h2 :: t2 ->
     if h1 > h2 then 1 else if h1 < h2 then -1 else compare t1 t2

let nb_vars = List.length

let degree = List.fold_left ( + ) 0

let rec mult m1 m2 = match m1, m2 with
  | [], _ -> m2
  | _, [] -> m1
  | h1 :: t1, h2 :: t2 -> (h1 + h2) :: mult t1 t2
  
let rec list_eq n d =
  List.map (fun m -> (List.fold_left ( - ) d m) :: m) (list_le (n - 1) d)

and list_le n d =
  if n <= 0 || d <= 0 then [[]] else list_le n (d - 1) @ list_eq n d

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
     Format.printf
       "@[%a@]"
       (Utils.fprintf_list ~sep:"@ " (fun fmt (n, e) ->
                                        if e = 1 then
                                          Format.fprintf fmt "%s" n
                                        else
                                          Format.fprintf fmt "%s^%i" n e))
       l

let pp = pp_names []

let filter_newton_polytope s p =
  Format.printf
    "@[<2>%d monomials before filtering:@ @[%a@]@]@."
    (List.length s) (Utils.fprintf_list ~sep:",@ " pp) s;
  Format.printf
    "@[<2>filtering by:@ @[%a@]@]@."
    (Utils.fprintf_list ~sep:",@ " pp) p;
  (* bounding box of the Newton polytope: min_i p_ji <= 2 s_j <= max_i p_ji *)
  let s =
    let rec pw_le x y = match x, y with
      | [], _ -> true
      | _, [] -> false
      | hx :: tx, hy :: ty -> hx <= hy && pw_le tx ty in
    let mi, ma = match p with [] -> [], [] | m :: p ->
      let rec pw f x y = match x, y with
        | [], [] -> []
        | [], _ -> pw f [0] y
        | _, [] -> pw f x [0]
        | hx :: tx, hy :: ty -> f hx hy :: pw f tx ty in
      let mi, ma =
        List.fold_left (fun (mi, ma) m -> pw min mi m, pw max ma m) (m, m) p in
      of_list mi, of_list ma in
    List.filter
      (fun m ->
       let m = List.map (( * ) 2) m in
       pw_le mi m && pw_le m ma) s in
  (* keep monomials s_i of s such that 2 s_i is in p *)
  let skeep, sfilter =
    let rec inter x y = match x, y with
      | [], _ | _, [] -> [], x
      | hx :: tx, hy :: ty ->
         let c = compare (List.map (( * ) 2) hx) hy in
         if c = 0 then let k, f = inter tx ty in hx :: k, f
         else if c < 0 then let k, f = inter tx y in k, hx :: f
         else (* c > 0 *) let k, f = inter x ty in k, hx :: f in
    inter (List.sort compare s) (List.sort compare p) in
  (* look for separating hyperplane to rule out monomials not in the
     Newton polynomial *)
  let s =
    (* let prfa = Utils.fprintf_array ~sep:",@ " Format.pp_print_float in *)
    let n =
      let f n l = max n (nb_vars l) in
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
      let zcoeffs = center si in
      let cstrs = Array.of_list (List.map center p) in
      let pbounds = Array.make (Array.length cstrs) (neg_infinity, 1.) in
      let xbounds = Array.make n (-1000000., 1000000.) in
      (* Format.printf "@[<v2>si = %a@," pp si; *)
      (* Format.printf "obj = @[%a@]@," prfa zcoeffs; *)
      (* Format.printf *)
      (*   "cstrs = @[<v>%a@]@." *)
      (*   (Utils.fprintf_array *)
      (*      ~sep:"@," *)
      (*      (fun fmt a -> *)
      (*       Format.fprintf *)
      (*         fmt "@[%a@] <= 1" prfa a)) cstrs; *)
      let lp = Glpk.make_problem Glpk.Maximize zcoeffs cstrs pbounds xbounds in
      Glpk.set_message_level lp 1;
      Glpk.use_presolver lp true;
      Glpk.simplex lp;
      if Glpk.get_obj_val lp < 0.50001 then None
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
              !obj < 0.50001 in
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
    "@[<2>keep:@ @[%a@]@]@." (Utils.fprintf_list ~sep:",@ " pp) skeep;
  Format.printf
    "@[<2>to filter:@ @[%a@]@]@." (Utils.fprintf_list ~sep:",@ " pp) sfilter;
  Format.printf
    "@[<2>%d monomials after filtering:@ @[%a@]@]@."
    (List.length s) (Utils.fprintf_list ~sep:",@ " pp) s;
  s
