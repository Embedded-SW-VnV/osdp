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

(* type invariant: all elements of the list are non negative and (the
   list is empty or the last element of the list is non zero) *)
type t = int list

let rec of_list = function
  | [] -> []
  | h :: t ->
     match of_list t with
     | [] -> if h = 0 then [] else [h]
     | t -> h :: t

let to_list m = m

let one = []

let var ?d i =
  let d = match d with Some d -> d | None -> 1 in
  let rec aux i = if i <= 0 then [d] else 0 :: aux (i - 1) in
  if d <= 0 || i < 0 then one else aux i

let rec mult m1 m2 = match m1, m2 with
  | [], _ -> m2
  | _, [] -> m1
  | h1 :: t1, h2 :: t2 -> (h1 + h2) :: mult t1 t2

let rec derive m i = match m with
  | [] -> 0, []
  | h :: t ->
     if i = 0 then
       if h > 0 then h, (if h > 1 || t <> [] then h - 1 :: t else [])
       else 0, []
     else
       match derive t (i - 1) with
       | 0, _ -> 0, []
       | j, t -> j, (h :: t)
                      
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

let nb_vars = List.length

let rec is_var = function
  | [d] -> Some (d, 0)
  | 0 :: l ->
     begin
       match is_var l with
       | Some (d, i) -> Some (d, i + 1)
       | None -> None
     end
  | _ -> None
                            
let rec list_eq n d =
  let add_diff m = (List.fold_left ( - ) d m) :: m in
  if n <= 0 then [[]] else Utils.map add_diff (list_le (n - 1) d)

and list_le n d =
  if n <= 0 || d <= 0 then [[]] else
    List.rev_append (List.rev (list_le n (d - 1))) (list_eq n d)

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
       (Utils.pp_list ~sep:"@ * " (fun fmt (n, e) ->
                                   if e = 1 then
                                     Format.fprintf fmt "%s" n
                                   else
                                     Format.fprintf fmt "%s^%d" n e))
       l

let pp = pp_names []

type monomial = t

module Set = Set.Make (struct type t = monomial let compare = compare end)

module Map = Map.Make (struct type t = monomial let compare = compare end)

module Var = struct
  type t = string
  let print = Format.pp_print_string
  let compare = String.compare
  let is_int _ = false
end

(*
module Float = struct
  type t = float
  let add = ( +. )
  let mult = ( *. )
  let compare = Pervasives.compare
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
*)

module Rat = struct
  type t = Q.t
  let add = Q.add
  let mult = Q.mul
  let compare = Q.compare
  let equal = Q.equal
  let zero = Q.zero
  let one = Q.one
  let m_one = Q.minus_one
  let is_zero n = equal n zero
  let to_string = Q.to_string
  let print = Q.pp_print
  let is_int _ = false
  let div = Q.div
  let sub = Q.sub
  let is_one v = equal v one
  let is_m_one v = equal v m_one
  let sign = Q.sign
  let min = Q.min
  let abs = Q.abs
  let minus = Q.neg
end

module Ex = struct
  type t = unit
  let empty = ()
  let union _ _ = ()
  let print _ _ = ()
end

module Sim = OcplibSimplex.Basic.Make (Var) (Rat) (Ex)

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
  (* look for separating hyperplane to rule out monomials not in the
     Newton polynomial *)
  let s =
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
        List.mapi (fun n c -> "x" ^ string_of_int n, Q.of_float c) l in
      let nb = ref 0 in
      let add_cstr b large sim c =
        let large = if large then Rat.m_one else Rat.zero in
        match List.filter (fun (_, c) -> not Rat.(equal c zero)) c with
        | [] -> sim
        | [v, c] ->
           let l, u =
             if Rat.sign c > 0 then None, Some (Rat.div b c, large)
             else Some (Rat.div b c, Rat.minus large), None in
           Sim.Assert.var sim v l () u ()
        | _ ->
           let s = "s" ^ string_of_int !nb in
           incr nb;
           let c = Sim.Core.P.from_list c in
           Sim.Assert.poly sim c s None () (Some (b, large)) () in
      let center' x = Array.to_list (center x) in
      let z = si |> List.map (( * ) 2) |> center' |> p_of_list in
      let m_z = List.map (fun (v, c) -> v, Rat.minus c) z in
      let cstrs = p |> List.map center' |> List.map p_of_list in
      let sim = Sim.Core.empty ~is_int:false ~check_invs:false ~debug:0 in
      let sim = add_cstr Rat.m_one false sim m_z in
      let sim = List.fold_left (add_cstr Rat.one true) sim cstrs in
      let sim, opt = Sim.Solve.maximize sim (Sim.Core.P.from_list z) in
      match Sim.Result.get opt sim with
      | (Sim.Core.Unknown | Sim.Core.Unsat _) -> None
      | (Sim.Core.Sat sol | Sim.Core.Unbounded sol | Sim.Core.Max (_, sol)) ->
         let a = Array.make n Rat.zero in
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
              let obj = ref Rat.zero in
              for i = 0 to n - 1 do
                obj := Rat.(add !obj (mult a.(i) (Q.of_float sj.(i))))
              done;
              Rat.(compare !obj one) <= 0 in
            filter s (List.filter test sfilter) in
    filter skeep sfilter in
  s

let filter_newton_polytope s p =
  Format.printf
    "@[<2>%d monomials before filtering:@ @[%a@]@]@."
    (List.length s) (Utils.pp_list ~sep:",@ " pp) s;
  Format.printf
    "@[<2>filtering by:@ @[%a@]@]@."
    (Utils.pp_list ~sep:",@ " pp) p;
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
  let res = filter_newton_polytope_ocplib_simplex s p in
  Format.printf
    "@[<2>%d monomials after filtering:@ @[%a@]@]@."
    (List.length s) (Utils.pp_list ~sep:",@ " pp) res;
  res
