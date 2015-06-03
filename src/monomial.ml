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
                      
let rec list_eq n d =
  if n <= 0 then [[]] else
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
       (Utils.pp_list ~sep:"@ " (fun fmt (n, e) ->
                                 if e = 1 then
                                   Format.fprintf fmt "%s" n
                                 else
                                   Format.fprintf fmt "%s^%i" n e))
       l

let pp = pp_names []

(* filter_newton_polytope is the actual function (using Glpk) or the
   identity depending on whether the compilation was done with or
   without Glpk. *)
include Monomial_glpk_opt

let filter_newton_polytope s p =
  (* Format.printf *)
  (*   "@[<2>%d monomials before filtering:@ @[%a@]@]@." *)
  (*   (List.length s) (Utils.fprintf_list ~sep:",@ " pp) s; *)
  (* Format.printf *)
  (*   "@[<2>filtering by:@ @[%a@]@]@." *)
  (*   (Utils.fprintf_list ~sep:",@ " pp) p; *)
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
  filter_newton_polytope s p

type monomial = t

module Set = Set.Make (struct type t = monomial let compare = compare end)

module Map = Map.Make (struct type t = monomial let compare = compare end)
