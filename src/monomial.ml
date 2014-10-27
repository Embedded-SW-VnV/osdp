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

type t = int list

let of_list l = l
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
  if n <= 0 then [[]]
  else if d <= 0 then list_eq n 0
  else list_le n (d - 1) @ list_eq n d

let pp ?names fmt m =
  let rec name_vars i names = function
    | [] -> []
    | h :: t ->
       let n, names =
         match names with [] -> "x" ^ string_of_int i, [] | n :: t -> n, t in
       (n, h) :: name_vars (i + 1) names t in
  let names = match names with None -> [] | Some names -> names in
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

let pp_no_names = pp ~names:[]
