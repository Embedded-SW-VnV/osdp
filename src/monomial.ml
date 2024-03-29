(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2016  P. Roux and P.L. Garoche
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

let rec lcm m1 m2 = match m1, m2 with
  | [], _ -> m2
  | _, [] -> m1
  | h1 :: t1, h2 :: t2 -> max h1 h2 :: lcm t1 t2

let gcd m1 m2 =
  let rec aux m1 m2 = match m1, m2 with
    | ([], _ | _, []) -> []
    | h1 :: t1, h2 :: t2 -> min h1 h2 :: aux t1 t2 in
  of_list (aux m1 m2)

let rec divide m1 m2 = match m1, m2 with
  | [], _ -> true
  | _, [] -> false
  | h1 :: t1, h2 :: t2 -> h1 <= h2 && divide t1 t2

exception Not_divisible

let div m1 m2 =
  let rec aux m1 m2 = match m1, m2 with
    | _, [] -> m1
    | [], _ -> raise Not_divisible
    | h1 :: t1, h2 :: t2 ->
       if h1 < h2 then raise Not_divisible
       else h1 - h2 :: aux t1 t2 in
  of_list (aux m1 m2)

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
       if c <> 0 then c else Stdlib.compare h1 h2 in
  let c = Stdlib.compare (degree m1) (degree m2) in
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
