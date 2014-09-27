(*
 * SMT-AI: an abstract interpreter to be used by a k-induction model checker
 * Copyright (C) 2010  P.L. Garoche and P. Roux
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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

module IntOrdered = struct type t = int let compare = compare end
module IntSet = Set.Make (IntOrdered)

module IntMap = struct
  include Mappe.Make (Sette.Make (IntOrdered))
  let revert fold map = fold (fun key i res -> add i key res) map empty
end

let list_merge l =
  List.fold_left
    (fun res l' -> 
       List.fold_left
         (fun res' elem -> if List.mem elem res' then res' else elem::res')
         res l') [] l

let rec fprintf_list ~sep f fmt = function
  | []   -> ()
  | [e]  -> f fmt e
  | x::r -> Format.fprintf fmt "%a%(%)%a" f x sep (fprintf_list ~sep f) r

let fprintf_hash k f fmt hash =
  Format.fprintf fmt "@[[";
  Hashtbl.iter
    (fun key content -> Format.fprintf fmt "%a: %a;@."  k key f content) hash;
  Format.fprintf fmt "]@]" 

let format_string s l =
  let s' = String.make l ' ' in
  String.blit s 0 s' 0 (min (String.length s) l);
  s'

let split_string s l =
  let sl = String.length s in
  let nb = (sl / l) + (if sl mod l = 0 then 0 else 1) in
  let rec split start_i idx =
    if idx >= nb - 1 then
      let subs = String.sub s start_i (sl - start_i) in
	[format_string subs l]
    else
      (String.sub s start_i l)::(split (start_i + l) (idx + 1))
  in
    split 0 0

let desome x = match x with None -> raise (Failure "desome") | Some y -> y

let profile c =
  let be = Unix.gettimeofday () in
  let r = Lazy.force c in
  let en = Unix.gettimeofday () in
  let elapsed_time = en -. be in
  r, elapsed_time

let string_of_float f =
  let f = ceil (f *. 100.) /. 100. in
  let s = string_of_float f in
  try
    let dotpos = String.index s '.' in
    String.sub s 0 (dotpos + 3)
  with Not_found | Invalid_argument _ -> s

let string_of_num s = 
  let s = Num.float_of_num s in
  string_of_float s

let num_of_strings neg left right exp =
  let neg_num neg = 
    if neg then
      Num.minus_num
    else
      fun x -> x
  in
  let base = 
    match right with
    | None -> 
      neg_num neg (Num.num_of_int (int_of_string left))
    | Some right -> (
      let nb_dec = String.length right in
      let n_s = left ^ right in
      let d_s = "1" ^ String.make nb_dec '0' in
      try
	let n = Num.num_of_string n_s in
	let d = Num.num_of_string d_s in
	neg_num neg (Num.div_num n d)
      with Failure s -> raise (Failure (s ^ ": n = " ^ n_s ^", d = " ^ d_s))
    )
  in
  match exp with 
    Some (posneg_exp, exp) -> 
      let e = int_of_string exp in
      Num.mult_num base (Num.power_num (Num.num_of_int 10) (neg_num posneg_exp (Num.num_of_int e)))
  | _ -> base

let num_of_string s =
  let regexp = Str.regexp "\\(-\\)?\\([0-9]+\\)\\.\\([0-9]+\\)?\\(e\\(-\\)?\\([0-9]+\\)\\)?" in
  let _ = Str.string_match regexp s 0 in
  let neg = try let _ = Str.matched_group 1 s in true with Not_found -> false in
  let left = Str.matched_group 2 s in
  let right = try Some (Str.matched_group 3 s) with Not_found -> None in
  let exp_opt = 
    try 
      let negsign = (match Str.matched_group 5 s with "+" -> false | "-" -> true | _ -> assert false) in
      Some (negsign, Str.matched_group 6 s) 
    with Not_found -> None in
  num_of_strings neg left right exp_opt 

let num_of_float f =
  let s = string_of_float f in 
  num_of_string s

		    
let merge_sorted_lists
    (cmp: 'a -> 'a -> int) 
    (f:'a -> 'a -> 'a option)  
    (l1:'a list) 
    (l2: 'a list) 
    : 'a list = 
  let rec aux l1 l2 = 
    match l1, l2 with 
    | [], l2 -> l2
    | l1, [] -> l1
    | h1::t1, h2::t2 ->
      let comp = cmp h1 h2 in
      if comp < 0 then 
	h1 :: (aux t1 l2)
      else if comp = 0 then
	match f h1 h2 with
	  Some r -> r::(aux t1 t2)
	| None -> aux t1 t2
      else h2 :: (aux l1 t2)
  in
  aux l1 l2

