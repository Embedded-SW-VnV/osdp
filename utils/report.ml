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

exception Error

(* Formatters and printers *)
 

(* Printing computation information, with string based methods *)
let select kind n = 
  match kind with
  | "" -> !Global.verbosity >= n
  | "sdp" -> !Global.sdp_verbosity >= n
  | "policy" -> !Global.policy_verbosity >= n
  | _ -> raise (Failure ("Unrecognized verbose kind " ^ kind))

let print_above n ?(kind="") s =
  if select kind n then 
    Printf.printf "%s\n%!" (Lazy.force s)

let log = print_above 1

let debug = print_above 2

let printf_above n ?(kind="")  format_fun =
  if select kind n then 
    Format.printf "%t" format_fun

let logf ?(level=1) = printf_above level
let debugf ?(level=2) = printf_above level
