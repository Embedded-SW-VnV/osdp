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


let format_indent i s =
  let step_indent = 2 in
  let margin = 120 in
  let max_indent = 60 in
  let indent_off =
    if step_indent * i <= max_indent then step_indent * i + 1
    else max_indent + 1 in
  let res_len = ref (indent_off - 1) in
  let line_len = ref (indent_off - 1) in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then begin
      res_len := !res_len + indent_off + 1;
      line_len := indent_off
    end else if !line_len >= margin then begin
      res_len := !res_len + indent_off + 2;
      line_len := indent_off
    end else begin
      incr res_len;
      incr line_len
    end
  done;
  let s' = String.create !res_len in
  let i' = ref 0 in
  let indent off = 
    String.fill s' !i' off ' ';
    i' := !i' + off;
    line_len := off in
  indent (indent_off - 1);
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then begin
      s'.[!i'] <- '\n';
      incr i';
      indent indent_off
    end else if !line_len >= margin then begin
      s'.[!i'] <- '\n';
      incr i';
      indent indent_off;
      s'.[!i'] <- s.[i];
      incr i';
    end else begin
      s'.[!i'] <- s.[i];
      incr i';
      incr line_len
    end
  done;
  s'

let debug_indent ?(kind="") i s =
  print_above i ~kind:kind (lazy (format_indent (i+2) (Lazy.force s)))

let error_loc loc msg =
  let loc_str = Location.print_to_string loc in
  Printf.eprintf "%sError: %s\n" loc_str msg;
  raise Error

let error_loc_file loc file msg =
  let filename = !Global.filename in
  try
    Global.filename := file;
    error_loc loc msg
  with Error ->
    Global.filename := filename;
    raise Error

let error_loc_previous loc loc_prev msg =
  try error_loc loc msg
  with Error ->
    error_loc loc_prev "previous occurence was here."
