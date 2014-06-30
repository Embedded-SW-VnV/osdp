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

open Lexing
open Parsing

type t = { loc_line : int; loc_start : int ; loc_end : int }

let dummy = {loc_line = 1; loc_start = 0; loc_end = 0}

let location_of_positions start_p end_p =
  { loc_line = start_p.pos_lnum;
    loc_start = start_p.pos_cnum - start_p.pos_bol;
    loc_end = end_p.pos_cnum - start_p.pos_bol }  (* start_p for bol *)
    
(** To be called in a parser rule. *)
let get_current_location () =
  let start_p = symbol_start_pos () in
  let end_p = symbol_end_pos () in
  location_of_positions start_p end_p

(** Get position of lexer, useful for lexing errors. *)
let get_current_location_from_lexbuf lexbuf =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  location_of_positions start_p end_p

(** Output a location. *)
let print_to_string loc =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d: "
    !Global.filename loc.loc_line loc.loc_start loc.loc_end
