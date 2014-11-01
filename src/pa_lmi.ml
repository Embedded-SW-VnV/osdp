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

(** Camlp4 quotation for {{:./Lmi.Float.html#TYPEmatrix_expr}Lmi.Float.matrix_expr}. *)

(** See file examples/demo.ml for examples of use.
    Syntax:
    {[
n ::= 0 | [1-9][0-9]*

f ::= n | "0x" [0-9]+ "p" "-"? [0-9]+
    | ("0" | [1-9][0-9]* ) "." [0-9]* | "." [0-9]+

id ::= [a-z][a-zA-Z0-9_']*  (OCaml id)

uid ::= [A-Z][a-zA-Z0-9_']*

vid ::= id | uid

i ::= "?" """ vid """ | "?" id

ncid ::= n | id | "$" OCaml code (type int) "$"

e ::= id | i
    | "zeros" "(" ncid "," ncid ")" | "eye" "(" ncid ")"
    | "krsym" "(" ncid "," ncid "," ncid ")"
    | "[" b "]" | "lift" "(" e "," ncid "," ncid "," ncid "," ncid ")"
    | e "'" | "-" e
    | f "*:" e | id "*:" e | i "*:" e
    | e "+" e | e "-" e | e "*" e | "(" e ")"
    | f

b ::= l | l ";" b

l ::= e | e "," l

lmi ::= e | e "<=" "0" | e ">=" "0" | e "<=" e | e ">=" e
    ]} *)

(**/**)
module Error = struct 
  type t = Lexing.lexbuf * string 
  exception E of t 
  let print fmt (lexbuf, s) =
    let loc =
      let start_p = Lexing.lexeme_start_p lexbuf in
      let end_p = Lexing.lexeme_end_p lexbuf in
      Camlp4.PreCast.Loc.of_tuple
        (start_p.Lexing.pos_fname,
         start_p.Lexing.pos_lnum, start_p.Lexing.pos_bol, start_p.Lexing.pos_cnum,
         end_p.Lexing.pos_lnum, end_p.Lexing.pos_bol, end_p.Lexing.pos_cnum,
         false) in
    Format.fprintf fmt "%a: Error: %s.@." Camlp4.PreCast.Loc.print loc s
  let to_string = Format.asprintf "%a@." print
end

let parse _loc _ s =
  (* let _ = Parsing.set_trace true in *)
  let lexbuf =
    let lexbuf = Lexing.from_string s in
    let start_p =
      let fname, lnum, bol, cnum, _, _, _, _ =
        Camlp4.PreCast.Loc.to_tuple _loc in
      { Lexing.pos_fname = fname;
        pos_lnum = lnum;
        pos_bol = bol;
        pos_cnum = cnum } in
    lexbuf.Lexing.lex_abs_pos <- start_p.Lexing.pos_cnum;
    lexbuf.Lexing.lex_start_p <- start_p;
    lexbuf.Lexing.lex_curr_p <- start_p;
    lexbuf in
  try
    Pa_lmi_parser.lmi Pa_lmi_lexer.token lexbuf
  with
  | Failure s
  | Pa_lmi_lexer.Lexing_error s -> raise (Error.E (lexbuf, s))
  | Parsing.Parse_error -> raise (Error.E (lexbuf, "syntax error"))

let _ =
  let module M = Camlp4.ErrorHandler.Register(Error) in ();
  Camlp4.PreCast.Quotation.add
    "lmi" Camlp4.PreCast.Quotation.DynAst.expr_tag parse;
  Camlp4.PreCast.Quotation.default := "lmi"
(**/**)
