{
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

open Pa_parser

exception Lexing_error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let blank = [' ' '\r' '\t']
let nl = ['\r']?['\n']

rule token = parse
  | nl { Lexing.new_line lexbuf; token lexbuf }
  | blank+ { token lexbuf }
  | '\'' { SQUOTE }
  | '^' { HAT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRA }
  | ']' { RBRA }
  | ',' { COMMA }
  | ';' { SEMICOL }
  | "zeros" { ZEROS }
  | "eye" { EYE }
  | "kron" { KRON }
  | "krsym" { KRSYM }
  | "lift" { LIFT }
  | "d/d" { DERIV }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | ("0x" digit+ 'p' '-'? digit+) as n { FLOAT n }
  | (('0' | (['1'-'9'] digit*)) '.' digit*) ('e' ('+'? | '-') digit+)? as n { FLOAT n }
  | ('.' digit+) ('e' ('+'? | '-') digit+)? as n { FLOAT n }
  | '0' { INT0 }
  | (['1'-'9'] digit*) as n { INT n }
  | 'x' ((digit+) as n) { MID (int_of_string n) }
  | (['a'-'z' '_'] (alpha|digit|['_' '\''])*) as n { ID n }
  | '$' { AQ (antiquotation 0 lexbuf) }
  | eof { EOF }
  | _ { raise (Lexing_error "unknown char") }

and antiquotation n = parse
  | '$' { "" } 
  | nl { Lexing.new_line lexbuf; "\n" ^ antiquotation n lexbuf }
  | ('<' [^'<']* '<') as s { s ^ antiquotation (n + 1) lexbuf }
  | ">>" { if n <= 0 then ">>" else ">>" ^ antiquotation (n - 1) lexbuf }
  | ([^'$' '<' '>']*) as s { s ^ antiquotation n lexbuf }
  | ['<' '>'] as s { String.make 1 s ^ antiquotation n lexbuf }
  | eof { raise (Lexing_error "unterminated antiquotation") }
