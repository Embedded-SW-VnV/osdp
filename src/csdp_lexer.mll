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

open Csdp_parser

exception Lexing_error of string
}

let sign = ['+' '-']
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']
let blank = [' ' '\r' '\t']
let nl = ['\r']?['\n']

rule token = parse
  | nl { NL }
  | blank+ { token lexbuf }
  | "Success: SDP solved" { SUCCESS }
  | "Success: SDP is primal infeasible" { PRIMALINFEASIBLE }
  | "Success: SDP is dual infeasible" { DUALINFEASIBLE }
  | "Partial Success: SDP solved with reduced accuracy" { PARTIALSUCCESS }
  | "Stuck at edge of primal feasibility, giving up." { NEARPRIMALINFEASIBLE }
  | "Stuck at edge of dual feasibility, giving up." { NEARDUALINFEASIBLE }
  | "Primal objective value:" { OBJVALPRIMAL }
  | "Dual objective value:" { OBJVALDUAL }
  | (sign? "0x" (hex* '.')? hex+ 'p' sign? digit+) as n { FLOAT (float_of_string n) }
  | (sign? ('0' | (['1'-'9'] digit*)) '.' digit* ('e' sign? digit+)?) as n { FLOAT (float_of_string n) }
  | (sign? '.' digit+ ('e' sign? digit+)?) as n { FLOAT (float_of_string n) }
  | sign? '0' { FLOAT 0. }
  | (sign? ['1'-'9'] digit* ('e' sign? digit+)?) as n { FLOAT (float_of_string n) }
  | eof { EOF }
  | _ { token lexbuf }
