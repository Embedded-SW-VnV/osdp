{
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

(* TODO: other return values *)

open Sdpa_parser

exception Lexing_error of string
}

let sign = ['+' '-']
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']
let blank = [' ' '\r' '\t']
let nl = ['\r']?['\n']

rule token = parse
  | nl { token lexbuf }
  | blank+ { token lexbuf }
  | "phase.value" { PHASEDVALUE }
  | "noINFO" { NOINFO }
  | "pFEAS" { PFEAS }
  | "dFEAS" { DFEAS }
  | "pdFEAS" { PDFEAS }
  | "pdINF" { PDINF }
  | "pFEAS_dINF" { PFEASDINF }
  | "pINF_dFEAS" { PINFDFEAS }
  | "pdOPT" { PDOPT }
  | "pUNBD" { PUNBD }
  | "dUNBD" { DUNBD }
  | "objValPrimal" { OBJVALPRIMAL }
  | "objValDual" { OBJVALDUAL }
  | "xVec" { XVEC }
  | "xMat" { XMAT }
  | "yMat" { YMAT }
  | '{' { LBRA }
  | '}' { RBRA }
  | (sign? "0x" (hex* '.')? hex+ 'p' sign? digit+) as n { FLOAT (float_of_string n) }
  | (sign? ('0' | (['1'-'9'] digit*)) '.' digit* ('e' sign? digit+)?) as n { FLOAT (float_of_string n) }
  | (sign? '.' digit+ ('e' sign? digit+)?) as n { FLOAT (float_of_string n) }
  | sign? '0' { FLOAT 0. }
  | (sign? ['1'-'9'] digit* ('e' sign? digit+)?) as n { FLOAT (float_of_string n) }
  | eof { EOF }
  | _ { token lexbuf }
