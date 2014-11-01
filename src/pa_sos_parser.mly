%{
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

open Camlp4.PreCast

let loc () =
  let start_p = Parsing.symbol_start_pos () in
  let end_p = Parsing.symbol_end_pos () in
  Loc.of_tuple
    (start_p.Lexing.pos_fname,
     start_p.Lexing.pos_lnum, start_p.Lexing.pos_bol, start_p.Lexing.pos_cnum,
     end_p.Lexing.pos_lnum, end_p.Lexing.pos_bol, end_p.Lexing.pos_cnum,
     false)

let monom _loc v d =
  let rec aux n =
    if n <= 0 then <:expr< [$ d $] >> else <:expr< 0 :: $ aux (n - 1) $ >> in
  aux v
%}

%token <string> FLOAT
%token <string> INT
%token <int> MID
%token <string> ID
%token <string> UID
%token <string> AQ
%token QMARK DQUOTE HAT TIMESSEMI PLUS MINUS TIMES
%token LPAR RPAR COMMA LEQ GEQ
%token UMINUS PVM EOF

%left PLUS MINUS
%left TIMES
%nonassoc TIMESSEMI
%nonassoc UMINUS LPAR
%nonassoc PVM
%left HAT

%type <Camlp4.PreCast.Ast.expr> sos
%start sos

%%

f:
| INT { let _loc = loc () in <:expr< $flo: $1 $ >> }
| FLOAT { let _loc = loc () in <:expr< $flo: $1 $ >> }

vid:
| ID { $1 }
| UID { $1 }

i:
| QMARK DQUOTE vid DQUOTE { let _loc = loc () in
                            <:expr< Osdp.Ident.create $str: $3 $ >> }
| QMARK ID { let _loc = loc () in <:expr< $lid: $2 $ >> }

ncid:
| INT { let _loc = loc () in <:expr< $int: $1 $ >> }
| ID { let _loc = loc () in <:expr< $lid: $1 $ >> }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

vm:
| MID %prec PVM { let _loc = loc () in
                  <:expr< Osdp.Monomial.of_list ($ monom _loc $1 <:expr< 1 >> $) >> }
| MID HAT ncid { let _loc = loc () in
                 <:expr< Osdp.Monomial.of_list ($ monom _loc $1 $3 $) >> }

monom:
| vm { $1 }
| monom vm  { let _loc = loc () in <:expr< Osdp.Monomial.mult $ $1 $ $ $2 $ >> }

expr:
| ID { let _loc = loc () in <:expr< $lid: $1 $ >> }
| QMARK ID { let _loc = loc () in <:expr< Osdp.Sos.Float.PLvar $lid: $2 $ >> }
| monom { let _loc = loc () in
          <:expr< Osdp.Sos.Float.PLconst
                    (Osdp.Sos.Float.Poly.of_list
                       [$ $1 $, Osdp.Sos.Float.Poly.Coeff.one]) >> }
| f monom { let _loc = loc () in
            <:expr< Osdp.Sos.Float.PLconst
                      (Osdp.Sos.Float.Poly.of_list
                         [($ $2 $, $ $1 $)]) >> }
| i TIMESSEMI expr { let _loc = loc () in
                     <:expr< Osdp.Sos.Float.PLmult_scalar ($ $1 $, $ $3 $) >> }
| expr PLUS expr { let _loc = loc () in
                   <:expr< Osdp.Sos.Float.PLadd ($ $1 $, $ $3 $) >> }
| expr MINUS expr { let _loc = loc () in
                    <:expr< Osdp.Sos.Float.PLsub ($ $1 $, $ $3 $) >> }
| MINUS expr %prec UMINUS { let _loc = loc () in
                            <:expr< Osdp.Sos.Float.PLsub
                                      (Osdp.Sos.Float.PLconst
                                         Osdp.Sos.Float.Poly.zero,
                                       $ $2 $) >> }
| expr TIMES expr { let _loc = loc () in
                    <:expr< Osdp.Sos.Float.PLmult ($ $1 $, $ $3 $) >> }
| expr HAT ncid { let _loc = loc () in
                  <:expr<Osdp.Sos.Float.PLpower ($ $1 $, $ $3 $) >> }
| expr LPAR l RPAR { let _loc = loc () in
                     <:expr< Osdp.Sos.Float.PLcompose ($ $1 $, $ $3 $) >> }
| LPAR expr RPAR { $2 }
| f { let _loc = loc () in
      <:expr< Osdp.Sos.Float.PLconst
                (Osdp.Sos.Float.Poly.of_list
                   [Osdp.Monomial.of_list [], $ $1 $]) >> }

l:
| le { $1 }
| le COMMA l { let _loc = loc () in <:expr< $ $1 $ @ $ $3 $ >> }

le:
| expr { let _loc = loc () in <:expr< [$ $1 $] >> }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

sos:
| expr EOF { $1 }
| expr LEQ expr EOF { let _loc = loc () in
                      <:expr< Osdp.Sos.Float.PLsub ($ $3 $, $ $1 $) >> }
| expr GEQ expr EOF { let _loc = loc () in
                      <:expr< Osdp.Sos.Float.PLsub ($ $1 $, $ $3 $) >> }
