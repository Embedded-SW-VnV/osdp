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

let id_of_list loc l =
  let id_of_str s =
    if 'A' <= s.[0] && s.[0] <= 'Z'
       || s = "::" || s = "[]" then Ast.IdUid (loc, s)
    else Ast.IdLid (loc, s) in
  Ast.ExId (loc, Ast.idAcc_of_list (List.map id_of_str l))

let osf loc idl el =
  List.fold_left
    (fun f e -> Ast.ExApp (loc, f, e))
    (id_of_list loc (["Osdp"; "Sos"; "Float"] @ idl)) el

let osfl = osf (loc ())

let cons l e1 e2 =
  Ast.ExApp (l, Ast.ExApp (l, Ast.ExId (l, Ast.IdUid (l, "::")), e1), e2)

let empty_list l = Ast.ExId (l, Ast.IdUid (l, "[]"))

let slist l e = cons l e (empty_list l)

let pair l e1 e2 = Ast.ExTup (l, Ast.ExCom (l, e1, e2))

let monom loc v d =
  let rec aux n =
    if n <= 0 then slist loc d
    else cons loc (Ast.ExInt (loc, "0")) (aux (n - 1)) in
  Ast.ExApp (loc, id_of_list loc ["Osdp"; "Monomial"; "of_list"], aux v)
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
| INT { Ast.ExFlo (loc (), $1) }
| FLOAT { Ast.ExFlo (loc (), $1) }

vid:
| ID { $1 }
| UID { $1 }

i:
| QMARK DQUOTE vid DQUOTE { let l = loc () in
                            Ast.ExApp (l,
                                       id_of_list l ["Osdp"; "Ident"; "create"],
                                       Ast.ExStr (l, $3)) }
| QMARK ID { id_of_list (loc ()) [$2] }

ncid:
| INT { Ast.ExInt (loc (), $1) }
| ID { id_of_list (loc ()) [$1] }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

vm:
| MID %prec PVM { let l = loc () in monom l $1 (Ast.ExInt (l, "1")) }
| MID HAT ncid { monom (loc ()) $1 $3 }

monom:
| vm { $1 }
| monom vm  { let l = loc () in
              Ast.ExApp (l,
                         Ast.ExApp (l,
                                    id_of_list l ["Osdp"; "Monomial"; "mult"],
                                    $1),
                         $2) }

expr:
| ID { id_of_list (loc ()) [$1] }
| QMARK ID { let l = loc () in osf l ["PLvar"] [id_of_list l [$2]] }
| monom { let l = loc () in
          osf l ["PLconst"] [osf l ["Poly"; "of_list"]
                                 [slist l (pair l $1 (Ast.ExFlo (l, "1.")))]] }
| f monom { let l = loc () in
          osf l ["PLconst"] [osf l ["Poly"; "of_list"]
                                 [slist l (pair l $2 $1)]] }
| i TIMESSEMI expr { osfl ["PLmult_scalar"] [$1; $3] }
| expr PLUS expr { osfl ["PLadd"] [$1; $3] }
| expr MINUS expr { osfl ["PLsub"] [$1; $3] }
| MINUS expr %prec UMINUS { let l = loc () in
                            osf l ["PLsub"]
                                [osf l ["PLconst"] [osf l ["Poly"; "zero"] []];
                                 $2] }
| expr TIMES expr { osfl ["PLmult"] [$1; $3] }
| expr HAT ncid { osfl ["PLpower"] [$1; $3] }
| expr LPAR l RPAR { osfl ["PLcompose"] [$1; $3] }
| LPAR expr RPAR { $2 }
| f { let l = loc () in
      osf l ["PLconst"]
          [osf l ["Poly"; "of_list"]
               [slist l (pair l (Ast.ExApp (l, id_of_list l ["Osdp"; "Monomial"; "of_list"], empty_list l)) $1)]] }

l:
| le { $1 }
| le COMMA l { let l = loc () in
               Ast.ExApp (l, Ast.ExApp (l, id_of_list l ["@"], $1), $3) }

le:
| expr { slist (loc ()) $1 }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

sos:
| expr EOF { $1 }
| expr LEQ expr EOF { osfl ["PLsub"] [$3; $1] }
| expr GEQ expr EOF { osfl ["PLsub"] [$1; $3] }
