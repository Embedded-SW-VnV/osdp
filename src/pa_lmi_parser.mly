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
    if 'a' <= s.[0] && s.[0] <= 'z' then Ast.IdLid (loc, s)
    else Ast.IdUid (loc, s) in
  Ast.ExId (loc, Ast.idAcc_of_list (List.map id_of_str l))

let olf loc idl el =
  List.fold_left
    (fun f e -> Ast.ExApp (loc, f, e))
    (id_of_list loc (["Osdp"; "Lmi"; "Float"] @ idl)) el

let olfl = olf (loc ())

let cons l e1 e2 =
  Ast.ExApp (l, Ast.ExApp (l, Ast.ExId (l, Ast.IdUid (l, "::")), e1), e2)

let empty_list l = Ast.ExId (l, Ast.IdUid (l, "[]"))

let slist l e = cons l e (empty_list l)

let simplify_mat_float e =
  let is_id l = function
    | Ast.ExId (_, i) ->
       let l' = Ast.list_of_ident i [] in
       begin
         try
           List.for_all
             (function
               | s, Ast.IdUid (_, s') | s, Ast.IdLid (_, s') -> s = s'
               | _ -> false)
             (List.combine l l')
         with Invalid_argument _ -> false
       end
    | _ -> false in

  let is_1x1_float e =
    let is_slist_slist e =
      let is_slist = function
        | Ast.ExApp (_, Ast.ExApp (_, ic, h), ie) ->
           if is_id ["::"] ic && is_id ["[]"] ie then Some h else None
        | _ -> None in
      match is_slist e with None -> None | Some e -> is_slist e in
    let aux = function
      | Ast.ExApp (_, c, Ast.ExApp (_, oll, llf)) ->
         if is_id ["Osdp"; "Lmi"; "Float"; "MEconst"] c
            && is_id ["Osdp"; "Lmi"; "Float"; "Mat"; "of_list_list"] oll then
           match is_slist_slist llf with
           | Some (Ast.ExFlo (l, f))
           | Some (Ast.ExInt (l, f)) ->
              Some (Ast.ExFlo (l, f))
           | _ -> None
         else None
      | _ -> None in
    match e with
    | Ast.ExApp (_, mi, m) when is_id ["Osdp"; "Lmi"; "Float"; "MEminus"] mi ->
       begin
         match aux m with
         | Some (Ast.ExFlo (l, f)) -> Some (Ast.ExFlo (l, "-" ^ f))
         | _ -> None
       end
    | _ -> aux e in

  let rec map_ExSem f = function
    | Ast.ExSem (l, e1, e2) ->
       begin
         match f e1, map_ExSem f e2 with
         | Some e1, Some e2 -> Some (cons l e1 e2)
         | _ -> None
       end
    | e ->
       match f e with
       | None -> None
       | Some e -> Some (slist (Ast.loc_of_expr e) e) in

  let loc = loc () in
  let simplify_mat_line = function
    | Ast.ExArr (l, e) -> map_ExSem is_1x1_float e
    | _ -> None in
  match map_ExSem simplify_mat_line e with
  | Some llf ->
     olf loc ["MEconst"] [olf loc ["Mat"; "of_list_list"] [llf]]
  | None -> olf loc ["MEblock"] [Ast.ExArr (loc, e)]
%}

%token <string> FLOAT
%token <string> INT
%token <string> ID
%token <string> UID
%token <string> AQ
%token INT0 QMARK DQUOTE SQUOTE TIMESSEMI PLUS MINUS TIMES
%token ZEROS EYE KRSYM LIFT
%token LPAR RPAR LBRA RBRA COMMA SEMICOL LEQ GEQ
%token UMINUS FINT0 EOF

%left PLUS MINUS
%left TIMES
%nonassoc TIMESSEMI
%nonassoc UMINUS
%nonassoc SQUOTE

%nonassoc FINT0
%nonassoc EOF

%type <Camlp4.PreCast.Ast.expr> lmi
%start lmi

%%

f:
| INT0 %prec FINT0 { Ast.ExFlo (loc (), "0.") }
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
| INT0 { Ast.ExInt (loc (), "0") }
| INT { Ast.ExInt (loc (), $1) }
| ID { id_of_list (loc ()) [$1] }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

expr:
| ID { id_of_list (loc ()) [$1] }
| i { olfl ["MEvar"] [$1] }
| ZEROS LPAR ncid COMMA ncid RPAR { olfl ["MEzeros"] [$3; $5] }
| EYE LPAR ncid RPAR { olfl ["MEeye"] [$3] }
| KRSYM LPAR ncid COMMA ncid COMMA ncid RPAR { olfl ["MEkronecker_sym"]
                                                    [$3; $5; $7] }
| LBRA b RBRA { simplify_mat_float $2 }
| LIFT LPAR expr COMMA ncid COMMA ncid COMMA ncid COMMA ncid RPAR { olfl ["MElift_block"] [$3; $5; $7; $9; $11] }
| expr SQUOTE { olfl ["MEtranspose"] [$1] }
| MINUS expr %prec UMINUS { olfl ["MEminus"] [$2] }
| f TIMESSEMI expr { olfl ["MEscale_const"] [$1; $3] }
| ID TIMESSEMI expr { let l = loc () in olf l ["MEscale_const"]
                                            [id_of_list l [$1]; $3] }
| i TIMESSEMI expr { olfl ["MEscale_var"] [$1; $3] }
| expr PLUS expr { olfl ["MEadd"] [$1; $3] }
| expr MINUS expr { olfl ["MEsub"] [$1; $3] }
| expr TIMES expr { olfl ["MEmult"] [$1; $3] }
| LPAR expr RPAR { $2 }
| f { let l = loc () in
      olf l ["MEconst"] [olf l ["Mat"; "of_list_list"] [slist l (slist l $1)]] }

b:
| l { Ast.ExArr (loc (), $1) }
| l SEMICOL b { Ast.ExSem (loc (), Ast.ExArr (Ast.loc_of_expr $1, $1), $3) }

l:
| expr { $1 }
| expr COMMA l { Ast.ExSem (loc (), $1, $3) }

lmi:
| expr EOF { $1 }
| expr LEQ INT0 EOF { olfl ["MEminus"] [$1] }
| expr GEQ INT0 EOF { $1 }
| expr LEQ expr EOF { olfl ["MEsub"] [$3; $1] }
| expr GEQ expr EOF { olfl ["MEsub"] [$1; $3] }
