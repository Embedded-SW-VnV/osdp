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

let olf loc idl el =
  List.fold_left
    (fun f e -> Ast.ExApp (loc, f, e))
    (id_of_list loc (["Osdp"; "Lmi"; "Float"] @ idl)) el

let olfl = olf (loc ())

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
%token INT0 QMARK DQUOTE SQUOTE HAT TIMESSEMI PLUS MINUS TIMES
%token ZEROS EYE KRSYM LIFT
%token LPAR RPAR LBRA RBRA COMMA SEMICOL LEQ GEQ
%token UMINUS PVM FINT0 EOF

%left PLUS MINUS
%left TIMES
%nonassoc TIMESSEMI
%nonassoc UMINUS LPAR
%nonassoc SQUOTE

%nonassoc PVM
%left HAT

%nonassoc FINT0
%nonassoc EOF

%type <Camlp4.PreCast.Ast.expr> lmi
%type <Camlp4.PreCast.Ast.expr> sos
%start lmi
%start sos

%%

f:
| INT0 %prec FINT0 { Ast.ExFlo (loc (), "0.") }
| INT { Ast.ExFlo (loc (), $1) }
| FLOAT { Ast.ExFlo (loc (), $1) }

id:
| MID { "x" ^ string_of_int $1 }
| ID { $1 }

vid:
| id { $1 }
| UID { $1 }

i:
| QMARK DQUOTE vid DQUOTE { let l = loc () in
                            Ast.ExApp (l,
                                       id_of_list l ["Osdp"; "Ident"; "create"],
                                       Ast.ExStr (l, $3)) }
| QMARK id { id_of_list (loc ()) [$2] }

ncid:
| INT0 { Ast.ExInt (loc (), "0") }
| INT { Ast.ExInt (loc (), $1) }
| id { id_of_list (loc ()) [$1] }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

exprl:
| id { id_of_list (loc ()) [$1] }
| i { olfl ["MEvar"] [$1] }
| ZEROS LPAR ncid COMMA ncid RPAR { olfl ["MEzeros"] [$3; $5] }
| EYE LPAR ncid RPAR { olfl ["MEeye"] [$3] }
| KRSYM LPAR ncid COMMA ncid COMMA ncid RPAR { olfl ["MEkronecker_sym"]
                                                    [$3; $5; $7] }
| LBRA b RBRA { simplify_mat_float $2 }
| LIFT LPAR exprl COMMA ncid COMMA ncid COMMA ncid COMMA ncid RPAR { olfl ["MElift_block"] [$3; $5; $7; $9; $11] }
| exprl SQUOTE { olfl ["MEtranspose"] [$1] }
| MINUS exprl %prec UMINUS { olfl ["MEminus"] [$2] }
| f TIMESSEMI exprl { olfl ["MEscale_const"] [$1; $3] }
| id TIMESSEMI exprl { let l = loc () in olf l ["MEscale_const"]
                                             [id_of_list l [$1]; $3] }
| i TIMESSEMI exprl { olfl ["MEscale_var"] [$1; $3] }
| exprl PLUS exprl { olfl ["MEadd"] [$1; $3] }
| exprl MINUS exprl { olfl ["MEsub"] [$1; $3] }
| exprl TIMES exprl { olfl ["MEmult"] [$1; $3] }
| LPAR exprl RPAR { $2 }
| f { let l = loc () in
      olf l ["MEconst"] [olf l ["Mat"; "of_list_list"] [slist l (slist l $1)]] }

b:
| l { Ast.ExArr (loc (), $1) }
| l SEMICOL b { Ast.ExSem (loc (), Ast.ExArr (Ast.loc_of_expr $1, $1), $3) }

l:
| exprl { $1 }
| exprl COMMA l { Ast.ExSem (loc (), $1, $3) }

lmi:
| exprl EOF { $1 }
| exprl LEQ INT0 EOF { olfl ["MEminus"] [$1] }
| exprl GEQ INT0 EOF { $1 }
| exprl LEQ exprl EOF { olfl ["MEsub"] [$3; $1] }
| exprl GEQ exprl EOF { olfl ["MEsub"] [$1; $3] }

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

exprs:
| ID { id_of_list (loc ()) [$1] }
| QMARK id { let l = loc () in osf l ["PLvar"] [id_of_list l [$2]] }
| monom { let l = loc () in
          osf l ["PLconst"] [osf l ["Poly"; "of_list"]
                                 [slist l (pair l $1 (Ast.ExFlo (l, "1.")))]] }
| f monom { let l = loc () in
            osf l ["PLconst"] [osf l ["Poly"; "of_list"]
                                   [slist l (pair l $2 $1)]] }
| i TIMESSEMI exprs { osfl ["PLmult_scalar"] [$1; $3] }
| exprs PLUS exprs { osfl ["PLadd"] [$1; $3] }
| exprs MINUS exprs { osfl ["PLsub"] [$1; $3] }
| MINUS exprs %prec UMINUS { let l = loc () in
                             osf l ["PLsub"]
                                 [osf l ["PLconst"] [osf l ["Poly"; "zero"] []];
                                  $2] }
| exprs TIMES exprs { osfl ["PLmult"] [$1; $3] }
| exprs HAT ncid { osfl ["PLpower"] [$1; $3] }
| exprs LPAR ls RPAR { osfl ["PLcompose"] [$1; $3] }
| LPAR exprs RPAR { $2 }
| f { let l = loc () in
      osf l ["PLconst"]
          [osf l ["Poly"; "of_list"]
               [slist l (pair l (Ast.ExApp (l, id_of_list l ["Osdp"; "Monomial"; "of_list"], empty_list l)) $1)]] }

ls:
| le { $1 }
| le COMMA ls { let l = loc () in
                Ast.ExApp (l, Ast.ExApp (l, id_of_list l ["@"], $1), $3) }

le:
| exprs { slist (loc ()) $1 }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

sos:
| exprs EOF { $1 }
| exprs LEQ exprs EOF { osfl ["PLsub"] [$3; $1] }
| exprs GEQ exprs EOF { osfl ["PLsub"] [$1; $3] }
