%{
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
%}

%token <float> FLOAT
%token SUCCESS PARTIALSUCCESS PRIMALINFEASIBLE DUALINFEASIBLE
%token NEARPRIMALINFEASIBLE NEARDUALINFEASIBLE
%token OBJVALPRIMAL OBJVALDUAL NL EOF

%type <SdpRet.t * (float * float)> retobj
%type <(int * int * int * float) list * float list * (int * int * int * float) list> resXyZ
%start retobj resXyZ

%%

garbageunit:
| FLOAT { }
| NL { }

garbage:
| /* empty */ { }
| garbageunit garbage { }

ret:
| SUCCESS { SdpRet.Success }
| PARTIALSUCCESS { SdpRet.PartialSuccess }
| PRIMALINFEASIBLE { SdpRet.PrimalInfeasible }
| DUALINFEASIBLE { SdpRet.DualInfeasible }
| NEARPRIMALINFEASIBLE { SdpRet.NearPrimalInfeasible }
| NEARDUALINFEASIBLE { SdpRet.NearDualInfeasible }

obj:
| OBJVALPRIMAL FLOAT NL OBJVALDUAL FLOAT { $2, $5 }

retobj:
| garbage ret garbage obj garbage EOF { $2, $4 }
| garbage ret garbage EOF { $2, (0., 0.) }
| garbage obj garbage EOF { SdpRet.Unknown, $2 }
| garbage EOF { SdpRet.Unknown, (0., 0.) }

y:
| NL { [] }
| FLOAT y { $1 :: $2 }

XZentry:
| FLOAT FLOAT FLOAT FLOAT FLOAT {
      int_of_float $1, int_of_float $2, int_of_float $3, int_of_float $4, $5
    }

XZ:
| /* empty */ { [] }
| XZentry NL XZ { $1 :: $3 }
| XZentry XZ { $1 :: $2 }

resXyZ:
| y XZ EOF {
      let rec sort ((x, z) as acc) l = match l with
        | [] -> acc
        | (m, b, i, j, f) :: t ->
           if m = 1 then sort (x, ((b, i, j, f) :: z)) t
           else sort (((b, i, j, f) :: x), z) t in
      let x, z = sort ([], []) $2 in x, $1, z
    }
