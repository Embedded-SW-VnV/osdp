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
%}

%token <float> FLOAT
%token LBRA RBRA
%token PHASEDVALUE NOINFO PFEAS DFEAS PDFEAS PDINF PFEASDINF PINFDFEAS PDOPT PUNBD DUNBD
%token OBJVALPRIMAL OBJVALDUAL XVEC XMAT YMAT EOF

%type <SdpRet.t * (float * float) * (float array array array * float array * float array array array)> resobjyx
%start resobjyx

%%

garbageunit:
| LBRA { }
| RBRA { }
| FLOAT { }
| XMAT { }

garbage:
| /* empty */ { }
| garbageunit garbage { }

ret:
| NOINFO { SdpRet.Unknown }
| PFEAS { SdpRet.PartialSuccess }
| DFEAS { SdpRet.PartialSuccess }
| PDFEAS { SdpRet.Success }
| PDINF { SdpRet.Unknown }
| PFEASDINF { SdpRet.PrimalInfeasible }
| PINFDFEAS { SdpRet.DualInfeasible }
| PDOPT { SdpRet.Success }
| PUNBD { SdpRet.NearDualInfeasible }
| DUNBD { SdpRet.NearPrimalInfeasible }

retcode:
| PHASEDVALUE ret { $2 }

obj:
| OBJVALPRIMAL FLOAT OBJVALDUAL FLOAT { $2, $4 }

vec:
| /* empty */ { [] }
| FLOAT vec { $1 :: $2 }

xvec:
| XVEC LBRA vec RBRA { Array.of_list $3 }

mat:
| /* empty */ { [] }
| LBRA vec RBRA mat { Array.of_list $2 :: $4 }
| FLOAT { [[|$1|]] }

blockmat:
| /* empty */ { [] }
| LBRA mat RBRA blockmat { Array.of_list $2 :: $4 }

xmat:
| XMAT LBRA blockmat RBRA { Array.of_list $3 }

ymat:
| YMAT LBRA blockmat RBRA { Array.of_list $3 }

resobjyx:
| garbage retcode garbage obj garbage xvec xmat ymat garbage EOF { $2, $4, ($8, $6, $7) } 
