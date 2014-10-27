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

(** This automatically installs printers in the toplevel. Magic lines
    are the following
    {[
    #use "topfind";;
    #require "osdp";;
    ]}
    These last two lines can be added to ~/.ocamlinit if needed to
    avoid typing them again and again. *)

(* This is highly inspired from num_top.ml for the Num library. *)

(**/**)
let print_outcome = false
let error_fmt = Format.err_formatter

let printers = [
  "Osdp.Ident.pp";
  "Osdp.LinExpr.Q.pp";
  "Osdp.LinExpr.Float.pp";
  "Osdp.Lmi.Float.pp";
  "Osdp.Matrix.Q.pp";
  "Osdp.Matrix.Float.pp";
  "Osdp.Monomial.pp_no_names";
  "Osdp.Polynomial.Q.pp_no_names";
  "Osdp.Polynomial.Float.pp_no_names";
  "Osdp.Scalar.Q.pp";
  "Osdp.Scalar.Float.pp";
  "Osdp.Sos.Float.pp_no_names";
]

let eval_phrase s =
  let lexbuf = Lexing.from_string s in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome error_fmt phrase

let install_all () =
  List.fold_left
    (fun outcome phrase ->
     outcome && eval_phrase (Printf.sprintf "#install_printer %s;;" phrase))
    true printers

let _ =
  if not (install_all ()) then
    begin
      Format.fprintf
        error_fmt
        "Something weird happened while installing Osdp library printers";
      Format.pp_print_flush error_fmt ()
    end
(**/**)