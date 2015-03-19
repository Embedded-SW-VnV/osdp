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

type matrix = (int * int * float) list
type block_diag_matrix = (int * matrix) list

type solver = Sdpa | SdpaGmp

type options = {
  solver : solver;
  max_iteration : int;
  stop_criterion : float;
  initial : float;
  precision : int
}

let default = {
  solver = Sdpa;
  max_iteration = 100;
  stop_criterion = 1.0E-7;
  initial = 1.0E2;
  precision = 200
}

module IntMap = Map.Make (struct type t = int let compare = compare end)

let output_dat block_struct obj constraints =
  let filename, oc = Filename.open_temp_file "sdpa_input_" ".dat-s" in
  (* number of constraints *)
  output_string oc (string_of_int (List.length constraints) ^ "\n");
  (* number of blocks *)
  let _, (max_idx, _) = IntMap.max_binding block_struct in
  output_string oc (string_of_int max_idx ^ "\n");
  (* size of blocks *)
  IntMap.iter
    (fun _ (_, sz) -> output_string oc ((string_of_int sz) ^ " "))
    block_struct;
  output_char oc '\n';
  (* c_i (scalar parts of constraints) *)
  List.iter
    (fun (_, f) -> output_string oc ((string_of_float f) ^ " "))
    constraints;
  output_char oc '\n';
  (* objective F_0 and matrices F_i (matrix parts of constraints) *)
  let pr_mat i m =
    let pr_block (j, m) =
      let j, _ = IntMap.find j block_struct in
      List.iter
        (fun (i', j', f) ->
         let i', j' = j' + 1, i' + 1 in
         output_string oc (string_of_int i ^ " " ^ string_of_int j ^ " ");
         output_string oc (string_of_int i' ^ " " ^ string_of_int j' ^ " ");
         output_string oc (string_of_float f ^ "\n"))
        m in
    List.iter pr_block m in
  pr_mat 0 obj;
  List.iteri (fun i (m, _) -> pr_mat (i + 1) m) constraints;
  close_out oc;
  filename

let output_param options =
  let filename, oc = Filename.open_temp_file "sdpa_param_" ".sdpa" in
  output_string oc (string_of_int options.max_iteration
                    ^ "\tunsigned int maxIteration;\n");
  output_string oc (string_of_float options.stop_criterion
                    ^ "\tdouble 0.0 < epsilonStar;\n");
  output_string oc (string_of_float options.initial
                    ^ "\tdouble 0.0 < lambdaStar;\n");
  output_string oc "2.0\tdouble 1.0 < omegaStar;\n";
  output_string oc "-1.0E5\tdouble lowerBound;\n";
  output_string oc "1.0E5\tdouble upperBound;\n";
  output_string oc "0.1\tdouble 0.0 <= betaStar < 1.0;\n";
  output_string oc "0.2\tdouble 0.0 <= betaBar < 1.0, betaStar <= betaBar;\n";
  output_string oc "0.9\tdouble 0.0 < gammaStar < 1.0;\n";
  output_string oc (string_of_float options.stop_criterion
                    ^ "\tdouble 0.0 < epsilonDash;\n");
  output_string
    oc
    (match options.solver with
     | Sdpa ->
        "%a\tchar*  xPrint   (default %+8.3e,   NOPRINT skips printout)\n\
         %a\tchar*  XPrint   (default %+8.3e,   NOPRINT skips printout)\n\
         %a\tchar*  YPrint   (default %+8.3e,   NOPRINT skips printout)\n\
         %a\tchar*  infPrint (default %+10.16e, NOPRINT skips printout)\n"
     | SdpaGmp -> string_of_int options.precision ^ "\tprecision;\n");
  close_out oc;
  filename

let read_output block_struct filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let ret, (pobj, dobj), (res_X, res_y) =
    Sdpa_parser.resobjyx Sdpa_lexer.token lexbuf in
  close_in ic;
  let res_X =
    IntMap.fold (fun i (j, _) l -> (i, res_X.(j - 1)) :: l) block_struct [] in
  ret, (dobj, pobj), (List.rev res_X, res_y)

let solve ?options obj constraints =
  let block_struct =
    let struct_block_diag =
      let size = List.fold_left (fun m (i, j, _) -> max m (max i j + 1)) 0 in
      List.fold_left
        (fun m (i, b) ->
         let sz = max (size b) (try IntMap.find i m with Not_found -> 0) in
         IntMap.add i sz m) in
    let struc = struct_block_diag IntMap.empty obj in
    let struc =
      List.fold_left
        (fun struc (m, _) -> struct_block_diag struc m)
        struc constraints in
    let struc, _ =
      IntMap.fold
        (fun i sz (m, i') -> IntMap.add i (i', sz) m, i' + 1)
        struc (IntMap.empty, 1) in
    struc in
  (* block_struct is now a binding from each block index i to (i', sz)
     with i' a new index starting from 1 and sz the size of the block. *)
  let dat_s_filename = output_dat block_struct obj constraints in
  let options = match options with Some options -> options | None -> default in
  let param_filename = output_param options in
  let out_filename = Filename.temp_file "sdpa_output_" ".out" in
  let cmd =
    let sdpa = match options.solver with
      | Sdpa -> Sdpa_paths.sdpa
      | SdpaGmp -> Sdpa_paths.sdpa_gmp in
    if sdpa = "no" then failwith "caml_osdp: compiled without SDPA support!";
    Format.asprintf
      "%s -ds %s -o %s -p %s > /dev/null"
      sdpa dat_s_filename out_filename param_filename in
  let ret = Sys.command cmd in
  let res =
    if ret = 0 then read_output block_struct out_filename
    else SdpRet.Unknown, (0., 0.), ([], [||]) in
  Sys.remove dat_s_filename;
  Sys.remove param_filename;
  Sys.remove out_filename;
  res
