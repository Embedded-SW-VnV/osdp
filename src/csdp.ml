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

type matrix = (int * int * float) list
type block_diag_matrix = (int * matrix) list

type options = {
  verbose : int;
  max_iteration : int;
}

let default = {
  verbose = 0;
  max_iteration = 100;
}

module IntMap = Map.Make (struct type t = int let compare = compare end)

let output_dat dirname block_struct obj constraints =
  let filename = Filename.concat dirname "prob.dat-s" in
  let oc = Pervasives.open_out filename in
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

let output_param dirname options =
  let filename = Filename.concat dirname "param.csdp" in
  let oc = Pervasives.open_out filename in
  output_string
    oc "axtol=1.0e-8\n\
        atytol=1.0e-8\n\
        objtol=1.0e-8\n\
        pinftol=1.0e8\n\
        dinftol=1.0e8\n";
  output_string
    oc ("maxiter=" ^ string_of_int options.max_iteration ^ "\n");
  output_string
    oc "minstepfrac=0.90\n\
        maxstepfrac=0.97\n\
        minstepp=1.0e-8\n\
        minstepd=1.0e-8\n\
        usexzgap=1\n\
        tweakgap=0\n\
        affine=0\n";
  (* we need max 1 . to get at least the objective values
     (verbose 0 is handled by a file redirection below) *)
  output_string
    oc ("printlevel=" ^ string_of_int (max 1 options.verbose) ^ "\n");
  output_string
    oc "perturbobj=1\n\
        fastmode=0\n";
  close_out oc;
  filename

let read_output block_struct filename =
  let try_parse filename rule k =
    let default = SdpRet.Unknown, (0., 0.), ([], [||], []) in
    match try Some (open_in filename) with Sys_error _ -> None with
    | None -> default
    | Some ic ->
       let lexbuf = Lexing.from_channel ic in
       match
         try Some (rule Csdp_lexer.token lexbuf)
         with Parsing.Parse_error -> None
       with None -> default | Some res -> k res in
  try_parse (filename ^ ".out") Csdp_parser.retobj (fun (ret, pdobj) ->
  if not (SdpRet.is_success ret) then ret, pdobj, ([], [||], []) else
    try_parse filename Csdp_parser.resXyZ (fun (res_X, res_y, res_Z) ->
    let res_X, res_Z =
      let tr l =
        let a = Array.make (IntMap.cardinal block_struct) [||] in
        IntMap.iter
          (fun _ (j, sz) -> a.(j - 1) <- Array.make_matrix sz sz 0.)
          block_struct;
        List.iter
          (fun (b, i, j, f) ->
            a.(b - 1).(i - 1).(j - 1) <- f;
            a.(b - 1).(j - 1).(i - 1) <- f)
          l;
        IntMap.fold (fun i (j, _) l -> (i, a.(j - 1)) :: l) block_struct []
        |> List.rev in
      tr res_X, tr res_Z in
    let res_y = Array.of_list res_y in
    ret, pdobj, (res_X, res_y, res_Z)))

let solve ?options obj constraints =
  let dirname =
    let name = Filename.temp_file "csdp" "" in
    Sys.remove name;
    Unix.mkdir name 0o700;
    name in
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
  let dat_s_filename = output_dat dirname block_struct obj constraints in
  let options = match options with Some options -> options | None -> default in
  let param_filename = output_param dirname options in
  let out_filename = Filename.concat dirname "prob.sol" in
  let cmd =
    let csdp = Csdp_path.csdp in
    if csdp = "no" then failwith "caml_osdp: compiled without CSDP support!";
    Format.asprintf
      "sh -c \"cd %s && %s %s %s %s %s.out\""
      dirname csdp dat_s_filename out_filename
      (if options.verbose > 0 then "| tee" else ">")
      out_filename in
  let _ = Sys.command cmd in
  let res = read_output block_struct out_filename in
  begin
    try
      Sys.remove dat_s_filename;
      Sys.remove param_filename;
      Sys.remove out_filename;
      Sys.remove (out_filename ^ ".out");
      Unix.rmdir dirname
    with Sys_error _ | Unix.Unix_error _ -> ()
  end;
  res
