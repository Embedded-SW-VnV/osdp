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

let iso8601_now () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  let ms = int_of_float (Float.rem (t *. 1000.) 1000.) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec ms

(* Minimal JSON string escaping: fields here are always simple
   caller-supplied labels/numbers/status strings, not arbitrary user
   input, so this only needs to be correct, not fast. *)
let json_escape s =
  let b = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c when Char.code c < 0x20 ->
      Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let to_json_line fields =
  let fields = fields @ [ "timestamp", iso8601_now () ] in
  "{" ^ String.concat ","
          (List.map (fun (k, v) ->
             Printf.sprintf "\"%s\":\"%s\"" (json_escape k) (json_escape v))
             fields)
  ^ "}"

let record ~log fields =
  let dir = Filename.dirname log in
  if dir <> "." && not (Sys.file_exists dir) then
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let fd = Unix.openfile log [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
      let line = to_json_line fields ^ "\n" in
      let len = String.length line in
      let rec write_all pos =
        if pos < len then
          let n = Unix.write_substring fd line pos (len - pos) in
          write_all (pos + n)
      in
      write_all 0))
