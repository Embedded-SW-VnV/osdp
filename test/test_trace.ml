(* Unit test for osdp.trace: records are appended (never truncated),
   each row is valid JSON with a timestamp, and concurrent appends from
   several processes don't interleave/corrupt each other's lines. *)

let read_lines path =
  let ic = open_in path in
  let rec go acc =
    match input_line ic with
    | line -> go (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  go []

let contains_substring ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0

(* Extremely small hand-rolled JSON object check: every line must look
   like a flat {"k":"v",...} object and contain a "timestamp" key. Not a
   general JSON parser -- osdp_trace.ml controls the escaping, so this
   only needs to catch gross breakage (interleaved/truncated lines). *)
let looks_like_one_json_object line =
  String.length line >= 2
  && line.[0] = '{'
  && line.[String.length line - 1] = '}'
  && contains_substring ~needle:"\"timestamp\":\"" line

let fail fmt = Format.kasprintf (fun s -> print_string s; exit 1) fmt

let () =
  let log = Filename.temp_file "osdp_trace_test_" ".jsonl" in
  Sys.remove log (* record must create it *);

  Osdp_trace.record ~log [ "a", "1"; "b", "x" ];
  Osdp_trace.record ~log [ "a", "2"; "c", "with \"quotes\" and \\backslash\" and\nnewline" ];

  let lines = read_lines log in
  if List.length lines <> 2 then
    fail "FAIL: expected 2 lines after 2 records, got %d\n" (List.length lines);
  List.iter (fun l ->
    if not (looks_like_one_json_object l) then
      fail "FAIL: line does not look like a JSON object with a timestamp: %s\n" l
  ) lines;

  (* Third call: the log already has content -- record must append, not
     truncate what's already there. *)
  Osdp_trace.record ~log [ "a", "3" ];
  let lines' = read_lines log in
  if List.length lines' <> 3 then
    fail "FAIL: expected 3 lines after a 3rd record (log must accumulate, not truncate), got %d\n"
      (List.length lines');

  (* Concurrent appenders: fork several children each appending one row;
     the lockf-guarded write must keep every row on its own line. *)
  let log2 = Filename.temp_file "osdp_trace_test_concurrent_" ".jsonl" in
  Sys.remove log2;
  let nb_children = 8 in
  for i = 0 to nb_children - 1 do
    match Unix.fork () with
    | 0 ->
      Osdp_trace.record ~log:log2 [ "child", string_of_int i ];
      exit 0
    | _ -> ()
  done;
  for _ = 1 to nb_children do ignore (Unix.wait ()) done;
  let lines2 = read_lines log2 in
  if List.length lines2 <> nb_children then
    fail "FAIL: expected %d lines from %d concurrent appenders, got %d (interleaved/lost write?)\n"
      nb_children nb_children (List.length lines2);
  List.iter (fun l ->
    if not (looks_like_one_json_object l) then
      fail "FAIL: concurrent-write line is corrupted: %s\n" l
  ) lines2;

  Sys.remove log; Sys.remove log2;
  Format.printf "All osdp_trace tests passed.@."
