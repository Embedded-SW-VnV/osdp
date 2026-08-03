(* Shared test knobs, set from the shell rather than hardcoded, so the
   same test suite can be swept across solvers/padding without editing
   test code:
     OSDP_TEST_SOLVER=csdp|mosek|sdpa|sdpa_gmp|sdpa_dd
     OSDP_TEST_PAD=<float>       (0. disables padding)
     OSDP_TEST_VERBOSE=1         (print residuals even when a check passes)
   Any unset variable leaves the test's own default untouched. *)

let solver_of_string s =
  match String.lowercase_ascii s with
  | "csdp" -> Osdp.Sdp.Csdp
  | "mosek" -> Osdp.Sdp.Mosek
  | "sdpa" -> Osdp.Sdp.Sdpa
  | "sdpa_gmp" | "sdpagmp" -> Osdp.Sdp.SdpaGmp
  | "sdpa_dd" | "sdpadd" -> Osdp.Sdp.SdpaDd
  | _ -> invalid_arg (Printf.sprintf "OSDP_TEST_SOLVER: unknown solver %S" s)

let solver = Option.map solver_of_string (Sys.getenv_opt "OSDP_TEST_SOLVER")
let pad = Option.map float_of_string (Sys.getenv_opt "OSDP_TEST_PAD")
let verbose = Sys.getenv_opt "OSDP_TEST_VERBOSE" <> None

let override (o : Osdp.Sos.Float.options) : Osdp.Sos.Float.options =
  let sdp = match solver with
    | None -> o.sdp
    | Some solver -> { o.sdp with Osdp.Sdp.solver } in
  let pad = match pad with None -> o.pad | Some p -> p in
  { o with sdp; pad }
