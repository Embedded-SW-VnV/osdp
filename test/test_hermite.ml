(* End-to-end tests for the Hermite polynomial SOS decomposition.
   Each check asserts the expected outcome; failure exits non-zero. *)

module Sos = Osdp.Sos.Float

let () =
  let open Sos in
  let x = ??0 and y = ??1 in
  let hermite  = { default with basis = Osdp.SosBasis.Hermite } in
  let classical = { default with basis = Osdp.SosBasis.Classical } in

  let check_success label ret =
    if not (Osdp.SdpRet.is_success ret) then begin
      Format.eprintf "FAIL %s: expected success, got %a@." label Osdp.SdpRet.pp ret;
      exit 1
    end in

  let check_failure label ret =
    if Osdp.SdpRet.is_success ret then begin
      Format.eprintf "FAIL %s: expected failure, got %a@." label Osdp.SdpRet.pp ret;
      exit 1
    end in

  let check_value label v expected eps =
    if abs_float (v -. expected) > eps then begin
      Format.eprintf "FAIL %s: expected %g, got %g@." label expected v;
      exit 1
    end in

  (* 1. x^2 + 1 is SOS (Hermite) *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [x * x + !1.] in
  check_success "Hermite x^2+1 feasible" ret;

  (* 2. x^2 + y^2 + 1 is SOS (Hermite, multivariate) *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [x * x + y * y + !1.] in
  check_success "Hermite x^2+y^2+1 feasible" ret;

  (* 3. The constant -1 is not SOS in any basis *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [!(-1.)] in
  check_failure "Hermite -1 infeasible" ret;

  (* 4. Hermite: max t s.t. x^2 + 1 - t is SOS  →  t* = min_x (x^2+1) = 1 *)
  let t = make "t" in
  let ret, (v, _), _, _ = solve ~options:hermite (Maximize t) [x * x + !1. - t] in
  check_success "Hermite max-t solve" ret;
  check_value   "Hermite max-t value" v 1.0 1e-4;

  (* 5. Classical gives the same optimal value for the same problem *)
  let t = make "t" in
  let ret, (v, _), _, _ = solve ~options:classical (Maximize t) [x * x + !1. - t] in
  check_success "Classical max-t solve" ret;
  check_value   "Classical max-t value" v 1.0 1e-4;

  (* 6. Degree-4 case: x^4 + 1 is SOS (Hermite).
     The Gram matrix is diagonal Q = diag(3/4, 1/4, 1/16) in the basis
     [H_0, H_1, H_2], which is strictly positive definite. *)
  let p = x * x * x * x + !1. in
  let ret, _, _, _ = solve ~options:hermite Purefeas [p] in
  check_success "Hermite x^4+1 feasible" ret;

  Format.printf "All Hermite SOS tests passed.@."
