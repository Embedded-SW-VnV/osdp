(* End-to-end tests for the Hermite polynomial SOS decomposition.
   Part 1: feasibility / optimality checks (outcome-level).
   Part 2: witness evaluation — verify that v(x)^T Q v(x) ≈ p(x) for
           several x values, for both Classical and Hermite witnesses. *)

module Sos = Osdp.Sos.Float
module M   = Osdp.Monomial

(* ------------------------------------------------------------------ *)
(* Witness evaluator                                                    *)
(* ------------------------------------------------------------------ *)

(* Probabilist's (monic) Hermite polynomial He_n(x): leading coefficient
   is always 1, unlike the physicist's H_n (leading coefficient 2^n). *)
let rec herm n x =
  match n with
  | 0 -> 1.
  | 1 -> x
  | _ -> x *. herm (n-1) x -. float_of_int (n-1) *. herm (n-2) x

let rec fact_float n = if n <= 1 then 1. else float_of_int n *. fact_float (n-1)

(* E_gamma[He_n^2] = n! under the standard normal measure gamma, so
   He_n / sqrt(n!) is the orthonormal rescaling used by HermiteNormalized. *)
let herm_normalized n x = herm n x /. sqrt (fact_float n)

(* Evaluate a basis element m at float point [pt], using eval_dim for
   each dimension.  Handles monomials shorter than [pt] correctly
   because Monomial.of_list strips trailing zeros. *)
let eval_mono eval_dim m pt =
  let rec go ns ps = match ns, ps with
    | [], _ -> 1.
    | _ :: _, [] -> 1.
    | n :: nt, x :: xt -> eval_dim n x *. go nt xt
  in
  go (M.to_list m) pt

let eval_classical_mono = eval_mono (fun e x -> x ** float_of_int e)
let eval_hermite_mono   = eval_mono herm
let eval_hermite_normalized_mono = eval_mono herm_normalized

(* v(pt)^T Q v(pt) — Q is a full symmetric float matrix. *)
let eval_gram eval_mono (v, q) pt =
  let n = Array.length v in
  let vs = Array.init n (fun i -> eval_mono v.(i) pt) in
  let s = ref 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      s := !s +. vs.(i) *. q.(i).(j) *. vs.(j)
    done
  done;
  !s

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let only1 = function [x] -> x | _ -> assert false

let tol_outcome = 1e-4  (* objective value tolerance *)
let tol_witness = 1e-3  (* witness evaluation tolerance *)

(* Polynomial functions for witness comparison.
   Defined here (not inside open Sos) to keep standard float operators. *)
let p_x2_1  pt = let x = List.nth pt 0 in x *. x +. 1.
let p_x2y2_1 pt = let a = List.nth pt 0 and b = List.nth pt 1 in
                  a *. a +. b *. b +. 1.
let p_x4_1  pt = let x = List.nth pt 0 in x *. x *. x *. x +. 1.

let check_success label ret =
  if not (Osdp.SdpRet.is_success ret) then begin
    Format.eprintf "FAIL %s: expected success, got %a@."
      label Osdp.SdpRet.pp ret;
    exit 1
  end

let check_failure label ret =
  if Osdp.SdpRet.is_success ret then begin
    Format.eprintf "FAIL %s: expected failure, got %a@."
      label Osdp.SdpRet.pp ret;
    exit 1
  end

let check_value label v expected eps =
  if abs_float (v -. expected) > eps then begin
    Format.eprintf "FAIL %s: expected %g, got %g@." label expected v;
    exit 1
  end

(* Assert that eval_gram of [witness] at every point in [pts] is close
   to [poly_at pt].  Uses the supplied basis evaluator [eval_mono]. *)
let check_witness label eval_mono witness pts poly_at =
  List.iter (fun pt ->
    let got      = eval_gram eval_mono witness pt in
    let expected = poly_at pt in
    if abs_float (got -. expected) > tol_witness then begin
      Format.eprintf "FAIL %s: witness=%g expected=%g@." label got expected;
      exit 1
    end
  ) pts

(* ------------------------------------------------------------------ *)
(* Tests — Part 1: feasibility / optimality                             *)
(* ------------------------------------------------------------------ *)

let () =
  let open Sos in
  let x = ??0 and y = ??1 in
  let hermite   = { default with basis = Osdp.SosBasis.Hermite } in
  let hermite_n = { default with basis = Osdp.SosBasis.HermiteNormalized } in
  let classical = { default with basis = Osdp.SosBasis.Classical } in

  (* 1. x^2+1 is SOS (Hermite). *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [x * x + !1.] in
  check_success "Hermite x^2+1 feasible" ret;
  let ret, _, _, _ = solve ~options:hermite_n Purefeas [x * x + !1.] in
  check_success "HermiteNormalized x^2+1 feasible" ret;

  (* 2. x^2+y^2+1 is SOS (Hermite, multivariate). *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [x*x + y*y + !1.] in
  check_success "Hermite x^2+y^2+1 feasible" ret;
  let ret, _, _, _ = solve ~options:hermite_n Purefeas [x*x + y*y + !1.] in
  check_success "HermiteNormalized x^2+y^2+1 feasible" ret;

  (* 3. -1 is not SOS in any basis. *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [!(-1.)] in
  check_failure "Hermite -1 infeasible" ret;
  let ret, _, _, _ = solve ~options:hermite_n Purefeas [!(-1.)] in
  check_failure "HermiteNormalized -1 infeasible" ret;

  (* 4. max t s.t. x^2+1-t SOS (Hermite) → t*=min_x(x^2+1)=1. *)
  let t = make "t" in
  let ret, (v, _), _, _ = solve ~options:hermite (Maximize t) [x*x + !1. - t] in
  check_success "Hermite max-t solve" ret;
  check_value   "Hermite max-t value" v 1.0 tol_outcome;

  (* 5. Same problem in Classical / HermiteNormalized bases → same optimum
     (the basis only changes the internal certificate search, never the
     feasible set, so all three must agree). *)
  let t = make "t" in
  let ret, (v, _), _, _ = solve ~options:classical (Maximize t) [x*x + !1. - t] in
  check_success "Classical max-t solve" ret;
  check_value   "Classical max-t value" v 1.0 tol_outcome;

  let t = make "t" in
  let ret, (v, _), _, _ = solve ~options:hermite_n (Maximize t) [x*x + !1. - t] in
  check_success "HermiteNormalized max-t solve" ret;
  check_value   "HermiteNormalized max-t value" v 1.0 tol_outcome;

  (* 6. x^4+1 (degree-4) is SOS (Hermite). *)
  let ret, _, _, _ = solve ~options:hermite Purefeas [x*x*x*x + !1.] in
  check_success "Hermite x^4+1 feasible" ret;
  let ret, _, _, _ = solve ~options:hermite_n Purefeas [x*x*x*x + !1.] in
  check_success "HermiteNormalized x^4+1 feasible" ret;

(* ------------------------------------------------------------------ *)
(* Tests — Part 2: witness evaluation                                   *)
(*                                                                      *)
(* For each polynomial, solve in both bases, then verify that           *)
(* v(x)^T Q v(x) ≈ p(x) at several test points.  Classical and        *)
(* Hermite basis elements require different evaluators.                 *)
(* ------------------------------------------------------------------ *)

  (* 7. x^2+1: witness in all three bases. *)
  let _, _, _, ws_c = solve ~options:classical Purefeas [x*x + !1.] in
  let _, _, _, ws_h = solve ~options:hermite   Purefeas [x*x + !1.] in
  let _, _, _, ws_hn = solve ~options:hermite_n Purefeas [x*x + !1.] in
  let w_c = only1 ws_c and w_h = only1 ws_h and w_hn = only1 ws_hn in
  let pts1d = [[-1.5]; [-1.]; [0.]; [0.7]; [2.]; [-0.3]] in
  check_witness "Classical witness x^2+1" eval_classical_mono w_c pts1d p_x2_1;
  check_witness "Hermite   witness x^2+1" eval_hermite_mono   w_h pts1d p_x2_1;
  check_witness "HermiteNormalized witness x^2+1" eval_hermite_normalized_mono w_hn pts1d p_x2_1;

  (* 8. x^2+y^2+1: witness in all three bases. *)
  let _, _, _, ws_c = solve ~options:classical Purefeas [x*x + y*y + !1.] in
  let _, _, _, ws_h = solve ~options:hermite   Purefeas [x*x + y*y + !1.] in
  let _, _, _, ws_hn = solve ~options:hermite_n Purefeas [x*x + y*y + !1.] in
  let w_c = only1 ws_c and w_h = only1 ws_h and w_hn = only1 ws_hn in
  let pts2d = [[0.;0.]; [1.;0.]; [0.;1.]; [1.;1.];
               [-0.5; 1.5]; [2.; -1.]; [0.3; 0.7]] in
  check_witness "Classical witness x^2+y^2+1" eval_classical_mono w_c pts2d p_x2y2_1;
  check_witness "Hermite   witness x^2+y^2+1" eval_hermite_mono   w_h pts2d p_x2y2_1;
  check_witness "HermiteNormalized witness x^2+y^2+1" eval_hermite_normalized_mono w_hn pts2d p_x2y2_1;

  (* 9. x^4+1: witness in all three bases. *)
  let _, _, _, ws_c = solve ~options:classical Purefeas [x*x*x*x + !1.] in
  let _, _, _, ws_h = solve ~options:hermite   Purefeas [x*x*x*x + !1.] in
  let _, _, _, ws_hn = solve ~options:hermite_n Purefeas [x*x*x*x + !1.] in
  let w_c = only1 ws_c and w_h = only1 ws_h and w_hn = only1 ws_hn in
  check_witness "Classical witness x^4+1" eval_classical_mono w_c pts1d p_x4_1;
  check_witness "Hermite   witness x^4+1" eval_hermite_mono   w_h pts1d p_x4_1;
  check_witness "HermiteNormalized witness x^4+1" eval_hermite_normalized_mono w_hn pts1d p_x4_1;

  (* 10. check function works on Classical witness (Hermite-unaware). *)
  let ret, _, _, ws = solve ~options:classical Purefeas [x*x*x*x + !1.] in
  let w_c = only1 ws in
  check_success "Classical x^4+1 solve for check" ret;
  if not (check (x*x*x*x + !1.) w_c) then begin
    Format.eprintf "FAIL Classical check x^4+1@."; exit 1
  end;

  Format.printf "All Hermite SOS tests passed.@."
