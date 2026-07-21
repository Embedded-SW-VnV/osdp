(* Regression coverage for a Putinar-style SOS certificate that combines a
   domain-multiplier product with Compose, i.e. the shape used by
   "prove f(x) lies in a set" constructions:

     v(f(x)) - lambda(x) * s(x)   must be SOS,  lambda must be SOS

   where v is an unknown SOS polynomial in y-space, f is a change of
   variables applied through Compose, lambda is an unknown SOS multiplier,
   and s(x) is a domain-defining polynomial (e.g. 1-x0^2-x1^2 for the unit
   disk). This is exactly the pattern used by build_positivity_constraint /
   surapproximate_image in the saimcop project (approx/analyse.ml).

   Part 1 checks the two ingredients in isolation (Compose alone, and a
   multiplier product alone): both succeed in every basis.

   Part 2 combines them (the shape above). This used to be a known gap:
   Classical solved it, but Hermite and HermiteNormalized reported
   PrimalInfeasible on the exact same constraints, even though
   SOS-feasibility at a fixed Gram-matrix dimension cannot depend on which
   basis is used to search for the certificate (Classical succeeding was a
   witness that the problem is genuinely feasible). Root cause: unlike
   Classical.refine_basis (which prunes candidate Gram-matrix elements via
   the Newton polytope filter), Hermite/HermiteNormalized's refine_basis
   used to be the identity (no pruning), so the retained Gram matrix was
   considerably larger for the same nominal degree -- confirmed by forcing
   Hermite onto Classical's pruned candidate set via options.monoms, which
   flipped the same PrimalInfeasible into Success. Now that
   Hermite.refine_basis also delegates to the Newton polytope filter (see
   sosBasis.ml), all three bases succeed here. See TODO for the caveat
   that this reuses a filter derived from monomial semantics, which is an
   empirically-validated heuristic for Hermite indices rather than a
   proven-tight bound. *)

module Sos = Osdp.Sos.Float
module Polynomial = Osdp.Polynomial.Float

let bases =
  [ "Classical", Osdp.SosBasis.Classical;
    "Hermite", Osdp.SosBasis.Hermite;
    "HermiteNormalized", Osdp.SosBasis.HermiteNormalized ]

let check_success label ret =
  if not (Osdp.SdpRet.is_success ret) then begin
    Format.eprintf "FAIL %s: expected success, got %a@."
      label Osdp.SdpRet.pp ret;
    exit 1
  end

(* ------------------------------------------------------------------ *)
(* Part 1: the two ingredients in isolation.                           *)
(* ------------------------------------------------------------------ *)

(* Compose alone: v(f(x)) is SOS, no multiplier/domain constraint. *)
let () =
  let open Sos in
  let v = make ~n:1 ~d:2 "v" in
  let f = [const Polynomial.(!1. + ??0 * ??0)] in (* f(x) = 1 + x^2 *)
  let vf = compose v f in
  List.iter (fun (label, basis) ->
    let options = { default with basis } in
    let ret, _, _, _ = solve ~options Purefeas [vf] in
    check_success (Printf.sprintf "Compose alone (%s)" label) ret
  ) bases

(* Multiplier alone: p - lambda(x) * s(x) is SOS (with lambda itself SOS),
   no Compose involved. p = 3 (rather than the boundary case p = 0) so a
   strictly interior feasible point exists (e.g. lambda = 2 constant gives
   3 - 2*(1-x0^2-x1^2) = 1 + 2x0^2 + 2x1^2, strictly positive everywhere):
   a Purefeas problem whose only feasible point is the all-zero Gram
   matrix (Slater's condition failing) is a separate, known source of
   solver non-convergence unrelated to the basis choice, and would
   confound this test. *)
let () =
  let open Sos in
  let lambda = make ~n:2 ~d:2 "lambda" in
  let s = Polynomial.(!1. - ??0 * ??0 - ??1 * ??1) in (* unit disk *)
  let cons = Sos.(!3. - lambda * const s) in
  List.iter (fun (label, basis) ->
    let options = { default with basis } in
    let ret, _, _, _ = solve ~options Purefeas [cons; lambda] in
    check_success (Printf.sprintf "Multiplier alone (%s)" label) ret
  ) bases

(* ------------------------------------------------------------------ *)
(* Part 2: combined (Compose + multiplier product together).           *)
(* ------------------------------------------------------------------ *)

let () =
  let open Sos in
  let v = make ~n:1 ~d:2 "v" in
  let lambda = make ~n:2 ~d:0 "lambda" in
  let s = Polynomial.(!1. - ??0 * ??0 - ??1 * ??1) in (* unit disk *)
  let f = [const Polynomial.(??0 * ??0 + ??1)] in
  let vf = compose v f in
  let cons = Sos.(vf - lambda * const s) in
  let ret_classical, _, _, _ =
    solve ~options:{ default with basis = Osdp.SosBasis.Classical }
      Purefeas [cons; lambda] in
  check_success "Compose+multiplier combined (Classical)" ret_classical;

  let ret_hermite, _, _, _ =
    solve ~options:{ default with basis = Osdp.SosBasis.Hermite }
      Purefeas [cons; lambda] in
  check_success "Compose+multiplier combined (Hermite)" ret_hermite;

  let ret_hermite_n, _, _, _ =
    solve ~options:{ default with basis = Osdp.SosBasis.HermiteNormalized }
      Purefeas [cons; lambda] in
  check_success "Compose+multiplier combined (HermiteNormalized)" ret_hermite_n

let () = Format.printf "All Compose+multiplier tests passed.@."
