(* Coefficient residual tests for SOS witnesses.
   For a witness (v, Q) returned by solve, the SOS encoding guarantees
     p  ≈  v(x)^T Q v(x)
   coefficient-by-coefficient in the classical monomial basis.
   These tests verify that max_m |coeff(p,m) - coeff(v^T Q v, m)| < tol
   for both Classical and Hermite bases, using prod_to_classical to expand
   the Gram form without going through float point evaluation. *)

module Sos = Osdp.Sos.Float
module H   = Osdp.SosBasis.Hermite   (Osdp.Scalar.Q)
module C   = Osdp.SosBasis.Classical (Osdp.Scalar.Q)
module M   = Osdp.Monomial

(* ------------------------------------------------------------------ *)
(* Infrastructure                                                       *)
(* ------------------------------------------------------------------ *)

(* Expand v^T Q v as a classical polynomial stored in a Monomial.Map.
   [prod_to_classical] implements the basis-specific product:
     Classical: v_i * v_j = monomial product (coefficient 1)
     Hermite  : H_{v_i} * H_{v_j} = sum of classical monomials via
                the combinatorial product formula *)
let gram_as_classical prod_to_classical (v, q) =
  let n = Array.length v in
  let map = ref M.Map.empty in
  let add m c =
    let prev = try M.Map.find m !map with Not_found -> 0. in
    map := M.Map.add m (prev +. c) !map
  in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      List.iter (fun (m, coeff) ->
        add m (q.(i).(j) *. Osdp.Scalar.Q.to_float coeff)
      ) (prod_to_classical v.(i) v.(j))
    done
  done;
  !map

(* Maximum absolute coefficient difference between [expected] and [computed].
   Checks all monomials appearing in either map, so extra terms in the
   Gram form (which should be near zero) are also penalised. *)
let max_coeff_residual expected computed =
  let all_monoms =
    let add_keys map set =
      M.Map.fold (fun m _ s -> M.Set.add m s) map set in
    add_keys expected (add_keys computed M.Set.empty)
  in
  M.Set.fold (fun m acc ->
    let exp = try M.Map.find m expected with Not_found -> 0. in
    let got = try M.Map.find m computed with Not_found -> 0. in
    max acc (abs_float (exp -. got))
  ) all_monoms 0.

let of_coeffs l =
  List.fold_left (fun map (m, c) -> M.Map.add m c map) M.Map.empty l

let tol = 1e-4

let fail fmt = Format.kasprintf (fun s -> print_string s; exit 1) fmt

let check_residual label expected_coeffs prod_to_classical witness =
  let computed  = gram_as_classical prod_to_classical witness in
  let residual  = max_coeff_residual (of_coeffs expected_coeffs) computed in
  if Test_env.verbose then
    Printf.printf "%-28s residual = %.3e\n%!" label residual;
  if residual > tol then
    fail "FAIL %s: coefficient residual = %.2e (tol = %.2e)\n"
      label residual tol

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let one_wit = function [w] -> w | _ -> assert false

(* Solve for feasibility, assert success, return the single witness. *)
let solve1 opts p =
  let open Sos in
  let ret, _, _, ws = solve ~options:opts Purefeas [p] in
  if not (Osdp.SdpRet.is_success ret) then begin
    Format.eprintf "solve failed: %a@." Osdp.SdpRet.pp ret; exit 1
  end;
  one_wit ws

(* ------------------------------------------------------------------ *)
(* Expected polynomial coefficients                                     *)
(* (Monomial.of_list strips trailing zeros, so [2;0] → [2].)           *)
(* ------------------------------------------------------------------ *)

(* x^2 + 1 *)
let p_x2_1_coeffs = [ M.of_list [],   1.;
                       M.of_list [2],  1. ]

(* x^2 + y^2 + 1  (y^2 → [0;2], x^2 → [2] after trailing-zero strip) *)
let p_x2y2_1_coeffs = [ M.of_list [],    1.;
                         M.of_list [2],   1.;
                         M.of_list [0;2], 1. ]

(* x^4 + 1 *)
let p_x4_1_coeffs = [ M.of_list [],   1.;
                       M.of_list [4],  1. ]

(* x^4 - 2x^2 + 3  (= (x^2-1)^2 + 2, clearly SOS) *)
let p_x4_m2x2_3_coeffs = [ M.of_list [],   3.;
                             M.of_list [2], -2.;
                             M.of_list [4],  1. ]

(* x^4 + y^4 + 1  — degree-4 bivariate with constant, clearly SOS *)
let p_x4y4_1_coeffs = [ M.of_list [],    1.;
                         M.of_list [4],   1.;
                         M.of_list [0;4], 1. ]

(* ------------------------------------------------------------------ *)
(* Tests                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  let open Sos in
  let x = ??0 and y = ??1 in
  let cl = Test_env.override { default with basis = Osdp.SosBasis.Classical } in
  let hm = Test_env.override { default with basis = Osdp.SosBasis.Hermite   } in

  (* x^2 + 1 *)
  let p = x*x + !1. in
  check_residual "Classical x^2+1"    p_x2_1_coeffs C.prod_to_classical (solve1 cl p);
  check_residual "Hermite   x^2+1"    p_x2_1_coeffs H.prod_to_classical (solve1 hm p);

  (* x^2 + y^2 + 1 *)
  let p = x*x + y*y + !1. in
  check_residual "Classical x^2+y^2+1"  p_x2y2_1_coeffs C.prod_to_classical (solve1 cl p);
  check_residual "Hermite   x^2+y^2+1"  p_x2y2_1_coeffs H.prod_to_classical (solve1 hm p);

  (* x^4 + 1 *)
  let p = x*x*x*x + !1. in
  check_residual "Classical x^4+1"    p_x4_1_coeffs C.prod_to_classical (solve1 cl p);
  check_residual "Hermite   x^4+1"    p_x4_1_coeffs H.prod_to_classical (solve1 hm p);

  (* x^4 - 2x^2 + 3 *)
  let p = x*x*x*x - 2. *. (x*x) + !3. in
  check_residual "Classical x^4-2x^2+3"  p_x4_m2x2_3_coeffs C.prod_to_classical (solve1 cl p);
  check_residual "Hermite   x^4-2x^2+3"  p_x4_m2x2_3_coeffs H.prod_to_classical (solve1 hm p);

  (* x^4 + y^4 + 1  — degree-4 bivariate SOS *)
  let p = x*x*x*x + y*y*y*y + !1. in
  check_residual "Classical x^4+y^4+1" p_x4y4_1_coeffs C.prod_to_classical (solve1 cl p);
  check_residual "Hermite   x^4+y^4+1" p_x4y4_1_coeffs H.prod_to_classical (solve1 hm p);

  Format.printf "All residual tests passed.@."
