(* Generates the osdp-side rows of the full solver x basis x padding
   comparison table (see scripts/full_test_comparison.sh). Reuses the
   same six test polynomials and the same exact-coefficient residual
   technique as test_residuals.ml, but loops over every combination
   instead of a single fixed configuration, and times each solve.
   Emits one CSV line per combination on stdout:
     suite;solver;basis;pad;success;sdpret;max_residual;time_sec *)

module Sos = Osdp.Sos.Float
module H   = Osdp.SosBasis.Hermite   (Osdp.Scalar.Q)
module HN  = Osdp.SosBasis.HermiteNormalized (Osdp.Scalar.Q)
module C   = Osdp.SosBasis.Classical (Osdp.Scalar.Q)
module M   = Osdp.Monomial

let tol = 1e-4

let solvers =
  [ "csdp", Osdp.Sdp.Csdp;
    "mosek", Osdp.Sdp.Mosek;
    "sdpa", Osdp.Sdp.Sdpa;
    "sdpa_gmp", Osdp.Sdp.SdpaGmp;
    "sdpa_dd", Osdp.Sdp.SdpaDd ]

let bases =
  [ "classical", Osdp.SosBasis.Classical;
    "hermite", Osdp.SosBasis.Hermite;
    "hermite_normalized", Osdp.SosBasis.HermiteNormalized ]

let pads = [2.; 0.]

(* [p], its expected classical coefficients, and which basis-specific
   prod_to_classical to use for the residual reconstruction (same set
   as test_residuals.ml). *)
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

let max_coeff_residual expected computed =
  let all_monoms =
    let add_keys map set = M.Map.fold (fun m _ s -> M.Set.add m s) map set in
    add_keys expected (add_keys computed M.Set.empty)
  in
  M.Set.fold (fun m acc ->
    let exp = try M.Map.find m expected with Not_found -> 0. in
    let got = try M.Map.find m computed with Not_found -> 0. in
    max acc (abs_float (exp -. got))
  ) all_monoms 0.

let of_coeffs l =
  List.fold_left (fun map (m, c) -> M.Map.add m c map) M.Map.empty l

let prod_to_classical_of_basis = function
  | Osdp.SosBasis.Classical -> C.prod_to_classical
  | Osdp.SosBasis.Hermite -> H.prod_to_classical
  | Osdp.SosBasis.HermiteNormalized -> HN.prod_to_classical

(* The six test polynomials from test_residuals.ml, each with its
   expected classical coefficients (used to compute the residual). *)
let test_polys () =
  let open Sos in
  let x = ??0 and y = ??1 in
  [ "x^2+1",       x*x + !1.,
    [ M.of_list [], 1.; M.of_list [2], 1. ];
    "x^2+y^2+1",   x*x + y*y + !1.,
    [ M.of_list [], 1.; M.of_list [2], 1.; M.of_list [0;2], 1. ];
    "x^4+1",       x*x*x*x + !1.,
    [ M.of_list [], 1.; M.of_list [4], 1. ];
    "x^4-2x^2+3",  x*x*x*x - 2. *. (x*x) + !3.,
    [ M.of_list [], 3.; M.of_list [2], -2.; M.of_list [4], 1. ];
    "x^4+y^4+1",   x*x*x*x + y*y*y*y + !1.,
    [ M.of_list [], 1.; M.of_list [4], 1.; M.of_list [0;4], 1. ] ]

let run_one (solver_name, solver) (basis_name, basis) pad =
  let open Sos in
  let options =
    { default with basis; pad; sdp = { Osdp.Sdp.default with verbose = 0 } } in
  let prod_to_classical = prod_to_classical_of_basis basis in
  let t0 = Unix.gettimeofday () in
  try
    let worst_ret = ref Osdp.SdpRet.Success in
    let worst_residual = ref 0. in
    List.iter (fun (_label, p, expected_coeffs) ->
      let ret, _, _, ws = solve ~options ~solver Purefeas [p] in
      (match ret with
       | Osdp.SdpRet.Success -> ()
       | _ -> if worst_ret.contents = Osdp.SdpRet.Success then worst_ret := ret);
      if Osdp.SdpRet.is_success ret then begin
        let witness = match ws with [w] -> w | _ -> assert false in
        let computed = gram_as_classical prod_to_classical witness in
        let residual = max_coeff_residual (of_coeffs expected_coeffs) computed in
        if residual > worst_residual.contents then worst_residual := residual
      end
    ) (test_polys ());
    let t1 = Unix.gettimeofday () in
    let success =
      Osdp.SdpRet.is_success worst_ret.contents
      && Stdlib.(<=) worst_residual.contents tol in
    Printf.printf "CSV;osdp;%s;%s;%g;%b;%s;%.3e;%.4f\n%!"
      solver_name basis_name pad success
      (Format.asprintf "%a" Osdp.SdpRet.pp worst_ret.contents)
      worst_residual.contents (t1 -. t0)
  with e ->
    let t1 = Unix.gettimeofday () in
    Printf.printf "CSV;osdp;%s;%s;%g;false;error(%s);nan;%.4f\n%!"
      solver_name basis_name pad (Printexc.to_string e) (t1 -. t0)

let () =
  List.iter (fun s ->
    List.iter (fun b ->
      List.iter (fun p -> run_one s b p) pads
    ) bases
  ) solvers
