(* Unit tests for the SosBasis module.
   Tests: to_classical, prod_to_classical, list_le/list_eq for both
   Classical and Hermite bases instantiated on Scalar.Q. *)

module H = Osdp.SosBasis.Hermite   (Osdp.Scalar.Q)
module C = Osdp.SosBasis.Classical (Osdp.Scalar.Q)
module M = Osdp.Monomial

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

(* Evaluate a [(Monomial.t * Q.t) list] classical polynomial at a float
   point.  Handles monomials shorter than [pt] (missing dims = x_i^0). *)
let eval_classical coeffs pt =
  List.fold_left (fun acc (m, c) ->
    let exps = M.to_list m in
    let xpow =
      let rec go es ps = match es, ps with
        | [], _ -> 1. | _, [] -> 1.
        | e :: et, x :: xt -> (x ** float_of_int e) *. go et xt
      in go exps pt
    in
    acc +. Osdp.Scalar.Q.to_float c *. xpow
  ) 0. coeffs

(* Probabilist's (monic) Hermite polynomial He_n(x): He_0=1, He_1=x,
   He_{n+1} = x He_n - n He_{n-1}. Leading coefficient is always 1. *)
let rec herm n x =
  match n with
  | 0 -> 1.
  | 1 -> x
  | _ -> x *. herm (n-1) x -. float_of_int (n-1) *. herm (n-2) x

let tol = 1e-10

let fail fmt = Format.kasprintf (fun s -> print_string s; exit 1) fmt

let check label got expected =
  if abs_float (got -. expected) > tol then
    fail "FAIL %s: got %g, expected %g\n" label got expected

(* ------------------------------------------------------------------ *)
(* Hermite to_classical: verify H_n expands correctly as a classical    *)
(* polynomial by sampling at several x values.                          *)
(* ------------------------------------------------------------------ *)

let () =
  let xs = [0.; 1.; -1.; 1.7; -2.3; 0.5] in
  for n = 0 to 5 do
    let poly = H.to_classical (M.of_list [n]) in
    List.iter (fun x ->
      check (Printf.sprintf "H_%d(%g) classical" n x)
        (eval_classical poly [x]) (herm n x)
    ) xs
  done

(* ------------------------------------------------------------------ *)
(* Hermite to_classical: multivariate.                                  *)
(* H_{[a;b]}(x0,x1) = H_a(x0) * H_b(x1).                              *)
(* ------------------------------------------------------------------ *)

let () =
  let pts = [(1.3, -0.7); (0., 1.); (-1., -1.); (2., 0.5); (0.3, 1.8)] in
  let cases = [(2, 1); (1, 3); (3, 0); (0, 2); (2, 2)] in
  List.iter (fun (na, nb) ->
    let poly = H.to_classical (M.of_list [na; nb]) in
    List.iter (fun (x0, x1) ->
      check (Printf.sprintf "H_%d(%g)*H_%d(%g)" na x0 nb x1)
        (eval_classical poly [x0; x1])
        (herm na x0 *. herm nb x1)
    ) pts
  ) cases

(* Three-variable case H_{[1;2;1]} *)
let () =
  let poly = H.to_classical (M.of_list [1; 2; 1]) in
  let pts = [(1., -1., 0.5); (0.3, 0.7, -1.2); (-0.5, 1.5, 2.)] in
  List.iter (fun (x0, x1, x2) ->
    check (Printf.sprintf "H_1(%g)*H_2(%g)*H_1(%g)" x0 x1 x2)
      (eval_classical poly [x0; x1; x2])
      (herm 1 x0 *. herm 2 x1 *. herm 1 x2)
  ) pts

(* ------------------------------------------------------------------ *)
(* Hermite prod_to_classical: H_a(x) * H_b(x) in classical coords.     *)
(* ------------------------------------------------------------------ *)

let () =
  let xs = [0.; 1.; 1.5; -0.8; 2.1; -1.3] in
  let cases = [(0, 0); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2);
               (3, 1); (3, 2); (4, 0); (4, 2)] in
  List.iter (fun (na, nb) ->
    let prod = H.prod_to_classical (M.of_list [na]) (M.of_list [nb]) in
    List.iter (fun x ->
      check (Printf.sprintf "prod H_%d*H_%d at %g" na nb x)
        (eval_classical prod [x])
        (herm na x *. herm nb x)
    ) xs
  ) cases

(* Multivariate: H_{[a0;a1]} * H_{[b0;b1]} at several 2-d points. *)
let () =
  let pts = [(1.2, -0.5); (0., 1.); (-1., 0.7)] in
  let cases = [([1; 0], [0; 1]); ([2; 1], [1; 0]); ([1; 1], [1; 1])] in
  List.iter (fun (a, b) ->
    let prod = H.prod_to_classical (M.of_list a) (M.of_list b) in
    let ha = H.to_classical (M.of_list a) in
    let hb = H.to_classical (M.of_list b) in
    List.iter (fun (x0, x1) ->
      check (Printf.sprintf "prod H[%s]*H[%s] at (%g,%g)"
               (String.concat "," (List.map string_of_int a))
               (String.concat "," (List.map string_of_int b))
               x0 x1)
        (eval_classical prod [x0; x1])
        (eval_classical ha [x0; x1] *. eval_classical hb [x0; x1])
    ) pts
  ) cases

(* ------------------------------------------------------------------ *)
(* Classical prod_to_classical: m1 * m2 = m1+m2.                       *)
(* ------------------------------------------------------------------ *)

let () =
  let cases =
    [ [1],    [2],    [1.5];
      [3],    [1],    [0.8];
      [1; 2], [2; 0], [1.2; -0.5];
      [0; 1], [1; 1], [1.0; -1.0];
      [2; 0], [0; 3], [0.7;  1.4] ]
  in
  List.iter (fun (a, b, pt) ->
    let prod = C.prod_to_classical (M.of_list a) (M.of_list b) in
    let va = eval_classical [(M.of_list a, Osdp.Scalar.Q.one)] pt in
    let vb = eval_classical [(M.of_list b, Osdp.Scalar.Q.one)] pt in
    check (Printf.sprintf "Classical [%s]*[%s]"
             (String.concat "," (List.map string_of_int a))
             (String.concat "," (List.map string_of_int b)))
      (eval_classical prod pt) (va *. vb)
  ) cases

(* ------------------------------------------------------------------ *)
(* Enumeration: list_le and list_eq sizes.                              *)
(* ------------------------------------------------------------------ *)

let () =
  (* Univariate: list_le 1 d has d+1 elements (degrees 0..d). *)
  for d = 0 to 6 do
    let got = List.length (H.list_le 1 d) in
    if got <> d + 1 then
      fail "FAIL H.list_le 1 %d: got %d, expected %d\n" d got (d+1)
  done;
  (* Bivariate: list_le 2 d has C(d+2,2) = (d+1)(d+2)/2 elements. *)
  for d = 0 to 5 do
    let expected = (d + 1) * (d + 2) / 2 in
    let got = List.length (H.list_le 2 d) in
    if got <> expected then
      fail "FAIL H.list_le 2 %d: got %d, expected %d\n" d got expected
  done;
  (* Trivariate: list_le 3 d has C(d+3,3) = (d+1)(d+2)(d+3)/6 elements. *)
  for d = 0 to 4 do
    let expected = (d+1) * (d+2) * (d+3) / 6 in
    let got = List.length (H.list_le 3 d) in
    if got <> expected then
      fail "FAIL H.list_le 3 %d: got %d, expected %d\n" d got expected
  done;
  (* list_eq 1 d has exactly 1 element (H_d). *)
  for d = 0 to 5 do
    let got = List.length (H.list_eq 1 d) in
    if got <> 1 then
      fail "FAIL H.list_eq 1 %d: got %d, expected 1\n" d got
  done;
  (* Classical basis: same counts as Hermite (same index set). *)
  for d = 0 to 6 do
    let got = List.length (C.list_le 1 d) in
    if got <> d + 1 then
      fail "FAIL C.list_le 1 %d: got %d, expected %d\n" d got (d+1)
  done;
  for d = 0 to 5 do
    let expected = (d + 1) * (d + 2) / 2 in
    let got = List.length (C.list_le 2 d) in
    if got <> expected then
      fail "FAIL C.list_le 2 %d: got %d, expected %d\n" d got expected
  done

(* ------------------------------------------------------------------ *)
(* get_sos_size: should equal length of list_le n (d/2).               *)
(* ------------------------------------------------------------------ *)

let () =
  let cases = [(1,2); (1,4); (1,6); (2,2); (2,4); (3,2)] in
  List.iter (fun (n, d) ->
    let expected = List.length (H.list_le n (d / 2)) in
    let got = H.get_sos_size n d in
    if got <> expected then
      fail "FAIL H.get_sos_size %d %d: got %d, expected %d\n" n d got expected;
    let expected_c = List.length (C.list_le n (d / 2)) in
    let got_c = C.get_sos_size n d in
    if got_c <> expected_c then
      fail "FAIL C.get_sos_size %d %d: got %d, expected %d\n" n d got_c expected_c
  ) cases

let () = Format.printf "All SosBasis unit tests passed.@."
