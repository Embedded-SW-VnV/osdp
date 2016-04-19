(* Prove that p >= 0 with
 * 
 * p(x1, x2) = 37 - x2^2 + x1^3 - 2 x1^2 x2 + 2 x2^3 - 12 x1^4
 *             - 10 x1^2 x2^2 - 6 x1 x2^3 - 6 x2^4
 *
 * is an inductive invariant, enabling to prove that (x1, x2)
 * remains in [-2, 2]^2 for the program
 *
 * (x1, x2) \in { x1, x2 | x1^2 <= 1 /\ x2^2 <= 1 }
 * while (1) {
 *   pre_x1 = x1; pre_x2 = x2;
 *   if (x1 >= x2) {
 *     x1 = 0.687 * pre_x1 + 0.558 * pre_x2 - 0.0001 * pre_x1 * pre_x2;
 *     x2 = -0.292 * pre_x1 + 0.773 * pre_x2;
 *   } else {
 *     x1 = 0.369 * pre_x1 + 0.532 * pre_x2 - 0.0001 * pre_x1ˆ2;
 *     x2 = -1.27 * pre_x1 + 0.12 * pre_x2 - 0.0001 * prex_x1 * pre_x2;
 *   }
 * }
 *)

module Sos = struct
  include Osdp.Sos.Q
  let ( / ) n m = Q.of_int n /. Q.of_int m
end

let options = { Sos.default with
                Sos.verbose = 0(*3*);
                Sos.sdp =
                  { Osdp.Sdp.default with
                    Osdp.Sdp.solver = Osdp.Sdp.Csdp } }

let x1, x2 = Sos.(??0, ??1)

(* initial condition x1^2 <= 1 encoded as 1 - x1^2 (>= 0) *)
let pI1 = Sos.(1 / 1 - x1**2)
(* initial condition x2^2 <= 1 *)
let pI2 = Sos.(1 / 1 - x2**2)
(* guard x1 <= x2 (then branch) *)
let g0 = Sos.(x1 - x2)
(* assignment in then branch *)
let t0 = Sos.([687 / 1000 * x1 + 558 / 1000 * x2 - 1 / 10000 * x1 * x2;
               (-292) / 1000 * x1 + 773 / 1000 * x2])
(* guard x1 >= x2 (else branch) *)
let g1 = Sos.(x2 - x1)
(* assignment in else branch *)
let t1 = Sos.([369 / 1000 * x1 + 532 / 1000 * x2 - 1 / 10000 * x1**2;
               (-127) / 100 * x1 + 12 / 100 * x2 - 1 / 10000 * x1 * x2])

let rnd n = n / 2 * 2

(* chack that invariant p >= 0 satisfy initial conditions and is inductive *)
let check_inv p =
  let deg = Sos.Poly.degree p in
  let check_init =
    (* p - \sigma1 pI1 - \sigma2 pI2 >= 0, \sigma{1,2} >= 0 *)
    let init, sigmas =
      let sigma1, _ = Sos.var_poly "s1" 2 (rnd (deg - Sos.degree pI1)) in
      let sigma2, _ = Sos.var_poly "s2" 2 (rnd (deg - Sos.degree pI2)) in
      Sos.(!!p - sigma1 * pI1 - sigma2 * pI2), [sigma1; sigma2] in
    let ret, _, _, _ = Sos.solve ~options Sos.Purefeas (init :: sigmas) in
    ret = Osdp.SdpRet.Success in
  Format.printf "  check_init: %B@." check_init;
  let check_t0 =
    (* p \circ t0 - \sigma p - \sigma_0 g0 >= 0, \sigma >= 0, \sigma_0 >=0 *)
    let ind0, sigmas =
      let deg0 = List.fold_left max 0 (List.map Sos.degree t0) in
      let sigma, _ = Sos.var_poly "s" 2 (rnd ((deg0 - 1) * deg)) in
      let sigma0, _ = Sos.var_poly "s0" 2 (rnd (deg * deg0 - Sos.degree g0)) in
      Sos.(compose !!p t0 - sigma * !!p - sigma0 * g0), [sigma; sigma0] in
    let ret, _, _, _ = Sos.solve ~options Sos.Purefeas (ind0 :: sigmas) in
    ret = Osdp.SdpRet.Success in
  Format.printf "  check_ind0: %B@." check_t0;
  let check_t1 =
    (* p \circ t1 - \sigma p - \sigma_1 g1 >= 0, \sigma >= 0, \sigma_1 >=0 *)
    let ind1, sigmas =
      let deg1 = List.fold_left max 0 (List.map Sos.degree t1) in
      let sigma, _ = Sos.var_poly "s" 2 (rnd ((deg1 - 1) * deg)) in
      let sigma1, _ = Sos.var_poly "s1" 2 (rnd (deg * deg1 - Sos.degree g1)) in
      Sos.(compose !!p t1 - sigma * !!p - sigma1 * g1), [sigma; sigma1] in
    let ret, _, _, _ = Sos.solve ~options Sos.Purefeas (ind1 :: sigmas) in
    ret = Osdp.SdpRet.Success in
  Format.printf "  check_ind1: %B@." check_t1;
  check_init && check_t0 && check_t1

(* chack that p >= 0 implies |x1| <= 2 /\ |x2| <= 2 *)
let check_cond p =
  let deg = Sos.Poly.degree p in
  let check_cond0 =
    (* \sigma (4 - x1^2) - p >= 0, \sigma > 0 *)
    let cond0, sigmas =
      let sigma, _ = Sos.var_poly "s" 2 (rnd (deg - 2)) in
      Sos.(sigma * (4 / 1 - ??0**2) - !!p), [sigma] in
    let ret, _, _, _ = Sos.solve ~options Sos.Purefeas (cond0 :: sigmas) in
    ret = Osdp.SdpRet.Success in
  Format.printf "  check_cond0 (|x1| <= 2): %B@." check_cond0;
  let check_cond1 =
    (* \sigma (4 - x2^2) - p >= 0, \sigma > 0 *)
    let cond1, sigmas =
      let sigma, _ = Sos.var_poly "s" 2 (rnd (deg - 2)) in
      Sos.(sigma * (4 / 1 - ??1**2) - !!p), [sigma] in
    let ret, _, _, _ = Sos.solve ~options Sos.Purefeas (cond1 :: sigmas) in
    ret = Osdp.SdpRet.Success in
  Format.printf "  check_cond1 (|x2| <= 2): %B@." check_cond1;
  check_cond0 && check_cond1

let check p =
  let c_inv = check_inv p in
  Format.printf "Invariant p >= 0 proved: %B@." c_inv;
  let c_cond = check_cond p in
  Format.printf "Condition proved: %B@." c_cond;
  Format.printf "Invariant and condition proved: %B@." (c_inv && c_cond)

let _ =
  let p =
    let i n = Sos.Poly.(!(Q.of_int n)) in
    let x1, x2 = Sos.Poly.(??0, ??1) in
    Sos.Poly.(i 37
              - i 1 * x2**2
              + i 1 * x1**3
              - i 2 * x1**2 * x2
              + i 2 * x2**3
              - i 12 * x1**4
              - i 10 * x1**2 * x2**2
              - i 6 * x1 * x2**3
              - i 6 * x2**4) in
  Format.printf "p = %a@." (Sos.Poly.pp_names ["x1"; "x2"]) p;
  check p
