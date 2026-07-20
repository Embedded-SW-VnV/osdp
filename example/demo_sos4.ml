(* Example 1 from:
   Dejan Jovanovic
   Solving Nonlinear Integer Arithmetic with MCSAT
   VMCAI 2017

forall x y : int,
  not ((* H(x, y) := (x^2+y^2-1)^3 - x^2y^3 *)
       (* D(x, y) := H(x+1, 2y) *)
       (* J(x, y) := H(2x+1, 2y-1) *)
       D(x, y) < 0 /\ J(x, y) < 0).
*)

module Sos = struct
  include Osdp.Sos.Q
  let ( *. ) n m = Q.of_int n *. m
end

let options = { Sos.default with
                Sos.verbose = 3(*3*);
                (* Sos.pad = 0.; *)
                (* Sos.pad_list = [2.]; *)
                Sos.sdp =
                  { Osdp.Sdp.default with
                    Osdp.Sdp.verbose = 1;
                    Osdp.Sdp.solver = Osdp.Sdp.Csdp } }

let _ =
  let x, y = Sos.(??0, ??1) in
  let h = Sos.((x**2 + y**2 - !Q.one)**3 - x**2 * y**3) in
  Format.printf "h = %a@." Sos.pp h;
  let d = Sos.(compose h [x + !Q.one; 2 *. y]) in
  Format.printf "d = %a@." Sos.pp d;
  let j = Sos.(compose h [2 *. x + !Q.one; 2 *. y - !Q.one]) in
  Format.printf "j = %a@." Sos.pp j;
  let d = Sos.(!(Q.of_ints 1 96) * (- !Q.one - d)) in
  Format.printf "d = %a@." Sos.pp Sos.(of_list (to_list d));
  let j = Sos.(!(Q.of_ints 1 2) * j) in
  let j = Sos.(!(Q.of_ints 1 264) * (- !Q.one - j)) in
  Format.printf "j = %a@." Sos.pp Sos.(of_list (to_list j));
  let sd = Sos.make "sd" in
  let sj = Sos.make "sj" in
  let ret, _, vals, witnesses =
    Sos.solve
      ~options (* Sos.Purefeas *) (Sos.Minimize Sos.(sd + sj))
      [Sos.(- sd * d - sj * j); sd; sj] in
  let proved =
    match ret, witnesses with
    | Osdp.SdpRet.Success, (m, _) :: _ ->
       (* check that we proved sum > 0 (and not just sum >= 0) *)
       Array.length m > 0 && Osdp.Monomial.(compare m.(0) one) = 0
    | _ -> false in
  Format.printf "proved = %B@." proved;
