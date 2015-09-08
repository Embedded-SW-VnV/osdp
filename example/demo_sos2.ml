(* Example from Yalmip tutorial:
   min (1+x y)^2 - x y + (1 - y)^2 s.t. |x| <= 1, |y| <= 1
   or max t s.t. (1+x y)^2 - x y + (1 - y)^2 - t
                 - q1 (1-x) - q2 (1+X) - q3 (1-y) - q4 (1+y) is SOS
   and q1, q2, q3 and q4 are SOS. *)

let solver = Osdp.Sdp.Mosek

open Osdp.Sos.Float

let _ =
  let lower = var "lower" in
  let q1, _ = var_poly "q1" 2 2 in
  let q2, _ = var_poly "q2" 2 2 in
  let q3, _ = var_poly "q3" 2 2 in
  let q4, _ = var_poly "q4" 2 2 in
  let p = <:sos< (1 + x0 x1)^2 - x0 x1 + (1 - x1)^2 >> in
  let e = <:sos< p - lower - q1 * (1-x0) - q2 * (1+x0) - q3 * (1-x1) - q4 * (1+x1) >> in
  let () = Format.printf "e = %a@." pp e in
  let ret, (pobj, dobj), vars, _ =
    solve ~solver (Maximize lower) [e; q1; q2; q3; q4] in
  let () = Format.printf "ret = %a@." Osdp.SdpRet.pp ret in
  let () = Format.printf "pobj, dobj = %g, %g@." pobj dobj in
  try
    let lower' = value lower vars in
    Format.printf "%a = %g@." pp lower lower'
  with Not_found -> Format.printf "SDP error.@."
