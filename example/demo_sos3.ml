(* Does: x <= 1 -> x^2 + 1 >= 0 hold ?
   Encoded as: x^2 + 1 - q (1 - x) SOS, q SOS *)

let solver = Osdp.Sdp.Csdp

module Sos = Osdp.Sos.Float

let _ =
  let q = Sos.make ~n:1 ~d:2 "q" in
  let p = Sos.(??0**2 + !1.) in
  let e = Sos.(p - q * (!1. - ??0)) in
  let () = Format.printf "e = %a@." Sos.pp e in
  let ret, _, _, _ = Sos.solve ~solver Sos.Purefeas [e; q] in
  Format.printf "ret = %a@." Osdp.SdpRet.pp ret
