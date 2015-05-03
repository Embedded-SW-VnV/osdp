(* Example from Yalmip tutorial:
   min (1+x y)^2 - x y + (1 - y)^2
   or max t s.t. (1+x y)^2 - x y + (1 - y)^2 - t is SOS. *)

let solver = Osdp.Sdp.Sdpa

(* SOS decomposition done manually. *)
let _ =
  let obj = [12, 1.], [] in
  let c1 = [12, 1.], [0, 0, 1.], 2. in
  let c2 = [], [1, 0, 1.], (-2.) in
  let c3 = [], [2, 0, 1.], 1. in
  let c4 = [], [1, 1, 1.], 1. in
  let c5 = [], [2, 1, 1.], 0. in
  let c6 = [], [2, 2, 1.], 1. in
  let c =
    List.map
      (fun (v, m, b) -> v, [42, m], b, b)
    [c1; c2; c3; c4; c5; c6] in
  let ret, (pobj, dobj), (rx, rX, ry) =
    Osdp.Sdp.solve_ext_sparse ~solver obj c [12, neg_infinity, infinity] in
  Format.printf "ret = %a@." Osdp.SdpRet.pp ret;
  Format.printf "pobj, dobj = %g, %g@." pobj dobj; 
  List.iter
    (fun (i, f) -> Format.printf "x%d = %g@." i f)
    rx;
  List.iter
    (fun (i, m) ->
     Format.printf
       "X%d = %a@." i
       Osdp.Matrix.Float.pp (Osdp.Matrix.Float.of_array_array m))
    rX;
  Array.iteri (fun i f -> Format.printf "y%d = %g@." i f) ry

(* Using SOS module from OSDP. *)
let _ =
  let lower = Osdp.Sos.Float.var "lower" in
  let p = <:sos< (1 + x0 x1)^2 - x0 x1 + (1 - x1)^2 >> in
  let e = <:sos< p - lower >> in
  let () = Format.printf "e = %a@." Osdp.Sos.Float.pp e in
  let ret, (pobj, dobj), vars, _ =
    Osdp.Sos.Float.solve ~solver (Osdp.Sos.Float.Maximize lower) [e] in
  let () = Format.printf "ret = %a@." Osdp.SdpRet.pp ret in
  let () = Format.printf "pobj, dobj = %g, %g@." pobj dobj in
  try
    let lower' = Osdp.Sos.Float.value lower vars in
    Format.printf "%a = %g@." Osdp.Sos.Float.pp lower lower'
  with Not_found -> Format.printf "SDP error.@."
