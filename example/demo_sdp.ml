(* Simple SDP example (from SDPA manual):
   maximize   tr([-11, 0; 0, 23] X)
   subject to tr([10, 4; 4, 0] X) = 48
              tr([0, 0; 0, -8] X) = -8
              tr([0, -8; -8, -2] X) = 20
              X sdp *)

let options = { Osdp.Sdp.default with
                Osdp.Sdp.solver = Osdp.Sdp.Sdpa;
                Osdp.Sdp.verbose = 1 }

(* Uncomment the following to use SDPA-GMP or SDPA-DD: *)

(* let options = { *)
(*   Osdp.Sdp.solver = Osdp.Sdp.SdpaGmp; *)
(*   (\* Osdp.Sdp.solver = Osdp.Sdp.SdpaDd; *\) *)
(*   verbose = 1; *)
(*   max_iteration = 200; *)
(*   stop_criterion = 1.0E-30; *)
(*   initial = 1.0E4; *)
(*   precision = 200 *)
(* } *)

let _ =
  let obj = [1, [0, 0, -11.; 1, 1, 23.]] in
  let c1 = Osdp.Sdp.Eq ([1, [0, 0, 10.; 1, 0, 4.]], 48.) in
  let c2 = Osdp.Sdp.Eq ([1, [1, 1, -8.]], -8.) in
  let c3 = Osdp.Sdp.Eq ([1, [1, 0, -8.; 1, 1, -2.]], 20.) in
  let ret, (pobj, dobj), (rX, ry, rZ) =
    Osdp.Sdp.solve_sparse ~options obj [c1; c2; c3] in
  Format.printf "ret = %a@." Osdp.SdpRet.pp ret;
  Format.printf "pobj, dobj = %g, %g@." pobj dobj; 
  List.iter
    (fun (i, m) ->
     Format.printf
       "X%d = %a@." i
       Osdp.Matrix.Float.pp (Osdp.Matrix.Float.of_array_array m))
    rX;
  Array.iteri (fun i f -> Format.printf "y%d = %g@." i f) ry;
  List.iter
    (fun (i, m) ->
     Format.printf
       "Z%d = %a@." i
       Osdp.Matrix.Float.pp (Osdp.Matrix.Float.of_array_array m))
    rZ;
