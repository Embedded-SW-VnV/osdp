(* Simple SDP example (from CSDP manual):
   maximize   tr(blkdiag([2, 1;
                          1, 2],
                         [3, 0, 1;
                          0, 2, 0;
                          1, 0, 3],
                         0) X)
   subject to tr(blkdiag([3, 1;
                          1, 3],
                         0,
                         [1, 0;
                          0, 0]) X) = 1
              tr(blkdiag(0,
                         [3, 0, 1;
                          0, 4, 0;
                          1; 0; 5],
                         [0, 0;
                          0, 1]) X) = 2
              X psd *)

let options = { Osdp.Sdp.default with
                Osdp.Sdp.solver = Osdp.Sdp.Csdp;
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
  let obj = [1, [0, 0, 2.; 1, 0, 1.; 1, 1, 2.];
             2, [0, 0, 3.; 1, 1, 2.; 2, 0, 1.; 2, 2, 3.]] in
  let c1 = Osdp.Sdp.Eq ([1, [0, 0, 3.; 1, 0, 1.; 1, 1, 3.];
                         3, [0, 0, 1.]],
                        1.) in
  let c2 = Osdp.Sdp.Eq ([2, [0, 0, 3.; 1, 1, 4.; 2, 0, 1.; 2, 2, 5.];
                         3, [1, 1, 1.]],
                        2.) in
  let init = None in
  let ret, (pobj, dobj), (rX, ry, rZ) =
    Osdp.Sdp.solve_sparse ~options ?init obj [c1; c2] in
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
