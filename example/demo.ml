let solver = Osdp.Sdp.Csdp  (* Sdp.Mosek *)

open Osdp.Lmi.Float

let _ =
  Format.printf "LMI@.";
  let a = Mat.of_list_list [[1.5; -0.7]; [1.; 0.]] in
  let p_id = Osdp.Ident.create "p" in
  let e1 = MEsub (MEvar p_id,
                  MEmult (MEmult (MEtranspose (MEconst a),
                                  MEvar p_id),
                          MEconst a)) in
  let e2 = MEsub (MEvar p_id, MEeye 2) in
  (* let e1 = << ?p_id - a' * ?p_id * a >> in *)
  (* let e2 = << ?p_id - eye(2) >> in *)
  Format.printf "e1 = %a@." pp e1;
  Format.printf "e2 = %a@." pp e2;
  let _, _, vars = solve ~solver Purefeas [e1; e2] in
  try
    let p = Osdp.Ident.Map.find p_id vars in
    match p with
    | Scalar _ -> Format.printf "SDP error.@."
    | Mat p -> Format.printf "%a = %a@." Osdp.Ident.pp p_id Mat.pp p
  with Not_found -> Format.printf "SDP error.@."

let _ = Format.printf "@."

open Osdp.Sos.Float

let _ =
  Format.printf "SOS@.";
  let deg = 4 in
  let names = ["x"; "y"] in
  let pp = pp ~names in
  let pp_poly = Poly.pp ~names in
  let pol_of_list l =
    Poly.of_list (List.map (fun (s, m) -> Osdp.Monomial.of_list m, s) l) in
  let a =
    List.map pol_of_list [[1.5, [1]; -0.7, [0; 1]]; [1., [1]; 0., [0; 1]]] in
  let p = { name = Osdp.Ident.create "p";
            nb_vars = 2;
            degree = deg;
            homogeneous = true } in
  let e1 = PLsub (PLvar p,
                  PLcompose (PLvar p, List.map (fun p -> PLconst p) a)) in
  let e2 = PLsub (PLvar p, PLconst (pol_of_list [1., [deg]; 1., [0; deg]])) in
  (* let e1 = << ?p - p(1.5 x0 - 0.7 x1, x0) >> in *)
  (* let e2 = << ?p - (x0^4 + x1^4) >> in *)
  Format.printf "e1 = %a@." pp e1;
  Format.printf "e2 = %a@." pp e2;
  let _, _, vars = solve ~solver Purefeas [e1; e2] in
  try
    let p' = Osdp.Ident.Map.find p.name vars in
    match p' with
    | Scalar _ -> Format.printf "SDP error.@."
    | Poly p' -> Format.printf "%a = %a@." Osdp.Ident.pp p.name pp_poly p'
  with Not_found -> Format.printf "SDP error.@."
