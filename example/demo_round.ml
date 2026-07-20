let solver = Osdp.Sdp.Csdp

module Sos = Osdp.Sos.Q

let psatz d eqs leqs pol =
  let n =
    List.fold_left
      (fun n p -> max n (Sos.Poly.nb_vars p))
      0 (pol :: eqs @ leqs) in
  let rec enumerate_products d ps =
    if d < 0 then [] else if d = 0 then [Sos.Poly.one] else
      match ps with
      | [] -> [Sos.Poly.one]
      | p :: ps ->
        let dp = Sos.Poly.degree p in
        List.map (Sos.Poly.mult p) (enumerate_products (d - dp) ps)
        @ enumerate_products d ps in
  let products =
    enumerate_products d leqs
    |> List.filter (fun p -> Sos.Poly.(compare p one) <> 0) in
  let lambdas, sigmas, sum =
    let sigmas, sum =
      List.fold_left
        (fun (sigmas, sum) p ->
           let sigma =
             let d = (d - Sos.Poly.degree p) / 2 * 2 in
             Sos.make ~n ~d "s" in
           sigma :: sigmas, Sos.(sigma * !!p + sum))
        ([], Sos.(~- !!pol)) products in
    let lambdas, sum =
      List.fold_left
        (fun (lambdas, sum) p ->
           let lambda = Sos.make ~n ~d:(d - Sos.Poly.degree p) "l" in
           lambda :: lambdas, Sos.(lambda * !!p + sum))
        ([], sum) eqs in
    lambdas, sigmas, Sos.(~- sum) in
  (* Format.printf "sum = %a@." (Sos.pp_names ["a"; "b"; "x"]) sum; *)
  let options = {
    Sos.default with
    Sos.verbose = 0(*3*);
    Sos.scale = false;
    Sos.trace_obj = true;
    Sos.dualize = true;
    Sos.monoms = [Osdp.Monomial.list_le n (d / 2 * 2)];
    Sos.pad = 0.;
    Sos.sdp = {
      Osdp.Sdp.default with
      Osdp.Sdp.verbose = 1;
      Osdp.Sdp.solver = solver } } in
  let ret, _, values, wl = Sos.solve ~options Sos.Purefeas (sum :: sigmas) in
  let res =
    if not (Osdp.SdpRet.is_success ret) then None
    else Sos.check_round ~options ~values (sum :: sigmas) wl in
  match res with
  | None -> None
  | Some (values, _) -> Some (lambdas, sigmas, sum, values)

let _ =
  (* attempt to prove
     a1 >= 0 /\ a2 >= 0 /\
     a1 * a1 + a2 * a2 = b1 * b1 + b2 * b2 + 2 /\
     a1 * b1 + a2 * b2 = 0 /\
     b1 * b2 - a1 * a2 > 0
     is unsat *)
  let names = ["a1"; "a2"; "b1"; "b2"] in
  let a1, a2, b1, b2 = Sos.Poly.(??0, ??1, ??2, ??3) in
  let les = Sos.Poly.([a1; a2]) in
  let eqs = Sos.Poly.([a1 * a1 + a2 * a2 - b1 * b1 - b2 * b2 - !(Q.of_int 2);
                       a1 * b1 + a2 * b2]) in
  let lts = Sos.Poly.([b1 * b2 - a1 * a2]) in

  (* attempt to prove
     3 * x + 7 * a < 4 /\ 3 < 2 * x /\ a >= 0
     is unsat *)
  (* let names = ["x"; "a"] in *)
  (* let x, a = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([a]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([!(Q.of_int 4) - Q.of_int 3 *. x - Q.of_int 7 *. a; *)
  (*                      !(Q.of_int (-3)) + Q.of_int 2 *. x]) in *)

  (* attempt to prove
     b ^ 2 < 4 * a * c /\ a * x ^ 2 + b * x + c = 0
     is unsat by proving
     - 4 * a ^ 2 * x ^ 2 - 4 * a * b * x - b ^ 2 > 0
     is unsat *)
  (* let names = ["a"; "b"; "x"] in *)
  (* let a, b, x = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([Q.of_int (-4) *. a ** 2 * x ** 2 - Q.of_int 4 *. a * b * x - b ** 2]) in *)

  (* attempt to prove
     0 <= x /\ x <= 1 /\ 0 <= y /\ y <= 1
     /\ x ^ 2 + y ^ 2 >= 1
     /\ (x - 1) ^ 2 + y ^ 2 >= 1
     /\ x ^ 2 + (y - 1) ^ 2 >= 1
     /\ (x - 1) ^ 2 + (y - 1) ^ 2 >= 1
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = *)
  (*   let un = Sos.Poly.(!(Q.one)) in *)
  (*   Sos.Poly.([x; un - x; y; un - y; *)
  (*              x ** 2 + y ** 2 - un; *)
  (*              (x - un) ** 2 + y ** 2 - un; *)
  (*              x ** 2 + (y - un) ** 2 - un; *)
  (*              (x - un) ** 2 + (y - un) ** 2 - un; *)
  (*              (x - un) ** 2 + (y - un) ** 2 - un]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([]) in *)

  (* attempt to prove
     0 <= b /\ 0 <= c /\ 0 <= x /\ 0 <= y /\
     (x ^ 2 = c) /\ (y ^ 2 = a ^ 2 * c + b)
     /\ a * c > y * x
     is unsat by proving
     0 <= x /\ 0 <= y /\ 0 <= x ^ 2
     /\ 0 <= y ^ 2 - a ^ 2 * x ^2
     /\ 0 < a * x ^2 - x * y
     is unsat *)
  (* let names = ["x"; "y"; "a"] in *)
  (* let x, y, a = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([x; y; x * x; y ** 2 - a ** 2 * x ** 2]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([a * x ** 2 - x * y]) in *)

  (* attempt to prove
     0 <= x /\ 0 <= y /\ 0 <= z /\ x + y + z <= 3
     /\ x * y + x * z + y * z < 3 * x * y * z
     is unsat *)
  (* let names = ["x"; "y"; "z"] in *)
  (* let x, y, z = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([x; y; z; !(Q.of_int 3) - x - y - z]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([Q.of_int 3 *. x * y * z - x * y - x * z - y * z]) in *)

  (* attempt to prove
     x ^ 2 + y ^ 2 + z ^ 2 = 1) /\ (x + y + z) ^ 2 > 3
     is unsat *)
  (* let names = ["x"; "y"; "z"] in *)
  (* let x, y, z = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([]) in *)
  (* let eqs = Sos.Poly.([x ** 2 + y ** 2 + z ** 2 - !(Q.of_int 1)]) in *)
  (* let lts = Sos.Poly.([(x + y + z) ** 2 - !(Q.of_int 3)]) in *)

  (* attempt to prove
     w ^ 2 + x ^ 2 + y ^ 2 + z ^ 2 = 1
     /\ (w + x + y + z) ^ 2 > 4
     is unsat *)
  (* let names = ["x"; "y"; "z"; "w"] in *)
  (* let x, y, z, w = Sos.Poly.(??0, ??1, ??2, ??3) in *)
  (* let les = Sos.Poly.([]) in *)
  (* let eqs = Sos.Poly.([w ** 2 + x ** 2 + y ** 2 + z ** 2 - !(Q.of_int 1)]) in *)
  (* let lts = Sos.Poly.([(w + x + y + z) ** 2 - !(Q.of_int 4)]) in *)

  (* attempt to prove
     x >= 1 /\ y >= 1 /\ x * y < x + y - 1
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([x - !(Q.of_int 1); y - !(Q.of_int 1)]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([x + y - !(Q.of_int 1) - x * y]) in *)

  (* attempt to prove
     x > 1 /\ y > 1 /\ x * y <= x + y - 1
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([x + y - !(Q.of_int 1) - x * y]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([x - !(Q.of_int 1); y - !(Q.of_int 1)]) in *)

  (* attempt to prove
     2 <= x /\ x <= 125841 / 50000 /\
     2 <= y /\ y <= 125841 / 50000 /\
     2 <= z /\ z <= 125841 / 50000
     /\ 2 * (x * z + x * y + y * z) - (x * x + y * y + z * z) < 0
     is unsat *)
  (* let names = ["x"; "y"; "z"] in *)
  (* let x, y, z = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([x - !(Q.of_int 2); !(Q.of_ints 125841 50000) - x; *)
  (*                      y - !(Q.of_int 2); !(Q.of_ints 125841 50000) - y; *)
  (*                      z - !(Q.of_int 2); !(Q.of_ints 125841 50000) - z]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([(x * x + y * y + z * z) - Q.of_int 2 *. (x * z + x * y + y * z)]) in *)

  (* attempt to prove
     2 <= x /\ x <= 4 /\ 2 <= y /\ y <= 4 /\ 2 <= z /\ z <= 4
     /\ 2 * (x * z + x * y + y * z) - (x * x + y * y + z * z) < 0
     is unsat *)
  (* let names = ["x"; "y"; "z"] in *)
  (* let x, y, z = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([x - !(Q.of_int 2); !(Q.of_int 4) - x; *)
  (*                      y - !(Q.of_int 2); !(Q.of_int 4) - y; *)
  (*                      z - !(Q.of_int 2); !(Q.of_int 4) - z]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([(x * x + y * y + z * z) - Q.of_int 2 *. (x * z + x * y + y * z)]) in *)

  (* attempt to prove
     2 <= x /\ x <= 4 /\ 2 <= y /\ y <= 4 /\ 2 <= z /\ z <= 4
     /\ 2 * (x * z + x * y + y * z) - (x * x + y * y + z * z) < 12
     is unsat *)
  (* let names = ["x"; "y"; "z"] in *)
  (* let x, y, z = Sos.Poly.(??0, ??1, ??2) in *)
  (* let les = Sos.Poly.([x - !(Q.of_int 2); !(Q.of_int 4) - x; *)
  (*                      y - !(Q.of_int 2); !(Q.of_int 4) - y; *)
  (*                      z - !(Q.of_int 2); !(Q.of_int 4) - z]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([!(Q.of_int 12) + (x * x + y * y + z * z) - Q.of_int 2 *. (x * z + x * y + y * z)]) in *)

  (* attempt to prove  <- trop lent (il y a un truc mal codé quelque part)
     (x - y - 2 * x ^ 4 = 0) /\ 0 <= x /\ x <= 2 /\ 0 <= y /\ y <= 3
     /\ y ^ 2 - 7 * y - 12 * x + 17 >= 0
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([x; !(Q.of_int 2) - x; *)
  (*                      y; !(Q.of_int 3) - y; *)
  (*                      y ** 7 - Q.of_int 7 *. y - Q.of_int 12 *. x + !(Q.of_int 17)]) in *)
  (* let eqs = Sos.Poly.([x - y - Q.of_int 2 *. x ** 4]) in *)
  (* let lts = Sos.Poly.([]) in *)

  (* attempt to prove
     0 <= x /\ 0 <= y /\ (x * y = 1)
     /\ x + y > x ^ 2 + y ^ 2
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([x; y]) in *)
  (* let eqs = Sos.Poly.([x * y - !(Q.of_int 1)]) in *)
  (* let lts = Sos.Poly.([x + y - x ** 2 - y ** 2]) in *)

  (* attempt to prove
     0 <= x /\ 0 <= y /\ (x * y = &1)
     /\ x * y * (x + y) > x ^ 2 + y ^ 2
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([x; y]) in *)
  (* let eqs = Sos.Poly.([x * y - !(Q.of_int 1)]) in *)
  (* let lts = Sos.Poly.([x * y * (x + y) - x ** 2 - y ** 2]) in *)

  (* attempt to prove
     0 <= x /\ 0 <= y
     /\ x * y * (x + y) ^ 2 > (x ^ 2 + y ^ 2) ^ 2
     is unsat *)
  (* let names = ["x"; "y"] in *)
  (* let x, y = Sos.Poly.(??0, ??1) in *)
  (* let les = Sos.Poly.([x; y]) in *)
  (* let eqs = Sos.Poly.([]) in *)
  (* let lts = Sos.Poly.([x * y * (x + y) ** 2 - (x ** 2 + y ** 2) ** 2]) in *)

  let pol = List.fold_left Sos.Poly.mult Sos.Poly.one lts in
  let les = les @ lts in
  let try_deg d =
    Format.printf "Trying degree %d@." d;
    let e = Sos.Poly.degree pol in
    let k = if e = 0 then 0 else d / e in
    let rec find_pow i =
      if i > k then None else
        let () = Format.printf " Trying pow %d@." i in
        match psatz d eqs les Sos.Poly.(~- (pol ** i)) with
        | Some _ as r -> Format.printf "  psatz success@."; r
        | None -> find_pow (i + 1) in
    find_pow 0 in
  let rec find_deg d =
    if d > 12 then None else match try_deg d with
      | Some _ as r -> r
      | None -> find_deg (d + 1) in
  match find_deg 0 with
  | None -> Format.printf "No proof found@."
  | Some (lambdas, sigmas, sum, values) ->
    Format.printf "Proof found:@.";
    let ppe = Sos.pp_names names in
    let ppp = Sos.Poly.pp_names names in
    Format.printf "sum = %a@." ppe sum;
    List.iter
      (fun lambda ->
         Format.printf "%a = %a@." ppe lambda ppp (Sos.value_poly lambda values))
      lambdas;
    List.iter
      (fun sigma ->
         Format.printf "%a = %a@." ppe sigma ppp (Sos.value_poly sigma values))
      sigmas
