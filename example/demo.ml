(* Uncomment following lines for use in the [ocaml] toplevel. *)
(* #use "topfind";; *)
(* #require "osdp";; *)

let solver = Osdp.Sdp.Csdp  (* Sdp.Mosek *)

open Osdp.Lmi.Float

let () = Format.printf "LMI@."
let a = Mat.of_list_list [[1.5; -0.7]; [1.; 0.]]
let p_id = Osdp.Ident.create "p"
let a = << [1.5, -0.7; 1, 0] >>  (* or <:lmi< ... > *)
let e1 = << ?p_id - a ' * ?p_id * a >>
let e2 = << ?p_id - eye(2) >>
let () = Format.printf "e1 = %a@." pp e1
let () = Format.printf "e2 = %a@." pp e2
let _, _, vars = solve ~solver Purefeas [e1; e2]
let () =
  try
    let p = Osdp.Ident.Map.find p_id vars in
    match p with
    | Scalar _ -> Format.printf "SDP error.@."
    | Mat p -> Format.printf "%a = %a@." Osdp.Ident.pp p_id Mat.pp p
  with Not_found -> Format.printf "SDP error.@."

let () = Format.printf "@."

open Osdp.Sos.Float

let () = Format.printf "SOS@."
let deg = 4
let names = ["x"; "y"]
let pp = pp ~names
let pp_poly = Poly.pp ~names
let p = { name = Osdp.Ident.create "p";
          nb_vars = 2;
          degree = deg;
          homogeneous = true }
let a0 = <:sos< 1.5 x0 - 0.7 x1 >>
let a1 = <:sos< x0 >>
let e1 = <:sos< ?p - ?p(a0, x0) >>
(* or let l = [a0; a1] in let e1 = <:sos< ?p - ?p($l$) >> *)
let e2 = <:sos< ?p - (x0^4 + x1^4) >>
let () = Format.printf "e1 = %a@." pp e1
let () = Format.printf "e2 = %a@." pp e2
let _, _, vars = solve ~solver Purefeas [e1; e2]
let () =
  try
    let p' = Osdp.Ident.Map.find p.name vars in
    match p' with
    | Scalar _ -> Format.printf "SDP error.@."
    | Poly p' -> Format.printf "%a = %a@." Osdp.Ident.pp p.name pp_poly p'
  with Not_found -> Format.printf "SDP error.@."
