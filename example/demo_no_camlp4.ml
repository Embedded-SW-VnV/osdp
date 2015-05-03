(* Uncomment following lines for use in the [ocaml] toplevel. *)
(* #use "topfind";; *)
(* #require "osdp";; *)

(* To compile: *)
(* % ocamlbuild -use-ocamlfind demo_no_camlp4.native *)
(* with a _tags file containing a line <*>: package(osdp) *)
(* or *)
(* % ocamlfind ocamlopt -linkpkg -package osdp -o demo_no_camlp4 demo_no_camlp4.ml *)
(* or directly *)
(* % ocamlopt -I $(ocamlfind query zarith) -I $(ocamlfind query osdp) zarith.cmxa osdp.cmxa -o demo_no_camlp4 demo_no_camlp4.ml *)

let solver = Osdp.Sdp.Csdp  (* Osdp.Sdp.Mosek, Osdp.Sdp.Sdpa,... *)

open Osdp.Lmi.Float

let () = Format.printf "LMI@."
let a = Mat.of_list_list [[1.5; -0.7]; [1.; 0.]]
let p = var "p"
let e1 = Sub (p,
              Mult (Mult (Transpose (Const a),
                          p),
                    Const a))
let e2 = Sub (p, Eye 2)
let () = Format.printf "e1 = %a@." pp e1
let () = Format.printf "e2 = %a@." pp e2
let _, _, vars = solve ~solver Purefeas [e1; e2]
let () =
  try
    let p' = value_mat p vars in
    Format.printf "%a = %a@." pp p Mat.pp p'
  with Not_found -> Format.printf "SDP error.@."

let () = Format.printf "@."

open Osdp.Sos.Float

let () = Format.printf "SOS@."
let deg = 4
let names = ["x"; "y"]
let pp = pp_names names
let pol_of_list l =
  Poly.of_list (List.map (fun (s, m) -> Osdp.Monomial.of_list m, s) l)
let a = List.map pol_of_list [[1.5, [1]; -0.7, [0; 1]]; [1., [1]; 0., [0; 1]]]
let p = var_poly "p" 2 ~homogen:true deg
let e1 = Sub (p, Compose (p, List.map (fun p -> Const p) a))
let e2 = Sub (p, Const (pol_of_list [1., [deg]; 1., [0; deg]]))
let () = Format.printf "e1 = %a@." pp e1
let () = Format.printf "e2 = %a@." pp e2
let _, _, vars, _ = solve ~solver Purefeas [e1; e2]
let () =
  try
    let p' = value_poly p vars in
    Format.printf "%a = %a@." pp p (Poly.pp_names names) p'
  with Not_found -> Format.printf "SDP error.@."
