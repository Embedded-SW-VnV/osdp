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
let ret, _, vars = solve ~solver Purefeas [e1; e2]
let () = Format.printf "%a@." Osdp.SdpRet.pp ret
let () = Format.printf "%a@." Mat.pp (value_mat p vars)

let () = Format.printf "@."

module Sos = Osdp.Sos.Float

let () = Format.printf "SOS@."
let deg = 4
let p, _ = Sos.var_poly "p" 2 ~homogen:true deg
let x, y = Sos.(??0, ??1)
let names = ["x"; "y"]
let a0 = Sos.(1.5 *. x - 0.7 *. y)
let a1 = x
let e1 = Sos.(p - Compose (p, [a0; x]))
(* or let l = [a0; a1] in let e1 = Sos.(p - Compose (p, l)) *)
let e2 = Sos.(p - (x**4 + y**4))
let () = Format.printf "e1 = %a@." (Sos.pp_names names) e1
let () = Format.printf "e2 = %a@." (Sos.pp_names names) e2
let ret, _, vars, _ = Sos.solve ~solver Sos.Purefeas [e1; e2]
let () = Format.printf "%a@." Osdp.SdpRet.pp ret
let () = Format.printf "%a@." (Sos.Poly.pp_names names) (Sos.value_poly p vars)
