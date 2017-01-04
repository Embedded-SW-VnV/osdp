(* Uncomment following lines for use in the [ocaml] toplevel. *)
(* #use "topfind";; *)
(* #require "osdp";; *)

(* To compile: *)
(* % ocamlbuild -use-ocamlfind demo.native *)
(* with a _tags file containing a line <*>: package(osdp) *)
(* or *)
(* % ocamlfind ocamlopt -linkpkg -package osdp -o demo demo.ml *)
(* or directly *)
(* % ocamlopt -I $(ocamlfind query zarith) -I $(ocamlfind query osdp) zarith.cmxa osdp.cmxa -o demo demo.ml *)

let solver = Osdp.Sdp.Csdp  (* Osdp.Sdp.Mosek, Osdp.Sdp.Sdpa,... *)

module Lmi = Osdp.Lmi.Float

let () = Format.printf "LMI@."
let a = Lmi.Mat.of_list_list [[1.5; -0.7]; [1.; 0.]]
let p = Lmi.var "p" 2
let e1 = Lmi.(p - ~: !!a * p * !!a)
let e2 = Lmi.(p - eye 2)
let () = Format.printf "e1 = %a@." Lmi.pp e1
let () = Format.printf "e2 = %a@." Lmi.pp e2
let ret, _, vars = Lmi.solve ~solver Lmi.Purefeas [e1; e2]
let () = Format.printf "%a@." Osdp.SdpRet.pp ret
let () = Format.printf "%a@." Lmi.Mat.pp (Lmi.value_mat p vars)

let () = Format.printf "@."

module Sos = Osdp.Sos.Float

let () = Format.printf "SOS@."
let deg = 4
let p = Sos.make ~n:2 ~d:deg ~homogen:true "p"
let x, y = Sos.(??0, ??1)
let names = ["x"; "y"]
let a0 = Sos.(1.5 *. x - 0.7 *. y)
let a1 = x
let e1 = Sos.(p - compose p [a0; a1])
let e2 = Sos.(p - (x**4 + y**4))
let () = Format.printf "e1 = %a@." (Sos.pp_names names) e1
let () = Format.printf "e2 = %a@." (Sos.pp_names names) e2
let ret, _, vars, _ = Sos.solve ~solver Sos.Purefeas [e1; e2]
let () = Format.printf "%a@." Osdp.SdpRet.pp ret
let () = Format.printf "%a@." (Sos.Poly.pp_names names) (Sos.value_poly p vars)
