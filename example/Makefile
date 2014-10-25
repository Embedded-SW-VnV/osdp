ocamlbuild:
	ocamlbuild -use-ocamlfind demo.native

ocamlfind:
	ocamlfind ocamlopt -linkpkg -package num -package osdp -o demo demo.ml

ocamlopt:
	ocamlopt -I $$(ocamlfind query osdp) nums.cmxa osdp.cmxa -o demo demo.ml

clean:
	ocamlbuild -clean
	rm -f demo *.cmi *.cmo *.cmx *.o *~

.PHONY: clean
