ocamlbuild:
	ocamlbuild -use-ocamlfind demo.native
# Ocamlbuild requires a _tags file with <*>: package(osdp)

ocamlfind:
	ocamlfind ocamlopt -linkpkg -package osdp -o demo demo.ml

ocamlopt:
	ocamlopt -I $$(ocamlfind query zarith) -I $$(ocamlfind query osdp) zarith.cmxa osdp.cmxa -o demo demo.ml

clean:
	ocamlbuild -clean
	rm -f demo *.cmi *.cmo *.cmx *.o *~

.PHONY: clean
