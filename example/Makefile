ocamlbuild:
	ocamlbuild -use-ocamlfind demo.native
# Ocamlbuild requires a _tags file with <*>: package(osdp), package(camlp4), syntax(camlp4o)

ocamlfind:
	ocamlfind ocamlopt -linkpkg -package osdp -package camlp4 -syntax camlp4o -o demo demo.ml

ocamlopt:
	ocamlopt -I $$(ocamlfind query zarith) -I $$(ocamlfind query osdp) zarith.cmxa osdp.cmxa -pp 'camlp4o -I $$(ocamlfind query osdp) pa_sos.cma' -o demo demo.ml

clean:
	ocamlbuild -clean
	rm -f demo *.cmi *.cmo *.cmx *.o *~

.PHONY: clean
