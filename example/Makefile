BINS=demo demo_no_camlp4 demo_sdp demo_sos demo_sos2 demo_no_camlp4_sos2

all: $(BINS)

demo_no%: demo_no%.ml
	ocamlfind ocamlopt -linkpkg -package osdp -o $@ $<

dem%: dem%.ml
	ocamlfind ocamlopt -linkpkg -package osdp -package camlp4 -syntax camlp4o -o $@ $<

ocamlbuild:
	ocamlbuild -use-ocamlfind demo.native
# Ocamlbuild requires a _tags file with <*>: package(osdp), package(camlp4), syntax(camlp4o)

ocamlfind:
	ocamlfind ocamlopt -linkpkg -package osdp -package camlp4 -syntax camlp4o -o demo demo.ml

ocamlopt:
	ocamlopt -I $$(ocamlfind query zarith) -I $$(ocamlfind query osdp) zarith.cmxa osdp.cmxa -pp 'camlp4o -I $$(ocamlfind query osdp) pa_osdp.cma' -o demo demo.ml

clean:
	ocamlbuild -clean
	rm -f $(BINS) *.cmi *.cmo *.cmx *.o *~

.PHONY: clean
