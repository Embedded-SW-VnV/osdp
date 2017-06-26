BINS=demo demo_sdp demo_sos demo_sos2 demo_sos3 demo_sos4 demo_invariant

all: $(BINS)

dem%: dem%.ml
	ocamlfind ocamlopt -linkpkg -package osdp -o $@ $<

ocamlbuild:
	ocamlbuild -use-ocamlfind demo.native
# Ocamlbuild requires a _tags file with <*>: package(osdp)

ocamlfind:
	ocamlfind ocamlopt -linkpkg -package osdp -o demo demo.ml

ocamlopt:
	ocamlopt -I $$(ocamlfind query zarith) -I $$(ocamlfind query osdp) zarith.cmxa osdp.cmxa -o demo demo.ml

clean:
	ocamlbuild -clean
	rm -f $(BINS) *.cmi *.cmo *.cmx *.o *~

.PHONY: clean
