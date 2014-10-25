OCAMLC=../revert_cclib.sh ocamlc
OCAMLOPT=../revert_cclib.sh ocamlopt
OCAMLBUILD=/usr/bin/ocamlbuild -classic-display -no-links \
-ocamlc "${OCAMLC}" -ocamlopt "${OCAMLOPT}"

osdp:
	$(OCAMLBUILD) osdp.cma osdp.cmxa osdp.cmxs

doc:
	$(OCAMLBUILD) osdp.docdir/index.html

install:
	ocamlfind install osdp META _build/libosdp.a _build/osdp.a _build/osdp.cma _build/osdp.cmxa _build/osdp.cmxs _build/osdp.cmi _build/*.mli

uninstall:
	ocamlfind remove osdp

clean:
	$(OCAMLBUILD) -clean
	rm -f *~

dist-clean: clean
	rm -f myocamlbuild.ml

.PHONY: osdp doc clean dist-clean install uninstall
