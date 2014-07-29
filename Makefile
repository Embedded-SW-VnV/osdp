OCAMLC=../revert_cclib.sh ocamlc
OCAMLOPT=../revert_cclib.sh ocamlopt
OCAMLBUILD=/udd/deri/awerey/.opam/4.01.0/bin/ocamlbuild -classic-display -no-links \
-ocamlc "${OCAMLC}" -ocamlopt "${OCAMLOPT}"

prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
datarootdir=${prefix}/share
sharedir=${datarootdir}

LOCAL_BINDIR=.
LOCAL_LIBCDIR=lib

osdp:
	$(OCAMLBUILD) TODO
	mkdir -p $(LOCAL_LIBDIR)

test:
	$(OCAMLBUILD) test/test.native

lib:
	$(OCAMLBUILD) osdp.cmxa
	$(OCAMLBUILD) osdp.cma
	
install:
	ocamlfind install osdp META _build/osdp.cma _build/osdp.cmxa _build/sdp/sdp.cmi _build/sdp/sdp.cmx _build/sdp/sdp.mli _build/sdp/LMI.cmi _build/sdp/LMI.cmx _build/sdp/LMI.mli _build/sdp/matrix.cmx _build/sdp/matrix.cmi _build/sdp/matrix.mli _build/cholewski/posdef.cmi _build/cholewski/posdef.cmx _build/cholewski/posdef.mli _build/sdp/csdp.cmx _build/sdp/csdp.cmo _build/sdp/csdp.cmi _build/sdp/csdp.mli _build/sdp/csdp_stubs.o _build/sdp/csdp.o

clean:
	$(OCAMLBUILD) -clean
	rm -f *~ sdp/*~ 

dist-clean: clean
	rm -f Makefile myocamlbuild.ml global.ml Makefile config.log config.status configure

/usr/local/share/csdp/param.csdp:
	mkdir -p /usr/local/share/csdp/
	cp external/param.csdp /usr/local/share/csdp/

install-deps: /usr/local/share/csdp/param.csdp


.PHONY: osdp test clean install install-deps dist-clean lib install
