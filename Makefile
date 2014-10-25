LOCAL_DOCDIR=doc

osdp:
	make -C src osdp

doc:
	make -C src doc
	rm -rf $(LOCAL_DOCDIR)
	cp -rf src/_build/osdp.docdir $(LOCAL_DOCDIR)

/usr/local/share/csdp/param.csdp:
	mkdir -p /usr/local/share/csdp/
	cp external/param.csdp /usr/local/share/csdp/

install-deps: /usr/local/share/csdp/param.csdp

install:
	make -C src install

uninstall:
	make -C src uninstall

clean:
	make -C src clean
	make -C example clean
	rm -f *~

dist-clean: clean
	rm -rf doc autom4te.cache config.log config.status

.PHONY: osdp doc install-deps install uninstall clean dist-clean
