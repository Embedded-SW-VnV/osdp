# OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
# programming (SDP) solvers.
# Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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

distclean: clean
	make -C src distclean
	rm -rf doc autom4te.cache config.log config.status 

.PHONY: osdp doc install-deps install uninstall clean dist-clean