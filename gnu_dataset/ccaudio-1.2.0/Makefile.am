# Copyright (C) 2006 Free Software Foundation.
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AUTOMAKE_OPTIONS = no-dependencies dist-shar
ACLOCAL_AMFLAGS = -I m4
EXTRA_DIST = reconfig ccaudio.spec ccaudio.spec.in ccaudio.proto pkginfo.in \
	freebsd/Makefile.in freebsd/pkg-* 
PKGBASE = "/opt/OSTaudio"
PKGNAME = "OSTaudio"

SUBDIRS = src dso doc 
DIST_SUBDIRS = m4 config src dso doc win32

kdoc:
	(cd src ; make kdoc)

force:
	-@$(BIN_TRUE)

docs: force
	@(cd doc; make docs)

pkg:	
	$(MAKE) $(AM_MAKEFLAGS) prefix=$(PKGBASE) install
	pkgmk -b  $(PKGBASE) -f $(PACKAGE).proto
	pkgtrans -s /var/spool/pkg $(PKGNAME)-$(host_alias).pkg $(PKGNAME)
	mv /var/spool/pkg/$(PKGNAME)-*.pkg . 
	rm -rf $(PKGBASE)
	rm -rf /var/spool/pkg/$(PKGNAME)

lsm:	$(distdir).tar.gz
	sed -e s/SIZE/$(shell expr `wc -c *.tar.gz | cut -d\  -f2` / 1024)/ -e s/VERSION/$(VERSION)/ -e s/DATE/$(shell date +%Y%m%d)/ <$(top_srcdir)/ccaudio.lsm.in >ccaudio.lsm

zip:
	zip -r9 ccaudio-$(VERSION).zip src/*.cpp src/*.h README ChangeLog AUTHORS NEWS TODO

ports:
	if test -f $(distdir).tar.gz ; then cp $(distdir).tar.gz /usr/ports/distfiles ; fi
	mkdir -p freebsd/files
	(cd freebsd ; make makesum)
	(cd freebsd ; make clean)
	(cd freebsd ; shar Makefile distinfo pkg-descr pkg-plist pkg-comment > $(distdir).ports )
	(cd freebsd ; make package)

macosx:
	(cd src ; make macosx)
                                                                                
darwin:
	(cd src ; make macosx)
                                                                                
doxy:
	(cd doc ; doxygen Doxyfile)
	(cd doc ; doxygen Doxyman)

man:
	(cd doc/man3 ; $(INSTALL_DATA) *.3cc $(DESTDIR)/$(mandir)/man3 )

