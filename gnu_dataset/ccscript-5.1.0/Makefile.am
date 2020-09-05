# Copyright (C) 2009-2014 David Sugar, Tycho Softworks.
# Copyright (C) 2015 Cherokees of Idaho.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AUTOMAKE_OPTIONS = no-dependencies dist-zip
EXTRA_DIST = autogen.sh README *.pc.in *.spec.in *.spec \
	cmake-abi.sh CMakeLists.txt PKGBUILD.in \
	ccscript-config.h.cmake COPYING* directive.in autoconf/*

SUBDIRS = src inc test

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ccscript.pc

srpm:
	rm -f *.rpm
	rpmbuild  -bs --nodeps --define "_sourcedir ." --define "_srcrpmdir ." --sign ccscript.spec

deb:	dist
	rm -f *.deb *.debian.tar.gz *.dsc *.changes
	cape-source --sign ccscript-${VERSION}.tar.gz .

lint:
	cppcheck --force -q .

dist-hook:  cleandist

cleandist:
	@rm -f ${PACKAGE}-[0-9]*.tar.gz
	@rm -f ${PACKAGE}-[0-9]*.tar.bz2
	@rm -f ${PACKAGE}-[0-9]*.shar.gz
	@rm -f ${PACKAGE}-[0-9]*.zip
	@rm -f *${PACKAGE}*.rpm
	@rm -f *${PACKAGE}*.deb
	@rm -f ${PACKAGE}_*.gz
	@rm -f ${PACKAGE}_*.dsc
	@rm -f *.changes

