# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.4 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

# $Id: Makefile.am,v 1.4 1999/04/26 00:28:00 tudor Exp $


SHELL = /bin/sh

srcdir = .
top_srcdir = ..
prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man
includedir = ${prefix}/include
oldincludedir = /usr/include

DESTDIR =

pkgdatadir = $(datadir)/git
pkglibdir = $(libdir)/git
pkgincludedir = $(includedir)/git

top_builddir = ..

ACLOCAL = aclocal
AUTOCONF = autoconf
AUTOMAKE = automake
AUTOHEADER = autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL} $(AM_INSTALL_PROGRAM_FLAGS)
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL_PROGRAM}
transform = s,x,x,

NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
host_alias = x86_64-unknown-linux-gnu
host_triplet = 
CC = gcc
CFLAGS = -g -O2
CLEANFILES = core tutu gogu *.out t tt mwatch.log mtrace.log
CPP = gcc -E
DEFS = -DHAVE_CONFIG_H
HOST = 
INCLUDES = 
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
LDFLAGS = 
LIBOBJS = 
LIBS = -lreadline  -ltermcap
MAKE = make
MAKEINFO = /home/carolhanna/Security_Project/gnu_dataset/git-4.3.20/missing makeinfo
PACKAGE = git
PRODUCT = GNU Interactive Tools
VERSION = 4.3.20

EXTRA_DIST = .gitrc.aixterm .gitrc.ansi .gitrc.common.in 	     .gitrc.generic .gitrc.hft .gitrc.hpterm .gitrc.linux 	     .gitrc.mach .gitrc.minix .gitrc.thix .gitrc.vt100 	     .gitrc.vt102 .gitrc.xterm .gitrc.xterm-color

pkgdata_DATA = .gitrc.aixterm .gitrc.ansi .gitrc.common 	       .gitrc.generic .gitrc.hft .gitrc.hpterm .gitrc.linux 	       .gitrc.mach .gitrc.minix .gitrc.thix .gitrc.vt100 	       .gitrc.vt102 .gitrc.xterm .gitrc.xterm-color


GIT_GENERICS = .gitrc.sun .gitrc.sun-cmd .gitrc.eterm .gitrc.pc3
GIT_XTERMS = .gitrc.xterms
GIT_ANSIS = .gitrc.iris-ansi .gitrc.iris-ansi-net
GIT_COLOR_XTERMS = .gitrc.dtterm .gitrc.rxvt .gitrc.xterm-debian
GIT_LINUXS = .gitrc.console
GIT_HPTERMS = .gitrc.hp
GIT_VT100S = .gitrc.vt125 .gitrc.vt200 .gitrc.vt201 .gitrc.vt220 		   .gitrc.vt240 .gitrc.vt300 .gitrc.vt320 .gitrc.vt400 		   .gitrc.vt420 .gitrc.screen

mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = ../config.h
CONFIG_CLEAN_FILES =  .gitrc.common
DATA =  $(pkgdata_DATA)

DIST_COMMON =  .gitrc.common.in ChangeLog Makefile.am Makefile.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = gtar
GZIP_ENV = --best
all: all-redirect
.SUFFIXES:
$(srcdir)/Makefile.in: Makefile.am $(top_srcdir)/configure.in $(ACLOCAL_M4) 
	cd $(top_srcdir) && $(AUTOMAKE) --gnu --include-deps term/Makefile

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

.gitrc.common: $(top_builddir)/config.status .gitrc.common.in
	cd $(top_builddir) && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

install-pkgdataDATA: $(pkgdata_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	@list='$(pkgdata_DATA)'; for p in $$list; do \
	  if test -f $(srcdir)/$$p; then \
	    echo " $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(pkgdatadir)/$$p"; \
	    $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(pkgdatadir)/$$p; \
	  else if test -f $$p; then \
	    echo " $(INSTALL_DATA) $$p $(DESTDIR)$(pkgdatadir)/$$p"; \
	    $(INSTALL_DATA) $$p $(DESTDIR)$(pkgdatadir)/$$p; \
	  fi; fi; \
	done

uninstall-pkgdataDATA:
	@$(NORMAL_UNINSTALL)
	list='$(pkgdata_DATA)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(pkgdatadir)/$$p; \
	done
tags: TAGS
TAGS:


distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = term

distdir: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done
info-am:
info: info-am
dvi-am:
dvi: dvi-am
check-am: all-am
check: check-am
installcheck-am:
installcheck: installcheck-am
install-exec-am:
install-exec: install-exec-am

install-data-am: install-pkgdataDATA
	@$(NORMAL_INSTALL)
	$(MAKE) $(AM_MAKEFLAGS) install-data-hook
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am: uninstall-pkgdataDATA uninstall-local
uninstall: uninstall-am
all-am: Makefile $(DATA)
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) AM_INSTALL_PROGRAM_FLAGS=-s install
installdirs:
	$(mkinstalldirs)  $(DESTDIR)$(pkgdatadir)


mostlyclean-generic:

clean-generic:
	-test -z "$(CLEANFILES)" || rm -f $(CLEANFILES)

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-generic mostlyclean-am

clean: clean-am

distclean-am:  distclean-generic clean-am

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-generic distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: uninstall-pkgdataDATA install-pkgdataDATA tags distdir info-am \
info dvi-am dvi check check-am installcheck-am installcheck \
install-exec-am install-exec install-data-am install-data install-am \
install uninstall-local uninstall-am uninstall all-redirect all-am all \
installdirs mostlyclean-generic distclean-generic clean-generic \
maintainer-clean-generic clean mostlyclean distclean maintainer-clean


install-data-hook:
	for i in $(GIT_XTERMS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.xterm $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_COLOR_XTERMS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.xterm-color $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_HPTERMS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.hpterm $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_GENERICS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.generic $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_VT100S);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.vt100 $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_ANSIS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.ansi $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_LINUXS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	    ln $(datadir)/git/.gitrc.linux $(datadir)/git/$$i;\
	done

uninstall-local:
	for i in $(GIT_XTERMS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_COLOR_XTERMS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_HPTERMS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_GENERICS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_VT100S);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_ANSIS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done
	\
	for i in $(GIT_LINUXS);\
	do\
	    rm -f $(datadir)/git/$$i;\
	done

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
