#
#	Top-level Makefile
#
#   Copyright 2009 Free Software Foundation, Inc.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

SHELL		=	/bin/sh

BASE		=	.
PARENT		=	.
UTILDIR		=	util
SRCDIR		=	src
BUILDDIR	=	build
MSGFILE		=	GNUBATCH.msg
HELPPARSE	=	$(UTILDIR)/helpparse
prefix		=	/usr/local
exec_prefix	=	${prefix}
datarootdir	=	${prefix}/share
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
SBINDIR		=	$(DESTDIR)${exec_prefix}/sbin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnubatch
DATADIR		=	$(DESTDIR)${datarootdir}/gnubatch
MANDIR		=	$(DESTDIR)${datarootdir}/man
PDFDIR		=	$(DESTDIR)${datarootdir}/gnubatch/pdf
SYSCONFD	=	$(DESTDIR)${prefix}/etc
CONFFILE	=	gnubatch.conf
HOSTFILE	=	gnubatch.hosts
BATCHUSER	=	gnubatch
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
STAGED_INSTALL	=	false

all:	buildd build-src docdir

buildd:	utild
	$(HELPPARSE) $(MSGFILE) $(SRCDIR) $(BUILDDIR)
	touch $@

build-src:
	cd $(BUILDDIR);$(MAKE) all
	touch $@

docdir:
	cd doc;$(MAKE) all
	touch $@

utild:
	cd $(UTILDIR);$(MAKE) all
	touch $@

install:	all
	cd $(UTILDIR);make install prefix="$(prefix)" exec_prefix="$(exec_prefix)" datarootdir="$(datarootdir)"
	cd $(BUILDDIR);$(MAKE) install prefix="$(prefix)" exec_prefix="$(exec_prefix)" datarootdir="$(datarootdir)"
	cd doc;$(MAKE) install prefix="$(prefix)" exec_prefix="$(exec_prefix)" datarootdir="$(datarootdir)"
	$(INSTALL) -d -o $(BATCHUSER) -g root -m 755 $(SYSCONFD)
	[ -f $(SYSCONFD)/$(CONFFILE) ] || $(INSTALL) -o root -m 644 $(CONFFILE) $(SYSCONFD)
	echo '# Get local address from GNU' >$(HOSTFILE)
	echo >>$(HOSTFILE)
	echo 'localaddress GSN(www.gnu.org,80)' >>$(HOSTFILE)
	[ -f $(SYSCONFD)/$(HOSTFILE) ] || $(INSTALL) -o root -m 644 $(HOSTFILE) $(SYSCONFD)
	$(STAGED_INSTALL) || $(BINDIR)/gbch-uchange -R
	$(STAGED_INSTALL) || $(BINDIR)/gbch-uchange -p ALL `who am i|sed -e 's/ .*//'`

install-strip:	all
	cd $(UTILDIR);make install-strip prefix="$(prefix)" exec_prefix="$(exec_prefix)" datarootdir="$(datarootdir)"
	cd $(BUILDDIR);$(MAKE) install-strip prefix="$(prefix)" exec_prefix="$(exec_prefix)" datarootdir="$(datarootdir)"
	cd doc;$(MAKE) install prefix="$(prefix)" exec_prefix="$(exec_prefix)" datarootdir="$(datarootdir)"
	$(INSTALL) -d -o $(BATCHUSER) -g root -m 755 $(SYSCONFD)
	[ -f $(SYSCONFD)/$(CONFFILE) ] || $(INSTALL) -o root -m 644 $(CONFFILE) $(SYSCONFD)
	echo '# Get local address from GNU' >$(HOSTFILE)
	echo >>$(HOSTFILE)
	echo 'localaddress GSN(www.gnu.org,80)' >>$(HOSTFILE)
	[ -f $(SYSCONFD)/$(HOSTFILE) ] || $(INSTALL) -o root -m 644 $(HOSTFILE) $(SYSCONFD)
	$(STAGED_INSTALL) || $(BINDIR)/gbch-uchange -R
	$(STAGED_INSTALL) || $(BINDIR)/gbch-uchange -p ALL `who am i|sed -e 's/ .*//'`

install-user user-install:
	/usr/bin/perl $(BASE)/Userinst

install-net:
	/usr/bin/perl $(BASE)/Netinst

clean:
	cd $(UTILDIR);$(MAKE) clean
	rm -fr $(BUILDDIR) buildd utild build-src docdir
	cd doc;$(MAKE) clean

distclean:	clean
	cd $(UTILDIR);$(MAKE) distclean
	cd $(SRCDIR);$(MAKE) distclean
	$(RM) $(SRCDIR)/hdrs/files.h $(SRCDIR)/hdrs/infiles.h 
	$(RM) config.cache config.h config.status config.log Makefile
	$(RM) Checkuserinst Userinst Netinst gnubatch.conf ingnubatch.conf gnubatch.hosts
	cd doc;$(MAKE) distclean

