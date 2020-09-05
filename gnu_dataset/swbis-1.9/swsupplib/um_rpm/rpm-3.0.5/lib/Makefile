srcdir = .
topdir = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib/um_rpm/rpm-3.0.5


include $(topdir)/Makefile.inc

CC = gcc
CXX = g++
CPP = gcc -E
CPPFLAGS = 



# Makefile for rpm library.
SHELL = /bin/bash

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
infodir = ${prefix}/share/info
mandir = ${prefix}/share/man
includedir = ${prefix}/include
oldincludedir = /usr/include
pkgdatadir = $(datadir)/rpm
pkglibdir = $(libdir)/rpm
pkgincludedir = $(includedir)/rpm
top_builddir = ..

ACLOCAL = aclocal-1.11
AUTOCONF = autoconf
AUTOMAKE = automake-1.11
AUTOHEADER = autoheader

am__cd = CDPATH="$${ZSH_VERSION+.}$(PATH_SEPARATOR)" && cd
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_SCRIPT = $(install_sh) -c
INSTALL_SCRIPT = ${INSTALL}
INSTALL_HEADER = $(INSTALL_DATA)
transform = s,x,x,
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
build_alias = 
build_triplet = x86_64-unknown-linux-gnuoldld
host_alias = 
host_triplet = x86_64-unknown-linux-gnuoldld
target_alias = 
target_triplet = x86_64-unknown-linux-gnuoldld

EXEEXT = 
OBJEXT = o
PATH_SEPARATOR = :
AMTAR = tar
AR = ar
AS = @AS@
AWK = gawk
BUILD_INCLUDED_LIBINTL = @BUILD_INCLUDED_LIBINTL@
BUILD_RPMNLSTOOLS = 
BZIP2BIN = /bin/bzip2
CATOBJEXT = @CATOBJEXT@
DATADIRNAME = @DATADIRNAME@
DEPDIR = .deps
DLLTOOL = @DLLTOOL@
ECHO = @ECHO@
FINDPROVIDES = /usr/local/lib/rpm/find-provides
FINDREQUIRES = /usr/local/lib/rpm/find-requires
FIXPERMS = a+rX,g-w,o-w
GENCAT = @GENCAT@
GLIBC21 = @GLIBC21@
GMSGFMT = @GMSGFMT@
GZIPBIN = /bin/gzip
HAVE_LIB = @HAVE_LIB@
INCPATH =  -I/usr/local/include
INSTALL_STRIP_PROGRAM = $(install_sh) -c -s
INSTOBJEXT = @INSTOBJEXT@
INTLBISON = @INTLBISON@
INTLLIBS = @INTLLIBS@
INTLOBJS = @INTLOBJS@
INTL_LIBTOOL_SUFFIX_PREFIX = @INTL_LIBTOOL_SUFFIX_PREFIX@
LDFLAGS_STATIC =  -all-static
LIB = @LIB@
LIBDIR = ${exec_prefix}/%{_lib}
LIBICONV = @LIBICONV@
LIBINTL = 
LIBMISC = 
LIBRPMALIAS_FILENAME = /usr/local/lib/rpm/rpmpopt
LIBRPMRC_FILENAME = /usr/local/lib/rpm/rpmrc
LIBS =  -L/usr/local/lib  -lz
LIBTOOL = @LIBTOOL@
LN_S = ln -s
LOCALEDIR = /usr/local/share/locale
LTLIB = @LTLIB@
LTLIBICONV = @LTLIBICONV@
LTLIBINTL = @LTLIBINTL@
MACROFILES = /usr/local/lib/rpm/macros
MKDIR = /bin/mkdir
MKDIR_P = /bin/mkdir -p
MKINSTALLDIRS = @MKINSTALLDIRS@
OBJDUMP = @OBJDUMP@
PACKAGE = rpm
PGPBIN = /usr/bin/pgp
POSUB = @POSUB@
RANLIB = ranlib
ROOT_GROUP = root
RPM = 
RPMCANONARCH = unknown
RPMCANONOS = linux-gnuoldld
RPMCANONVENDOR = unknown
RPMCONFIGDIR = /usr/local/lib/rpm
STRIP = 
SYSCONFIGDIR = /etc/rpm
TOP_SOURCEDIR = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib/um_rpm/rpm-3.0.5
U = 
USE_INCLUDED_LIBINTL = @USE_INCLUDED_LIBINTL@
USE_NLS = @USE_NLS@
VERSION = 3.0.5
__CAT = /bin/cat
__CHGRP = /bin/chgrp
__CHGRP_RHF = %{__chgrp} -Rhf
__CHMOD = /bin/chmod
__CHOWN = /bin/chown
__CHOWN_RHF = %{__chown} -Rhf
__CP = /bin/cp
__CPIO = /bin/cpio
__GREP = /bin/grep
__ID = /usr/bin/id
__ID_U = %{__id} -u
__INSTALL = /usr/bin/install
__LD = /usr/bin/ld
__MAKE = /usr/bin/make
__MKDIR = /bin/mkdir
__MV = /bin/mv
__NM = /usr/bin/nm
__OBJCOPY = /usr/bin/objcopy
__OBJDUMP = /usr/bin/objdump
__PATCH = /usr/bin/patch
__RM = /bin/rm
__RSH = /usr/bin/rsh
__SED = /bin/sed
__SSH = /usr/bin/ssh
__STRIP = /usr/bin/strip
__TAR = /bin/tar
am__include = include
am__quote = 
install_sh = ${SHELL} /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib/um_rpm/rpm-3.0.5/install-sh
testdir = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib/um_rpm/rpm-3.0.5/tests
tmpdir = /var/tmp
varprefix = /var/local

#########################################################
#########################################################
#########################################################

DO_DEBUG = 
ifeq ($(DO_DEBUG),)
.c.o:
	$(CC) $(CFLAGS) -c -o $*.o  $<
DEBUGLIBOBJECTS =
else
.c.o:
	$(CC) $(CFLAGS) -c -o $*.o  $<; $(CC) $(CFLAGS) -S -gstabs+ -o $*.S $<
DEBUGLIBOBJECTS = 
endif

LIBOBJECTS = \
	falloc.o formats.o  header.o macro.o md5.o md5sum.o \
	messages.o misc.o package.o rpmerr.o rpmio.o rpmlead.o \
	rpmmalloc.o signature.o tagName.o tagtable.o url.o setenv.o

LIBTAR = librpm.a

ifeq (.depend,$(wildcard .depend))
TARGET=everything
else
TARGET=depend everything
endif

all: $(TARGET)

everything: $(LIBTAR)

$(LIBTAR): $(LIBOBJECTS) 
	$(AR) r $@ $(LIBOBJECTS)
	$(RANLIB) $@

$(PROGS): $(LIBOBJECTS)

clean:
	rm -f *.S *.a *.o *~ $(PROGS) a.out

distdir:
install-data:
install-exec:
install-html:
install-dvi:
install-pdf:
install-ps:
install-strip:
install-info:
installdirs:
installcheck:
install:
uninstall:
info:
dvi:
html:
pdf:
ps:
dist:
check:
tags:
ctags:
maintainer-clean: distclean
mostlyclean: clean

distclean: clean
	rm -f .depend Makefile

depend:
	( > .depend )

#	($(CPP) $(CFLAGS) -M $(srcdir)/*.c > .depend; exit 0)

ifeq (.depend,$(wildcard .depend))
include .depend
endif
