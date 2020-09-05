# Makefile for libffcall

#### Start of system configuration section. ####

# Directories used by "make":
srcdir = .

# Directories used by "make install":
prefix = /usr/local
local_prefix = /usr/local
exec_prefix = ${prefix}
libdir = ${exec_prefix}/lib
includedir = ${prefix}/include
mandir = ${datarootdir}/man
datadir = ${datarootdir}
datarootdir = ${prefix}/share

# Programs used by "make":
# C compiler
CC = gcc
CFLAGS = -g -O2
CPP = gcc -E
# Both C and C++ compiler
CPPFLAGS = 
INCLUDES = -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I$(srcdir)/vacall_r
LDFLAGS = 
LIBTOOL = $(SHELL) $(top_builddir)/libtool
LIBTOOL_COMPILE = $(LIBTOOL) --mode=compile
LIBTOOL_LINK = $(LIBTOOL) --mode=link
LIBTOOL_INSTALL = $(LIBTOOL) --mode=install
LIBTOOL_UNINSTALL = $(LIBTOOL) --mode=uninstall
AR = ar
AR_FLAGS = rc
RANLIB = ranlib
RM = rm -f


# Programs used by "make install":
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

#### End of system configuration section. ####

SHELL = /bin/sh

# This package does not support parallel make.
# So, turn off parallel execution (at least in GNU make >= 4.0).
GNUMAKEFLAGS = -j1

# Needed by $(LIBTOOL).
top_builddir = .

# Limit the set of exported symbols, on those platforms where libtool supports it.
# Currently this excludes the symbols from gnulib modules.
LIBFFCALL_EXPORTED_SYMBOLS_REGEX = '^ffcall_|^avcall_|^callback_|_callback$$'

# Before making a release, change this according to the libtool documentation,
# section "Library interface versions".
LIBFFCALL_VERSION_INFO = 0:2:0

all : all-subdirs libffcall.la

all-subdirs : ffcall-version.h force
	cd gnulib-lib && $(MAKE) all
	cd avcall && $(MAKE) all
	cd vacall && $(MAKE) all
	cd trampoline && $(MAKE) all
	cd callback && $(MAKE) all

ffcall-version.h : $(srcdir)/ffcall-version.in.h
	cd .. && ./config.status --header=ffcall-version.h:ffcall-version.in.h

ffcall-version.lo : $(srcdir)/ffcall-version.c ffcall-version.h config.h
	$(LIBTOOL_COMPILE) $(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/ffcall-version.c

libffcall.la : ffcall-version.lo avcall/avcall.lo avcall/avcall-libapi.lo avcall/avcall-structcpy.lo callback/vacall_r/libvacall.la callback/trampoline_r/libtrampoline.la callback/callback-libapi.lo
	$(LIBTOOL_LINK) $(CC) -o libffcall.la -rpath $(libdir) -no-undefined -export-symbols-regex $(LIBFFCALL_EXPORTED_SYMBOLS_REGEX) -version-info $(LIBFFCALL_VERSION_INFO) ffcall-version.lo avcall/avcall.lo avcall/avcall-libapi.lo avcall/avcall-structcpy.lo callback/vacall_r/vacall.lo callback/vacall_r/vacall-libapi.lo callback/vacall_r/vacall-structcpy.lo callback/trampoline_r/*.lo callback/callback-libapi.lo gnulib-lib/libgnu.la $(LDFLAGS) $(LTLIBTHREAD)

install : force
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(libdir)
	$(LIBTOOL_INSTALL) $(INSTALL_DATA) libffcall.la $(DESTDIR)$(libdir)/libffcall.la
	mkdir -p $(DESTDIR)$(includedir)
	$(INSTALL_DATA) ffcall-version.h $(DESTDIR)$(includedir)/ffcall-version.h
	$(INSTALL_DATA) $(srcdir)/ffcall-abi.h $(DESTDIR)$(includedir)/ffcall-abi.h
	cd gnulib-lib && $(MAKE) install
	cd avcall && $(MAKE) install
	cd vacall && $(MAKE) install
	cd trampoline && $(MAKE) install
	cd callback && $(MAKE) install

installdirs : force
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(libdir)
	mkdir -p $(DESTDIR)$(includedir)
	cd gnulib-lib && $(MAKE) installdirs
	cd avcall && $(MAKE) installdirs
	cd vacall && $(MAKE) installdirs
	cd trampoline && $(MAKE) installdirs
	cd callback && $(MAKE) installdirs

uninstall : force
	cd gnulib-lib && $(MAKE) uninstall
	cd avcall && $(MAKE) uninstall
	cd vacall && $(MAKE) uninstall
	cd trampoline && $(MAKE) uninstall
	cd callback && $(MAKE) uninstall
	$(LIBTOOL_UNINSTALL) $(RM) $(DESTDIR)$(libdir)/libffcall.la
	$(RM) $(DESTDIR)$(includedir)/ffcall-abi.h
	$(RM) $(DESTDIR)$(includedir)/ffcall-version.h

check : force
	cd gnulib-lib && $(MAKE) check
	cd avcall && $(MAKE) check
	cd vacall && $(MAKE) check
	cd trampoline && $(MAKE) check
	cd callback && $(MAKE) check

extracheck : force
	cd gnulib-lib && $(MAKE) extracheck
	cd avcall && $(MAKE) extracheck
	cd vacall && $(MAKE) extracheck
	cd trampoline && $(MAKE) extracheck
	cd callback && $(MAKE) extracheck

MOSTLYCLEANDIRS = .libs _libs
MOSTLYCLEANFILES = \
  *.o *.lo core \
  libffcall.*

mostlyclean : force
	cd gnulib-lib && $(MAKE) mostlyclean
	cd avcall && $(MAKE) mostlyclean
	cd vacall && $(MAKE) mostlyclean
	cd trampoline && $(MAKE) mostlyclean
	cd callback && $(MAKE) mostlyclean
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)

clean : force
	cd gnulib-lib && $(MAKE) clean
	cd avcall && $(MAKE) clean
	cd vacall && $(MAKE) clean
	cd trampoline && $(MAKE) clean
	cd callback && $(MAKE) clean
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)

DISTCLEANFILES = \
  config.status config.log config.cache Makefile config.h ffcall-version.h libtool \
  stamp-h1 stamp-h2

distclean : force
	cd gnulib-lib; if test -f Makefile; then $(MAKE) distclean; fi
	cd avcall; if test -f Makefile; then $(MAKE) distclean; fi
	cd vacall; if test -f Makefile; then $(MAKE) distclean; fi
	cd trampoline; if test -f Makefile; then $(MAKE) distclean; fi
	cd callback; if test -f Makefile; then $(MAKE) distclean; fi
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)
	$(RM) $(DISTCLEANFILES)

maintainer-clean : force
	cd gnulib-lib; if test -f Makefile; then $(MAKE) maintainer-clean; fi
	cd avcall; if test -f Makefile; then $(MAKE) maintainer-clean; fi
	cd vacall; if test -f Makefile; then $(MAKE) maintainer-clean; fi
	cd trampoline; if test -f Makefile; then $(MAKE) maintainer-clean; fi
	cd callback; if test -f Makefile; then $(MAKE) maintainer-clean; fi
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)
	$(RM) $(DISTCLEANFILES)


# List of source files (committed in version control).
SOURCE_FILES = \
  COPYING DEPENDENCIES INSTALL.os2 INSTALL.windows NEWS PLATFORMS README \
  ChangeLog \
  VERSION \
  Makefile.devel \
  Makefile.maint \
  Makefile.in \
  configure.ac \
  m4/as-underscore.m4 \
  m4/cc-gcc.m4 \
  m4/codeexec.m4 \
  m4/endianness.m4 \
  m4/general.m4 \
  m4/getpagesize.m4 \
  m4/ireg.m4 \
  m4/ln.m4 \
  m4/mach-vm.m4 \
  m4/mmap.m4 \
  m4/mprotect.m4 \
  m4/proto.m4 \
  m4/shm.m4 \
  m4/smallstruct.m4 \
  common/asm-alpha.sh \
  common/asm-arm.sh common/asm-arm.h \
  common/asm-hppa.sh common/asm-hppa.h \
  common/asm-hppa64.sh common/asm-hppa64.h \
  common/asm-i386.sh common/asm-i386.h \
  common/asm-m68k.sh common/asm-m68k.h \
  common/asm-mips.sh common/asm-mips.h \
  common/asm-powerpc.sh \
  common/asm-riscv.sh \
  common/asm-s390.sh \
  common/asm-sparc.sh common/asm-sparc.h \
  common/asm-x86_64.sh common/asm-x86_64.h \
  common/noexecstack.h common/noexecstack-arm.h \
  common/structcpy.c \
  common/uniq-u.c \
  ffcall-version.in.h dummy/ffcall-version.h \
  ffcall-abi.h \
  ffcall-stdint.h \
  ffcall-version.c \
  testcases.c
# List of distributed files imported from other packages or directories.
LIBTOOL_IMPORTED_FILES = \
  build-aux/ltmain.sh \
  m4/libtool.m4 \
  m4/lt~obsolete.m4 \
  m4/ltoptions.m4 \
  m4/ltsugar.m4 \
  m4/ltversion.m4
GNULIB_IMPORTED_FILES = \
  build-aux/ar-lib \
  build-aux/compile \
  build-aux/config.guess \
  build-aux/config.sub \
  build-aux/install-sh \
  gnulib-m4/*.m4
AUTOMAKE_IMPORTED_FILES = \
  build-aux/missing
IMPORTED_FILES = \
  $(LIBTOOL_IMPORTED_FILES) $(GNULIB_IMPORTED_FILES) $(AUTOMAKE_IMPORTED_FILES)
# List of files copied by autogen.sh.
COPIED_FILES = \
  callback/trampoline_r/PORTING \
  callback/trampoline_r/cache.c \
  callback/trampoline_r/cache-alpha.c \
  callback/trampoline_r/cache-hppa.c
# List of distributed files generated by Makefile.maint.
GENERATED_FILES = \
  aclocal.m4 \
  configure \
  config.h.in
# List of distributed files.
DISTFILES = $(SOURCE_FILES) $(IMPORTED_FILES) $(COPIED_FILES) $(GENERATED_FILES)

distdir : $(DISTFILES)
	for file in $(DISTFILES); do \
	  if test -f $$file; then dir='.'; else dir='$(srcdir)'; fi; \
	  destdir=`echo '$(distdir)'/$$file | sed -e 's|//*[^/]*$$||'`; \
	  test -d "$$destdir" || mkdir -p "$$destdir"; \
	  cp -p "$$dir/$$file" '$(distdir)'/$$file || exit 1; \
	done
	test -d '$(distdir)'/gnulib-lib || mkdir '$(distdir)'/gnulib-lib; cd gnulib-lib && $(MAKE) distdir distdir='$(distdir)'/gnulib-lib
	test -d '$(distdir)'/avcall || mkdir '$(distdir)'/avcall; cd avcall && $(MAKE) distdir distdir='$(distdir)'/avcall
	test -d '$(distdir)'/vacall || mkdir '$(distdir)'/vacall; cd vacall && $(MAKE) distdir distdir='$(distdir)'/vacall
	test -d '$(distdir)'/trampoline || mkdir '$(distdir)'/trampoline; cd trampoline && $(MAKE) distdir distdir='$(distdir)'/trampoline
	test -d '$(distdir)'/callback || mkdir '$(distdir)'/callback; cd callback && $(MAKE) distdir distdir='$(distdir)'/callback


# Creating a distribution tarball.
# Example: make dist VERSION=1.13-pre-20161227
PACKAGE = libffcall
VERSION = 2.2
TAR = tar
GZIP = gzip

dist : force
	tmpdistdir=$(PACKAGE)-$(VERSION); \
	abstmpdistdir=`pwd`/$$tmpdistdir; \
	rm -rf $$tmpdistdir $$tmpdistdir.tar $$tmpdistdir.tar.gz \
	  && mkdir $$tmpdistdir \
	  && $(MAKE) distdir distdir="$$abstmpdistdir" \
	  && $(TAR) chof $$tmpdistdir.tar --owner=root --group=root $$tmpdistdir \
	  && $(GZIP) -9 $$tmpdistdir.tar \
	  && rm -rf $$tmpdistdir


force :

