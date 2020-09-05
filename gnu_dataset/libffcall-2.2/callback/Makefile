# Makefile for callback

#### Start of system configuration section. ####

HOST = x86_64-pc-linux-gnu
CPU = x86_64

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
htmldir = $(datadir)/html

# Programs used by "make":
# C compiler
CC = gcc
CFLAGS = -g -O2
CPP = gcc -E
# C++ compiler
CXX = g++
CXXFLAGS = 
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
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

# Libtool options for linking with the thread library.
LTLIBTHREAD = 

#### End of system configuration section. ####

SHELL = /bin/sh

# Needed by $(LIBTOOL).
top_builddir = ..

# Limit the set of exported symbols, on those platforms where libtool supports it.
# Currently this excludes the symbols from gnulib modules.
LIBCALLBACK_EXPORTED_SYMBOLS_REGEX = '^callback_|_callback$$|^trampoline_r_data0$$'

# Before making a release, change this according to the libtool documentation,
# section "Library interface versions".
LIBCALLBACK_VERSION_INFO = 1:2:0

all : all-subdirs libcallback.la $(srcdir)/callback.3 $(srcdir)/callback.html

all-subdirs : force
	cd vacall_r && $(MAKE) all
	cd trampoline_r && $(MAKE) all

callback-libapi.lo : $(srcdir)/callback-libapi.c $(srcdir)/callback.h $(srcdir)/trampoline_r/trampoline_r.h ../config.h
	$(LIBTOOL_COMPILE) $(CC) $(INCLUDES) -I$(srcdir)/trampoline_r $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/callback-libapi.c

callback-compat.lo : $(srcdir)/callback-compat.c ../config.h
	$(LIBTOOL_COMPILE) $(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/callback-compat.c

libcallback.la : vacall_r/libvacall.la trampoline_r/libtrampoline.la callback-libapi.lo callback-compat.lo
	$(LIBTOOL_LINK) $(CC) -o libcallback.la -rpath $(libdir) -no-undefined -export-symbols-regex $(LIBCALLBACK_EXPORTED_SYMBOLS_REGEX) -version-info $(LIBCALLBACK_VERSION_INFO) vacall_r/vacall.lo vacall_r/vacall-libapi.lo vacall_r/vacall-structcpy.lo trampoline_r/*.lo callback-libapi.lo callback-compat.lo ../gnulib-lib/libgnu.la $(LDFLAGS) $(LTLIBTHREAD)

# Installs the library and include files only. Typically called with only
# $(libdir) and $(includedir) - don't use $(prefix) and $(exec_prefix) here.
install-lib : all force
	cd vacall_r && $(MAKE) install-lib libdir='$(libdir)' includedir='$(includedir)'
	mkdir -p $(libdir)
	$(LIBTOOL_INSTALL) $(INSTALL_DATA) libcallback.la $(libdir)/libcallback.la
	mkdir -p $(includedir)
	$(INSTALL_DATA) $(srcdir)/callback.h $(includedir)/callback.h

install : force
	cd vacall_r && $(MAKE) install
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(exec_prefix)
	mkdir -p $(DESTDIR)$(libdir)
	$(LIBTOOL_INSTALL) $(INSTALL_DATA) libcallback.la $(DESTDIR)$(libdir)/libcallback.la
	mkdir -p $(DESTDIR)$(includedir)
	$(INSTALL_DATA) $(srcdir)/callback.h $(DESTDIR)$(includedir)/callback.h
	mkdir -p $(DESTDIR)$(mandir)
	mkdir -p $(DESTDIR)$(mandir)/man3
	$(INSTALL_DATA) $(srcdir)/callback.3 $(DESTDIR)$(mandir)/man3/callback.3
	mkdir -p $(DESTDIR)$(datadir)
	mkdir -p $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(srcdir)/callback.html $(DESTDIR)$(htmldir)/callback.html

installdirs : force
	cd vacall_r && $(MAKE) installdirs
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(exec_prefix)
	mkdir -p $(DESTDIR)$(libdir)
	mkdir -p $(DESTDIR)$(includedir)
	mkdir -p $(DESTDIR)$(mandir)
	mkdir -p $(DESTDIR)$(mandir)/man3
	mkdir -p $(DESTDIR)$(datadir)
	mkdir -p $(DESTDIR)$(htmldir)

uninstall : force
	cd vacall_r && $(MAKE) uninstall
	$(LIBTOOL_UNINSTALL) $(RM) $(DESTDIR)$(libdir)/libcallback.la
	$(RM) $(DESTDIR)$(includedir)/callback.h
	$(RM) $(DESTDIR)$(mandir)/man3/callback.3
	$(RM) $(DESTDIR)$(htmldir)/callback.html

test1.o : $(srcdir)/test1.c $(srcdir)/callback.h $(srcdir)/vacall_r/vacall_r.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/test1.c

test1 : test1.o libcallback.la
	$(LIBTOOL_LINK) $(CC) $(CFLAGS) -x none test1.o libcallback.la $(LDFLAGS) -o test1

minitests.o : $(srcdir)/minitests.c $(srcdir)/tests.c $(srcdir)/callback.h $(srcdir)/vacall_r/vacall_r.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/minitests.c

minitests.s : $(srcdir)/minitests.c $(srcdir)/tests.c $(srcdir)/callback.h $(srcdir)/vacall_r/vacall_r.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -S $(srcdir)/minitests.c

minitests : minitests.o libcallback.la
	$(LIBTOOL_LINK) $(CC) $(CFLAGS) -x none minitests.o libcallback.la $(LDFLAGS) -o minitests

minitests-c++.o : $(srcdir)/minitests-c++.cc $(srcdir)/minitests.c $(srcdir)/tests.c $(srcdir)/callback.h $(srcdir)/vacall_r/vacall_r.h
	$(CXX) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) -c $(srcdir)/minitests-c++.cc

minitests-c++ : minitests-c++.o libcallback.la
	$(LIBTOOL_LINK) $(CXX) $(CXXFLAGS) -x none minitests-c++.o libcallback.la $(LDFLAGS) -o minitests-c++

check-subdirs : force
	cd vacall_r && $(MAKE) check
	cd trampoline_r && $(MAKE) check

check : all check-subdirs test1 minitests
	./test1
	./minitests > minitests.out
	LC_ALL=C uniq -u < minitests.out > minitests.output.$(HOST)
	test '!' -s minitests.output.$(HOST)
	./minitests-c++ > minitests-c++.out
	LC_ALL=C uniq -u < minitests-c++.out > minitests-c++.output.$(HOST)
	test '!' -s minitests-c++.output.$(HOST)
check : minitests-c++

tests.o : $(srcdir)/tests.c $(srcdir)/callback.h $(srcdir)/vacall_r/vacall_r.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/tests.c

tests.s : $(srcdir)/tests.c $(srcdir)/callback.h $(srcdir)/vacall_r/vacall_r.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -S $(srcdir)/tests.c

tests : tests.o libcallback.la
	$(LIBTOOL_LINK) $(CC) $(CFLAGS) -x none tests.o libcallback.la $(LDFLAGS) -o tests

extracheck-subdirs : force
	cd vacall_r && $(MAKE) extracheck
	cd trampoline_r && $(MAKE) extracheck

extracheck : all extracheck-subdirs tests
	./tests > tests.out
	LC_ALL=C uniq -u < tests.out > tests.output.$(HOST)
	test '!' -s tests.output.$(HOST)

MOSTLYCLEANDIRS = .libs _libs
MOSTLYCLEANFILES = \
  *.o *.lo core \
  libcallback.* \
  test1.o test1 \
  minitests.o minitests.s minitests minitests.out \
  minitests-c++.o minitests-c++ minitests-c++.out \
  tests.o tests.s tests tests.out

mostlyclean : force
	cd vacall_r && $(MAKE) mostlyclean
	cd trampoline_r && $(MAKE) mostlyclean
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)

clean : force
	cd vacall_r && $(MAKE) clean
	cd trampoline_r && $(MAKE) clean
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)

DISTCLEANFILES = \
  Makefile \
  minitests.output.* minitests-c++.output.* tests.output.*

distclean : force
	cd vacall_r && if test -f Makefile; then $(MAKE) distclean; fi
	cd trampoline_r && if test -f Makefile; then $(MAKE) distclean; fi
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)
	$(RM) $(DISTCLEANFILES)

maintainer-clean : force
	cd vacall_r && if test -f Makefile; then $(MAKE) maintainer-clean; fi
	cd trampoline_r && if test -f Makefile; then $(MAKE) maintainer-clean; fi
	$(RM) -r $(MOSTLYCLEANDIRS)
	$(RM) $(MOSTLYCLEANFILES)
	$(RM) $(DISTCLEANFILES)


# List of source files (committed in version control).
SOURCE_FILES = \
  COPYING MIGRATION PLATFORMS README callback.3 callback.html elf-hack.txt \
  Makefile.maint \
  Makefile.in \
  callback.h \
  callback-libapi.c \
  callback-compat.c \
  test1.c \
  minitests.c minitests-c++.cc \
  tests.c
# List of distributed files generated by Makefile.maint.
GENERATED_FILES = \
  callback.man
# List of distributed files.
DISTFILES = $(SOURCE_FILES) $(GENERATED_FILES)

distdir : $(DISTFILES)
	for file in $(DISTFILES); do \
	  if test -f $$file; then dir='.'; else dir='$(srcdir)'; fi; \
	  cp -p "$$dir/$$file" '$(distdir)'/$$file || exit 1; \
	done
	test -d '$(distdir)'/vacall_r || mkdir '$(distdir)'/vacall_r; cd vacall_r && $(MAKE) distdir distdir='$(distdir)'/vacall_r
	test -d '$(distdir)'/trampoline_r || mkdir '$(distdir)'/trampoline_r; cd trampoline_r && $(MAKE) distdir distdir='$(distdir)'/trampoline_r


force :
