# Makefile for trampoline

#### Start of system configuration section. ####

HOST = x86_64-pc-linux-gnu
CPU = x86_64
OS = linux-gnu

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
INCLUDES = -I. -I$(srcdir) -I..
INCLUDES_WITH_GNULIB = $(INCLUDES) -I../gnulib-lib -I$(srcdir)/../gnulib-lib
ASPFLAGS = `if test false = true; then echo '-DASM_UNDERSCORE'; fi`
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

OBJECTS = trampoline.lo 

# Limit the set of exported symbols, on those platforms where libtool supports it.
# Currently this excludes the symbols from gnulib modules.
LIBTRAMPOLINE_EXPORTED_SYMBOLS_REGEX = 'trampoline'

# Before making a release, change this according to the libtool documentation,
# section "Library interface versions".
LIBTRAMPOLINE_VERSION_INFO = 1:2:0

all : $(OBJECTS) libtrampoline.la $(srcdir)/trampoline.3 $(srcdir)/trampoline.html

trampoline.lo : $(srcdir)/trampoline.c $(srcdir)/trampoline.h
	$(LIBTOOL_COMPILE) $(CC) $(INCLUDES_WITH_GNULIB) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/trampoline.c

tramp-hppa.lo : tramp-hppa.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c tramp-hppa.s

tramp-hppa.s : $(srcdir)/tramp-hppa-macro.S $(srcdir)/../common/noexecstack.h
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/tramp-hppa-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e "s,!,',g" > tramp-hppa.s

tramp-hppa64.lo : tramp-hppa64.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c tramp-hppa64.s

tramp-hppa64.s : $(srcdir)/tramp-hppa64-macro.S $(srcdir)/../common/noexecstack.h
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/tramp-hppa64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e "s,!,',g" > tramp-hppa64.s

tramp-powerpc.lo : tramp-powerpc.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c tramp-powerpc.s

tramp-powerpc.s : $(srcdir)/tramp-powerpc-aix.S
	$(CPP) $(srcdir)/tramp-powerpc-aix.S > tramp-powerpc.s

tramp-powerpc64.lo : tramp-powerpc64.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c tramp-powerpc64.s

tramp-powerpc64.s : $(srcdir)/tramp-powerpc64-aix.S $(srcdir)/../common/noexecstack.h
	$(CPP) -I$(srcdir)/../common $(srcdir)/tramp-powerpc64-aix.S > tramp-powerpc64.s

tramp-ia64.lo : tramp-ia64.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c tramp-ia64.s

tramp-ia64.s : $(srcdir)/tramp-ia64-macro.S $(srcdir)/../common/noexecstack.h
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/tramp-ia64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > tramp-ia64.s

cache-sparc.lo : cache-sparc.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-sparc.s

cache-sparc.s : $(srcdir)/cache-sparc-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common - < $(srcdir)/cache-sparc-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,//.*$$,,' -e 's,\$$,#,g' -e 's,# ,#,g' > cache-sparc.s

cache-sparc64.lo : cache-sparc64.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-sparc64.s

cache-sparc64.s : $(srcdir)/cache-sparc64-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common - < $(srcdir)/cache-sparc64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,//.*$$,,' -e 's,\$$,#,g' -e 's,# ,#,g' > cache-sparc64.s

cache-alpha.lo : cache-alpha.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-alpha.s

cache-alpha.s : $(srcdir)/cache-alpha-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/cache-alpha-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > cache-alpha.s

cache-hppa.lo : cache-hppa.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-hppa.s

cache-hppa.s : $(srcdir)/cache-hppa-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/cache-hppa-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e "s,!,',g" > cache-hppa.s

cache-hppa64.lo : cache-hppa64.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-hppa64.s

cache-hppa64.s : $(srcdir)/cache-hppa64-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/cache-hppa64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e "s,!,',g" > cache-hppa64.s

cache-powerpc.lo : cache-powerpc.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-powerpc.s

cache-powerpc.s : $(srcdir)/cache-powerpc-linux-macro.S $(srcdir)/cache-powerpc-macos.s
	case "$(OS)" in \
	  macos* | darwin*) syntax=macos;; \
	  *) syntax=linux;; \
	esac; \
	case $${syntax} in \
	  macos) \
	    grep -v '\.machine' $(srcdir)/cache-powerpc-$${syntax}.s > cache-powerpc.s || exit 1 ;; \
	  linux) \
	    $(CPP) $(ASPFLAGS) $(srcdir)/cache-powerpc-$${syntax}-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > cache-powerpc.s || exit 1 ;; \
	  *) \
	    cp $(srcdir)/cache-powerpc-$${syntax}.s cache-powerpc.s || exit 1 ;; \
	esac

cache-powerpc64-elfv2.lo : cache-powerpc64-elfv2.s
	$(LIBTOOL_COMPILE) $(CC) -x none -c cache-powerpc64-elfv2.s

cache-powerpc64-elfv2.s : $(srcdir)/cache-powerpc64-elfv2-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/cache-powerpc64-elfv2-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > cache-powerpc64-elfv2.s

libtrampoline.la : $(OBJECTS) ../gnulib-lib/libgnu.la
	$(LIBTOOL_LINK) $(CC) -o libtrampoline.la -rpath $(libdir) -no-undefined -export-symbols-regex $(LIBTRAMPOLINE_EXPORTED_SYMBOLS_REGEX) -version-info $(LIBTRAMPOLINE_VERSION_INFO) $(OBJECTS) ../gnulib-lib/libgnu.la $(LDFLAGS) $(LTLIBTHREAD)

install : all force
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(exec_prefix)
	mkdir -p $(DESTDIR)$(libdir)
	$(LIBTOOL_INSTALL) $(INSTALL_DATA) libtrampoline.la $(DESTDIR)$(libdir)/libtrampoline.la
	mkdir -p $(DESTDIR)$(includedir)
	$(INSTALL_DATA) $(srcdir)/trampoline.h $(DESTDIR)$(includedir)/trampoline.h
	mkdir -p $(DESTDIR)$(mandir)
	mkdir -p $(DESTDIR)$(mandir)/man3
	$(INSTALL_DATA) $(srcdir)/trampoline.3 $(DESTDIR)$(mandir)/man3/trampoline.3
	mkdir -p $(DESTDIR)$(datadir)
	mkdir -p $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(srcdir)/trampoline.html $(DESTDIR)$(htmldir)/trampoline.html

installdirs : force
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(exec_prefix)
	mkdir -p $(DESTDIR)$(libdir)
	mkdir -p $(DESTDIR)$(includedir)
	mkdir -p $(DESTDIR)$(mandir)
	mkdir -p $(DESTDIR)$(mandir)/man3
	mkdir -p $(DESTDIR)$(datadir)
	mkdir -p $(DESTDIR)$(htmldir)

uninstall : force
	$(LIBTOOL_UNINSTALL) $(RM) $(DESTDIR)$(libdir)/libtrampoline.la
	$(RM) $(DESTDIR)$(includedir)/trampoline.h
	$(RM) $(DESTDIR)$(mandir)/man3/trampoline.3
	$(RM) $(DESTDIR)$(htmldir)/trampoline.html

test1.o : $(srcdir)/test1.c $(srcdir)/trampoline.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/test1.c

test1 : test1.o libtrampoline.la
	$(LIBTOOL_LINK) $(CC) $(CFLAGS) -x none test1.o libtrampoline.la $(LDFLAGS) -o test1

test2.o : $(srcdir)/test2.c $(srcdir)/trampoline.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/test2.c

test2 : test2.o libtrampoline.la
	$(LIBTOOL_LINK) $(CC) $(CFLAGS) -x none test2.o libtrampoline.la $(LDFLAGS) -o test2

test2-c++.o : $(srcdir)/test2-c++.cc $(srcdir)/test2.c $(srcdir)/trampoline.h
	$(CXX) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) -c $(srcdir)/test2-c++.cc

test2-c++ : test2-c++.o libtrampoline.la
	$(LIBTOOL_LINK) $(CXX) $(CXXFLAGS) -x none test2-c++.o libtrampoline.la $(LDFLAGS) -o test2-c++

check1 : all test1
	./test1

check : all test1 test2
	./test1
	./test2
	./test2-c++
	touch tests.passed.$(HOST)
check : test2-c++

extracheck : check

mostlyclean : clean

clean : force
	$(RM) $(OBJECTS) `echo $(OBJECTS) | sed -e 's/\.lo/.o/g'` tramp-hppa.s tramp-hppa64.s tramp-powerpc.s tramp-powerpc64.s tramp-ia64.s cache-sparc.s cache-sparc64.s cache-alpha.s cache-hppa.s cache-hppa64.s cache-powerpc.s cache-powerpc64-elfv2.s libtrampoline.* core
	$(RM) -r .libs _libs
	$(RM) test1.o test1 test2.o test2 test2-c++.o test2-c++

distclean : clean
	$(RM) Makefile tests.passed.*

maintainer-clean : distclean


# List of source files (committed in version control or generated by Makefile.devel).
SOURCE_FILES = \
  COPYING \
  PLATFORMS PORTING README trampoline.3 trampoline.html \
  Makefile.devel \
  Makefile.maint \
  Makefile.in \
  trampoline.h \
  trampoline.c \
  tramp-hppa-macro.S \
  tramp-hppa64-macro.S \
  tramp-powerpc-aix.S \
  tramp-powerpc64-aix.S \
  tramp-ia64-macro.S \
  cache.c \
  cache-alpha.c cache-alpha-linux.s cache-alpha-macro.S \
  cache-hppa.c cache-hppa-linux.s cache-hppa-macro.S cache-hppa64-linux.s cache-hppa64-macro.S \
  cache-powerpc.c cache-powerpc-linux.s cache-powerpc-linux-macro.S cache-powerpc-macos.s \
  cache-powerpc64.c cache-powerpc64-elfv2-linux.s cache-powerpc64-elfv2-macro.S \
  cache-sparc.c cache-sparc-linux.s cache-sparc-macro.S cache-sparc64-linux.s cache-sparc64-macro.S \
  test1.c \
  test2.c test2-c++.cc
# List of distributed files generated by Makefile.maint.
GENERATED_FILES = \
  trampoline.man
# List of distributed files.
DISTFILES = $(SOURCE_FILES) $(GENERATED_FILES)

distdir : $(DISTFILES)
	for file in $(DISTFILES); do \
	  if test -f $$file; then dir='.'; else dir='$(srcdir)'; fi; \
	  cp -p "$$dir/$$file" '$(distdir)'/$$file || exit 1; \
	done


force :
