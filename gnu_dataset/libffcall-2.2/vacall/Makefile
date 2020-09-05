# Makefile for vacall

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
INCLUDES = -I. -I$(srcdir) -I.. -I$(srcdir)/..
INCLUDES_WITH_GNULIB = $(INCLUDES) -I../gnulib-lib -I$(srcdir)/../gnulib-lib
ASPFLAGS = `if test false = true; then echo '-DASM_UNDERSCORE'; fi`
LDFLAGS = 
AR = ar
AR_FLAGS = rc
RANLIB = ranlib
MV = mv
LN = ln
RM = rm -f


# Programs used by "make install":
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

#### End of system configuration section. ####

SHELL = /bin/sh

OBJECTS = vacall.o vacall-libapi.o vacall-structcpy.o

all : $(OBJECTS) libvacall.a $(srcdir)/vacall.3 $(srcdir)/vacall.html

vacall.o : vacall-$(CPU).o
	$(RM) vacall.o
	$(LN) vacall-$(CPU).o vacall.o

vacall-i386.o : vacall-i386.s
	$(CC) -x none -c vacall-i386.s

vacall-i386.s : $(srcdir)/vacall-i386-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common - < $(srcdir)/vacall-i386-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,@ ,@,g' -e 's,//.*$$,,' -e 's/##//g' > vacall-i386.s

# vacall-i386.o : $(srcdir)/vacall-i386-msvc.c
# 	$(CC) $(INCLUDES) -I$(srcdir)/../common $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/vacall-i386-msvc.c -o vacall-i386.o

vacall-sparc.o : vacall-sparc.s
	$(CC)  -x none -c vacall-sparc.s

vacall-sparc.s : $(srcdir)/vacall-sparc-macro.S
	$(CPP)  $(ASPFLAGS) -I$(srcdir)/../common - < $(srcdir)/vacall-sparc-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,//.*$$,,' -e 's,\$$,#,g' -e 's,# ,#,g' > vacall-sparc.s

vacall-sparc64.o : vacall-sparc64.s
	$(CC) -x none -c vacall-sparc64.s

vacall-sparc64.s : $(srcdir)/vacall-sparc64-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common - < $(srcdir)/vacall-sparc64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,//.*$$,,' -e 's,\$$,#,g' -e 's,# ,#,g' > vacall-sparc64.s

vacall-m68k.o : vacall-m68k.s
	$(CC) -x none -c vacall-m68k.s

vacall-m68k.s : $(srcdir)/vacall-m68k.mit.S $(srcdir)/vacall-m68k.motorola.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-m68k.motorola.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' | if test false = true; then sed -e 's/\$$//g'; else sed -e 's/\$$/%/g'; fi > vacall-m68k.s

vacall-mips.o : vacall-mips.s
	$(CC) -x none -c vacall-mips.s

vacall-mips.s : $(srcdir)/vacall-mips-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-mips-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-mips.s

vacall-mipsn32.o : vacall-mipsn32.s
	$(CC) -x none -c vacall-mipsn32.s

vacall-mipsn32.s : $(srcdir)/vacall-mipsn32-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-mipsn32-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-mipsn32.s

vacall-mips64.o : vacall-mips64.s
	$(CC) -x none -c vacall-mips64.s

vacall-mips64.s : $(srcdir)/vacall-mips64-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-mips64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-mips64.s

vacall-alpha.o : vacall-alpha.s
	$(CC) -x none -c vacall-alpha.s

vacall-alpha.s : $(srcdir)/vacall-alpha-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-alpha-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-alpha.s

vacall-hppa.o : vacall-hppa.s
	$(CC) -x none -c vacall-hppa.s

vacall-hppa.s : $(srcdir)/vacall-hppa-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-hppa-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e "s,!,',g" > vacall-hppa.s

vacall-hppa64.o : vacall-hppa64.s
	$(CC) -x none -c vacall-hppa64.s

vacall-hppa64.s : $(srcdir)/vacall-hppa64-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-hppa64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e "s,!,',g" > vacall-hppa64.s

vacall-arm.o : vacall-arm.s
	$(CC) -x none -c vacall-arm.s

vacall-arm.s : $(srcdir)/vacall-arm-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-arm-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//,@,g' -e 's,\$$,#,g' > vacall-arm.s

vacall-armhf.o : vacall-armhf.s
	$(CC) -x none -c vacall-armhf.s

vacall-armhf.s : $(srcdir)/vacall-armhf-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-armhf-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//,@,g' -e 's,\$$,#,g' > vacall-armhf.s

vacall-arm64.o : vacall-arm64.s
	$(CC) -x none -c vacall-arm64.s

vacall-arm64.s : $(srcdir)/vacall-arm64-macro.S
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-arm64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//,@,g' -e 's,\$$,#,g' > vacall-arm64.s

vacall-powerpc.o : vacall-powerpc.s
	$(CC) -x none -c vacall-powerpc.s

vacall-powerpc.s : $(srcdir)/vacall-powerpc-aix.s $(srcdir)/vacall-powerpc-linux-macro.S $(srcdir)/vacall-powerpc-macos.s $(srcdir)/vacall-powerpc-sysv4-macro.S
	case "$(OS)" in \
	  aix*) syntax=aix;; \
	  linux* | netbsd* | openbsd*) syntax=linux;; \
	  macos* | darwin*) syntax=macos;; \
	  *) syntax=sysv4;; \
	esac; \
	case $${syntax} in \
	  macos) \
	    grep -v '\.machine' $(srcdir)/vacall-powerpc-$${syntax}.s > vacall-powerpc.s || exit 1 ;; \
	  linux | sysv4) \
	    $(CPP) $(ASPFLAGS) $(srcdir)/vacall-powerpc-$${syntax}-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-powerpc.s || exit 1 ;; \
	  *) \
	    cp $(srcdir)/vacall-powerpc-$${syntax}.s vacall-powerpc.s || exit 1 ;; \
	esac

vacall-powerpc64.o : vacall-powerpc64.s
	$(CC) -x none -c vacall-powerpc64.s

vacall-powerpc64.s : $(srcdir)/vacall-powerpc64-aix.s $(srcdir)/vacall-powerpc64-linux.S
	case "$(OS)" in \
	  aix*) syntax=aix;; \
	  *) syntax=linux;; \
	esac; \
	case $${syntax} in \
	  linux) \
	    $(CPP) $(ASPFLAGS) $(srcdir)/vacall-powerpc64-$${syntax}.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-powerpc64.s || exit 1 ;; \
	  *) \
	    cp $(srcdir)/vacall-powerpc64-$${syntax}.s vacall-powerpc64.s || exit 1 ;; \
	esac

vacall-powerpc64-elfv2.o : vacall-powerpc64-elfv2.s
	$(CC) -x none -c vacall-powerpc64-elfv2.s

vacall-powerpc64-elfv2.s : $(srcdir)/vacall-powerpc64-elfv2-linux.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-powerpc64-elfv2-linux.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-powerpc64-elfv2.s

vacall-ia64.o : vacall-ia64.s
	$(CC) -x none -c vacall-ia64.s

vacall-ia64.s : $(srcdir)/vacall-ia64-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-ia64-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-ia64.s

vacall-x86_64.o : vacall-x86_64.s
	$(CC) -x none -c vacall-x86_64.s

vacall-x86_64.s : $(srcdir)/vacall-x86_64-macro.S $(srcdir)/vacall-x86_64-windows-macro.S
	case "$(OS)" in \
	  cygwin* | mingw*) variant='-windows';; \
	  *) variant='';; \
	esac; \
	$(CPP) $(ASPFLAGS) -I$(srcdir)/../common - < $(srcdir)/vacall-x86_64$${variant}-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,@ ,@,g' -e 's,//.*$$,,' -e 's/##//g' > vacall-x86_64.s

# vacall-x86_64.o : vacall-x86_64.asm
# 	ml64 -c -nologo vacall-x86_64.asm

# vacall-x86_64.asm : $(srcdir)/vacall-x86_64-windows-macro.S
# 	{ echo 'EXTERNDEF vacall_function:QWORD'; $(CPP) $(ASPFLAGS) -I$(srcdir)/../common $(srcdir)/vacall-x86_64-windows-macro.S | grep -v '^#'; echo 'END'; } > vacall-x86_64.asm

vacall-x86_64-x32.o : vacall-x86_64-x32.s
	$(CC) -x none -c vacall-x86_64-x32.s

vacall-x86_64-x32.s : $(srcdir)/vacall-x86_64-x32-linux.s
	cp $(srcdir)/vacall-x86_64-x32-linux.s vacall-x86_64-x32.s

vacall-s390.o : vacall-s390.s
	$(CC) -x none -c vacall-s390.s

vacall-s390.s : $(srcdir)/vacall-s390-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-s390-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-s390.s

vacall-s390x.o : vacall-s390x.s
	$(CC) -x none -c vacall-s390x.s

vacall-s390x.s : $(srcdir)/vacall-s390x-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-s390x-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-s390x.s

vacall-riscv32-ilp32d.o : vacall-riscv32-ilp32d.s
	$(CC) -x none -c vacall-riscv32-ilp32d.s

vacall-riscv32-ilp32d.s : $(srcdir)/vacall-riscv32-ilp32d-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-riscv32-ilp32d-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-riscv32-ilp32d.s

vacall-riscv64-lp64d.o : vacall-riscv64-lp64d.s
	$(CC) -x none -c vacall-riscv64-lp64d.s

vacall-riscv64-lp64d.s : $(srcdir)/vacall-riscv64-lp64d-macro.S
	$(CPP) $(ASPFLAGS) $(srcdir)/vacall-riscv64-lp64d-macro.S | grep -v '^ *#line' | grep -v '^#' | sed -e 's,% ,%,g' -e 's,//.*$$,,' > vacall-riscv64-lp64d.s

vacall-libapi.o : $(srcdir)/vacall-libapi.c $(srcdir)/vacall-internal.h $(srcdir)/vacall.h ../config.h
	$(CC) $(INCLUDES_WITH_GNULIB) $(CPPFLAGS) $(CFLAGS) -fno-strict-aliasing -c $(srcdir)/vacall-libapi.c

vacall-structcpy.o : $(srcdir)/vacall-structcpy.c $(srcdir)/../common/structcpy.c
	$(CC) -I$(srcdir)/../common $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/vacall-structcpy.c

libvacall.a : $(OBJECTS)
	$(RM) libvacall.a
	$(AR) $(AR_FLAGS) libvacall.a $(OBJECTS)
	$(RANLIB) libvacall.a

install : all force
	mkdir -p $(DESTDIR)$(prefix)
	mkdir -p $(DESTDIR)$(exec_prefix)
	mkdir -p $(DESTDIR)$(libdir)
	$(INSTALL_DATA) libvacall.a $(DESTDIR)$(libdir)/libvacall.a
	mkdir -p $(DESTDIR)$(includedir)
	$(INSTALL_DATA) $(srcdir)/vacall.h $(DESTDIR)$(includedir)/vacall.h
	mkdir -p $(DESTDIR)$(mandir)
	mkdir -p $(DESTDIR)$(mandir)/man3
	$(INSTALL_DATA) $(srcdir)/vacall.3 $(DESTDIR)$(mandir)/man3/vacall.3
	mkdir -p $(DESTDIR)$(datadir)
	mkdir -p $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(srcdir)/vacall.html $(DESTDIR)$(htmldir)/vacall.html

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
	$(RM) $(DESTDIR)$(libdir)/libvacall.a
	$(RM) $(DESTDIR)$(includedir)/vacall.h
	$(RM) $(DESTDIR)$(mandir)/man3/vacall.3
	$(RM) $(DESTDIR)$(htmldir)/vacall.html

minitests.o : $(srcdir)/minitests.c $(srcdir)/tests.c $(srcdir)/vacall.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/minitests.c

minitests.s : $(srcdir)/minitests.c $(srcdir)/tests.c $(srcdir)/vacall.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -S $(srcdir)/minitests.c

minitests : minitests.o libvacall.a
	$(CC) $(CFLAGS) -x none minitests.o libvacall.a $(LDFLAGS) -o minitests

minitests-c++.o : $(srcdir)/minitests-c++.cc $(srcdir)/minitests.c $(srcdir)/tests.c $(srcdir)/vacall.h
	$(CXX) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) -c $(srcdir)/minitests-c++.cc

minitests-c++ : minitests-c++.o libvacall.a
	$(CXX) $(CXXFLAGS) -x none minitests-c++.o libvacall.a $(LDFLAGS) -o minitests-c++

check : all minitests
	./minitests > minitests.out
	LC_ALL=C uniq -u < minitests.out > minitests.output.$(HOST)
	test '!' -s minitests.output.$(HOST)
	./minitests-c++ > minitests-c++.out
	LC_ALL=C uniq -u < minitests-c++.out > minitests-c++.output.$(HOST)
	test '!' -s minitests-c++.output.$(HOST)
check : minitests-c++

tests.o : $(srcdir)/tests.c $(srcdir)/vacall.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/tests.c

tests.s : $(srcdir)/tests.c $(srcdir)/vacall.h
	$(CC) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) -S $(srcdir)/tests.c

tests : tests.o libvacall.a
	$(CC) $(CFLAGS) -x none tests.o libvacall.a $(LDFLAGS) -o tests

extracheck : all tests
	./tests > tests.out
	LC_ALL=C uniq -u < tests.out > tests.output.$(HOST)
	test '!' -s tests.output.$(HOST)

mostlyclean : clean

clean : force
	$(RM) *.o *.a core
	$(RM) vacall-i386.s vacall-sparc.s vacall-sparc64.s vacall-m68k.s vacall-mips.s vacall-mipsn32.s vacall-mips64.s vacall-alpha.s vacall-hppa.s vacall-hppa64.s vacall-arm.s vacall-armhf.s vacall-arm64.s vacall-powerpc.s vacall-powerpc64.s vacall-powerpc64-elfv2.s vacall-ia64.s vacall-x86_64.s vacall-x86_64.asm vacall-x86_64-x32.s vacall-s390.s vacall-s390x.s vacall-riscv32-ilp32d.s vacall-riscv64-lp64d.s
	$(RM) minitests.o minitests.s minitests minitests.out
	$(RM) minitests-c++.o minitests-c++ minitests-c++.out
	$(RM) tests.o tests.s tests tests.out

distclean : clean
	$(RM) Makefile minitests.output.* minitests-c++.output.* tests.output.*

maintainer-clean : distclean


# List of source files (committed in version control or generated by Makefile.devel).
SOURCE_FILES = \
  COPYING \
  PLATFORMS README vacall.3 vacall.html \
  Makefile.devel \
  Makefile.maint \
  Makefile.in \
  vacall.h vacall-internal.h \
  vacall-alpha.c vacall-alpha-linux.s vacall-alpha-macro.S \
  vacall-arm.c vacall-arm-linux.s vacall-arm-linux-pic.s vacall-arm-macro.S \
  vacall-armhf.c vacall-armhf-linux.s vacall-armhf-linux-pic.s vacall-armhf-macro.S \
  vacall-arm64.c vacall-arm64-macro.S \
  vacall-hppa.c vacall-hppa-linux.s vacall-hppa-macro.S \
  vacall-hppa64.c vacall-hppa64-linux.s vacall-hppa64-macro.S \
  vacall-i386.c vacall-i386-linux.s vacall-i386-linux-pic.s vacall-i386-macro.S \
  vacall-ia64.c vacall-ia64-linux.s vacall-ia64-macro.S \
  vacall-m68k.c vacall-m68k-linux.s vacall-m68k-sun.s vacall-m68k.mit.S vacall-m68k.motorola.S \
  vacall-mips.c vacall-mipseb-linux.s vacall-mipsel-linux.s vacall-mipseb-macro.S vacall-mipsel-macro.S \
  vacall-mipsn32.c vacall-mipsn32eb-linux.s vacall-mipsn32el-linux.s vacall-mipsn32eb-macro.S vacall-mipsn32el-macro.S \
  vacall-mips64.c vacall-mips64eb-linux.s vacall-mips64el-linux.s vacall-mips64eb-macro.S vacall-mips64el-macro.S \
  vacall-powerpc.c \
  vacall-powerpc-aix.s \
  vacall-powerpc-linux.s vacall-powerpc-linux-macro.S vacall-powerpc-macos.s vacall-powerpc-sysv4-macro.S \
  vacall-powerpc64.c vacall-powerpc64-aix.s vacall-powerpc64-linux.S vacall-powerpc64-elfv2-linux.S \
  vacall-riscv32.c vacall-riscv32-ilp32d-linux.s vacall-riscv32-ilp32d-macro.S \
  vacall-riscv64.c vacall-riscv64-lp64d-linux.s vacall-riscv64-lp64d-macro.S \
  vacall-s390.c vacall-s390-linux.s vacall-s390-macro.S \
  vacall-s390x.c vacall-s390x-linux.s vacall-s390x-macro.S \
  vacall-sparc.c vacall-sparc-linux.s vacall-sparc-linux-pic.s vacall-sparc-macro.S \
  vacall-sparc64.c vacall-sparc64-linux.s vacall-sparc64-linux-pic.s vacall-sparc64-macro.S \
  vacall-x86_64.c vacall-x86_64-linux.s vacall-x86_64-macro.S vacall-x86_64-x32-linux.s \
  vacall-x86_64-windows.c vacall-x86_64-windows.s vacall-x86_64-windows-macro.S \
  vacall-libapi.c \
  vacall-structcpy.c \
  minitests.c minitests-c++.cc \
  tests.c
# List of distributed files generated by Makefile.maint.
GENERATED_FILES = \
  vacall.man \
  vacall-i386-msvc.c
# List of distributed files.
DISTFILES = $(SOURCE_FILES) $(GENERATED_FILES)

distdir : $(DISTFILES)
	for file in $(DISTFILES); do \
	  if test -f $$file; then dir='.'; else dir='$(srcdir)'; fi; \
	  cp -p "$$dir/$$file" '$(distdir)'/$$file || exit 1; \
	done


force :

