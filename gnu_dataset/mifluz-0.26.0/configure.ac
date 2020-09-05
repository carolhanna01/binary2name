#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
AC_INIT([mifluz],[0.26.0],[mifluz-bugs@gnu.org])
AM_CONFIG_HEADER([config.h])
AC_CONFIG_SRCDIR(config.h.in)
dnl     information on the package
MAJOR_VERSION=[`expr $PACKAGE_VERSION : '\([0-9][0-9]*\)'`]
AC_SUBST(MAJOR_VERSION)
MINOR_VERSION=[`expr $PACKAGE_VERSION : '[0-9][0-9]*\.\([0-9][0-9]*\)'`]
AC_SUBST(MINOR_VERSION)
MICRO_VERSION=[`expr $PACKAGE_VERSION : '[0-9][0-9]*\.[0-9][0-9]*\.\([0-9][0-9]*\)'`]
AC_SUBST(MICRO_VERSION)
AM_INIT_AUTOMAKE([mifluz], $PACKAGE_VERSION, nosubst)
AC_PREREQ(2.50)

AC_COPYRIGHT([
Copyright (c) 1999, 2000, 2001 The ht://Dig Group
For copyright details, see the file COPYING in your distribution
or the GNU General Public License version 2 or later
])
AC_REVISION($Revision: 1.4 $)

dnl     checks for programs

AC_PROG_MAKE_SET

AC_PATH_PROG(AR, ar, ar)
AC_PATH_PROG(SHELL, sh, /bin/sh)
AC_PATH_PROG(SED, sed, /bin/sed)
AC_PATH_PROG(PERL, perl, /usr/bin/perl)
AC_PATH_PROG(FIND, find, /bin/find)
AC_PATH_PROG(GUNZIP, gunzip, /bin/gunzip)
AC_PATH_PROG(TEXI2DVI, texi2dvi)
AC_PATH_PROG(DVIPS, dvips)
AC_PATH_PROG(MAKEINFO, makeinfo)
AC_CHECK_PROGS(RRDTOOL, rrdtool)
AC_CHECK_PROGS(TAR, tar gtar gnutar, tar)
AM_PROG_TIME

AC_PROG_INSTALL()
AC_CANONICAL_HOST

AC_AIX
AM_PROG_LIBTOOL
AM_PROG_LEX
AC_PROG_YACC

AC_PROG_CC
AC_PROG_CXX
AM_PATH_CCACHE
AC_COMPILE_WARNINGS
NO_RTTI
NO_EXCEPTIONS

AM_WITH_DMALLOC()

dnl Process all options before using them.  This is necessary because there
dnl are dependencies among them.
AM_OPTIONS_SET

dnl This is where we handle stuff that autoconf can't handle: compiler,
dnl preprocessor and load flags, libraries that the standard tests don't
dnl look for.  The default optimization is -O.
dnl
dnl There are additional libraries we need for some compiler/architecture
dnl combinations.
dnl
dnl Some architectures require DB to be compiled with special flags and/or
dnl libraries for threaded applications
dnl
dnl The makefile CC may be different than the CC used in config testing,
dnl because the makefile CC may be set to use $(LIBTOOL).
dnl
dnl XXX
dnl Don't override anything if it's already set from the environment.
optimize_def="-O"
case "$host_os" in
aix4.*)	   optimize_def="-O2"
	   CC=${CC-"xlc_r"}
	   CPPFLAGS="-D_THREAD_SAFE $CPPFLAGS";;
bsdi3*)	   CC=${CC-"shlicc2"}
	   optimize_def="-O2"
	   LIBS="-lipc $LIBS";;
bsdi*)	   optimize_def="-O2";;
freebsd*)  optimize_def="-O2"
	   CPPFLAGS="-D_THREAD_SAFE -pthread $CPPFLAGS";;
hpux*)	   CPPFLAGS="-D_REENTRANT $CPPFLAGS";;
irix*)	   optimize_def="-O2"
	   CPPFLAGS="-D_SGI_MP_SOURCE $CPPFLAGS";;
linux*)	   optimize_def="-O2"
	   CPPFLAGS="-D_REENTRANT $CPPFLAGS";;
mpeix*)	   CPPFLAGS="-D_POSIX_SOURCE -D_SOCKET_SOURCE $CPPFLAGS"
	   LIBS="-lsocket -lsvipc $LIBS";;
osf*)	   CPPFLAGS="-D_REENTRANT $CPPFLAGS";;
sco3.2v4*) CC=${CC-"cc -belf"}
	   LIBS="-lsocket -lnsl_s $LIBS";;
sco*)	   CC=${CC-"cc -belf"}
	   LIBS="-lsocket -lnsl $LIBS";;
solaris*)  CPPFLAGS="-D_REENTRANT $CPPFLAGS";;
esac

dnl If the user wants a debugging environment, add -g to the CFLAGS value.
dnl
dnl XXX
dnl Some compilers can't mix optimizing and debug flags.  The only way to
dnl handle this is to specify CFLAGS in the environment before configuring.
if test "$db_cv_debug" = yes; then
	AC_DEFINE(DEBUG,,[Define if you want a debugging version.])
	CFLAGS="$CFLAGS -g"
	CXXFLAGS="$CXXFLAGS -g"
fi

dnl Set some #defines based on configuration options.
if test "$db_cv_diagnostic" = yes; then
	AC_DEFINE(DIAGNOSTIC,,[Define if you want a version with run-time diagnostic checking. ])
fi
if test "$db_cv_debug_rop" = yes; then
	AC_DEFINE(DEBUG_ROP,,[Define if you want a version that logs read operations.])
fi
if test "$db_cv_debug_wop" = yes; then
	AC_DEFINE(DEBUG_WOP,,[Define if you want a version that logs write operations.])
fi

dnl Checks for typedefs, structures, and system/compiler characteristics.
AC_C_BIGENDIAN
AC_C_CONST
AC_HEADER_STAT
AC_HEADER_TIME
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

dnl Define any short-hand types we're missing.
AM_SHORTHAND_TYPES

dnl Checks for header files.
AC_HEADER_DIRENT
AC_CHECK_HEADERS(sys/select.h)
AC_CHECK_HEADERS(sys/time.h)

dnl Check for mutexes.  We do this here because it changes $LIBS.
AM_DEFINE_MUTEXES

dnl Check for system functions we optionally use.
AC_CHECK_FUNCS(getuid pstat_getdynamic sysconf)
AC_CHECK_FUNCS(sched_yield yield)

dnl Pread/pwrite.
dnl
dnl HP-UX has pread/pwrite, but it doesn't work with bigfile support.
dnl
dnl Linux has pread/pwrite, but the man pages say it requires a special
dnl #ifdef before the #include file, and even then it won't compile.
case "$host_os" in
hpux*|linux*)
	AC_MSG_WARN([pread/pwrite interfaces ignored on $host_os.]);;
*)	AC_CHECK_FUNCS(pread pwrite)
esac

dnl Check for fcntl(2) to deny child process access to file descriptors.
AC_CACHE_CHECK([for fcntl/F_SETFD], db_cv_fcntl_f_setfd, [dnl
AC_TRY_RUN([
#include <sys/types.h>
#include <fcntl.h>
main(){exit(fcntl(1, F_SETFD, 1) == -1);}],
	[db_cv_fcntl_f_setfd=yes], [db_cv_fcntl_f_setfd=no], [:])])
if test "$db_cv_fcntl_f_setfd" = yes; then
	AC_DEFINE(HAVE_FCNTL_F_SETFD,,[Define if fcntl/F_SETFD denies child access to file descriptors.])
fi

dnl Checks for system functions for which we don't have replacements.

dnl We require qsort(3) and select(2).
AC_CHECK_FUNCS(qsort, , AC_MSG_ERROR([No qsort library function.]))
AC_CHECK_FUNCS(select, , AC_MSG_ERROR([No select library function.]))

dnl Some versions of sprintf return a pointer to the first argument instead
dnl of a character count.  We assume that the return value of snprintf and
dnl vsprintf etc. will be the same as sprintf, and check the easy one.
AC_CACHE_CHECK([for int type sprintf return value], db_cv_sprintf_count, [dnl
AC_TRY_RUN([main(){char buf[20]; exit(sprintf(buf, "XXX") != 3);}],
	[db_cv_sprintf_count=yes], [db_cv_sprintf_count=no], [:])])
if test "$db_cv_sprintf_count" = no; then
	AC_DEFINE(SPRINTF_RET_CHARPNT,,[Define if your sprintf returns a pointer, not a length.])
fi

dnl Figure out how to create shared regions.
dnl
dnl First, we look for mmap.
dnl
dnl	BSD/OS has mlock(2), but it doesn't work.
dnl
dnl	Nextstep (version 3.3) apparently supports mmap(2) (the mmap symbol
dnl	is defined in the C library) but does not support munmap(2).  Don't
dnl	try to use mmap if we can't find munmap.
dnl
dnl	Ultrix has mmap(2), but it doesn't work.
mmap_ok=no
case "$host_os" in
ultrix*)
	AC_MSG_WARN([mmap(2) interface ignored on Ultrix.]);;
bsdi*)
	AC_MSG_WARN([mlock(2) interface ignored on BSD/OS.])
	mmap_ok=yes
	AC_CHECK_FUNCS(mmap munmap, , mmap_ok=no);;
*)
	mmap_ok=yes
	AC_CHECK_FUNCS(mlock munlock)
	AC_CHECK_FUNCS(mmap munmap, , mmap_ok=no);;
esac

dnl Second, we look for shmget.
dnl
dnl	SunOS has the shmget(2) interfaces, but there appears to be a missing
dnl	#include <debug/debug.h> file, so we ignore them.
shmget_ok=no
case "$host_os" in
sunos*)
	AC_MSG_WARN([shmget(2) interface ignored on SunOS.]);;
*)
	shmget_ok=yes
	AC_CHECK_FUNCS(shmget, , shmget_ok=no);;
esac

dnl We require either mmap/munmap(2) or shmget(2).
if test "$mmap_ok" = no -a "$shmget_ok" = no; then
	AC_MSG_ERROR([Neither mmap/munmap(2) or shmget(2) library functions.])
fi

dnl Checks for libraries.
CHECK_ZLIB
AM_ICONV
CHECK_UNAC
AC_SUBST(CXXFLAGS)dnl
AC_SUBST(LDFLAGS)dnl

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME

dnl More header checks--here use C++
AC_LANG(C++)
AC_CHECK_HEADERS(fcntl.h limits.h malloc.h sys/file.h sys/ioctl.h sys/time.h unistd.h getopt.h strings.h zlib.h)
#AC_CHECK_HEADER(fstream.h,nofstream=0,nofstream=1)
#if test "x$nofstream" = "x1" ; then
#AC_MSG_ERROR([To compile, you will need a C++ library. Try installing libstdc++.])
#fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_LANG(C)
AC_C_CONST
AC_STRUCT_TM

dnl Checks for library functions.
AC_CHECK_FUNCS(strdup strstr strtoul)
AC_REPLACE_FUNCS(getcwd strerror memcmp memcpy memmove raise)
AC_REPLACE_FUNCS(snprintf vsnprintf strncoll)
AC_REPLACE_FUNCS(strcasecmp strncasecmp strcasestr)
AC_FUNC_ALLOCA
AC_FUNC_STRFTIME
AC_FUNC_STRPTIME
AC_CHECK_FUNCS(localtime_r timegm)

dnl Vendors are doing 64-bit lseek in different ways.
dnl AIX, HP/UX and Solaris all use _FILE_OFFSET_BITS to specify a "big-file"
dnl environment.
dnl
dnl You can't build C++ with big-file support on HP-UX, the include files
dnl are wrong.  On Solaris 8, <fcntl.h> included with big-file support
dnl is not compatible with C++.
if test "$db_cv_bigfile" = no; then
	offset_bits=false
	case "$host_os" in
        solaris2.8|hpux*)
		if test "$db_cv_cxx" = "yes"; then
	AC_MSG_WARN([Large file and C++ API support are incompatible on HP-UX])
	AC_MSG_WARN([and Solaris 8; large file support has been turned off.])
		else
			offset_bits=true
		fi;;
	aix*|solaris*)
		offset_bits=true ;;
	linux*)	if test "$build_cpu" = "i686"
		then
			offset_bits=true
			AC_DEFINE(HAVE_LARGEFILE_SOURCE,,[Define if building big-file environment (Linux).])
		fi
		;;
	esac
	if $offset_bits
	then
		AC_DEFINE(HAVE_FILE_OFFSET_BITS,,[Define if building big-file environment (e.g., AIX, HP/UX, Solaris).])
	fi
fi

dnl Check if the c++ compiler knows bool
AC_LANG(C++)
AC_MSG_CHECKING(for bool data type?)
AC_TRY_COMPILE(,[bool i;],
  [AC_MSG_RESULT(yes);AC_DEFINE(HAVE_BOOL,,[Define if you have the bool type.])],
  [AC_MSG_RESULT(no)])

dnl Check if the c++ compiler knows true and false
AC_MSG_CHECKING(for true and false?)
AC_TRY_COMPILE(,[int i, j; i= true; j= false;],
  [AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_TRUE,,[Define if you C++ compiler doesn't knows true.])
     AC_DEFINE(HAVE_FALSE,,[Define if you C++ compiler doesn't knows false])],
  [AC_MSG_RESULT(no)])

dnl Any remaining tests probably want the C compiler
AC_LANG(C)
if test ! -d "./db"; then
  mkdir db
fi

AC_CONFIG_FILES([mifluz-config], [chmod +x mifluz-config])
AC_CONFIG_FILES([Makefile mifluz.spec clib/Makefile db/Makefile db/db.h mifluz/Makefile htdb/Makefile search/Makefile doc/Makefile test/Makefile test/test_functions examples/Makefile man/Makefile])
AC_OUTPUT()
