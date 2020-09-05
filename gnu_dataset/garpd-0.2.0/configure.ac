
dnl Process this file with autoconf to produce a configure script.
AC_INIT([garpd],[0.2.0],[garpd-devel@nongnu.org])
AC_REVISION([$Id$])
AM_INIT_AUTOMAKE([1.8])

dnl We require a modern autoconf and automake
AC_PREREQ([2.57])

dnl Locate the sources.
AC_CONFIG_SRCDIR([src/garpd.c])

dnl create a config.h file
AM_CONFIG_HEADER([config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile 
	doc/Makefile doc/garpd.8
	src/Makefile
	])

ISODATE=`date +%Y-%m-%d`
AC_SUBST([ISODATE])

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_MAKE_SET

dnl FreeBSD has argp as a separate library
dnl CAVEAT: argp-standalone library exports it's own strndup() function,
dnl not conforming to strndup(3). If we have no strndup() in libc (as
dnl in FreeBSD), and strndup() check is placed after argp check, argp's
dnl strndup() will be detected, which is wrong. So place argp check _after_
dnl strndup() check.
dnl
dnl Solaris has no argp library
dnl AC_CHECK_FUNC([argp_parse], 
dnl               [have_argp=yes],
dnl 	      AC_CHECK_LIB([argp], 
dnl                            [argp_parse], 
dnl                            [have_argp=yes; LIBS="-largp $LIBS"],
dnl                            [have_argp=no]))

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN
AC_C_CONST
AC_C_INLINE

dnl Checks for library functions.
AC_FUNC_ALLOCA

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([unistd.h getopt.h])

dnl Check for variables

dnl program_invocation_short_name is a GNU extension,
dnl we use getprogname() instead on FreeBSD
AC_MSG_CHECKING([for program_invocation_short_name])
AC_TRY_LINK([
		#define _GNU_SOURCE
		#include <errno.h>
	], [
		char *try = program_invocation_short_name;
	], [
		AC_DEFINE([HAVE_PROGRAM_SHORT_NAME], [1],
			[Define if you have program_invocation_shortname variable])
		AC_MSG_RESULT([yes])
	], [AC_MSG_RESULT([no])])

dnl Macro definitions

AC_OUTPUT
