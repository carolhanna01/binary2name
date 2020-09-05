dnl configure.in script for dominion
dnl Process this file with autoconf to produce a configure script.
AC_INIT(Makefile.am)
AM_INIT_AUTOMAKE(dominion, 2.8.2)

dnl AM_MAINTAINER_MODE

dnl to satisfy Automake:
dnl AC_ARG_PROGRAM

dnl The VERSION here is the "interim working version"; once I put
dnl a new distribution out, that's it with that version number, and
dnl I must immediately change the version number here.
dnl VERSION=2.6b
dnl AC_SUBST(VERSION)

dnl PACKAGE=dominion
dnl AC_SUBST(PACKAGE)

dnl Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

AM_CONFIG_HEADER(config.h)
dnl AC_CONFIG_HEADERS(config.h)

dnl AC_DEFINE_UNQUOTED(PREFIX, "${prefix}")
dnl AC_DEFINE_UNQUOTED(INFO_PATH, "${prefix}/info")
dnl AC_DEFINE_UNQUOTED(INFO_INTRO, "${prefix}/lib/dominion/doc/info_intro")
dnl AC_DEFINE_UNQUOTED(VERSION, "$VERSION")

dnl AM_PROG_INSTALL

dnl Now I see what mail programs and editors are available
AC_CHECK_PROGS(MAIL_PROG, Mail mailx elm pine)

dnl Checks for libraries.
AC_CHECK_LIB(termcap, main)
AC_CHECK_LIB(ncurses, main)
AC_CHECK_LIB(curses, main)
AC_CHECK_LIB(m, main)
AC_CHECK_LIB(crypt, main)

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h limits.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.
AC_TYPE_SIGNAL
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(getcwd gethostname getwd)
AC_CHECK_FUNCS(srandom srand48)
AC_CHECK_FUNCS(beep)
AC_CHECK_FUNCS(cbreak)
AC_CHECK_FUNCS(crmode)
AC_CHECK_FUNCS(strstr strchr)

AC_OUTPUT(Makefile)
