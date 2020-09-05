# Copyright (C) 2009-2014 David Sugar, Tycho Softworks.
# Copyright (C) 2015 Cherokees of Idaho.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AC_INIT([ccscript], [5.1.0])
AC_CONFIG_SRCDIR([src/interp.cpp])

LT_VERSION="5:6:0"
USES_UCOMMON_REQUIRED="7.0.0"

AC_CONFIG_AUX_DIR(autoconf)
AC_CANONICAL_SYSTEM
AC_PROG_CPP
AC_PROG_CC
AC_PROG_CXXCPP
AC_PROG_CXX
AC_LIBTOOL_WIN32_DLL
AM_PROG_LIBTOOL
AM_INIT_AUTOMAKE([dist-zip])
AM_CONFIG_HEADER(ccscript-config.h)

AC_C_RESTRICT
AC_C_VOLATILE
AC_C_INLINE

AC_SUBST(LT_VERSION)

CCSCRIPT_LIBS=""
CCSCRIPT_FLAGS=""

test -z "$plugindir" && plugindir='${libdir}'

AC_DEFUN([AC_SUBST_DIR], [
    ifelse($2,,,$1="[$]$2")
    result="***"
    prior="A"
    while test "$prior" != "$result" ; do
        prior=`(echo "[$]$1")`
        $1=`(
        test "x$prefix" = xNONE && prefix="$ac_default_prefix"
        test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"
        eval echo \""[$]$1"\"
        )`
        result=`(echo "[$]$1")`
    done
    AC_SUBST($1)
])

AC_ARG_ENABLE(debug,
    AC_HELP_STRING([--enable-debug],[compile for debugging]))
if test -z "$enable_debug" ; then
    enable_debug="no"
elif test $enable_debug = "yes" ; then
    CXXFLAGS="${CXXFLAGS} -g -DDEBUG"
fi

AC_ARG_WITH(pkg-config,
    AC_HELP_STRING([--with-pkg-config],[enable support for pkg-config]),[
    PKG_CHECK_MODULES(UCOMMON, ucommon >= $USES_UCOMMON_REQUIRED)
],[
    AC_PATH_PROG([UCOMMON],[ucommon-config],[none])
    if test $UCOMMON = "none" ; then
        AC_ERROR("required ucommon library missing")
    fi
    UCOMMON_CFLAGS=`$UCOMMON --cflags`
    UCOMMON_LIBS=`$UCOMMON --libs`

])

AC_CHECK_HEADERS(endian.h)
AC_CHECK_LIB([m], [sqrt], [PKG_CCSCRIPT_LIBS="$PKG_CCSCRIPT_LIBS -lm"])

if test "$enable_shared" != "yes" ; then
    AC_DEFINE(BUILD_STATIC, [1], [to disable plugin support])
fi

CCSCRIPT_FLAGS="$PKG_CCSCRIPT_FLAGS $UCOMMON_CFLAGS"
CCSCRIPT_LIBS="$PKG_CCSCRIPT_LIBS $UCOMMON_LIBS $ac_with_malloc"

if test "$sysconfdir" = '${prefix}/etc' ; then
    sysconfdir="/etc"
fi

AC_SUBST_DIR(default_cfgpath, sysconfdir)

case "$CCSCRIPT_FLAGS -I/usr/include" in
*-I${default_incpath}*)
    ;;
*)
    PKG_CCSCRIPT_FLAGS="$PKG_CCSCRIPT_FLAGS -I$default_incpath"
    ;;
esac

AC_SUBST_DIR(default_libpath, plugindir)

AC_DEFINE_UNQUOTED(DEFAULT_LIBPATH, "$default_libpath", [lib path])

AC_SUBST(plugindir)
AC_SUBST(USES_UCOMMON_REQUIRED)
AC_SUBST(PKG_CCSCRIPT_FLAGS)
AC_SUBST(PKG_CCSCRIPT_LIBS)
AC_SUBST(CCSCRIPT_FLAGS)
AC_SUBST(CCSCRIPT_LIBS)
AC_SUBST(CXXFLAGS)
AC_OUTPUT(Makefile inc/Makefile src/Makefile test/Makefile
ccscript.pc ccscript.spec PKGBUILD directive)

