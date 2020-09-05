# Copyright (C) 2006 Free Software Foundation.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AC_INIT(reconfig)
VERSION="1.2.0"
REQUIRES="1.0.0"
LT_VERSION="0:0"
LT_RELEASE="1.2"
AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_SYSTEM
AC_PROG_CXX
AC_PROG_CXXCPP
AM_PROG_LIBTOOL
AM_INIT_AUTOMAKE(ccaudio,[$VERSION])
AM_CONFIG_HEADER(src/private.h)

STAGE2=""
                                                                                
case "$target_os" in
darwin6*)
        STAGE2="macosx"
        ;;
esac
                                                                                
AC_SUBST(STAGE2)

OST_PROG_COMMON
OST_PROG_LIBVER
OST_PROG_LIBRARY(CCXX,[$LT_VERSION])
OST_AUTOMAKE_MODE
OST_MAINTAINER_MODE
OST_CCXX2_VERSION([$REQUIRES])
OST_CCXX2_DYNLOADER
AM_CONDITIONAL(MODULES, test $ost_cv_dynloader = yes)
OST_CDAUDIO
KDOC_DIR="\$(top_srcdir)/doc"
AC_SUBST(LT_VERSION)
AC_SUBST(LT_RELEASE)
AC_SUBST(KDOC_DIR)

if test -z "$MODULE_FLAGS" ; then
	MODULE_FLAGS="-module -shared"
	AC_SUBST(MODULE_FLAGS)
fi

AM_CONDITIONAL(MODULES, test "$ost_cv_dynloader" = yes)

WINVERSION="`echo $VERSION | sed 's/\./,/g'`,0"
AC_SUBST(WINVERSION)

AC_OUTPUT(src/Makefile config/Makefile m4/Makefile doc/Makefile win32/Makefile
dso/Makefile Makefile pkginfo ccaudio.spec freebsd/Makefile 
freebsd/pkg-plist win32/CCAUDIO.rc)

#if test -d doc/man3 ; then 
#	ls doc/man3/*.3cc | sed -e 's:^doc/man3/:man/man3/:' >>freebsd/pkg-plist ; fi

#if test -d doc/html ; then
#	ls doc/html | sed -e 's:^:share/doc/ccaudio/:' | grep -v private | grep -v globals >>freebsd/pkg-plist ; fi

