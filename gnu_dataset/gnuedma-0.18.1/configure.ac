dnl   EDMA: Entorno de Desarrollo Modular y Abierto
dnl   Object Oriented and Componentweare Framework
dnl
dnl   Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
dnl             by David Martínez Oliveira
dnl
dnl   This program is free software: you can redistribute it and/or modify
dnl   it under the terms of the GNU General Public License as published by
dnl   the Free Software Foundation, either version 3 of the License, or
dnl   (at your option) any later version.
dnl
dnl   This program is distributed in the hope that it will be useful,
dnl   but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl   GNU General Public License for more details.
dnl
dnl   You should have received a copy of the GNU General Public License
dnl   along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl

dnl Process this file with autoconf to produce a configure script.
m4_include([version.m4])

define([AC_CACHE_LOAD], )
define([AC_CACHE_SAVE], )


AC_INIT(gnuedma, VERSION_NUMBER)
AC_CONFIG_SRCDIR(src/ini1.c)

AM_INIT_AUTOMAKE


dnl Checks for programs.
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LN_S
AC_LIBTOOL_DLOPEN
AC_DISABLE_STATIC
AM_PROG_LIBTOOL

dnl Checks for libraries.
AC_CHECK_LIB(dl, main)
AC_CHECK_LIB(m, main)
AC_CHECK_LIB(pthread, main)

dnl For building GZFILE. For now disabled
dnl AC_CHECK_LIB(z, main)

dnl Checks for header files.
AC_HEADER_STDC
dnl AC_CHECK_HEADERS(fcntl.h sys/time.h unistd.h)


dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_HEADER_TIME

dnl Checks system...

dnl ------------------------------------------------------------
dnl Build CFLAGS customized for this buils environment
dnl --------------------------------------------------------------

AC_MSG_RESULT(--- Building for $build_cpu in a $build_os)
case "$build_os" in
        linux*)
                AC_MSG_RESULT([--------------------------])
                AC_MSG_RESULT([Building on Linux Platform])
                AC_MSG_RESULT([--------------------------])
                CFLAGS="-DLINUX "
		OS_EXT="so"
                ;;

        darwin*)
                AC_MSG_RESULT([---------------------------])
                AC_MSG_RESULT([Building on Darwin Platform])
                AC_MSG_RESULT([---------------------------])
                CFLAGS="-DDARWIN "
		OS_EXT="dylib"
                ;;
        *)
                AC_MSG_WARN([------------------------------------])
                AC_MSG_WARN([Unknow target OS. Build could fail])
                AC_MSG_WARN([Falling back to UNIX default options])
                AC_MSG_WARN([------------------------------------])
                CFLAGS="-DLINUX "
		OS_EXT="so"
        ;;

esac

dnl for now do a debug build
CFLAGS="$CFLAGS -DGCC -fPIC -Wall -g"
MYEXE_LIBS='-L${top_srcdir}/src/'

AC_SUBST(MYEXE_LIBS)
AC_SUBST(OS_EXT)

AC_MSG_RESULT([CFLAGS for this build is : ${CFLAGS}])


dnl TO BE REMOVED???
dnl AC_TYPE_SIGNAL
dnl AC_FUNC_VPRINTF
dnl AC_CHECK_FUNCS(getcwd mkdir select socket strstr)

dnl GNUEDMA_BASEDIR will change for different OSes
if test "$prefix" = "NONE"; then
	GNUEDMA_BASEDIR="/usr/local"
else
	GNUEDMA_BASEDIR="$prefix";
fi
	
AC_SUBST(GNUEDMA_BASEDIR)

AC_OUTPUT(Makefile \
	edma-config \
	edma-env.sh \
	install-components.sh \
	debian/Makefile \
	src/const.h \
	src/Makefile \
	doc/Makefile \
	doc/reference/Makefile \
	doc/tutorials/Makefile \
	classes/Makefile \
	classes/edma_tools/Makefile \
	classes/edma_tools/idf_wizard_support/Makefile \
	classes/edma_tools/general/Makefile \
	classes/support/Makefile \
	classes/support/portable1/Makefile \
	classes/support/portable1/files/Makefile \
	classes/support/portable1/files/nfile/Makefile \
	classes/support/portable1/files/stream/Makefile \
	classes/support/portable1/files/filesystem/Makefile \
	classes/support/portable1/files/ndirectory/Makefile \
	classes/support/portable1/network/Makefile \
	classes/support/portable1/network/sockets/Makefile \
	classes/support/portable1/network/servers/Makefile \
	classes/support/portable1/network/services/Makefile \
	classes/support/portable1/network/services/echo_service/Makefile \
	classes/support/portable1/network/services/datalog/Makefile \
	classes/support/portable1/datamng/Makefile \
	classes/support/sysdb/Makefile \
	classes/support/dii/Makefile \
	classes/support/patterns/Makefile \
	classes/systems/Makefile \
	classes/systems/runable/Makefile \
	classes/systems/procexec/Makefile \
	classes/systems/anna/Makefile \
	classes/examples/Makefile \
	classes/examples/hello_world/Makefile \
	classes/examples/hello_world_v1/Makefile \
	tools/Makefile \
	tools/EFileGen/Makefile \
	tools/ClassInstaller/Makefile \
	tools/cleanup/Makefile \
	tools/khums/Makefile \
	tools/nal/Makefile \
	tests/Makefile \
	tests/abstract_classes/Makefile \
	tests/casting/Makefile \
	tests/static_methods/Makefile  \
	tests/pap_test/Makefile \
	tests/anchor_points/Makefile \
	tests/siu/Makefile \
	tests/exceptions/Makefile \
	tests/inheritance/Makefile \
	tests/serializer/Makefile \
	tests/update/Makefile \
	tests/virtual_methods/Makefile \
	tests/clonning/Makefile \
	tests/datatypes/Makefile \
	tests/aop/Makefile \
	tests/com-like-interface/Makefile \
	tests/patterns/Makefile \
	tests/dii/Makefile \
	tests/newobj/Makefile \
	tests/ethreads/Makefile \
	tests/emi/Makefile \
	tests/nal/Makefile \
	tests/unit_tests/Makefile \
	tests/unit_tests/creation/Makefile \
	tests/unit_tests/locators/Makefile \
	tests/unit_tests/inheritance/Makefile \
	tests/unit_tests/virtual_methods/Makefile \
	tests/unit_tests/rt_classes/Makefile \
	tests/unit_tests/clonning/Makefile
)

AC_MSG_RESULT([-------------------------------------------------])
AC_MSG_RESULT([Base dir is: ${GNUEDMA_BASEDIR}])
AC_MSG_RESULT([-------------------------------------------------])
