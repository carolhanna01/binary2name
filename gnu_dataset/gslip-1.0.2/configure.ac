##                                               -*- Autoconf -*-
## Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([GNU gSLIP], [1.0.2], [bug-gslip@gnu.org], [gslip])

AC_CONFIG_FILES([
      Makefile
      src/Makefile
      src/libgslip/Makefile
      src/test/Makefile
])

# automake macro's - REQUIRED

# TAP test ionsertions
AM_INIT_AUTOMAKE([foreign -Wall -Werror dist-bzip2])
AC_PROG_AWK

# linking libraries using a POSIX archiver
AM_PROG_AR

# autoconfig macro's

AC_PROG_RANLIB
AC_COPYRIGHT([GPL])

AC_SUBST([PACKAGE_COPYRIGHT_YEAR], [2015])
AC_DEFINE_UNQUOTED([PACKAGE_COPYRIGHT_YEAR], [$PACKAGE_COPYRIGHT_YEAR],
                   [The copyright year for this package])


AC_REVISION([$Revision: 2.69 $])

AC_CONFIG_SRCDIR([src/libgslip/Slip.h])
AC_CONFIG_HEADERS([config.h])



# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB

# Checks for libraries.

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset strerror strtol strtoul])

# Checks for header files.
AC_CHECK_HEADERS([inttypes.h libintl.h stdint.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# AC_CONFIG_MACRO_DIR([/*.m4]) directory for additional macros
#                needs ACLOCAL_AMFLAGS = -I im Makefile.am files


AC_OUTPUT

