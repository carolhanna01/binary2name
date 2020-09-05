#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
m4_include([modules.af.m4])
AC_PREREQ([2.69])
AC_INIT([GNU Thales], [1.99], [KAction@gnu.org])
AM_INIT_AUTOMAKE([foreign])
GUILE_PROGS
GUILE_SITE_DIR
AUTOFRISK_CHECKS
AUTOFRISK_SUMMARY

guile_exec_prefix=`$PKG_CONFIG --print-errors --variable=exec_prefix guile-$GUILE_EFFECTIVE_VERSION`

GUILE_SITE=`echo $GUILE_SITE|sed s%^$guile_exec_prefix%%`
AC_SUBST([GUILE_SITE])


# Checks for programs.

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CONFIG_FILES([
	Makefile
	doc/Makefile
	src/Makefile
	src/thales/Makefile])

AC_OUTPUT
