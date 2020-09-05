#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([GnuDOS], [1.9], [mohammed_isam1984@yahoo.com])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror gnu])
#AC_CHECK_LIB([cups], [cupsGetJobs])
AM_PROG_AR
LT_INIT

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h sys/ioctl.h termios.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_CHECK_FUNCS([getcwd memset mkdir rmdir strchr strdup strrchr strstr])

AC_CONFIG_FILES([Makefile
                 src/corelib/Makefile
                 src/mino/Makefile
                 src/fog/Makefile
                 src/prime/Makefile
                 doc/Makefile
                 doc/corelib/Makefile
                 doc/mino/Makefile
                 doc/fog/Makefile
                 doc/prime/Makefile
                 info/Makefile
                 man/Makefile])
AC_OUTPUT
