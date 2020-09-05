#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([FULL-PACKAGE-NAME], [VERSION], [BUG-REPORT-ADDRESS])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE(dist-bzip2 dist-lzma 1.9)


# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.
AC_PATH_X
AC_CHECK_HEADERS([arpa/inet.h fcntl.h float.h memory.h netdb.h netinet/in.h stdlib.h string.h strings.h sys/file.h sys/ioctl.h sys/param.h sys/socket.h sys/time.h termios.h unistd.h])

LIBS="$LIBS -lX11 -lm -lXext"

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_INT32_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([alarm bzero gethostbyname gethostname gettimeofday isascii memset pow rint select socket sqrt strdup strpbrk strrchr strtol rand random])

AC_SUBST(CPPEXEC)
for i in /usr/ccs/lib/cpp /usr/local/bin/cpp /usr/lib/cpp /usr/bin/cpp /lib/cpp ; do
  if test -x $i ; then
    CPPEXEC=$i
  fi
done

OBVDIR=\$\(prefix\)/share/acm/
AC_SUBST(OBVDIR)

AC_CONFIG_FILES([Makefile
                 V/Makefile
                 V/lib/Makefile
                 V/test/Makefile
                 dis/Makefile
                 dis/disgen/Makefile
                 dis/lib/Makefile
                 doc/Makefile
                 objects/Makefile
                 src/Makefile])
AC_OUTPUT
