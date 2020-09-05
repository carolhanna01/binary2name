# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Initializations
AC_INIT([mcsim], [6.2.0], [bug-mcsim@gnu.org])

AC_PREREQ(2.68)                   # requires at least autoconf v...
AC_CONFIG_SRCDIR([mod/mod.c])     # safety check for configure run */
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror])
AM_PROG_AR
LT_INIT                           # init libtool

# Checks for general programs.
AC_PROG_CC

# If --with-mpi=auto is used, try to find MPI, but use standard CC if not found.
# If --with-mpi=yes is used, try to find MPI and fail if it isn't found.
# If --with-mpi=no is used, use a standard C compiler instead.
AC_ARG_WITH(mpi, [AS_HELP_STRING([--with-mpi],
    [compile with MPI (parallelization) support. If none is found,
    MPI is not used. Default: auto])
],,[with_mpi=auto])
#
AX_PROG_CC_MPI([test x"$with_mpi" != xno],[
  use_mpi=yes
  AC_CHECK_TOOLS([MPICC], [mpicc mpixlc_r mpixlc hcc mpxlc_r mpxlc \
                           sxmpicc mpifcc mpgcc mpcc cmpicc])
  # MPICC="mpicc"
  ],[
  use_mpi=no
  if test x"$with_mpi" = xyes; then
    AC_MSG_FAILURE([MPI compiler requested, but couldn't use MPI.])
  else
    AC_MSG_NOTICE([No-MPI requested, won't use MPI.])
  fi
])

# Checks for programs needed by xmcsim
AC_CHECK_PROG(WISH_FOUND,wish,yes)
AC_CHECK_PROG(PLOT_FOUND,gnuplot,yes)

# Checks for libraries; watch the order between libraries
AC_CHECK_LIB([lapack], [main])
AC_CHECK_LIB([sundials_nvecserial], [main])
AC_CHECK_LIB([sundials_cvodes], [main])
AC_CHECK_LIB([gslcblas], [main])
AC_CHECK_LIB([gsl], [gsl_cdf_gamma_P])
AC_CHECK_LIB([m], [main])
AC_CHECK_LIB([sbml], [main])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([float.h limits.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([floor modf pow sqrt strchr erfc nextafter])

# Output files
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile
                 mod/Makefile
                 sim/Makefile
                 doc/Makefile
                 sim/makemcsim
                 sim/makemcsimp
                 sim/makemcsimd
                 sim/makemcsims])

AC_CONFIG_FILES([examples/perc/Makefile], [chmod +x examples/perc/Makefile])

AC_OUTPUT

