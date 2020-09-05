#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT(fisicalab, 0.3.5)

# If GNUSTEP_MAKEFILES is undefined, try to use gnustep-config to determine it.
if test -z "$GNUSTEP_MAKEFILES"; then
  GNUSTEP_MAKEFILES=`gnustep-config --variable=GNUSTEP_MAKEFILES 2>&5`
fi

if test -z "$GNUSTEP_MAKEFILES"; then
  AC_MSG_ERROR([You must have the gnustep-make package installed and set up the GNUSTEP_MAKEFILES environment variable to contain the path to the makefiles directory before configuring!])
fi

AC_CONFIG_AUX_DIR([$GNUSTEP_MAKEFILES])

OBJC_FLAGS=`gnustep-config --objc-flags 2>&5`
GSL_PREFIX=`gsl-config --prefix 2>&5`
GNUSTEP_LOCAL_LIBRARIES=`gnustep-config --variable=GNUSTEP_LOCAL_LIBRARIES 2>&5`
GNUSTEP_SYSTEM_LIBRARIES=`gnustep-config --variable=GNUSTEP_SYSTEM_LIBRARIES 2>&5`

CPPFLAGS="$CPPFLAGS -x objective-c $OBJC_FLAGS"
LDFLAGS="$LDFLAGS -L$GSL_PREFIX/lib -L$GNUSTEP_LOCAL_LIBRARIES -L$GNUSTEP_SYSTEM_LIBRARIES"

# Checks for programs.
AC_PROG_CC

# Checks for libraries.
BASE=yes
GUI=yes
GSL=yes
AC_CHECK_LIB(gnustep-base,main,[],BASE=no)
AC_CHECK_LIB(gnustep-gui,main,[],GUI=no)
AC_CHECK_LIB(m,main,[],GSL=no)
AC_CHECK_LIB(gslcblas,main,[],GSL=no)
AC_CHECK_LIB(gsl,main,[],GSL=no)

if test $BASE = no; then
   AC_MSG_ERROR([`fisicalab requires the library gnustep-base.']);
fi

if test $GUI = no; then
   AC_MSG_ERROR([`fisicalab requires the library gnustep-gui.']);
fi

if test $GSL = no; then
   AC_MSG_ERROR([`fisicalab requires the library gsl.']);
fi

# Checks for header files.
AC_CHECK_HEADER([Foundation/Foundation.h], [],
[AC_MSG_ERROR([You should install the header files for pachage gnustep-base.])])
AC_CHECK_HEADER([AppKit/AppKit.h], [],
[AC_MSG_ERROR([You should install the header files for pachage gnustep-gui.])])
AC_CHECK_HEADERS([gsl/gsl_vector.h gsl/gsl_multiroots.h gsl/gsl_rng.h], [],
[AC_MSG_ERROR([You should install the header files for pachage gsl.])])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_CHECK_FUNCS([floor sqrt])

AC_CONFIG_FILES([GNUmakefile GNUmakefile.preamble])
AC_OUTPUT
