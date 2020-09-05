# Process this file with autoconf to produce a configure script.

AC_INIT([gnunet-qt],[0.8.1],[bug-gnunet@gnu.org])
# Every other copy of the package version number gets its value from here
AM_INIT_AUTOMAKE([gnunet-qt], 0.8.1)

# create a config.h file (Automake will add -DHAVE_CONFIG_H)
AM_CONFIG_HEADER(config.h)

AC_SUBST(VERSION)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

# Check system type
case "$host_os" in
*cygwin* | *mingw*)
      LIBPREFIX=lib
      DLLDIR=bin
     ;;
*)
      LIBPREFIX=
      DLLDIR=lib
;;
esac

# FIXME: should support --with-qt, but
# this would have to be done before the
# "case $host_os" which depends on QTDIR;
# However, the check itself may depend
# on the case-host_os defines, so yuck...
if test -z $QTDIR
then
# best guess
  QTDIR=/usr
fi

AC_PATH_XTRA

case "$host_os" in
*darwin* | *rhapsody* | *macosx*)
     AC_DEFINE_UNQUOTED(OSX,1,[This is an OS X system])
     CFLAGS="-no-cpp-precomp $CFLAGS"
     LDFLAGS="-flat_namespace -undefined suppress $LDFLAGS"
     DEFAULT_QTLIBS="-L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
      ;;
linux*)
     AC_DEFINE_UNQUOTED(LINUX,1,[This is a Linux system])
     DEFAULT_QTLIBS="-Wl,-rpath,$QTDIR/lib -L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
freebsd*)
     AC_DEFINE_UNQUOTED(SOMEBSD,1,[This is a BSD system])
     AC_DEFINE_UNQUOTED(FREEBSD,1,[This is a FreeBSD system])
     CFLAGS="-D_THREAD_SAFE $CFLAGS"
     DEFAULT_QTLIBS="-Wl,-rpath,$QTDIR/lib -L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
openbsd*)
     AC_DEFINE_UNQUOTED(SOMEBSD,1,[This is a BSD system])
     AC_DEFINE_UNQUOTED(OPENBSD,1,[This is an OpenBSD system])
     LIBS=`echo $LIBS | sed -e "s/-ldl//"`
     DEFAULT_QTLIBS="-Wl,-rpath,$QTDIR/lib -L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
netbsd*)
     AC_DEFINE_UNQUOTED(SOMEBSD,1,[This is a BSD system])
     AC_DEFINE_UNQUOTED(NETBSD,1,[This is a NetBSD system])
     LIBPREFIX=
     DLLDIR=lib
     DEFAULT_QTLIBS="-Wl,-rpath,$QTDIR/lib -L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
*solaris*)
     AC_DEFINE_UNQUOTED(SOLARIS,1,[This is a Solaris system])
     AC_DEFINE_UNQUOTED(_REENTRANT,1,[Need with solaris or errno doesnt work])
     DEFAULT_QTLIBS="-Wl,-rpath,$QTDIR/lib -L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
*arm-linux*)
     AC_DEFINE_UNQUOTED(LINUX,1,[This is a Linux system])
     CFLAGS="-D_REENTRANT -fPIC -pipe $CFLAGS"
     DEFAULT_QTLIBS="-Wl,-rpath,$QTDIR/lib -L$QTDIR/lib -lQtGui -lQtOpenGL -lQtCore $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS -lGLU -lGL -lpthread"
     DEFAULT_QTINC="-I$QTDIR/include/qt4 -I$QTDIR/include/qt4/QtGui -I$QTDIR/include/qt4/QtCore -I$QTDIR/include/qt4/QtOpenGL $X_CFLAGS -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
*cygwin*)
     AC_DEFINE_UNQUOTED(CYGWIN,1,[This is a Cygwin system])
     AC_DEFINE_UNQUOTED(WINDOWS,1,[This is a Windows system])
     LDFLAGS="$LDFLAGS -no-undefined"
     CFLAGS="-mms-bitfields $CFLAGS"
     AC_PROG_CXX
     DEFAULT_QTLIBS="-L$QTDIR/lib -lopengl32 -lglu32 -lgdi32 -luser32 -lmingw32 -lqtmain -lQtOpenGL4 -lQtGui4 -lQtCore4 -mthreads -Wl,-enable-stdcall-fixup -Wl,-enable-auto-import -Wl,-enable-runtime-pseudo-reloc -Wl,-subsystem,windows"
     DEFAULT_QTINC="-I$QTDIR/include -I$QTDIR/include/QtCore -I$QTDIR/include/QtGui -I$QTDIR/include/QtOpenGL -DUNICODE -DQT_LARGEFILE_SUPPORT -DQT_DLL -DQT_NO_DEBUG -DQT_OPENGL_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_THREAD_SUPPORT -DQT_NEEDS_QMAIN -frtti -fexceptions"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
*mingw*)
     AC_DEFINE_UNQUOTED(MINGW,1,[This is a MinGW system])
     AC_DEFINE_UNQUOTED(WINDOWS,1,[This is a Windows system])
     AC_DEFINE_UNQUOTED(_WIN32,1,[This is a Windows system])
     LDFLAGS="$LDFLAGS -no-undefined -Wl,--export-all-symbols"
     LIBS="$LIBS -lws2_32 -lplibc"
     CFLAGS="-mms-bitfields $CFLAGS"
     DEFAULT_QTLIBS="-L$QTDIR/lib -lopengl32 -lglu32 -lgdi32 -luser32 -lQtOpenGL4 -lQtGui4 -lQtCore4"
     DEFAULT_QTINC="-I$QTDIR/include -I$QTDIR/include/QtCore -I$QTDIR/include/QtGui -I$QTDIR/include/QtOpenGL"
     DEFAULT_QTBIN="$QTDIR/bin"
     ;;
*)
     AC_MSG_RESULT(Unrecognised OS $host_os)
     AC_DEFINE_UNQUOTED(OTHEROS,1,[Some strange OS])
     ;;
esac

# Checks for programs.
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_LIBTOOL_DLOPEN
AC_DISABLE_STATIC
AC_LIBTOOL_WIN32_DLL
AC_LIBLTDL_INSTALLABLE
AC_PROG_LIBTOOL
AC_CANONICAL_HOST
AC_LIB_LTDL
m4_ifdef([_AC_SEEN_TAG(libltdl)], [], [AC_CONFIG_SUBDIRS(libltdl)])
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
AC_SUBST(MKDIR_P)


# Checks for libraries.
AC_HEADER_MAJOR

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Check for C compiler support for -fno-strict-aliasing
CFLAGS="-Wall $CFLAGS"
# use '-fno-strict-aliasing', but only if the compiler can take it
if gcc -fno-strict-aliasing -S -o /dev/null -xc /dev/null >/dev/null 2>&1;
then
	QT_CFLAGS="-fno-strict-aliasing"
fi

# test for qmake (FIXME: do we still need qmake!?? -- how about checking for moc?
AC_CHECK_PROG([QMAKE], [qmake-qt4], [qmake-qt4], [qmake])

# test for libextractor
extractor=0
AC_MSG_CHECKING(for libextractor)
AC_ARG_WITH(extractor,
   [  --with-extractor=PFX    Base of libextractor installation],
   [AC_MSG_RESULT([$with_extractor])
    case $with_extractor in
      no)
        ;;
      yes)
        AC_CHECK_HEADERS(extractor.h,extractor=1)
        ;;
      *)
      	LDFLAGS="-L$with_extractor/lib $LDFLAGS"
      	LIBPATH="$with_extractor/lib $LIBPATH"
        CPPFLAGS="-I$with_extractor/include $CPPFLAGS"
        INCLUDEPATH="$with_extractor/include  $INCLUDEPATH"
        AC_CHECK_HEADERS(extractor.h,extractor=1)
        ;;
    esac
   ],
   [AC_MSG_RESULT([--with-extractor not specified])
    AC_CHECK_HEADERS(extractor.h,extractor=1)])
if test "$extractor" != 1
then
 AC_MSG_ERROR([gnunet-qt requires libextractor headers])
fi

# test for GNUnet core
gnunet=0
AC_MSG_CHECKING(for GNUnet core)
AC_ARG_WITH(gnunet,
   [  --with-gnunet=PFX       Base of GNUnet installation],
   [AC_MSG_RESULT([$with_gnunet])
    case $with_gnunet in
      no)
        ;;
      yes)
        AC_CHECK_HEADERS(GNUnet/gnunet_util.h,
          AC_CHECK_LIB([gnunetutil], [GNUNET_os_init],
            gnunet=1))
        ;;
      *)
        LDFLAGS="-L$with_gnunet/lib $LDFLAGS"
        LIBPATH="$with_gnunet/lib $LIBPATH"
        CPPFLAGS="-I$with_gnunet/include $CPPFLAGS"
        INCLUDEPATH="$with_gnunet/include $INCLUDEPATH"
        AC_CHECK_HEADERS(GNUnet/gnunet_util.h,
          AC_CHECK_LIB([gnunetutil], [GNUNET_init],
            EXT_LIB_PATH="-L$with_gnunet/lib $EXT_LIB_PATH"
            gnunet=1))
        ;;
    esac
   ],
   [AC_MSG_RESULT([--with-gnunet not specified])
    AC_CHECK_HEADERS(gnunet_util.h,
     AC_CHECK_LIB([GNUnet/gnunet_util.h], [GNUNET_os_init],
      gnunet=1))])
if test "$gnunet" != 1
then
 AC_MSG_ERROR([gnunet-qt requires GNUnet])
fi
AC_CHECK_HEADERS([GNUnet/gnunet_ecrs_lib.h GNUnet/gnunet_fsui_lib.h GNUnet/gnunet_stats_lib.h GNUnet/gnunet_getoption_lib.h],,
                 AC_MSG_ERROR([compiling gnunet-qt requires GNUnet core headers]))

SAVELIBS=$LIBS
AC_CHECK_LIB(gnunetutil,GNUNET_GE_LOG,,
             AC_MSG_ERROR([gnunet-qt requires libgnunetutil]))
AC_CHECK_LIB(gnunetecrs,GNUNET_ECRS_uri_to_string,,
             AC_MSG_ERROR([gnunet-qt requires libgnunetecrs]))
AC_CHECK_LIB(gnunetfsui,GNUNET_FSUI_start,,
             AC_MSG_ERROR([gnunet-qt requires libgnunetfsui]))
AC_CHECK_LIB(gnunetstatsapi,GNUNET_STATS_get_statistics,,
             AC_MSG_ERROR([gnunet-qt requires libgnunetstatsapi]))
LIBS=$SAVELIBS

# check for Qt 4 (yucky)
AC_LANG_PUSH([C++])
AC_MSG_CHECKING(for Qt)
if test -z "${QTBIN}" ; then
  QTBIN="$DEFAULT_QTBIN"
fi
if test -z "${QTLIBS}" ; then
  QTINC="$DEFAULT_QTINC"
  QTLIBS="$DEFAULT_QTLIBS"
  if ( pkg-config --help > /dev/null 2> /dev/null ) ; then
    if ( pkg-config --cflags QtCore && pkg-config --libs QtCore && pkg-config --cflags QtGui && pkg-config --libs QtGui ) ; then
      QTINC="`pkg-config --cflags QtCore` `pkg-config --cflags QtGui`"
      QTLIBS="`pkg-config --libs QtCore` `pkg-config --libs QtGui`"
    fi
  fi
fi
LDFLAGS="$QTLIBS $LDFLAGS"
INCLUDES="$INCLUDES $QTINC"
CPPFLAGS="$QTINC $CPPFLAGS"
CXXFLAGS="$CXXFLAGS $QTINC"
LIBS="$LIBS $QTLIBS"
PATH="$PATH:$QTBIN"
AC_CHECK_HEADERS([Qt/qpixmap.h],,AC_MSG_ERROR([gnunet-qt requires qpixmap.h]))
AC_SEARCH_LIBS([_ZN7QPixmap12defaultDepthEv], [QtGui QtGui4],,AC_MSG_ERROR([Error linking test qt4 program]))
AC_LANG_POP([C++])
AC_SUBST(INCLUDEPATH)
AC_SUBST(LIBPATH)

AC_OUTPUT([
Makefile
src/Makefile
src/common/Makefile
src/core/Makefile
src/plugins/Makefile
src/plugins/about/Makefile
src/plugins/general/Makefile
src/plugins/fs/Makefile
src/plugins/stats/Makefile
])
