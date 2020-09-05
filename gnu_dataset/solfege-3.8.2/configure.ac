# GNU Solfege - ear training for GNOME
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007  Tom Cato Amundsen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin ST, Fifth Floor, Boston, MA  02110-1301  USA

# Process this file with autoconf to produce a configure script.

AC_INIT([GNU Solfege],[3.8.2],[bug-solfege@gnu.org])
AC_CONFIG_HEADER(config.h)

PACKAGE=solfege
AC_SUBST(PACKAGE)

MAJOR_VERSION=3
MINOR_VERSION=8
PATCH_LEVEL=2
VERSION=3.8.2

AC_SUBST(MAJOR_VERSION)
AC_SUBST(MINOR_VERSION)
AC_SUBST(PATCH_LEVEL)
AC_SUBST(VERSION)

AC_PROG_CC

AM_PATH_PYTHON(2.3)
AM_CHECK_PYTHON_HEADERS(HAVE_PYTHON_H=yes, HAVE_PYTHON_H=no)

AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl Check for programs

AC_PATH_PROG(RSVG, rsvg)
AC_SUBST(RSVG)

AC_PATH_PROG(SWIG, swig)
AC_SUBST(SWIG)

AC_PATH_PROG(LILYPOND, lilypond)
AC_SUBST(LILYPOND)

AC_PATH_PROG(GS, gs)
AC_SUBST(GS)

MY_SWIG_TEST(SWIG_VERSION)
AC_SUBST(SWIG_VERSION)

MY_PATH_PROG(XGETTEXT, xgettext, [You need to install the gettext.])
MY_PATH_PROG(MSGFMT, msgfmt, [You need to install the gettext.])
MY_PATH_PROG(MSGMERGE, msgmerge, [You need to install the gettext.])
MY_PATH_PROG(MAKEINFO, makeinfo [You need to install the texinfo.])
MY_PATH_PROG(SED, sed)
MY_PATH_PROG(CAT, cat)

AC_ARG_ENABLE(pygtk-test, AC_HELP_STRING([--disable-pygtk-test],
    [Don't test for PyGTK, assume it is installed]),
    run_pygtk_test=$enableval,
    run_pygtk_test=yes)


PKG_PROG_PKG_CONFIG(0.17.0)

if test "x$run_pygtk_test" = xyes; then
  PKG_CHECK_MODULES(PYGTK, pygtk-2.0 >= 2.6.0)
fi

#PKG_CHECK_MODULES(XML2PO, xml2po >= 0.4)
AC_PATH_PROG(XML2PO, xml2po)
AC_SUBST(XML2PO)

AC_PATH_PROG(XSLTPROC, xsltproc)
AC_SUBST(XSLTPROC)

AC_ARG_ENABLE(docbook-stylesheet,
   AC_HELP_STRING([--enable-docbook-stylesheet=PATH],
   [PATH is filename including full path to the html/chunk.xsl
    stylesheet.]),
  [STYLESHEET=$enableval],
  [STYLESHEET=/usr/share/sgml/docbook/stylesheet/xsl/nwalsh/html/chunk.xsl])

AC_CHECK_FILE($STYLESHEET, HAVE_STYLESHEET=yes, HAVE_STYLESHEET=no)
AC_SUBST(STYLESHEET)
AC_SUBST(HAVE_STYLESHEET)

AC_CHECK_HEADER(sys/soundcard.h, HAVE_SOUNDCARD_H=yes, HAVE_SOUNDCARD_H=no)
AC_SUBST(HAVE_SOUNDCARD_H)

AC_ARG_ENABLE(oss-sound, AC_HELP_STRING([--disable-oss-sound],
    [Do not build module for OSS sound. Use this if you OS don't support OSS]),
    [ENABLE_OSS_SOUND=$enableval],
    [ENABLE_OSS_SOUND=$HAVE_SOUNDCARD_H])
AC_SUBST(ENABLE_OSS_SOUND)

if test $HAVE_SOUNDCARD_H = "no"; then
  if test $ENABLE_OSS_SOUND = "yes"; then
      AC_MSG_ERROR([Cannot find sys/soundcard.h
                  --enable-oss-sound require sys/soundcard.h])
  fi
fi

AC_ARG_ENABLE(winmidi, AC_HELP_STRING([--enable-winmidi],
    [Build the winmidi.pyd module. This is required to get sound on MS Windows.]),
    [ENABLE_WINMIDI=$enableval],
    [ENABLE_WINMIDI=no])
AC_SUBST(ENABLE_WINMIDI)

AC_ARG_ENABLE(tuner, AC_HELP_STRING([--enable-tuner],
    [the microphone experiment (default: disable). This code is dead, it
     does not work.]),
    [ENABLE_TUNER=$enableval],
    [ENABLE_TUNER=no])
AC_SUBST(ENABLE_TUNER)

dnl Checks for libraries.
if test "x$ENABLE_TUNER" = xyes; then
    AC_DEFINE(ENABLE_TUNER)
    AC_CHECK_LIB(rfftw, rfftw, [
        AC_DEFINE(HAVE_FFTW)
        LIBS="$LIBS -lfftw -lrfftw"], ,-lfftw)
fi

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(fcntl.h sys/ioctl.h unistd.h)

if test "x$ENABLE_OSS_SOUND" = xyes; then
  if test "x$HAVE_PYTHON_H" = xno; then
        AC_MSG_ERROR([
*** We need the python header files to build the OSS sound module.
*** The debian package is called python-dev, other distributions
*** probably have a similar name.])
  fi
fi
AC_CHECK_HEADERS(linux/awe_voice.h, [HAVE_LINUX_AWE_VOICE_H=yes])
AC_SUBST(HAVE_LINUX_AWE_VOICE_H)


dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

dnl Checks for library functions.
AC_PROG_GCC_TRADITIONAL
#AC_CONFIG_FILES

$CAT run-solfege.py.in | $SED "s/@VERSION@//" > debian-solfege.py.in

AC_CONFIG_FILES([
	 run-solfege.py debian-solfege.py src/configureoutput.py
         topdocs/defs.texi
         windowsinstaller.iss
         Makefile
         lessonfile_editor.py
         solfege.redhat9.spec
         solfege.suse9.spec
         autopackage/default.apspec
         help/C/solfege.xml
         ])
AC_OUTPUT

AC_MSG_NOTICE
AC_MSG_NOTICE([Please remember to rerun configure if you change swig versions!])
if test -z "$XML2PO"; then
AC_MSG_NOTICE
AC_MSG_NOTICE([xml2po was not found. This is no big deal, but it mean that the])
AC_MSG_NOTICE([translated user manuals will not be updated if you make changes])
AC_MSG_NOTICE([to the english manual.])
fi
