dnl Do not edit. -*- mode: autoconf; buffer-read-only: t; -*- vi: set ro:
dnl This file is created automatically from configure.boot.
dnl To recreate, run bootstrap from the top of Dico source tree.
dnl Process this file with -*- autoconf -*- to produce a configure script. 
# This file is part of GNU Dico
# Copyright (C) 1998-2000, 2008-2010, 2012-2019 Sergey Poznyakoff
#
# GNU Dico is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Dico is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Dico.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.63)
AC_INIT([GNU dico], 2.9, [bug-dico@gnu.org])
AC_CONFIG_SRCDIR([dicod/main.c])
AC_CONFIG_HEADERS(include/prog/config.h include/lib/config.h)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR(m4)	
AM_INIT_AUTOMAKE([1.11 nostdinc gnits tar-ustar dist-bzip2 dist-xz std-options subdir-objects])

dnl Enable silent rules by default:
AM_SILENT_RULES([yes])

dnl Some variables
AC_SUBST(DICO_MODDIR,'$(libdir)/$(PACKAGE)')

dnl Checks for programs.
AC_PROG_CC
gl_EARLY	
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_YACC
AC_PROG_LEX

LT_PREREQ(2.4)
LT_CONFIG_LTDL_DIR([libltdl])
LT_INIT([dlopen])
LTDL_INIT([nonrecursive])

dnl Checks for libraries.
AC_CHECK_LIB(socket, socket)
AC_CHECK_LIB(nsl, gethostbyaddr)
AC_CHECK_LIB(rt, nanosleep)

dnl Checks for header files.
AC_HEADER_DIRENT dnl not needed ?
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h limits.h strings.h sys/time.h \
                 sys/socket.h socket.h syslog.h unistd.h \
                 crypt.h readline/readline.h)

dnl Checks for typedefs, structures, and compiler characteristics.
gl_INIT

AC_C_CONST
AC_C_INLINE
AC_TYPE_PID_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_SYS_LARGEFILE
AC_CHECK_TYPE([socklen_t],,
  AC_DEFINE(socklen_t, int, [Define to int if <sys/types.h> does not define]),
[
#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_SOCKET_H
# include <socket.h>
#endif
])

dnl Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(gethostname select socket strdup strerror strtol \
               setegid setregid setresgid seteuid setreuid \
	       sysconf getdtablesize)

# Crypt
AC_CHECK_DECLS(crypt,,,[
#include <unistd.h>
#ifdef HAVE_CRYPT_H
# include <crypt.h>
#endif])
AH_BOTTOM([
#if !HAVE_DECL_CRYPT
char *crypt(const char *key, const char *salt);
#endif])

AC_CHECK_LIB(crypt, crypt)

# Gettext.
AM_ICONV
AM_GNU_GETTEXT([external], [need-formatstring-macros])
AM_GNU_GETTEXT_VERSION([0.18])
AC_CONFIG_LINKS(include/gettext.h:xdico/gnu/gettext.h)

LOG_FACILITY="LOG_DAEMON"

AC_ARG_VAR([LOG_FACILITY],
	   [Default syslog facility])
if test -n "$LOG_FACILITY"; then
   logfacility=`echo $LOG_FACILITY | tr a-z A-Z`
   case $logfacility in
   USER|DAEMON|AUTH|AUTHPRIV|MAIL|CRON|LOCAL[[0-7]])
      LOG_FACILITY=LOG_$logfacility;;
   LOG_USER|LOG_DAEMON|LOG_AUTH|LOG_AUTHPRIV|LOG_MAIL|LOG_CRON|LOG_LOCAL[[0-7]])
      LOG_FACILITY=$logfacility;;
   *) AC_MSG_ERROR([Invalid value of LOG_FACILITY]);;
   esac
fi		      
AC_DEFINE_UNQUOTED([LOG_FACILITY],$LOG_FACILITY,
                   [Default syslog facility.])

AC_ARG_VAR([DEFAULT_DICT_SERVER],
           [Set the name of the default DICT server for dico utility])
if test -z "$DEFAULT_DICT_SERVER" ; then
   DEFAULT_DICT_SERVER="gnu.org.ua"
fi

AC_ARG_VAR([DEFAULT_INCLUDE_DIR],
           [Default preprocessor include directory])
if test -z "$DEFAULT_INCLUDE_DIR"; then
   DEFAULT_INCLUDE_DIR='$(pkgdatadir)/include'
fi   
AC_ARG_VAR([DEFAULT_VERSION_INCLUDE_DIR],
           [Default version-specific include directory])
if test -z "$DEFAULT_VERSION_INCLUDE_DIR"; then
   DEFAULT_VERSION_INCLUDE_DIR='$(pkgdatadir)/$(VERSION)/include'
fi   
AC_ARG_VAR([DEFAULT_PIDFILE_NAME],
           [Default name for the dicod pid file])
if test -z "$DEFAULT_PIDFILE_NAME"; then
   DEFAULT_PIDFILE_NAME='$(localstatedir)/run/dicod.pid'
fi
AC_SUBST([DICO_PROG_CONFIG],['-I$(top_builddir)/include/prog'])
AC_SUBST([DICO_LIB_CONFIG],['-I$(top_builddir)/include/lib'])

AC_SUBST([DICO_PROG_INCLUDES],[dnl
'$(DICO_PROG_CONFIG)\
 -I$(top_srcdir)/include\
 -I$(top_builddir)/include\
 -I$(top_srcdir)/xdico/gnu\
 -I$(top_builddir)/xdico/gnu\
 $(GRECS_INCLUDES)'])

AC_SUBST([DICO_MODULE_INCLUDES],[dnl
'-I$(srcdir)\
 $(DICO_LIB_CONFIG)\
 -I$(top_srcdir)/include\
 -I$(top_builddir)/include\
 $(GRECS_INCLUDES)'])

# Grecs configuration system
GRECS_SETUP(grecs, [shared tests getopt git2chg sockaddr-list])
GRECS_HOST_PROJECT_INCLUDES='$(DICO_LIB_CONFIG) -I$(top_builddir)/include'

# Tcl/tk
AC_ARG_WITH([tk],
            AC_HELP_STRING([--with-tk],
	                   [build Tcl/Tk-based applications (GCIDER)]),
	    [
case "${withval}" in
  yes) status_tk=yes ;;
  no)  status_tk=no;;
  *) AC_MSG_ERROR([bad value ${withval} for --with-tk])
esac],[status_tk=yes])

if test $status_tk=yes; then
  AC_PATH_PROG([WISH], wish)
  if test -z "$WISH"; then
    status_tk=no
  fi
  AM_CONDITIONAL([TK_WISH_COND],[test $status_tk = yes])
fi
#
AC_ARG_WITH(autologin-file,
            AC_HELP_STRING([--with-autologin-file@<:@=NAME@:>@],
	                   [Use the autologin file (default NAME is .dicologin)]),
            [case $withval in
	     yes) DEFAULT_AUTOLOGIN_FILE=".dicologin";;
	     no)  DEFAULT_AUTOLOGIN_FILE=;;
	     *)   DEFAULT_AUTOLOGIN_FILE=$withval;;
	     esac],
	    [DEFAULT_AUTOLOGIN_FILE=".dicologin"])
if test -n "$DEFAULT_AUTOLOGIN_FILE" ; then
   AC_DEFINE_UNQUOTED(DEFAULT_AUTOLOGIN_FILE, "$DEFAULT_AUTOLOGIN_FILE",
                      [Define to the name of a netrc-style autologin file])
else
   AC_DEFINE_UNQUOTED(DEFAULT_AUTOLOGIN_FILE, NULL,
                      [Define to the name of a netrc-style autologin file])
fi

AC_ARG_WITH([readline],
            AC_HELP_STRING([--without-readline],
                           [do not use readline]),
            [
case "${withval}" in
  yes) status_readline=yes ;;
  no)  status_readline=no ;;
  *)   AC_MSG_ERROR(bad value ${withval} for --without-readline) ;;
esac],[status_readline=yes])

# Test for GNU Readline
AC_SUBST(READLINE_LIBS)

if test "$status_readline" != "no"; then
  
  dnl FIXME This should only link in the curses libraries if it's
  dnl really needed!
  
  dnl Check for Curses libs.
  CURSES_LIBS=
  for lib in ncurses curses termcap
  do
    AC_CHECK_LIB($lib, tputs, [CURSES_LIBS="-l$lib"; break])
  done

  saved_LIBS=$LIBS
  LIBS="$LIBS $CURSES_LIBS"
  AC_CHECK_LIB(readline, readline, mf_have_readline=yes)
  LIBS=$saved_LIBS
  
  if test "$mf_have_readline" = "yes"; then
    AC_CHECK_HEADERS(readline/readline.h,
  		     AC_DEFINE(WITH_READLINE,1,[Enable use of readline]))
    READLINE_LIBS="-lreadline $CURSES_LIBS"
    saved_LIBS=$LIBS
    LIBS="$LIBS $READLINE_LIBS"
    AC_CHECK_FUNCS(rl_completion_matches)
    LIBS=$saved_LIBS
    status_readline="yes"
  else
    if test "$status_readline" = "yes"; then
      AC_MSG_WARN(readline requested but does not seem to be installed)
    fi
    status_readline="no"
  fi
fi

AH_BOTTOM([
/* Some older versions of readline have completion_matches */
#ifndef HAVE_RL_COMPLETION_MATCHES
# define rl_completion_matches completion_matches
#endif])

# GNU SASL
status_gsasl=no
MU_CHECK_GSASL(0.2.5, [
    AC_DEFINE(WITH_GSASL,1,[Define if GNU SASL is present])
    status_gsasl=yes
    AC_SUBST(LIBDICOSASL,'$(top_builddir)/xdico/libdicosasl.a')])
AM_CONDITIONAL([COND_LIBDICOSASL],[test "$status_gsasl" = yes])

# Module dict.org
AC_CONFIG_FILES([modules/dict.org/tests/db/Makefile])

# Check for zlib
AC_CHECK_HEADER(zlib.h,,
 [AC_MSG_ERROR([required header zlib.h not found])])
AC_CHECK_LIB(z, inflate,,
 [AC_MSG_ERROR([required library libz not found])])


DICO_TESTS(modules/dict.org)
AC_CONFIG_FILES([modules/dict.org/Makefile])

# End of module dict.org

# Module gcide

DICO_TESTS(modules/gcide)
AC_CONFIG_FILES([modules/gcide/Makefile])

# End of module gcide

# Module guile

# Check for Guile
GINT_INIT([gint],[2.2.0 with-guile nodoc],
	  [status_guile=yes],
	  [status_guile=no])
AM_CONDITIONAL([GUILE_COND],[test $status_guile = yes])


DICO_TESTS(modules/guile)
AC_CONFIG_FILES([modules/guile/Makefile])

# End of module guile

# Module ldap


AC_ARG_WITH(ldap,
            [AC_HELP_STRING([--with-ldap],
		            [Configure to work with OpenLDAP])],
	    [status_ldap=$withval],
            [status_ldap=maybe])

if test $status_ldap != no; then
  AC_CHECK_LIB(ldap, ldap_bind,
               [status_ldap=yes],
               [if test $status_ldap = yes; then
	          AC_MSG_ERROR([required library ldap is not found])
		fi
 	        status_ldap=no])
fi
AM_CONDITIONAL([LDAP_COND], [test $status_ldap = yes])

AC_CONFIG_FILES([modules/ldap/Makefile])

# End of module ldap

# Module metaphone2

DICO_TESTS(modules/metaphone2)
AC_CONFIG_FILES([modules/metaphone2/Makefile])

# End of module metaphone2

# Module nprefix

DICO_TESTS(modules/nprefix)
AC_CONFIG_FILES([modules/nprefix/Makefile])

# End of module nprefix

# Module outline

DICO_TESTS(modules/outline)
AC_CONFIG_FILES([modules/outline/Makefile])

# End of module outline

# Module pam


status_pam=maybe
AC_ARG_ENABLE([pam],
              AC_HELP_STRING([--enable-pam],
                             [enable PAM]),
              [
case "${enableval}" in
  yes) status_pam=yes ;;
  no)  status_pam=no ;;
  *)   AC_MSG_ERROR([bad value ${enableval} for --enable-pam]) ;;
esac],[status_pam=maybe])

AC_SUBST(PAM_LIBS)
if test "$status_pam" != "no"; then
  pam=$status_pam
  AC_CHECK_HEADERS(security/pam_appl.h)
  if test "$ac_cv_header_security_pam_appl_h" = "yes"; then
    AC_CHECK_LIB(dl, dlopen, [PAM_LIBS=-ldl])
    AC_CHECK_LIB(pam, pam_start,
	         [status_pam=yes],
	         [status_pam=no], $PAM_LIBS)
  else
    status_pam=no
  fi
  if test $pam = yes && test $pam != $status_pam; then
      AC_MSG_ERROR([required module PAM cannot be built because of missing prerequisites])
  fi
fi

AM_CONDITIONAL([PAM_COND], [test $status_pam = yes])

AC_CONFIG_FILES([modules/pam/Makefile])

# End of module pam

# Module pcre


AC_ARG_WITH([pcre],
            AC_HELP_STRING([--with-pcre],
                           [compile Perl-compatible regular expressions support]),
            [
case "${withval}" in
  yes) status_pcre=yes ;;
  no)  status_pcre=no ;;
  *)   AC_MSG_ERROR(bad value ${withval} for --with-pcre) ;;
esac],[status_pcre=yes])

if test $status_pcre = yes; then
  AC_CHECK_HEADER([pcre.h], [], [status_pcre=no])
  if test $status_pcre = yes; then
    DICO_CHECK_LIB(pcre, main, [],
                   [:],
		   [status_pcre=no])
  fi
fi
AM_CONDITIONAL([PCRE_COND],[test $status_pcre = yes])

AC_CONFIG_FILES([modules/pcre/Makefile])

# End of module pcre

# Module python

AC_ARG_WITH([python],
            AC_HELP_STRING([--without-python],
                           [do not build Python interface]),
            [
case "${withval}" in
  yes) status_python=yes ;;
  no)  status_python=no ;;
  *)   AC_MSG_ERROR(bad value ${withval} for --without-python) ;;
esac],[status_python=yes])

PYTHON3_COND=0
if test "$status_python" = yes; then
  AC_ARG_VAR([PYTHON], [The name of python binary])
  AC_PATH_PROG([PYTHON], python)
  
  AC_ARG_VAR([PYTHON_CONFIG], [The name of python-config binary])
  AC_PATH_PROG([PYTHON_CONFIG], python-config)
  if test -n "$PYTHON_CONFIG"; then
    AC_SUBST(PYTHON_LIBS,`$PYTHON_CONFIG --libs`)
    AC_SUBST(PYTHON_INCLUDES,`$PYTHON_CONFIG --includes`)
    $PYTHON <<EOT
import sys
exit(1 if sys.version_info[[0]] == 3 else 0)
EOT
    PYTHON3_COND=$?
  else
    status_python=no     
  fi	
fi
AM_CONDITIONAL([PYTHON_COND],[test $status_python = yes])
AM_CONDITIONAL([PYTHON3_COND],[test $PYTHON3_COND -eq 1])

DICO_TESTS(modules/python)
AC_CONFIG_FILES([modules/python/Makefile])

# End of module python

# Module stratall

DICO_TESTS(modules/stratall)
AC_CONFIG_FILES([modules/stratall/Makefile])

# End of module stratall

# Module substr

DICO_TESTS(modules/substr)
AC_CONFIG_FILES([modules/substr/Makefile])

# End of module substr

# Module word

DICO_TESTS(modules/word)
AC_CONFIG_FILES([modules/word/Makefile])

# End of module word

# Module wordnet


AC_SUBST(WORDNET_LOPT)
AC_SUBST(WORDNET_IOPT)
AC_ARG_WITH([wordnet],
            AC_HELP_STRING([--with-wordnet@<:@=DIR@:>@],
                           [use wordnet]),
            [
case "${withval}" in
  yes) status_wordnet=req ;;
  no)  status_wordnet=no ;;
  /*)  status_wordnet=req
       WORDNET_LOPT=-L${withval}/lib
       WORDNET_IOPT=-I${withval}/include;;
  *)   AC_MSG_ERROR([argument for --with-wordnet must be an absoulute directory name]) ;;
esac],[status_wordnet=maybe])

AC_SUBST([WORDNET_LIBWN],[WN])
AC_ARG_WITH([libWN],
            AC_HELP_STRING([--with-libWN@<:@=NAME@:>@],
                           [use libNAME instead of libWN]),
            [WORDNET_LIBWN=${withval#lib}])

if test $status_wordnet != no; then
  save_LIBS=$LIBS
  save_CPPFLAGS=$CPPFLAGS
  LIBS="$LIBS $WORDNET_LOPT"
  CPPFLAGS="$CPPFLAGS $WORDNET_IOPT"
  AC_CHECK_HEADER([wn.h],
      [AC_CHECK_LIB($WORDNET_LIBWN, findtheinfo, status_wordnet=yes)])
  case $status_wordnet in
  req) AC_MSG_ERROR([required module wordnet cannot be built because of missing prerequisites]);;
  maybe) status_wordnet=no;;
  esac
  LIBS=$save_LIBS
  CPPFLAGS=$save_CPPFLAGS
fi

AM_CONDITIONAL([WORDNET_COND],[test $status_wordnet = yes])

AC_CONFIG_FILES([modules/wordnet/Makefile])

# End of module wordnet


    
# Imprimatur
IMPRIMATUR_INIT

# Tests
DICO_TESTS(lib)
DICO_TESTS(dicod)

AC_CONFIG_COMMANDS([status],[
cat <<EOT

*******************************************************************
Dico configured with the following settings:

GSASL ........................... $status_gsasl
Readline ........................ $status_readline
Preprocessor .................... $dicopp

Guile ........................... $status_guile
LDAP ............................ $status_ldap
PAM ............................. $status_pam
Perl-compatible regexs .......... $status_pcre
Python .......................... $status_python
WordNet ......................... $status_wordnet

Default server .................. $defserver
Autologin file .................. $autologin_file
*******************************************************************
EOT
],
[status_gsasl=$status_gsasl
status_readline=$status_readline
if test $use_ext_pp = no; then
  dicopp=no
else
  dicopp="$DEFAULT_PREPROCESSOR"
fi
defserver="$DEFAULT_DICT_SERVER"
autologin_file="$DEFAULT_AUTOLOGIN_FILE"
status_guile=$status_guile
status_ldap=$status_ldap
status_pam=$status_pam
status_pcre=$status_pcre
status_python=$status_python
status_wordnet=$status_wordnet
])

AC_CONFIG_FILES([Makefile
                 include/Makefile
                 include/dico/Makefile
		 examples/Makefile 
		 gint/Makefile
		 xdico/gnu/Makefile
		 xdico/Makefile
                 lib/Makefile
                 dicod/Makefile
		 modules/Makefile
		 dico/Makefile
		 scripts/Makefile
		 doc/Makefile
		 po/Makefile.in
		 app/python/dicod.conf])

AC_OUTPUT
