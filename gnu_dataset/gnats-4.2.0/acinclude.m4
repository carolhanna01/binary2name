# GNATS custom M4 Macros for use with autoconf/aclocal. 
#
# Process this file with aclocal to produce a configure script.
#
# Copyright (C) 2005 Free Software Foundation, Inc.
#
# This file is part of GNU GNATS.
# 
# GNU GNATS is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GNU GNATS is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU GNATS; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.
# CODING CONVENTIONS
#
# * Use "G_" prefix for each macro

# Check how to access the passwd database
define([G_PASSWD], [
  echo checking how to access passwd database
  PASSWD="cat /etc/passwd"
  if test -f /bin/domainname && test -n "`/bin/domainname`"; then
    if test -f /usr/bin/niscat && 
      /usr/bin/niscat passwd.org_dir > /dev/null 2>&1; then
      PASSWD="/usr/bin/niscat passwd.org_dir"
    elif test -f /usr/bin/ypcat && /usr/bin/ypcat passwd > /dev/null 2>&1; then
      PASSWD="/usr/bin/ypcat passwd"
    fi
  fi
  test -n "$verbose" && echo "    setting PASSWD to ${PASSWD}"
  AC_SUBST(PASSWD)
  ]
)


define([G_KRB4], [
  AC_MSG_CHECKING([Kerberos path])
  AC_ARG_WITH([krb4],
    AS_HELP_STRING(
      [--with-krb4=PATH],
      [Prefix PATH for Kerberos 4 support]),
    [ac_cv_use_krb4=$withval], [ac_cv_use_krb4=default]
  )
  dnl Default to system version if BSD-style, else /usr/kerberos always.
  if test $ac_cv_use_krb4 = "default" ; then
    if test ! -d /usr/kerberos && test -d /usr/include/kerberosIV ; then
      KRB4=/usr
    else
      KRB4=/usr/kerberos
    fi
  else
    KRB4=$ac_cv_use_krb4
  fi

  AC_MSG_RESULT($KRB4)
  AC_SUBST(KRB4)

  krb_h=
  KRBINCLUDE=
  BUILD_MKAUTH=
  NEED_MKAUTH=0
  # XXX this should use a different approach at some point
  AC_MSG_CHECKING([for krb.h])
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
      [[#include <des.h> #include <krb.h>]],
      [[int i;]])],[
        krb_h=yes
	krb_incdir=
        AC_MSG_RESULT(yes)],
    [AC_MSG_RESULT(no)
      if test -r "$KRB4/include/kerberosIV/krb.h" ; then
        krbincdir=$KRB4/include/kerberosIV
      else
        krbincdir=$KRB4/include
      fi

      if test "$cross_compiling" = "no" && test -r $krbincdir/krb.h; then
      	hold_cppflags="${CPPFLAGS}"
      	CPPFLAGS="${CPPFLAGS} -I${krbincdir}"
        AC_MSG_CHECKING(for krb.h in $krbincdir)
        AC_LINK_IFELSE(
          [AC_LANG_PROGRAM([[#include <des.h> #include <krb.h>]],
            [[int i;]])],
          [krb_h="yes"
	    krb_incdir="${krbincdir}"
            export krb_h krb_incdir
            AC_MSG_RESULT(yes)],
          [AC_MSG_RESULT(no)]
        )
        if test -z "$krb_incdir"; then
          CPPFLAGS="$hold_cppflags"
        fi
      fi
    ]
  )

  if test -n "$krb_h"; then
    krb_lib=
    if test "$cross_compiling" = "no" && test -r $KRB4/lib/libkrb.a; then
      # Fake some sensible log messages.
      AC_MSG_CHECKING(for -lkrb in $KRB4/lib)
      AC_MSG_RESULT(yes)
      krb_lib=-lkrb krb_libdir=$KRB4/lib
    else
      AC_CHECK_LIB(krb,main,krb_lib=-lkrb,
                   AC_CHECK_LIB(krb4,main,krb_lib=-lkrb4))
    fi

    if test x"$krb_lib" != x""; then
      AC_DEFINE([HAVE_KERBEROS], 1, [Kerberos 4 Support])
      test -n "${krb_libdir}" && LIBS="${LIBS} -L${krb_libdir}"
      LIBS="${LIBS} $krb_lib"
      AC_CHECK_LIB(des,main,[LIBS="${LIBS} -ldes"])
      AC_CHECK_LIB(des425,main,[LIBS="${LIBS} -ldes425"])
      AC_CHECK_LIB(krb5,main,[LIBS="${LIBS} -lkrb5"])
      AC_CHECK_LIB(crypto,main,[LIBS="${LIBS} -lcrypto"])
      AC_CHECK_LIB(com_err,main,[LIBS="${LIBS} -lcom_err"])
      AC_CHECK_FUNC(krb_mk_auth, :,
        # BSD Kerberos doesn't have these functions.
        NEED_MKAUTH=1
      )
      AC_CHECK_FUNCS(krb_get_err_text)
    else
      AC_DEFINE([HAVE_KERBEROS], 0, [Kerberos 4 Support])
    fi
  fi
  AM_CONDITIONAL([BUILD_MKAUTH],[test NEED_MKAUTH = 1])
  AC_SUBST(KRBINCLUDE)
  AC_SUBST(EXTRA_OBJS)
  dnl End setup kerberos
  ]
)

# vim:ft=config:
