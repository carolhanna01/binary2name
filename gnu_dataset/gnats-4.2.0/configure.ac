dnl Process this file with autoconf to produce a configure script.
dnl
dnl Copyright (C) 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
dnl Copyright (C) 1999, 2000 Juniper Networks, inc.
dnl Copyright (C) 1993, 1994, 1995, 1996, 1997 ??? (FIXME: see Changelog.v3)
dnl Copyright (C) 2001 Milan Zamazal
dnl
dnl This file is part of GNU GNATS.
dnl 
dnl GNU GNATS is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl 
dnl GNU GNATS is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with GNU GNATS; see the file COPYING.  If not, see
dnl <http://www.gnu.org/licenses/>.
dnl
AC_PREREQ([2.59])

dnl ----------------------------------------------------------------------------
dnl Initialize
dnl ----------------------------------------------------------------------------
AC_INIT([GNU GNATS],[4.2.0],[bug-gnats@gnu.org],[gnats])

dnl Must be called before AM_INIT_MAKEFILE macro
AC_CONFIG_AUX_DIR([config])

dnl Initialize automake
AM_INIT_AUTOMAKE([gnu dist-bzip2 subdir-objects 1.9])

dnl Auto-generated configuration file
AC_CONFIG_HEADERS([gnats/autoconf.h])

dnl Add config.guess and config.sub
AC_CANONICAL_HOST

dnl Override lisp installation directory
AM_PATH_LISPDIR

dnl default to using the file data store
GNATS_ALL=all-gnats
GNATS_INSTALL=install-gnats
DEFAULT_RELEASE=unknown-1.0
DEFAULT_MAIL_AGENT=false
BDAY_START=8
BDAY_END=17
BWEEK_START=1
BWEEK_END=5


dnl ----------------------------------------------------------------------------
dnl Checking system
dnl ----------------------------------------------------------------------------
dnl AC_USE_SYSTEM_EXTENSIONS is not available before autoconf 2.60, so define
dnl it to do what we want when we are on autoconf 2.59 instead of increasing
dnl AC_PREREQ, for wider compatibility with older systems.
m4_ifndef([AC_USE_SYSTEM_EXTENSIONS], [
AC_DEFUN([AC_USE_SYSTEM_EXTENSIONS], [
	AC_AIX
	AC_MINIX
	])
])
AC_USE_SYSTEM_EXTENSIONS

dnl Save to cache
AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Gathering information, but don't set defaults (done later)
dnl ----------------------------------------------------------------------------
dnl Optional Arguments
AC_ARG_WITH([kerberos],
  AS_HELP_STRING([--with-kerberos],
    [include code for Kerberos authentication[[no]]]),
  [ac_cv_use_kerberos=$withval], [ac_cv_use_kerberos=no])

dnl save cache so I can use it in the following args
AC_CACHE_SAVE

AC_ARG_ENABLE([gnats-service],
	AS_HELP_STRING([--enable-gnats-service=NAME],
		[set the network service or port to NAME [[support]]]),
	[ac_cv_use_gnats_service=$enableval],
	[ac_cv_use_gnats_service=no])

AC_ARG_ENABLE([gnats-user],
	AS_HELP_STRING([--enable-gnats-user=NAME],
		[set the username for GNATS from the passwd file to
		NAME [[gnats]]]),
	[ac_cv_use_gnats_user=$enableval],
	[ac_cv_use_gnats_user=no])

AC_ARG_ENABLE([gnatsd-user-access-file],
	AS_HELP_STRING([--enable-gnatsd-user-access-file=PATH],
		[set global GNATSD access file to PATH
		[[SYSCONFDIR/gnats/gnatsd.user_access]]]),
	[ac_cv_use_gnatsd_user_access_file=$enableval],
	[ac_cv_use_gnatsd_user_access_file=no])

AC_ARG_ENABLE([gnatsd-host-access-file],
	AS_HELP_STRING([--enable-gnatsd-host-access-file=PATH],
		[set global GNATSD access file to PATH
		[[SYSCONFDIR/gnats/gnatsd.host_access]]]),
	[ac_cv_use_gnatsd_host_access_file=$enableval],
	[ac_cv_use_gnatsd_host_access_file=no])

AC_ARG_ENABLE([gnats-dblist-file],
	AS_HELP_STRING([--enable-gnats-dblist-file=PATH],
		[specify file containing list of databases
		[[SYSCONFDIR/gnats/databases]]]),
	[ac_cv_use_gnats_dblist_file=$enableval],
	[ac_cv_use_gnats_dblist_file=no])

AC_ARG_ENABLE([gnats-default-db],
	AS_HELP_STRING([--enable-gnats-default-db=PATH],
		[specify the default database directory to use
		[[SHAREDSTATEDIR/gnats/gnatsdb]]]),
	[ac_cv_use_gnats_default_db=$enableval],
	[ac_cv_use_gnats_default_db=no])

AC_ARG_ENABLE([organization],
	AS_HELP_STRING([--enable-organization=NAME],
		[Sets the default organization to NAME [[unknown]]]),
	[ac_cv_use_default_organization=$enableval],
	[ac_cv_use_default_organization=no])

AC_ARG_ENABLE([submitter],
	AS_HELP_STRING([--enable-submitter=name],
		[Sets the default submitter to NAME [[unknown]]]),
	[ac_cv_use_default_submitter=$enableval],
	[ac_cv_use_default_submitter=no])

AC_ARG_ENABLE([ds-type],
	AS_HELP_STRING([--enable-ds-type=TYPE],
		[Sets the default datastore to TYPE. [[file]]]),
	[ac_cv_use_ds_type=$enableval], 
	[ac_cv_use_ds_type=no])

dnl Save to cache
AC_CACHE_SAVE

# Process information collected above, setting defaults
AC_MSG_CHECKING([for --enable-gnats-service])
if test "$ac_cv_use_gnats_service" != "no" ; then
	GNATS_SERVICE=$ac_cv_use_gnats_service
else
	GNATS_SERVICE=support
fi
AC_MSG_RESULT($GNATS_SERVICE)
AC_DEFINE_UNQUOTED(GNATS_SERVICE, "${GNATS_SERVICE}",
	[Default port to bind GNATS daemon])

AC_MSG_CHECKING([for --enable-gnats-user])
if test "$ac_cv_use_gnats_user" != "no" ; then
	GNATS_USER=$ac_cv_use_gnats_user
else
	GNATS_USER=gnats
fi
AC_MSG_RESULT([$GNATS_USER])
AC_DEFINE_UNQUOTED(GNATS_USER,"${GNATS_USER}",
	[Unprivileged user to GNATS daemon under])

AC_MSG_CHECKING([for --enable-gnatsd-user-access-file])
if test "$ac_cv_use_gnatsd_user_access_file" != "no" ; then
	GNATSD_USER_ACCESS_FILE=$ac_cv_use_gnatsd_user_access_file
else
	GNATSD_USER_ACCESS_FILE=${sysconfdir}/gnats/gnatsd.user_access
fi
AC_MSG_RESULT($GNATSD_USER_ACCESS_FILE)

AC_MSG_CHECKING([for --enable-gnatsd-host-access-file])
if test "$ac_cv_use_gnatsd_host_access_file" != "no" ; then
	GNATSD_HOST_ACCESS_FILE=$ac_cv_use_gnatsd_host_access_file
else
	GNATSD_HOST_ACCESS_FILE=${sysconfdir}/gnats/gnatsd.host_access
fi
AC_MSG_RESULT($GNATSD_HOST_ACCESS_FILE)

AC_MSG_CHECKING([for --enable-gnats-dblist-file])
if test "$ac_cv_use_gnats_dblist_file" != "no" ; then
	GLOBAL_DB_LIST_FILE=$ac_cv_use_gnats_dblist_file
else
	GLOBAL_DB_LIST_FILE=${sysconfdir}/gnats/databases
fi
AC_MSG_RESULT($GLOBAL_DB_LIST_FILE)

AC_MSG_CHECKING([for --enable-gnats-default-db])
if test "$ac_cv_use_gnats_default_db" != "no" ; then
	GNATS_DEFAULT_DB_DIR=$ac_cv_use_gnats_default_db
else
	GNATS_DEFAULT_DB_DIR=${sharedstatedir}/gnats/gnatsdb
fi
AC_MSG_RESULT($GNATS_DEFAULT_DB_DIR)

AC_MSG_CHECKING([for --enable-submitter])
if test "$ac_cv_use_default_submitter" != "no" ; then
	SUBMITTER=$ac_cv_use_default_submitter
else
	SUBMITTER=unknown
fi
AC_MSG_RESULT($SUBMITTER)

AC_MSG_CHECKING([for --enable-organization])
if test "$ac_cv_use_default_organization" != "no" ; then
	DEFAULT_ORGANIZATION=$ac_cv_use_default_organization
else
	DEFAULT_ORGANIZATION=unknown
fi
AC_MSG_RESULT($DEFAULT_ORGANIZATION)

# Setting up datastore type
AC_MSG_CHECKING([for --enable-ds-type])
if test -z "$ac_cv_use_ds_type" ; then
	DS_TYPE=file
elif test "$ac_cv_use_ds_type" = "no" ; then
	DS_TYPE=file
else
	DS_TYPE=$ac_cv_use_ds_type
fi
DSLIB_DIR="ds-$DS_TYPE"
AC_MSG_RESULT($ac_cv_use_ds_type)

# Automake conditional
AM_CONDITIONAL([DS_TYPE_FILE],[test "$DS_TYPE" = "file"])

# Also set DEFINE
if test "${DS_TYPE}" = "file"; then
	AC_DEFINE(DS_FILE,1,[Use flat-file datastore.])
fi

AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Checking for programs
dnl ----------------------------------------------------------------------------
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_YACC
AM_PROG_LEX
if test "x$LEX" == "flex" ; then
	# Lex options - use -X
	FLEX_LFLAGS="-X"
	FLEX_CFLAGS="-D_POSIX_SOURCE=1"
fi

AC_PATH_PROGS(M4, gm4 gnum4 m4, m4)
AC_PROG_AWK

dnl GNATS m4 Macro to find passwd utility. Found in acinclude.m4.
G_PASSWD

AC_CHECK_PROG(MKTEMP, mktemp, yes, no)

AC_CACHE_SAVE
dnl ----------------------------------------------------------------------------
dnl Checking for compiler configurations
dnl ----------------------------------------------------------------------------
AM_PROG_CC_C_O

dnl Compile flag checks
m4_include([ax_check_compile_flag.m4])

dnl Silence clang's complaints about -ansi not being used. Do 2-step check due
dnl to older gcc versions returning a 0 exit code for unrecognized options.
if ! ${CC} -Qunused-arguments 2>&1 | grep 'Qunused-arguments' > /dev/null ; then
	AX_CHECK_COMPILE_FLAG([-Qunused-arguments],
	  [CFLAGS="$CFLAGS -Qunused-arguments"])
fi

dnl Disable unit-at-a-time optimization if available, due to old runtime issues
AX_CHECK_COMPILE_FLAG([-fno-unit-at-a-time],
	[CFLAGS="$CFLAGS -fno-unit-at-a-time"], [], [-Werror])

if test "x$GCC" = "xyes" ; then
	dnl GCC flags added in the gnats/Makefile.am AM_CFLAGS
	GCC_CFLAGS="-ansi -Werror -Wall"
	
	dnl Newer gcc versions use -Wextra, older use -W for extra warnings
	AX_CHECK_COMPILE_FLAG([-Wextra],
	  [GCC_CFLAGS="$GCC_CFLAGS -Wextra"], [GCC_CFLAGS="$GCC_CFLAGS -W"])

	dnl If the new -Wpedantic option introduced in gcc-4.8 is available,
	dnl use it with -Wno-error=format to allow misc.c test strftime with %z.
	dnl Also add -Wno-unused-result since gcc-4.8 warns if asprintf is used
	dnl without checking its return value (has attribute warn_unused_result)
	dnl because we are using asprintf all over the place.
	dnl For older gcc versions use -pedantic and -Wno-format.
	AX_CHECK_COMPILE_FLAG([-Wpedantic],
	  [GCC_CFLAGS="$GCC_CFLAGS -Wpedantic -Wno-error=format -Wno-unused-result"],
	  [GCC_CFLAGS="$GCC_CFLAGS -pedantic -Wno-format"], [-Werror])

	dnl Use -Wwrite-strings unless we are dealing with a getopt.h like in
	dnl solaris 10 which does not have `const' char *name in struct option.
	AC_CHECK_HEADERS([getopt.h],
	  AX_CHECK_COMPILE_FLAG([-Wwrite-strings],
	    [GCC_CFLAGS="$GCC_CFLAGS -Wwrite-strings"],
	    [echo "  (-Wwrite-strings does not like getopt.h)"], [-Werror],
	    [AC_LANG_PROGRAM([[
	      #include <unistd.h>
	      #include <getopt.h>
	      struct option long_options[] = {"foo", 1, NULL, 'f'};
	    ]])]),
	  [GCC_CFLAGS="$GCC_CFLAGS -Wwrite-strings"])

	GCC_CFLAGS="$GCC_CFLAGS -Wmissing-prototypes -Wstrict-prototypes"
fi

dnl ----------------------------------------------------------------------------
dnl Checking for libraries
dnl ----------------------------------------------------------------------------
dnl  Needed on SCO for something or other.
AC_CHECK_LIB(intl, main)

dnl If we can't find connect, try looking in -lsocket and -lnsl.  The
dnl Irix 5 libc.so has connect and gethostbyname, but Irix 5 also has
dnl libsocket.so which has a bad implementation of gethostbyname (it
dnl only looks in /etc/hosts), so we only look for -lsocket if we need
dnl it.
AC_CHECK_FUNC(connect, :, [
dnl Needed on SCO for syslog.
AC_CHECK_LIB(socket, main)
dnl Needed under Solaris in addition to -lsocket for the network.
AC_CHECK_LIB(nsl, main)
])

dnl Needed on ISC for syslog.
AC_CHECK_LIB(inet, main)

dnl Needed under IRIX for a usable malloc.
AC_CHECK_LIB(malloc, main)

dnl Needed under Unixware 2.0 for syslog.
AC_CHECK_LIB(gen, main)

dnl Needed generally for encrypted passwords.
AC_CHECK_LIB(crypt, crypt)

AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Checking for headers
dnl ----------------------------------------------------------------------------
AC_HEADER_STDC
AC_CHECK_HEADERS([crypt.h fcntl.h libgen.h limits.h memory.h netdb.h \
string.h syslog.h unistd.h machine/endian.h sys/select.h])
AC_HEADER_DIRENT
AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Check declarations
dnl ----------------------------------------------------------------------------
AC_CHECK_DECLS([unsetenv, AF_INET6])
AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Checking for Types
dnl ----------------------------------------------------------------------------
AC_TYPE_SIZE_T

dnl Linux thang.
AC_PREPROC_IFELSE([AC_LANG_SOURCE([[#include <sys/socket.h>]])],
		[AC_EGREP_CPP([socklen_t],[#include <sys/socket.h>],
               AC_DEFINE(HAVE_SOCKLEN_T,1,socklen_t),
               AC_DEFINE(HAVE_SOCKLEN_T,0,socklen_t))],[])
AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Checking for functions
dnl ----------------------------------------------------------------------------
AC_FUNC_ALLOCA
AC_CHECK_FUNCS(basename ftime fileno getopt getopt_long mkdir mktemp mkstemp unsetenv asprintf vasprintf)

AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Kerberos configuration
dnl ----------------------------------------------------------------------------
dnl See if we want Kerberos.
AC_MSG_CHECKING([for --with-kerberos])
AC_MSG_RESULT($ac_cv_use_kerberos)
if test "$ac_cv_use_kerberos" = "yes" ; then
  G_KRB4
else
  AM_CONDITIONAL([BUILD_MKAUTH],[test 0 = 1])
fi

AC_CACHE_SAVE

dnl ----------------------------------------------------------------------------
dnl Misc
dnl ----------------------------------------------------------------------------

dnl FIXME: Is this even necessary?
AC_PATH_PROG(SENDMAIL,sendmail,[],$PATH:/usr/lib:/usr/sbin:/usr/ucblib)
if test -n "$SENDMAIL" ; then
  DEFAULT_MAIL_AGENT="$SENDMAIL -oi -t"
else
  AC_MSG_NOTICE([*** Warning: sendmail binary not available])
fi

# If the config file exists, it may have more recent info than send-pr
if test -n "$verbose" ; then
	echo "	setting GNATS_SERVICE to $GNATS_SERVICE"
	echo "	setting GNATS_USER to $GNATS_USER"
	echo "	setting SUBMITTER to $SUBMITTER"
	echo "	setting DEFAULT_RELEASE to $DEFAULT_RELEASE"
	echo "	setting DEFAULT_ORGANIZATION to $DEFAULT_ORGANIZATION"
	echo "	setting DSLIB_DIR to $DSLIB_DIR"
	echo "	setting BDAY_START to $BDAY_START"
	echo "	setting BDAY_END to $BDAY_END"
	echo "	setting BWEEK_START to $BWEEK_START"
	echo "	setting BWEEK_END to $BWEEK_END"
	echo "	setting DEFAULT_MAIL_AGENT to $DEFAULT_MAIL_AGENT"
fi

AC_SUBST(PROGS)dnl
AC_SUBST(MAN)dnl
AC_SUBST(GNATS_ALL)dnl
AC_SUBST(GNATS_INSTALL)dnl
AC_SUBST(GNATS_SERVICE)dnl
AC_SUBST(GNATS_USER)dnl
AC_SUBST(DEFAULT_RELEASE)dnl
AC_SUBST(DEFAULT_ORGANIZATION)dnl
AC_SUBST(DSLIB_DIR)dnl
AC_SUBST(SUBMITTER)dnl
AC_SUBST(DEFAULT_MAIL_AGENT)dnl
AC_SUBST(BWEEK_START)dnl
AC_SUBST(BWEEK_END)dnl
AC_SUBST(BDAY_START)dnl
AC_SUBST(BDAY_END)dnl
AC_SUBST(GNATSD_USER_ACCESS_FILE)dnl
AC_SUBST(GNATSD_HOST_ACCESS_FILE)dnl
AC_SUBST(GLOBAL_DB_LIST_FILE)dnl
AC_SUBST(GNATS_DEFAULT_DB_DIR)dnl
AC_SUBST(GCC_CFLAGS)dnl
AC_SUBST(FLEX_CFLAGS)dnl
AC_SUBST(FLEX_LFLAGS)dnl

# ***** End of configuration section *****

AC_CONFIG_FILES([Makefile \
	gnats/Makefile \
	gnats/ds-file/Makefile \
	doc/Makefile \
	doc/man/Makefile])

AC_OUTPUT
