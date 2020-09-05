# Process this file with autoconf to produce a configure script.

# Copyright (C) 2008, 2009, 2010, 2011, 2012,
#   2013, 2014, 2015, 2016, 2017, 2018, 2020 Free Software Foundation, Inc.

# This file is part of GNUnited Nations.

# GNUnited Nations is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# GNUnited Nations is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNUnited Nations.  If not, see <https://www.gnu.org/licenses/>.
AC_INIT([GNUnited Nations],
	[1.0],
	[bug-gnun@gnu.org],
	[gnun],
	[https://www.gnu.org/software/gnun/])

AC_PREREQ([2.63b])

AC_CONFIG_SRCDIR([GNUmakefile])	
AC_CONFIG_MACRO_DIR([m4])

## 1.11 is needed for the `pkglibexecdir' definition.
AM_INIT_AUTOMAKE([1.11 -Wall silent-rules])

# Checks for programs.  Missing some of them is not fatal, as the user
# might want to use `make report' only, or some other limited
# functionality.  Most variables are declared `precious' in order to
# override them both via `configure' and `make' when building
# translations.
CHECK_GNU_MAKE
AS_IF([test -z "$ifGNUmake"], [decent_make=yes], [decent_make=no])

AC_MSG_CHECKING([whether to use www.gnu.org Multiviews naming conventions])
AC_ARG_ENABLE([multiviews],
  [AS_HELP_STRING([--disable-multiviews],
  [Use pre-Multiviews naming conventions for English URLs (`foo.html'
 rather than `foo.en.html').])]
)
AS_CASE(["x$enable_multiviews"],[xno],,[enable_multiviews=yes])
AC_MSG_RESULT([$enable_multiviews])
AC_SUBST([MULTIVIEWS], [$enable_multiviews])

AC_MSG_CHECKING([whether to enable sorting])
AC_ARG_ENABLE([sort],
  [AS_HELP_STRING([--disable-sort],
  [Disable sorting lists.])]
)
AS_CASE(["x$enable_sort"],[xno],,[enable_sort=yes])

AC_MSG_CHECKING([whether `date' supports the `-d' option])
timestamp=`date -d "2001-09-09 01:47+0000" +%s 2> /dev/null || echo FAIL`
decent_date=no
# Some versions of date return a result that varies
# from 1000000020 to 1000000079.
AS_CASE(["x$timestamp"], [x10000000??],
[decent_date=yes
])
AC_MSG_RESULT([$decent_date])

AC_ARG_VAR([AWK], [the `awk' program])
AX_CHECK_AWK_GSUB([decent_awk=yes], [decent_awk=no])
AX_TRY_AWK_EXPOUT([match(a,b,c)],[],
[
  if(!match("abcdefg", /b((c)d)e(f)/, arr))
    print "no match"
  if (arr[1] != "cd" || arr[2] != "c" || arr[3] != "f"\
      || arr[0] != "bcdef" || 4 in arr)
    print "wrong grouping"
  print "OK"
], [OK], [match3=match], [match3=match_fallback])
AC_SUBST([MATCH3], [$match3])
AM_CONDITIONAL([HAVE_MATCH3], [test $match3 = match])

AX_ARG_PROG([BASH], [the `bash' program], [bash])
AS_CASE(["x$BASH"], [x],
[AC_MSG_ERROR([

No `bash' program found.  Try configure BASH=/path/to/bash
if you have it.
])
])

AC_ARG_VAR([GREP], [the `grep' program])
AC_ARG_VAR([EGREP], [the `egrep' program])
AC_PROG_EGREP

AC_ARG_VAR([SED], [the `sed' program])
AC_PROG_SED

# We really really really depend on GNU sed.
AC_MSG_CHECKING([whether sed supports --in-place option])
cat > conftest0.sed <<'EOF'
lorem ipsum dolor sit amet
EOF
sedtest=s/lor/minator/g
${SED} ${sedtest} < conftest0.sed > conftest.sed
${SED} --in-place ${sedtest} conftest0.sed
AS_IF(
[test $? = 0 && cmp -s conftest0.sed conftest.sed],
[AC_MSG_RESULT([yes])],
[AC_MSG_ERROR([

Unlike GNU sed, the `sed' program doesn't support --in-place option.
])
])

AC_MSG_CHECKING([whether sed supports the `e' option of the `s' command])
test="$(echo false | ${SED} 's/.*/true/e')"
AS_IF([test $? = 0 -a x"${test}" = x],
[AC_MSG_RESULT([yes])],
[AC_MSG_ERROR([

Unlike GNU sed, the `sed' program doesn't support the `e' option
of the `s' command.
])
])

AC_MSG_CHECKING([whether sed supports the `q' command with an exit code])
echo seventeen | ${SED} -n '1 {q 17}' > /dev/null
result=$?
echo zero | ${SED} -n '1 {q 0}' > /dev/null
AS_IF([test ${result} = 17 -a $? = 0],
[AC_MSG_RESULT([yes])],
[AC_MSG_ERROR([

Unlike GNU sed, the `sed' program doesn't support the `q' command
with an exit code.
])
])

AC_MSG_CHECKING([for sed-4.3 bug])

AS_IF([locale -a >/dev/null 2>/dev/null], [have_locale=yes], [have_locale=no])
utf8_locale=
AS_CASE([$have_locale], [yes], [
utf8_locale=`locale -a | $SED '/\(UTF-8\|utf8\)$/q;d'`])
AS_CASE([x"$utf8_locale"], [x], [
utf8_locale=en_US.utf8])

cat > conftest0 <<EOF
echo 'abcdefgh "014"' \
 | LC_ALL=$utf8_locale ${SED} \
    's%.*%&\na%;s/^abcdefgh "\([[^\n]]*\)"[[^\n]]*\n//' 2>&1 > /dev/null
EOF
buggy_sed=no
AS_IF([bash conftest0 2> /dev/null],
[AC_MSG_RESULT([no])],
[AC_MSG_RESULT([yes])
 buggy_sed=yes])

# Detect the program to do base64 encoding.
base64_encode=""
AX_ARG_PROG([BASE64], [the `base64' program], [base64])
AX_ARG_PROG([UUENCODE], [the `uuencode' program], [uuencode])
AS_CASE(["x$BASE64"], [x],
[AS_CASE(["x$UUENCODE"], [x], ,
[base64_encode=\
"$UUENCODE -m - | $SED ':c;N;$!bc;s/[^\n]*\n//;s/\n[^\n]*$//;s/\n//g'"
])],
[base64_encode="$BASE64 --wrap=0"
])
AS_CASE(["x$base64_encode"], [x],
[AC_MSG_WARN([

No program to do base64 encoding found.
Announcements may not work properly.

You may want to install base64 or uuencode.
])
base64_encode=cat
])
AC_SUBST([BASE64_ENCODE], [$base64_encode])

AX_ARG_PROG([MKTEMP], [the `mktemp' program], [mktemp])
AS_CASE(["x$MKTEMP"], [x],
[AC_MSG_ERROR([

No `mktemp' program found.  Try configure MKTEMP=/path/to/mktemp
if you have it.
])
])

AC_MSG_CHECKING([whether mktemp supports -t])
have_mktemp_t=no
mktemp_t=
AS_CASE(["x$MKTEMP"], [x],,
[tempfile=`"$MKTEMP" -t conf.XXXXXXX 2> /dev/null`
AS_CASE(["x$tempfile"], [x],,
[have_mktemp_t=yes
rm -f "$tempfile"
mktemp_t=-t
])])
AC_MSG_RESULT([$have_mktemp_t])
AC_SUBST([MKTEMP_T], [$mktemp_t])

AX_ARG_PROGS([MSGCAT], [the `msgcat' program], [msgcat gmsgcat])
AX_ARG_PROGS([MSGFMT], [the `msgfmt' program], [msgfmt gmsgfmt])
AX_ARG_PROGS([MSGEXEC], [the `msgexec' program], [msgexec gmsgexec])
AC_ARG_VAR([MSGMERGE], [the `msgmerge' program])
AS_CASE(["x$MSGMERGE"], [x], [msgmerges="msgmerge gmsgmerge"],
        [msgmerges="$MSGMERGE"])
AC_CACHE_CHECK([for msgmerge that supports --previous],
               [ac_cv_path_MSGMERGE],
  [AC_PATH_PROGS_FEATURE_CHECK([MSGMERGE], [$msgmerges],
    [[$ac_path_MSGMERGE --previous /dev/null /dev/null 2>/dev/null \
      && ac_cv_path_MSGMERGE=$ac_path_MSGMERGE]],
      [AC_MSG_RESULT([none found])
       AC_MSG_WARN([consider updating gettext])])])
AC_SUBST([MSGMERGE], [$ac_cv_path_MSGMERGE])
AS_IF([test -n "$MSGCAT" && test -n "$MSGEXEC" \
       && test -n "$MSGFMT" && test -n  "$MSGMERGE"],
      [decent_gettext=yes], [decent_gettext=no])
AM_CONDITIONAL([HAVE_GETTEXT], [test "$decent_gettext" = yes])

# Presumably if msgmerge supports --previous, msgattrib supports
# --clear-previous as well, but it's safer to check anyway.
AC_CACHE_CHECK([for msgattrib that supports --clear-previous],
               [ac_cv_path_MSGATTRIB],
  [AC_PATH_PROGS_FEATURE_CHECK([MSGATTRIB], [msgattrib gmsgattrib],
    [[$ac_path_MSGATTRIB --clear-previous /dev/null 2>/dev/null \
      && ac_cv_path_MSGATTRIB=$ac_path_MSGATTRIB]],
      [AC_MSG_RESULT([none found])
       AC_MSG_WARN([consider updating gettext])])])
AC_SUBST([MSGATTRIB], [$ac_cv_path_MSGATTRIB])
AS_IF([test -n "$MSGATTRIB"], [decent_msgattrib=yes], [decent_msgattrib=no])
AM_CONDITIONAL([NEW_MSGATTRIB], [test "$decent_msgattrib" = yes])

AC_MSG_CHECKING([whether msgattrib supports --previous])
msgattrib_previous=no
AS_IF(
[test "x$MSGATTRIB" != x && echo | "$MSGATTRIB" --previous > /dev/null 2>&1],
[msgattrib_previous=yes])
AC_SUBST([MSGATTRIB_PREV], [$msgattrib_previous])
AC_MSG_RESULT([$msgattrib_previous])

AX_ARG_PROG([WDIFF], [the `wdiff' program], [wdiff])
AS_CASE(["x$WDIFF"], [x], [have_wdiff=no], [have_wdiff=yes])
AM_CONDITIONAL([HAVE_WDIFF], [test -n "$WDIFF"])

AX_ARG_PROG([PO4A_GETTEXTIZE], [the `po4a-gettextize' program],
            [po4a-gettextize])

AX_ARG_PROG([PO4A_TRANSLATE], [the `po4a-translate' program], [po4a-translate])

# This is a weak test; should be replaced with a feature check.
AS_CASE(["x$PO4A_TRANSLATE"], [x], [], [
  AC_CACHE_CHECK([if po4a version is at least 0.37], [gnun_cv_recent_po4a],
                 [gnun_cv_recent_po4a=no
                 po4a_version=`$PO4A_TRANSLATE --version 2> /dev/null \
                   | $GREP po4a | $AWK '{print $3}' | $SED 's/.$//'`
                 AX_COMPARE_VERSION([$po4a_version], [ge], [0.37],
                 [gnun_cv_recent_po4a=yes])])
])
# Po4a is useless for our purpose if awk is missing.
recent_po4a=$gnun_cv_recent_po4a
AS_CASE([$decent_awk], [no], [recent_po4a=no])

AX_ARG_PROG([MAIL], [the `mail' program], [mail])

# This test is currently unnecessary, but might be useful if we rely
# on GNU Mailutils features in the future.
AS_CASE(["x$MAIL"], [x], [have_mail=no],
[have_mail=yes
AC_CACHE_CHECK([whether `mail' is from GNU Mailutils],
	       [gnun_cv_gnu_mailutils],
	       [gnun_cv_gnu_mailutils=no
	        if $MAIL --version 2> /dev/null \
		     | $GREP GNU 2>&1 > /dev/null ; then
		  gnun_cv_gnu_mailutils=yes ;
		fi])
])

# Check for supported VCS.  Currently the tests are run only for the
# sake of pretty-printing the summary at the end, but the results will
# be used for the future testsuite.
AC_PATH_PROG([CVS], [cvs])
AS_IF([test -n "$CVS"], [cvs="(CVS)"])
AC_PATH_PROG([SVN], [svn])
AS_IF([test -n "$SVN"], [svn="(Subversion)"])
AC_PATH_PROG([BZR], [bzr])
AS_IF([test -n "$BZR"], [bzr="(Bazaar)"])
AS_IF([test -n "$CVS" || test -n "$SVN" || test -n "$BZR"],
      [have_vcs=yes], [have_vcs=no])

AC_PATH_PROG([XMLLINT], [xmllint])
AS_CASE(["x$XMLLINT"], [x], [have_xmllint=no], [have_xmllint=yes])

cat > conftest.html <<'EOF'
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<title>Foo</title>
</head>
<body>
<p>Bar.</p>
</body>
</html>
EOF

AS_CASE([$have_xmllint], [yes],
[AC_MSG_CHECKING([if xmllint supports --path])
AS_IF([$XMLLINT --path $ac_top_srcdir/dtd conftest.html > /dev/null 2>&1],
[have_xmllint_path=yes],
[have_xmllint_path=no
 have_xmllint=no
])
AC_MSG_RESULT([$have_xmllint_path])
])

AS_CASE([$have_xmllint], [yes],
[AC_MSG_CHECKING([if xmllint supports --nonet])

have_xmllint_nonet=no
AS_IF([$XMLLINT --nonet --noout conftest.html > /dev/null 2>&1],
[have_xmllint_nonet=yes
AC_MSG_RESULT([yes])
],
[AC_MSG_RESULT([no (will install our copy of XHML DTDs)])
])

have_dtds=no
AS_CASE([$have_xmllint_nonet], [yes],
[AC_MSG_CHECKING([if XHTML DTDs are installed])
AS_IF([$XMLLINT --valid --nonet --noout conftest.html > /dev/null 2>&1],
[have_dtds=yes])

if test "$have_dtds" = no; then
  AC_MSG_RESULT([$have_dtds (will use our copy)])
else
  AC_MSG_RESULT([$have_dtds])
fi
])])

AX_ARG_PROG([SORT], [the `sort' program], [sort])
AS_CASE(["x$SORT"], [x], [have_sort=no], [have_sort=yes])

AX_ARG_PROG([LOCALEDEF], [the `localedef' program], [localedef])
AS_CASE(["x$LOCALEDEF"], [x], [have_localedef=no], [have_localedef=yes])
AC_SUBST([LOCALEDEF], [$LOCALEDEF])

have_sorted_lists=no
AS_CASE([$have_localedef$have_sort$enable_sort], [yesyesyes],
[have_sorted_lists=yes])

AM_CONDITIONAL([HAVE_SORT], [test "$have_sorted_lists" = yes])

# Our validation scripts depend on awk and xmllint.
AS_CASE([$have_xmllint], [yes],
        [have_validation=$decent_awk], [have_validation=no])

AM_CONDITIONAL([HAVE_VALIDATION], [test "$have_validation" = yes])
AM_CONDITIONAL([NO_DTD], [test "$have_dtds" = no])
AM_CONDITIONAL([HAVE_PO4A], [test "$recent_po4a" = yes])

AC_CONFIG_FILES([Makefile doc/Makefile dtd/Makefile config.mk])
AS_CASE(["$decent_gettext-$recent_po4a"], [yes-yes],
        [AC_CONFIG_FILES([gnun-preconvert])])
AS_CASE([$have_validation], [yes],
  [AC_CONFIG_FILES([expand-ssi.awk])])

AC_OUTPUT

cat <<EOF
m4_text_box([Results])
Basic make operations...                $decent_make
PO files manipulation, make report...   $decent_gettext
gnun-add-fuzzy-diff...                  $have_wdiff
POT/HTML generation...                  $recent_po4a
HTML validation...                      $have_validation
NOTIFY/ANNOUNCE support...              $have_mail
VCS support...                          $have_vcs $cvs $svn $bzr
Use Multiviews naming conventions...    $enable_multiviews
Sorting support                         $have_sorted_lists

Type "make" to build the scripts, "make install" to install them
along with the manuals, and "make installcheck" to run tests.

EOF

AS_CASE([$decent_date], [no],
[AC_MSG_WARN([

The `date' program seems to support no `-d' option.

We rely on it when delaying pages regeneration; this feature may not
work for you.

])])

AS_CASE([$buggy_sed], [yes], [
AC_MSG_WARN([

The `sed' program seems be affected by the bug discussed in
https://lists.gnu.org/archive/html/bug-sed/2017-01/msg00018.html
(known in GNU Sed 4.3 when used with UTF-8 locales).

Replace sed or use the C locale.

])])
