dnl Configure path for the Docbook to XHTML XSL stylesheet
dnl
dnl Sets DB2XHTML_STYLESHEET.
dnl
AC_DEFUN(AC_PATH_DB2XHTML_STYLESHEET,
[AC_MSG_CHECKING([for location of chunk.xsl])
DB2XHTML_STYLESHEET=/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/chunk.xsl
if test ! -f ${DB2XHTML_STYLESHEET}; then
  DB2XHTML_STYLESHEET=/usr/share/sgml/docbook/xsl-stylesheets/xhtml/chunk.xsl
fi
if test ! -f "${DB2XHTML_STYLESHEET}"; then
  AC_MSG_RESULT([not found, ``make dist'' will fail])
else
  AC_MSG_RESULT([${DB2XHTML_STYLESHEET}])
fi
AC_SUBST(DB2XHTML_STYLESHEET)
])
