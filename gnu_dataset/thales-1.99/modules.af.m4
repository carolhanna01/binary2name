AC_DEFUN([AUTOFRISK_CHECKS],[

GUILE_MODULE_REQUIRED(srfi srfi-26)
GUILE_MODULE_REQUIRED(srfi srfi-1)
GUILE_MODULE_REQUIRED(ice-9 pretty-print)
GUILE_MODULE_REQUIRED(ice-9 match)

probably_wont_work=""

AC_SUBST(probably_wont_work)
])


AC_DEFUN([AUTOFRISK_SUMMARY],[
if test ! "$probably_wont_work" = "" ; then
    p="         ***"
    echo "$p"
    echo "$p NOTE:"
    echo "$p The following modules probably won't work:"
    echo "$p   $probably_wont_work"
    echo "$p They can be installed anyway, and will work if their"
    echo "$p dependencies are installed later.  Please see README."
    echo "$p"
fi
])
