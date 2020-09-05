dnl Find the number of CPUs in the machine.
dnl
dnl Sets CPU_COUNT if successfull.
dnl
AC_DEFUN(AC_CPU_COUNT,
[AC_MSG_CHECKING([number of CPUs in this machine])
AC_RUN_IFELSE([AC_LANG_PROGRAM(
[[#include "lib/cpucount.c"
#include <stdio.h>]],
[[int c = cpucount();
  if(!c)
    exit(EXIT_FAILURE);
  FILE *f = fopen("/tmp/cpucount", "w"); /* FIXME: needs unique filename */
  if(!f) {
    fprintf(stderr, "error writing to /tmp/cpucount\n"); 
    exit(EXIT_FAILURE);
  }
  fprintf(f, "%d", c);
  fclose(f);
  exit(EXIT_SUCCESS);
]])],
        [CPU_COUNT=`cat /tmp/cpucount`]
        [AC_DEFINE_UNQUOTED(CPU_COUNT, $CPU_COUNT)],,)
AC_MSG_RESULT($CPU_COUNT)
])