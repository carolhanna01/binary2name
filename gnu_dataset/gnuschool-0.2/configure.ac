# Copyright (C) 2006 Peter Rios <peter@gnuschool.org>
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AC_INIT([gnuschool],
        [0.2],
        [Peter Rios gnuschool-bugs@gnu.org],
        [gnuschool])
AC_CONFIG_AUX_DIR(config)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE([dist-bzip2])

AC_PROG_CC
AC_PROG_INSTALL
AC_AIX
AC_ISC_POSIX
AC_MINIX
AC_STDC_HEADERS
AC_PROG_RANLIB

AC_CONFIG_FILES([
   Makefile
   doc/Makefile
   m4/Makefile
   src/Makefile
   src/db_admin/Makefile
   src/principals/Makefile
   src/students/Makefile
   src/teachers/Makefile
   src/teachers/attendance/Makefile
   src/teachers/grades/Makefile
   src/teachers/seats/Makefile
   src/teachers/students_info/Makefile
   src/teachers/tests_admin/Makefile
])

AC_OUTPUT
