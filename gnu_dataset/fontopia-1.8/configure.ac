# 
#    Copyright 2015, 2016, 2017, 2018 (c)
#    Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
# 
#    file: configure.ac
#    This file is part of fontopia.
#
#    fontopia is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    fontopia is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with fontopia.  If not, see <http://www.gnu.org/licenses/>.
#    

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([fontopia], [1.8], [mohammed_isam1984@yahoo.com])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror gnu])
AM_PROG_AR
LT_INIT

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([stdio.h stdlib.h getopt.h string.h asm/types.h limits.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_MMAP
AC_CHECK_FUNCS([malloc getcwd memset strcasecmp strchr strndup strrchr])

AC_C_INLINE

AC_CONFIG_FILES([Makefile
		info/Makefile
		man/Makefile])

AC_OUTPUT
