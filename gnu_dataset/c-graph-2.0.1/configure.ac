#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# File: configure.ac
#
# GNU C-Graph Version 2.0.1
# December 2018
# 
# Copyright (c) 2009, 2010, 2011, 2018 Adrienne Gaye Thompson
#
# This file is part of GNU C-Graph.
#
# GNU C-Graph is free software: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License 
# (the GPL) as published by the Free Software Foundation, either 
# version 3 of the License, or (at your option) any later version.
#
# GNU C-Graph is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GPL
# for more details.
#
# GNU C-Graph includes the file COPYING, which contains a copy of the GPL
# version 3.0. You may view the GPL online at
# <http://www.gnu.org/licenses/gpl.html>.
#



AC_PREREQ([2.68])
AC_INIT([C-Graph], [2.0.1], [agt@codeartnow.com])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([check-news])

# Checks for programs.
AC_CONFIG_SRCDIR([src/c-graph.F90])
AC_PROG_FC(g95 gfortran)
FCFLAGS="-O0 -g"

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 src/Makefile
		 doc/Makefile
		 logo/Makefile
		 man/Makefile])
AC_OUTPUT
