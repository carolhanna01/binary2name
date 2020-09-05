#### configure.ac
#### Created by Laurence D. Finston (LDF) 2003.11.11.

#### * Copyright and License.

#### This file is part of GNU 3DLDF, a package for three-dimensional drawing. 
#### Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013 The Free Software Foundation 

#### GNU 3DLDF is free software; you can redistribute it and/or modify 
#### it under the terms of the GNU General Public License as published by 
#### the Free Software Foundation; either version 3 of the License, or 
#### (at your option) any later version. 

#### GNU 3DLDF is distributed in the hope that it will be useful, 
#### but WITHOUT ANY WARRANTY; without even the implied warranty of 
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
#### GNU General Public License for more details. 

#### You should have received a copy of the GNU General Public License 
#### along with GNU 3DLDF; if not, write to the Free Software 
#### Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA 

#### GNU 3DLDF is a GNU package.  
#### It is part of the GNU Project of the  
#### Free Software Foundation 
#### and is published under the GNU General Public License. 
#### See the website http://www.gnu.org 
#### for more information.   
#### GNU 3DLDF is available for downloading from 
#### http://www.gnu.org/software/3dldf/LDF.html. 

#### Please send bug reports to Laurence.Finston@gmx.de
#### The mailing list help-3dldf@gnu.org is available for people to 
#### ask other users for help.  
#### The mailing list info-3dldf@gnu.org is for sending 
#### announcements to users. To subscribe to these mailing lists, send an 
#### email with ``subscribe <email-address>'' as the subject.  

#### The author can be contacted at: 

#### Laurence D. Finston                  
#### c/o Free Software Foundation, Inc.  
#### 51 Franklin St, Fifth Floor          
#### Boston, MA  02110-1301              
#### USA                               

#### Laurence.Finston@gmx.de


  
.# Process this file with autoconf to produce a configure script.
AC_INIT([GNU 3DLDF], [2.0.3], [Laurence.Finston@gmx.de], [3DLDF])
AC_CONFIG_SRCDIR([src/main.web])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIR([m4])
LT_INIT
AC_SUBST([LIBTOOL_DEPS])


# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_YACC
AM_PROG_LEX
AC_CHECK_PROG([HAVE_SHA224SUM], [sha224sum], 1, 0) 
AC_CHECK_PROG([HAVE_SHA1SUM], [sha1sum], 1, 0) 
AC_CHECK_PROG([HAVE_MD5SUM], [md5sum], 1, 0) 
AC_CHECK_PROG([HAVE_CKSUM], [cksum], 1, 0) 
AC_CHECK_PROG([HAVE_GZIP], [gzip], 1, 0) 

AC_DEFINE_UNQUOTED([HAVE_SHA224SUM], $HAVE_SHA224SUM, 
                   [Define to 1, if `sha224sum' is available, otherwise 0.])
AC_DEFINE_UNQUOTED([HAVE_SHA1SUM], $HAVE_SHA1SUM, 
                   [Define to 1, if `sha1sum' is available, otherwise 0.])
				   
AC_DEFINE_UNQUOTED([HAVE_MD5SUM], $HAVE_MD5SUM, 
                   [Define to 1, if `md5sum' is available, otherwise 0.])

AC_DEFINE_UNQUOTED([HAVE_CKSUM], $HAVE_CKSUM, 
                   [Define to 1, if `cksum' is available, otherwise 0.])

AC_DEFINE_UNQUOTED([HAVE_GZIP], $HAVE_GZIP, d
                   [Define to 1, if `gzip' is available, otherwise 0.])

# Checks for libraries.
#AC_CHECK_LIB(m, trunc)
AC_CHECK_LIB(gsl, gsl_matrix_alloc)

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([errno.h float.h limits.h stdlib.h \
                  unistd.h pthread.h gsl/gsl_complex.h \
                  gsl/gsl_complex_math.h gsl/gsl_matrix.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_C_CONST
#AC_C_INLINE
#AC_TYPE_SIZE_T
#AC_STRUCT_TM
AC_CHECK_TYPES([unsigned long long, unsigned long, unsigned int])

# Checks for library functions.
#AC_FUNC_MALLOC
#AC_FUNC_STAT
AC_SEARCH_LIBS(trunc, m)
AC_CHECK_FUNCS([floor sqrt trunc])

AC_CONFIG_FILES([Makefile src/Makefile doc/Makefile doc/old_doc/Makefile examples/Makefile])

# Checks for endianness.
AC_C_BIGENDIAN

AC_ARG_ENABLE([dummy],
AC_HELP_STRING([--enable-debug-compile],
[Enable conditionally compiled debugging output (this is the default).]),
[],[])

AC_ARG_ENABLE([debug-compile], 
[AC_HELP_STRING([--disable-debug-compile],
        [Disable conditionally compiled debugging output.])],
[if test $enableval == "yes"
then
echo "Enabling conditional compilation of debugging output"
AC_DEFINE([DEBUG_COMPILE], [1], 
  [Define to 0 or 1 to disable or enable conditional compilation of debugging output.])
else 
echo "Disabling conditional compilation of debugging output"
AC_DEFINE([DEBUG_COMPILE], [0], 
  [])
fi], 
[echo "Enabling conditional compilation of debugging output"
AC_DEFINE([DEBUG_COMPILE], [1], 
[])	
])

AC_OUTPUT
