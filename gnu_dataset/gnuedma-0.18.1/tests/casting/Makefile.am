#   EDMA: Entorno de Desarrollo Modular y Abierto
#   Object Oriented and Componentware Framework  
#
#   Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
#             by David Martínez Oliveira
#   
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

lib_LTLIBRARIES = libSUBCLASS1.la libSUBCLASS2.la libSUPER_CLASS.la

INCLUDES = -I ${top_srcdir}/include

REPO_NAME=edma_test
NAMESPACE=casting
idfdir=${prefix}/share/${REPO_NAME}/idf/${NAMESPACE}
libdir = ${prefix}/share/${REPO_NAME}/lib/${NAMESPACE}

EXTRA_DIST= SUBCLASS1.idf SUBCLASS2.idf SUPER_CLASS.idf \
  	    SUBCLASS1.ines SUBCLASS2.ines SUPER_CLASS.ines \
	    cast_test.c cast_test.mk edma_repos.lst cast_test.out

idf_DATA= SUBCLASS1.idf SUBCLASS2.idf SUPER_CLASS.idf

libSUBCLASS1_la_SOURCES = SUBCLASS1_imp.c
libSUBCLASS2_la_SOURCES = SUBCLASS2_imp.c
libSUPER_CLASS_la_SOURCES = SUPER_CLASS_imp.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in

