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

lib_LTLIBRARIES = libHELLO_WORLD.la \
		     libHELLO_WORLD2.la

INCLUDES = -I ${top_srcdir}/include

NAMESPACE = examples/hello

idfdir=${prefix}/share/edma/idf/${NAMESPACE}
libdir = ${exec_prefix}/lib/edma/${NAMESPACE}

EXTRA_DIST = HELLO_WORLD.idf HELLO_WORLD.ines \
						 HELLO_WORLD2.idf HELLO_WORLD2.ines
idf_DATA= HELLO_WORLD.idf HELLO_WORLD2.idf

libHELLO_WORLD_la_SOURCES = HELLO_WORLD_imp.c
libHELLO_WORLD2_la_SOURCES = HELLO_WORLD2_imp.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in
