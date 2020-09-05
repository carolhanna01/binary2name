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
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
lib_LTLIBRARIES = libHELLO_WORLD.la 

INCLUDES = -I ${top_srcdir}/include

REPO_NAME=edma_examples
NAMESPACE = examples/hello

idfdir=${prefix}/share/${REPO_NAME}/idf/${NAMESPACE}
libdir = ${prefix}/share/${REPO_NAME}/lib/${NAMESPACE}

EXTRA_DIST = HELLO_WORLD_1_0.idf HELLO_WORLD_1_0.ines 
idf_DATA= HELLO_WORLD_1_0.idf 

libHELLO_WORLD_la_SOURCES = HELLO_WORLD_imp.c
libHELLO_WORLD_la_LDFLAGS = -version-info 1:0

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in
