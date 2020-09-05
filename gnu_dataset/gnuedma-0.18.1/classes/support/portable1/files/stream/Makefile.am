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

lib_LTLIBRARIES =libSTREAM.la libMEMORY_STREAM.la

INCLUDES = -I ${top_srcdir}/include

NAMESPACE = system/files
idfdir=${prefix}/share/edma/idf/${NAMESPACE}
libdir = ${exec_prefix}/lib/edma/${NAMESPACE}

EXTRA_DIST= STREAM.idf  MEMORY_STREAM.idf\
						STREAM.ines MEMORY_STREAM.ines

idf_DATA= STREAM.idf MEMORY_STREAM.idf


libSTREAM_la_SOURCES=STREAM_imp.c
libMEMORY_STREAM_la_SOURCES=MEMORY_STREAM_imp.c

MAINTAINERCLEASTREAMS =  Makefile.in config.h.in stamp-h.in
