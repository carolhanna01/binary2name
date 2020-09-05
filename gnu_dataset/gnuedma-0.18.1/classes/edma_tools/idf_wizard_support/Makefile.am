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
#
lib_LTLIBRARIES = libEDMA_C_IMP_BUILDER.la \
		  libEDMAIDF_BUILDER.la \
		  libEDMA_INES_BUILDER.la \
		  libAUTOTOOLS_PROJECT.la \
		  libEDMA_C_INTERFACE_BUILDER.la \
		  libEDMA_COM_INTERFACE_BUILDER.la \
		  libEDMA_MAKE_BUILDER.la

#CFLAGS=-fPIC -c -DGCC -DLINUX 
#INCLUDES=-I../../../include
INCLUDES = -I ${top_srcdir}/include

NAMESPACE = tools/idfw
idfdir = ${prefix}/share/edma/idf/${NAMESPACE}
libdir = ${exec_prefix}/lib/edma/${NAMESPACE}
 
EXTRA_DIST =  EDMA_C_IMP_BUILDER.idf \
		EDMA_C_INTERFACE_BUILDER.idf EDMA_C_INTERFACE_BUILDER.ines \
		EDMA_COM_INTERFACE_BUILDER.idf EDMA_COM_INTERFACE_BUILDER.ines \
                EDMAIDF_BUILDER.idf \
                EDMA_INES_BUILDER.idf \
                EDMA_MAKE_BUILDER.idf AUTOTOOLS_PROJECT.idf \
		EDMA_C_IMP_BUILDER.ines \
                EDMAIDF_BUILDER.ines \
                EDMA_INES_BUILDER.ines \
                EDMA_MAKE_BUILDER.ines AUTOTOOLS_PROJECT.ines


idf_DATA = EDMA_C_IMP_BUILDER.idf \
		EDMA_C_INTERFACE_BUILDER.idf \
		EDMA_COM_INTERFACE_BUILDER.idf \
		EDMAIDF_BUILDER.idf \
		EDMA_INES_BUILDER.idf \
		EDMA_MAKE_BUILDER.idf AUTOTOOLS_PROJECT.idf

libEDMA_C_IMP_BUILDER_la_SOURCES =EDMA_C_IMP_BUILDER_imp.c
libEDMA_C_INTERFACE_BUILDER_la_SOURCES =EDMA_C_INTERFACE_BUILDER_imp.c
libEDMA_COM_INTERFACE_BUILDER_la_SOURCES =EDMA_COM_INTERFACE_BUILDER_imp.c
libEDMAIDF_BUILDER_la_SOURCES = EDMAIDF_BUILDER_imp.c
libEDMA_INES_BUILDER_la_SOURCES = EDMA_INES_BUILDER_imp.c
libEDMA_MAKE_BUILDER_la_SOURCES = EDMA_MAKE_BUILDER_imp.c
libAUTOTOOLS_PROJECT_la_SOURCES = AUTOTOOLS_PROJECT_imp.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in
