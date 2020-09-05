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

lib_LTLIBRARIES = libCLASS_METADATA.la libMULTI_SERIALIZER.la libEDMA_CIDF.la \
		  libEDMAIDF_PARSER.la  libSERIALIZER.la libEDMA_SCRIPT.la

#CFLAGS=-fPIC -c -DGCC -DLINUX 
#INCLUDES=-I../../../include
INCLUDES = -I ${top_srcdir}/include

NAMESPACE = system
idfdir = ${prefix}/share/edma/idf/${NAMESPACE}
libdir = ${exec_prefix}/lib/edma/${NAMESPACE}
 
EXTRA_DIST = CLASS_METADATA.idf  \
                EDMAIDF_PARSER.idf \
		CLASS_METADATA.ines \
                EDMAIDF_PARSER.ines \
		SERIALIZER.idf SERIALIZER.ines \
		MULTI_SERIALIZER.idf MULTI_SERIALIZER.ines \
		EDMA_SCRIPT.idf EDMA_SCRIPT.ines \
        	EDMA_CIDF.ines EDMA_CIDF.idf cidf_parser.h test.cidf


idf_DATA = CLASS_METADATA.idf SERIALIZER.idf  EDMA_CIDF.idf\
	EDMAIDF_PARSER.idf  MULTI_SERIALIZER.idf EDMA_SCRIPT.idf

libCLASS_METADATA_la_SOURCES =CLASS_METADATA_imp.c
libEDMAIDF_PARSER_la_SOURCES = EDMAIDF_PARSER_imp.c
libSERIALIZER_la_SOURCES = SERIALIZER_imp.c
libMULTI_SERIALIZER_la_SOURCES = MULTI_SERIALIZER_imp.c
libEDMA_SCRIPT_la_SOURCES = EDMA_SCRIPT_imp.c
libEDMA_CIDF_la_SOURCES = EDMA_CIDF_imp.c cidf_parser.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in
