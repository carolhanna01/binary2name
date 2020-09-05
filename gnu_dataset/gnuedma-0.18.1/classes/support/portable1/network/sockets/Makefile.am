#   EDMA: Entorno de Desarrollo Modular y Abierto
#   Object Oriented and Componentware Framework                                 
#   
#   Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
#                by David Martínez Oliveira
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

lib_LTLIBRARIES =libSOCKET_TCP.la libSOCKET_UDP.la libSOCKET_RAW.la \
		libFD_SET.la libCHANNEL.la

INCLUDES = -I ${top_srcdir}/include

REPO_NAME=edma_examples
NAMESPACE=system/net/sockets

idfdir=${prefix}/share/${REPO_NAME}/idf/${NAMESPACE}
libdir = ${prefix}/share/${REPO_NAME}/lib/${NAMESPACE}


EXTRA_DIST= SOCKET_TCP.idf  SOCKET_UDP.idf  SOCKET_RAW.idf  FD_SET.idf  CHANNEL.idf \
						SOCKET_TCP.ines SOCKET_UDP.ines SOCKET_RAW.ines FD_SET.ines CHANNEL.ines
					
idf_DATA= SOCKET_TCP.idf  SOCKET_UDP.idf  SOCKET_RAW.idf  FD_SET.idf CHANNEL.idf

libSOCKET_TCP_la_SOURCES=SOCKET_TCP_imp.c
libSOCKET_UDP_la_SOURCES=SOCKET_UDP_imp.c
libSOCKET_RAW_la_SOURCES=SOCKET_RAW_imp.c
libFD_SET_la_SOURCES=FD_SET_imp.c
libCHANNEL_la_SOURCES=CHANNEL_imp.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in
