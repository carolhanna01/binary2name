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

lib_LTLIBRARIES=libHELLO_WORLD_2_0.la

INCLUDES = -I ${top_srcdir}/include

# This is an update... have to be installed in the shared repository
NAMESPACE=hotswap

idfdir=${prefix}/share/edma/idf/${NAMESPACE}
libdir = ${exec_prefix}/lib/edma/${NAMESPACE}


EXTRA_DIST= update_test1.c update_test1.mk \
            update_test2.c update_test2.mk swap_obj.c swap_obj.mk \
            single_object_swap.c single_object_swap.mk \
            compound_object_swap.c compound_object_swap.mk \
            interface_change_swap.c interface_change_swap.mk \
	    HELLO_WORLD_2_0.ines HELLO_WORLD_2_0.idf hello2.py edma_repos.lst

idf_DATA= HELLO_WORLD_2_0.idf

libHELLO_WORLD_2_0_la_SOURCES=HELLO_WORLD_imp.c
libHELLO_WORLD_2_0_la_LDFLAGS=-version-info 2:0



MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in

