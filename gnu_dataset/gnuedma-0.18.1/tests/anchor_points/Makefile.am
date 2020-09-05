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

lib_LTLIBRARIES = libAP_TEST.la libAP_TEST_SUBCLASS.la libAP_TEST_SUBCLASS1.la


INCLUDES = -I ${top_srcdir}/include

REPO_NAME=edma_test
NAMESPACE=anchor_points
idfdir=${prefix}/share/${REPO_NAME}/idf/${NAMESPACE}
libdir = ${exec_prefix}/share/${REPO_NAME}/lib/${NAMESPACE}

EXTRA_DIST= AP_TEST.idf AP_TEST.ines ap_test.c ap_test.mk ap_remove.c ap_remove.mk \
	ap_overwrite.c ap_overwrite.mk AP_TEST_SUBCLASS.idf AP_TEST_SUBCLASS.ines \
	AP_TEST_SUBCLASS1.idf AP_TEST_SUBCLASS1.ines full_test.mk full_test.c \
	README edma_repos.lst

idf_DATA= AP_TEST.idf AP_TEST_SUBCLASS.idf AP_TEST_SUBCLASS1.idf


libAP_TEST_la_SOURCES = AP_TEST_imp.c
libAP_TEST_SUBCLASS_la_SOURCES = AP_TEST_SUBCLASS_imp.c
libAP_TEST_SUBCLASS1_la_SOURCES = AP_TEST_SUBCLASS1_imp.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in

