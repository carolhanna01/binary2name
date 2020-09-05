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

lib_LTLIBRARIES = libPERSON.la libGRADUATE.la libGRADUATE_MIXIN.la libGENERAL_TWO_SUPER.la \
  		  libDOG.la libGAP.la libPERSON_INNER.la libGRADUATE_INNER.la \
		  libEMPLOYER_INNER.la libEMPLOYER.la libGRADUATE_INNER1.la \
		  libBASE_CLASE.la libSC1_LEVEL1.la libSC2_LEVEL1.la libFINAL.la

INCLUDES = -I ${top_srcdir}/include

REPO_NAME=edma_test
NAMESPACE = inheritance
idfdir=${prefix}/share/${REPO_NAME}/idf/${NAMESPACE}
libdir = ${prefix}/share/${REPO_NAME}/lib/${NAMESPACE}

EXTRA_DIST = PERSON.idf  GRADUATE.idf GRADUATE_MIXIN.idf GENERAL_TWO_SUPER.idf DOG.idf\
	     GAP.idf PERSON_INNER.idf GRADUATE_INNER.idf EMPLOYER_INNER.idf EMPLOYER.idf\
	     PERSON.ines  GRADUATE.ines GRADUATE_MIXIN.ines GENERAL_TWO_SUPER.ines DOG.ines\
	     GAP.ines  PERSON_INNER.ines GRADUATE_INNER.ines EMPLOYER_INNER.ines EMPLOYER.ines\
	     SQUIZO.idf SQUIZO.ines GRADUATE_INNER1.ines GRADUATE_INNER1.idf\
	     BASE_CLASE.idf BASE_CLASE.ines SC1_LEVEL1.idf SC1_LEVEL1.ines SC2_LEVEL1.idf SC2_LEVEL1.ines \
	     FINAL.idf FINAL.ines  diamond_test.c diamond_test.mk \
	     inh_test.c inh_test.mk inner_test.c inner_test.mk inner_test1.c inner_test1.mk edma_repos.lst

idf_DATA = PERSON.idf GRADUATE.idf GENERAL_TWO_SUPER.idf GRADUATE_MIXIN.idf DOG.idf \
		GAP.idf PERSON_INNER.idf GRADUATE_INNER.idf EMPLOYER_INNER.idf EMPLOYER.idf \
		SQUIZO.idf GRADUATE_INNER1.idf BASE_CLASE.idf SC1_LEVEL1.idf SC2_LEVEL1.idf FINAL.idf 



libPERSON_la_SOURCES = PERSON_imp.c
libPERSON_INNER_la_SOURCES = PERSON_INNER_imp.c
libDOG_la_SOURCES = DOG_imp.c
libGAP_la_SOURCES = GAP_imp.c
libGRADUATE_la_SOURCES = GRADUATE_imp.c
libGRADUATE_INNER_la_SOURCES = GRADUATE_INNER_imp.c
libGRADUATE_INNER1_la_SOURCES = GRADUATE_INNER1_imp.c
libEMPLOYER_INNER_la_SOURCES = EMPLOYER_INNER_imp.c
libEMPLOYER_la_SOURCES = EMPLOYER_imp.c
libGRADUATE_MIXIN_la_SOURCES = GRADUATE_MIXIN_imp.c
libGENERAL_TWO_SUPER_la_SOURCES = GENERAL_TWO_SUPER_imp.c

libBASE_CLASE_la_SOURCES = BASE_CLASE_imp.c
libSC1_LEVEL1_la_SOURCES = SC1_LEVEL1_imp.c
libSC2_LEVEL1_la_SOURCES = SC2_LEVEL1_imp.c
libFINAL_la_SOURCES = FINAL_imp.c

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in

