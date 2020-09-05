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

lib_LTLIBRARIES = libEDMA.la
bin_PROGRAMS=sally

sally_SOURCES=sally.c


AM_CFLAGS=-fPIC -DGCC -DLINUX -D_GNU_SOURCE -g -Wall 
include_HEADERS= portable.h ../include/edma.h

#I think this should be specified in other way
EXTRA_DIST = clas.h classq.h classrt.h const.h anchor_points.h\
	dynlink.h dict_type.h emi.h error.h  hotswap.h \
	esp_typ.h helper.h idf.h inh1.h inh.h\
	ini1.h iniman.h linuxdl.h locators.h misc.h \
	multiidf.h noti.h obj.h objq.h clone_obj.h cast_obj.h\
	poli.h portable.h \
	pri1.h pri3.h pri3x.h procmap.h \
	ptypes.h shmem.h siu.h spri3.h \
	sys31.h systyp.h tclass.h tobj.h ttypes.h \
	repo.h class_mng.h \
	vglobal.h vserv.h \
	subsystems.h \
	linuxth.h ethread.h \
	sclass_ex.h sclasses.h \
	win32dis.h win32dl.h win32init.h win32mem.h


libEDMA_la_LDFLAGS = -version-info 0:18:0

libEDMA_la_LIBADD = -lpthread

libEDMA_la_SOURCES = anchor_points.c \
			clas.c \
			classq.c \
			classrt.c \
			emi.c \
			error.c \
	    		helper.c \
			hotswap.c \
			idf.c \
			inh1.c \
			inh.c \
			ini1.c \
			iniman.c \
			linuxdis.c \
			linuxdl.c \
			linuxinit.c \
			linuxmem.c \
			linuxth.c \
			ethread.c \
			locators.c \
			misc.c \
			multiidf.c \
			obj.c \
			cast_obj.c \
			clone_obj.c \
			objq.c \
			poli.c \
			pri1.c \
			pri3.c \
			pri3x.c \
			siu.c \
			spri3.c \
			subsystems.c \
			repo.c \
			class_mng.c \
			sclasses.c sclass_ex.c \
			sys31.c 

MAINTAINERCLEANFILES =  Makefile.in config.h.in stamp-h.in

