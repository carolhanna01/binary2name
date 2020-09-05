#------------------------------->  Makefile  <--------------------------------#
#- Copyright (C) 199x by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

# Makefile for Active Threads libraries and tests
#CC    =gcc -ansi -Wall -pedantic

#turned off -ansi for assembly
CC    =gcc
AR    =ar
#MAKE  =gmake
VPATH =.:../include

SCHED =../schedulers
MD    =../md
LIB   =../lib
TESTS =../tests

#
# Need to include from the current directory because "qt.h"
# will include <qtmd.h>.
#
# At least -O or higher is needed for inlining to work properly
# in gcc

#To enable inlining, comment out DEBUG_FLAGS
#If you do this, make sure -O or higher is used 
#(gcc fails to do inilining otherwise)
INC_DIR	      =-I../include -I$(MD)/$(ARCH)
#DEBUG_FLAGS   =-g -DAT_NO_INLINE
DEBUG_FLAGS   =-O3
CFLAGS	      =$(INC_DIR) $(DEBUG_FLAGS)

HDRS          =qt.h \
               $(MD)/$(ARCH)/qtmd.h \
               $(MD)/$(ARCH)/utils.h \
               $(MD)/$(ARCH)/lwp.h \
               $(MD)/$(ARCH)/md-atomic.h \
               at.h \
               at-inline.h \
               at-int.h \
               at-lwp.h \
               mutex.h \
               cond.h \
               pool.h \
               p_pool.h \
               sema.h \
               spinlock.h \
               thread.h \
               wrappers.h \
               bundle.h \
               const.h
#               rw.h \
#               barrier.h \

LDFLAGS	 =$(CFLAGS)
LIBS	 =libat.a libqt.a libschedulers.a
LINKER	 =$(CC)
MAKEFILE =Makefile
OBJS     =$(MD)/$(ARCH)/qtmdb.o meas.o


# Temporarily disabled hardware feedback */
#QTOBJS =qt.o $(MD)/$(ARCH)/qtmds.o $(MD)/$(ARCH)/hwperf.o 
QTOBJS =qt.o $(MD)/$(ARCH)/qtmds.o

ATOBJS =at.o mutex.o cond.o pool.o p_pool.o sema.o \
        wrappers.o $(MD)/$(ARCH)/lwp.o thread.o \
#        barrier.o rw.o

all: beware $(LIB)/libqt.a $(LIB)/libat.a $(LIB)/libschedulers.a

beware:		
	@if [ -z "$(ARCH)" ]; then \
	echo "-----------------------------------------------------------"; \
	echo "Please, use one of the following targets:";                   \
	echo "sparc-solaris, i386-solaris, axp-osf, hppa-hpux, mips-irix5"; \
	echo "i386-linux, i386-win32";                                      \
	echo "-----------------------------------------------------------"; \
	exit 1; \
	fi

sparc-solaris:		
	$(MAKE) all ARCH=sparc-solaris DEBUG_FLAGS=$(DEBUG_FLAGS)
	$(MAKE) tests ARCH=sparc-solaris

i386-solaris:	
	$(MAKE) all ARCH=i386-solaris

i386-linux:	
	$(MAKE) all ARCH=i386-linux

i386-win32:	
	$(MAKE) all ARCH=i386-win32

axp-osf:
	$(MAKE) all ARCH=axp-osf

hppa-hpux:
	$(MAKE) all ARCH=hppa-hpux

mips-irix5:		
	$(MAKE) all ARCH=mips-irix5 DEBUG_FLAGS=$(DEBUG_FLAGS)
	$(MAKE) tests ARCH=mips-irix5

$(LIB)/libqt.a:	libdir $(QTOBJS) 
	$(AR) crvs $(LIB)/libqt.a $(QTOBJS)
#	ranlib $(LIB)/libqt.a

clean:
	rm -f *.o 
	rm -f $(MD)/*/*.o
	rm -f $(SCHED)/*.o
	rm -f $(LIB)/libqt.a $(LIB)/libat.a $(LIB)/libschedulers.a
	$(MAKE) -C $(TESTS) clean

QT_H   =qt.h $(QTMD_H)
QTMD_H =$(MD)/$(ARCH)/qtmd.h

###
qtmdb.o: qtmdb.s b.h
meas.o: meas.c /usr/include/stdio.h $(QT_H) b.h at.h
qt.o: qt.c $(QT_H)
at.o: at.c at.h $(QT_H)
#$(MD)/$(ARCH)/qtmds.o: $(MD)/$(ARCH)/qtmds.s
#	$(CC) -c -g $(MD)/$(ARCH)/qtmds.s -o $(MD/$(ARCH)/qtmds.o

$(MD)/$(ARCH)/qtmds.o: $(MD)/$(ARCH)/qtmds.s
	$(MAKE) -C $(MD)/$(ARCH) qtmds.o

$(MD)/$(ARCH)/hwperf.o: 
	$(MAKE) -C $(MD)/$(ARCH) hwperf.o

at.h: at-inline.h wrappers.h mutex.h cond.h sema.h \
#      barrier.h rw.h
at-int.h: qt.h spinlock.h at.h at-inline.h at-lwp.h mutex.h cond.h const.h \
          pool.h p_pool.h $(MD)/$(ARCH)/utils.h thread.h wrappers.h \
#          rw.h 

pool.h:		spinlock.h wrappers.h
p_pool.h:	pool.h spinlock.h wrappers.h
bundle.h:	at-int.h 

at.o:		at-int.h at.c
mutex.o:	at-int.h mutex.c 
cond.o:		at-int.h cond.c 
# rw.o:		at-int.h rw.c 
pool.o:		pool.h pool.c
p_pool.o:	p_pool.h p_pool.c
sema.o:		at-int.h sema.c 
# barrier.o:	at-int.h barrier.c 

#$(MD)/$(ARCH)/lwp.o: $(MD)/$(ARCH)/lwp.h wrappers.h $(MD)/$(ARCH)/lwp.c
#	$(CC) -c $(CFLAGS) $(MD)/$(ARCH)/lwp.c -o $(MD)/$(ARCH)/lwp.o

$(MD)/$(ARCH)/lwp.o: $(MD)/$(ARCH)/lwp.h wrappers.h $(MD)/$(ARCH)/lwp.c
	$(MAKE) -C $(MD)/$(ARCH) lwp.o 
thread.o: at-int.h thread.c
wrappers.o: spinlock.h wrappers.c

$(LIB)/libat.a:	libdir at.o $(MD)/$(ARCH)/lwp.o wrappers.o pool.o p_pool.o thread.o \
                mutex.o cond.o sema.o $(HDRS) \
#               barrier.o rw.o
	$(AR) crvs $(LIB)/libat.a at.o $(MD)/$(ARCH)/lwp.o wrappers.o pool.o p_pool.o \
	                          thread.o mutex.o cond.o sema.o \
#	                          barrier.o rw.o
#	ranlib $(LIB)/libat.a

$(LIB)/libschedulers.a: 
	$(MAKE) -C $(SCHED) ARCH=$(ARCH) DEBUG_FLAGS="$(DEBUG_FLAGS)"

tests:
	$(MAKE) -C $(TESTS) tests ARCH=$(ARCH)

at_merge: all
	$(MAKE) all
	$(MAKE) -C $(TESTS) at_merge ARCH=$(ARCH)

libdir:
	[ -d $(LIB) ] || mkdir $(LIB)
