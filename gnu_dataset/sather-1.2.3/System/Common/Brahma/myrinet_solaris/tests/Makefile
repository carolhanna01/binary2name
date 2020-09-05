#                                                                      tab:8
#
# "Copyright (c) 1994 The Regents of the University of California.
# All rights reserved.
# 
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose, without fee, and without written agreement is
# hereby granted, provided that the above copyright notice and the following
# two paragraphs appear in all copies of this software.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
#
# Author: 		Richard P. Martin
# Version: 		11
# Creation Date:	Mon Feb  6 12:58:18 PST 1995
# Filename: 		Makefile for lam library
# History:
#


COBJS =  

CC = gcc
AR = ar

GLUNIX_LIB := glunix
GLUNIX_SRC := /usr/now/lib

MYRI_HOME := /u/sather/network/myrinet

#libs if using glunix
#LIBS = -R ${GLUNIX_SRC} -l${GLUNIX_LIB}  -lLAM -lsocket -lnsl -lposix4 -lLanaiDevice -lbfd -liberty 

#libs if not using glunix
LIBS = ../../lib/solaris_at_sparc_myrinet.a -lLanaiDevice -lbfd -liberty -lat -lqt -lschedulers -lkstat -lposix4

INCLUDES = -I../src -I../.. -I$(MYRI_HOME)/include -L../lib -L/usr/ucblib -L${MYRI_HOME}/lib/sparc_solaris -I${SATHER_HOME}/System/Common/ActiveThreads/src -I${SATHER_HOME}/System/Common/ActiveThreads/md/${ARCH} -L${SATHER_HOME}/System/Common/ActiveThreads/lib

ARCH=sparc-solaris
OPT_FLAGS = -DSOLARIS -fexpensive-optimizations  -fschedule-insns2 -O4
#OPT_FLAGS = -xO4 
DEBUG_FLAGS = -g -DDEBUG -DSOLARIS
CFLAGS =  -DBR_MYRINET_SOLARIS_AT ${OPT_FLAGS} -Winline 
#CFLAGS =  -DBR_MYRINET_SOLARIS_AT ${DEBUG_FLAGS} -Winline 

all: ping_pong ping_pong ping_bulk thru_bulk touchlcp

test_xfer: test_xfer.c 
	${CC}  ${CFLAGS} ${INCLUDES} -o test_xfer test_xfer.c $(LIBS) 

ping_pong: ping_pong.c
	${CC}  ${CFLAGS} ${INCLUDES} -o ping_pong ping_pong.c $(LIBS) 

ping_bulk: ping_bulk.c 
	${CC}  ${CFLAGS} ${INCLUDES} -o ping_bulk ping_bulk.c $(LIBS)

thru_bulk: thru_bulk.c
	${CC}  ${CFLAGS} ${INCLUDES} -o thru_bulk thru_bulk.c $(LIBS) 

test_med: test_med.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_med test_med.c $(LIBS) 

touchlcp: touchlcp.c
	${CC}  ${CFLAGS} ${INCLUDES} -o touchlcp touchlcp.c $(LIBS) 

clean:
	rm -f *.o test_xfer ping_pong ping_bulk thru_bulk test_med touchlcp

test_threads: test_threads.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_threads test_threads.c $(LIBS) 

test_short: test_short.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_short test_short.c $(LIBS) 

#test_long: test_long.c
#	${CC}  ${CFLAGS} ${INCLUDES} -o test_long test_long.c $(LIBS) 

test_store: test_store.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_store test_store.c $(LIBS) 

test_get: test_get.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_get test_get.c $(LIBS) 

test_create: test_create.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_create test_create.c $(LIBS) 

test_yield: test_yield.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_yield test_yield.c $(LIBS) 

test_barrier: test_barrier.c
	${CC}  ${CFLAGS} ${INCLUDES} -o test_barrier test_barrier.c $(LIBS) 




