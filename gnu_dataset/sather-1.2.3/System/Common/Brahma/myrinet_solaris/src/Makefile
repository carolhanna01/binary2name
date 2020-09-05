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

# Make sure MYRI_HOME is set!

#ICSI Myrinet home directory
MYRI_HOME = /u/sather/network/myrinet
#MYRI_HOME = /disks/barad-dur/now/CAL/myricom/myrinet.2.2a
#MYRI_HOME =  /disks/barad-dur/now/CAL/myricom/myrinet.3.04f-gam
#MYRI4_HOME = /disks/barad-dur/now/share/src/AM/am2-myricom-304f

LIB_DIR    = ../../library

#LANai3 compiler home directory
MYRI4_HOME = /u/sather/network/myricom-308

#MYRI4_HOME = /disks/barad-dur/now/share/src/AM/am2-myricom-304j

SRC_FILTER= l4lcp.c lcp.c l4lcp.c

#needed because I am too lazy to change this Makefile. 
# used to set GAM type, e.g. TCPAM, PAM etc. 
MPL = LAM

BIN = ../bin
LIB = ../lib
DEP = Make.depend

CC = gcc
# C flags if using glunix
# The flags: 
# USE_LANAI4: A Must 
# SOLARIS: A Must 
# USE_GLUNIX: Optional if you want to use glunix
# DEBUG: Optional. Highly recommended! 
# 
#CFLAGS = -DUSE_LANAI4 -DSOLARIS -DDEBUG -g -O4 -fschedule-insns2  -fexpensive-optimizations  -DUSE_GLUNIX -I. 

#cflags if not using glunix
#CFLAGS = -DUSE_LANAI4 -DSOLARIS -D_REENTRANT -DBR_MYRINET_SOLARIS -O4 -fschedule-insns2 -fexpensive-optimizations -I. 

#No optimizations for debugging
CFLAGS = $(THREAD_FLAGS) -DUSE_LANAI4 -DDEBUG -g -I. -Winline -I${MYRI_HOME}/include -I../.. 

#Use optimizations 
#CFLAGS = $(THREAD_FLAGS) -DUSE_LANAI4 -O4 -fschedule-insns2 -fexpensive-optimizations  -I. -I${MYRI_HOME}/include -I../.. 


# source/object files, libs, and includes
#
.SUFFIXES : .c .o .lan_o .lan_s .dat

#Magic. Do not mess with this unless you know what you're doing. 


#LIBS = ${LIB}/lib${MPL}.a ${LIB}/libAM1-5.a 
LIBS = ${LIB}/lib${MPL}.a 
INCS	= ${filter-out %_int.h, ${wildcard *.h}}
INSTALL_INCS = ${filter-out ${INCS_FILTER}, ${wildcard *.h}}

#COMMON_OBJS =  ${BIN}/am_glib.o  ${BIN}/lam_common.o 
COMMON_OBJS =  ${BIN}/lam_common.o 
LAM_OBJS =  ${BIN}/lam.o  ${BIN}/lam_barrier.o 
SYNC_OBJS = 
#AM15_OBJS =  ${BIN}/am1-5.o 

.PHONY: default clean clear depend  distclean

# Lanai stuff 

LANAI_HDRS = ep.h

LANAI4_INC = -I${MYRI4_HOME}/include 
CCLAN3     = /u/sather/network/myricom-308/bin/lanai3-gcc 


default: ${BIN} depend ${LIBS} 

# Do not change the -O0 flag unless you know what you're doing. 
l4lcp:  ep.h l4lcp.c 
	${CCLAN3} -m4.1 -O0 -DLANAI4 -DDEBUG ${LANAI4_INC} -o l4lcp l4lcp.c 

#l4lcp:  ep.h l4lcp.c 
#	${CCLAN3} -m4.1 -O2 -DLANAI4 ${LANAI4_INC} -o l4lcp l4lcp.c 

#l4lcp for use of big-contexts (timed, g is same, L slower. )
#l4lcp:  ep.h l4lcp.c 
#	${CCLAN3} -m4.1 -mbig-contexts -O2 -DBIG_CONTEXTS -DLANAI4 ${LANAI4_INC} -o l4lcp l4lcp.c lcp_utils.s

clean: 
	rm -f ${BIN}/*.o ${LIB}/*.a l4lcp

clear distclean:
	rm -rf ${BIN} ${LIB}

# dependencies
#
${LIBS}: ${OBJ}

# rules for the bin directory
${BIN}:
	mkdir $@

# rules for the library directory
${LIB}:
	mkdir $@

# rules for the library
#${LIB}/%.a:
#	ar cru $@ $^

${LIB}/lib${MPL}.a: ${COMMON_OBJS} ${LAM_OBJS}
	ar cru $@ ${COMMON_OBJS} ${LAM_OBJS}

#${LIB}/libAM1-5.a:  ${COMMON_OBJS} ${AM15_OBJS}
#	ar cru $@ ${COMMON_OBJS} ${AM15_OBJS}


#may or may not work. 
install:
	cp ${LIBS} ${SC_ROOT}/${MPL}/lib


INSTALL = ../lib/libLAM.a
MYRI_BIN_DIR = ../bin
MYRI_SRC = lam_common.c lam.c lam_barrier.c myrinet_solaris.c

${MYRI_BIN_DIR}/%.o: %.c 
	${CC} ${CFLAGS} -o $@ -c $<
${MYRI_BIN_DIR}/%.o: %.s
	${CC} ${CFLAGS}  -o $@ -c $<

solaris_sparc: $(MYRI_BIN_DIR)/lam_common.o $(MYRI_BIN_DIR)/lam.o $(MYRI_BIN_DIR)/lam_threads.o $(MYRI_BIN_DIR)/lam_barrier.o $(MYRI_BIN_DIR)/brahma.o
	ar rus ../../$(INSTALL) $(MYRI_BIN_DIR)/lam_common.o \
			  $(MYRI_BIN_DIR)/lam.o \
			  $(MYRI_BIN_DIR)/lam_threads.o \
			  $(MYRI_BIN_DIR)/lam_barrier.o \
			  $(MYRI_BIN_DIR)/brahma.o 
	rm $(MYRI_BIN_DIR)/*.o

solaris_at_sparc: $(MYRI_BIN_DIR)/lam_common.o $(MYRI_BIN_DIR)/lam.o $(MYRI_BIN_DIR)/at_threads.o $(MYRI_BIN_DIR)/lam_barrier.o $(MYRI_BIN_DIR)/brahma.o
	ar rus ../../$(INSTALL) $(MYRI_BIN_DIR)/lam_common.o \
			  $(MYRI_BIN_DIR)/lam.o \
			  $(MYRI_BIN_DIR)/at_threads.o \
			  $(MYRI_BIN_DIR)/lam_barrier.o \
			  $(MYRI_BIN_DIR)/brahma.o 
	rm $(MYRI_BIN_DIR)/*.o	

#to make pmake happy...
$(MYRI_BIN_DIR)/lam_common.o:  lam_common.c
	$(CC) -c $(CFLAGS) lam_common.c -o $(MYRI_BIN_DIR)/lam_common.o
$(MYRI_BIN_DIR)/lam.o:  lam.c
	$(CC) -c $(CFLAGS) lam.c -o $(MYRI_BIN_DIR)/lam.o
$(MYRI_BIN_DIR)/lam_threads.o:  lam_threads.c
	$(CC) -c $(CFLAGS) lam_threads.c -o $(MYRI_BIN_DIR)/lam_threads.o
$(MYRI_BIN_DIR)/at_threads.o:  at_threads.c
	$(CC) -c $(CFLAGS) at_threads.c -o $(MYRI_BIN_DIR)/at_threads.o
$(MYRI_BIN_DIR)/lam_barrier.o:  lam_barrier.c
	$(CC) -c $(CFLAGS) lam_barrier.c -o $(MYRI_BIN_DIR)/lam_barrier.o
$(MYRI_BIN_DIR)/brahma.o: ../../brahma.c
	gcc -c $(CFLAGS) $(INC_FLAGS) ../../brahma.c -o $(MYRI_BIN_DIR)/brahma.o

			  



