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

# Platforms:
#
#   serial		 - No concurrency
#   smp_solaris          - Threads, emulated clusters (solaris)
#   at_solaris_smp       - Threads, emulated clusters (Active Threads based)
#   tcpip                - TCP/IP AM (solaris)
#   myrinet              - Myrinet AM (solaris)

#   lwp_linux            - Threads, emulated clusters (linux/lwp)
#   smp_lwp              - Threads, emulated clusters (linux/lwp)
#   shmem                - Using shmem AM, for debugging (solaris)
#   meiko                - Meiko AM (solaris)
#
# The Brahma archive file may be created for a given platform "foo" like this:
#
#   make foo INSTALL=xxx.a

#These are used in all platforms
COMMON_DIR = solaris_common
LIB_DIR = library
BENCHMARK_DIR = benchmarks
TEST_DIR = tests
# set to -g for debugging
DEBUG_FLAG = 

###################################

all:
	@echo "There is no default make rule: read the Brahma Makefile."

beware:
	@if [ -z "$(INSTALL)" ]; then \
	   echo "You forgot to set the INSTALL location."; \
	   exit 1; \
	fi

###################################

SERIAL_SRC_DIR = serial
serial: beware $(SERIAL_SRC_DIR)/serial.c \
	       $(SERIAL_SRC_DIR)/serial.h
	gcc -O3 -DBR_SERIAL -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o

LWP_LINUX_SRC_DIR = lwp_linux
LWP_DIR = ../../pSather/lwp
lwp_linux: beware $(LWP_LINUX_SRC_DIR)/lwp_linux.c \
		  $(LWP_LINUX_SRC_DIR)/lwp_linux.h
	gcc -O3 -DBR_LWP_LINUX -I$(LWP_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o

SMP_SRC_DIR = smp_solaris
SMP_INC_DIR = -I. -I$(SMP_SRC_DIR) -I$(COMMON_DIR)
smp_solaris: beware $(SMP_SRC_DIR)/smp_solaris.c \
		    $(SMP_SRC_DIR)/smp_solaris.h \
		    $(COMMON_DIR)/solaris_sync.c \
		    $(COMMON_DIR)/solaris_sync.h \
		    $(COMMON_DIR)/solaris_types.h
	gcc $(DEBUG_FLAG) -O3 -DBR_SMP_SOLARIS $(SMP_INC_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o 

AT_SOL_SMP_SRC_DIR	= at_solaris_smp
AT_COMMON_DIR		= $(SATHER_HOME)/System/Common/Brahma/at_common  
AT_DIR			= $(SATHER_HOME)/System/Common/ActiveThreads
AT_SRC_DIR		= $(AT_DIR)/src
AT_MD_DIR		= $(AT_DIR)/md/$(ARCH)
AT_COMMON_INC_DIR	= -I$(AT_COMMON_DIR) -I$(AT_DIR)/include -I$(AT_MD_DIR)
AT_SMP_INC_DIR		= -I. -I$(AT_SOL_SMP_SRC_DIR) $(AT_COMMON_INC_DIR)
AT_LIB_DIR         = -L$(AT_DIR)/lib
AT_LIBS            = -lqt -lat -lschedulers
# turn off opt
at_solaris_smp: beware  ${AT_SOL_SMP_SRC_DIR}/at_solaris_smp.c \
			${AT_COMMON_DIR}/at_sync.c \
			${AT_SOL_SMP_SRC_DIR}/at_solaris_smp.h  \
			${AT_COMMON_DIR}/at_sync.h \
			${AT_COMMON_DIR}/at_types.h
	gcc $(DEBUG_FLAG) -O3 -DBR_AT_SOLARIS_SMP $(AT_SMP_INC_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o 

AT_LINUX_SMP_SRC_DIR   = at_linux_smp
AT_LINUX_SMP_INC_DIR   = -I. -I$(AT_LINUX_SMP_SRC_DIR) $(AT_COMMON_INC_DIR)
at_linux_smp: beware  ${AT_LINUX_SMP_SRC_DIR}/at_linux_smp.c \
                      ${AT_COMMON_DIR}/at_sync.c \
                      ${AT_LINUX_SMP_SRC_DIR}/at_linux_smp.h  \
                      ${AT_COMMON_DIR}/at_sync.h \
                      ${AT_COMMON_DIR}/at_types.h
	gcc $(DEBUG_FLAG) -O3 -DBR_AT_LINUX_SMP $(AT_LINUX_SMP_INC_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o

AT_WIN32_SMP_SRC_DIR   = at_win32_smp
AT_WIN32_SMP_INC_DIR   = -I. -I$(AT_WIN32_SMP_SRC_DIR) $(AT_COMMON_INC_DIR)
at_win32_smp: beware  ${AT_WIN32_SMP_SRC_DIR}/at_win32_smp.c \
                      ${AT_COMMON_DIR}/at_sync.c \
                      ${AT_WIN32_SMP_SRC_DIR}/at_win32_smp.h  \
                      ${AT_COMMON_DIR}/at_sync.h \
                      ${AT_COMMON_DIR}/at_types.h
	gcc $(DEBUG_FLAG) -O3 -DBR_AT_WIN32_SMP $(AT_WIN32_SMP_INC_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o
 
#these are simply included
${AT_COMMON_DIR}/at_sync.c:
${AT_SOL_SMP_SRC_DIR}/at_solaris_smp.c:
${AT_COMMON_DIR}/at_sync.h:
${AT_COMMON_DIR}/at_types.h:

OSF_AT_AXP_SMP_SRC_DIR	= osf_at_axp_smp
OSF_AT_AXP_SMP_INC_DIR	= -I. -I$(OSF_AT_AXP_SMP_SRC_DIR) $(AT_COMMON_INC_DIR)
osf_at_axp_smp: beware  ${OSF_AT_AXP_SMP_SRC_DIR}/osf_at_axp_smp.c \
			${AT_COMMON_DIR}/at_sync.c \
			${OSF_AT_AXP_SMP_SRC_DIR}/osf_at_axp_smp.h  \
			${AT_COMMON_DIR}/at_sync.h \
			$(AT_COMMON_DIR)/at_types.h
	gcc $(DEBUG_FLAG) -O3 -DBR_OSF_AT_AXP_SMP $(AT_SMP_INC_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o 

#these are simply included
${OSF_AT_AXP_SMP_SRC_DIR}/osf_at_axp_smp.c:

HPUX_AT_HPPA_SMP_SRC_DIR = hpux_at_hppa_smp
HPUX_AT_HPPA_SMP_INC_DIR = -I. -I$(HPUX_AT_HPPA_SMP_SRC_DIR) $(AT_COMMON_INC_DIR)
hpux_at_hppa_smp: beware  ${HPUX_AT_HPPA_SMP_SRC_DIR}/hpux_at_hppa_smp.c \
			${AT_COMMON_DIR}/at_sync.c \
			${HPUX_AT_HPPA_SMP_SRC_DIR}/hpux_at_hppa_smp.h  \
			${AT_COMMON_DIR}/at_sync.h \
			$(AT_COMMON_DIR)/at_types.h
	gcc $(DEBUG_FLAG) -O3 -DBR_HPUX_AT_HPPA_SMP $(AT_SMP_INC_DIR) -c brahma.c
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) brahma.o 

#these are simply included
${HPUX_AT_HPPA_SMP_SRC_DIR}/hpux_at_hppa_smp.c:

TCP_FLAGS = -Wall -DTCP -DQQ_THREADS -DSOLARIS_THREADS -D_REENTRANT -DBR_TCP_SOLARIS $(DEBUG_FLAG) 
TCP_SRC_DIR = tcp_solaris
TCP_INC_DIR = -I. -I$(TCP_SRC_DIR) -I$(COMMON_DIR)
tcp_solaris: beware $(TCP_SRC_DIR)/tcp_solaris.c \
		    $(TCP_SRC_DIR)/tcp_solaris.h \
		    $(COMMON_DIR)/solaris_sync.c \
		    $(COMMON_DIR)/solaris_sync.h \
		    $(COMMON_DIR)/solaris_types.h \
		    $(LIB_DIR)/br_barrier.c \
		    $(TCP_SRC_DIR)/br_tcpam.c \
		    $(TCP_SRC_DIR)/br_tcpam.h
	gcc $(DEBUG_FLAG) -DBR_TCP_SOLARIS $(TCP_INC_DIR) -c brahma.c -o $(TCP_SRC_DIR)/brahma.o
	gcc $(DEBUG_FLAG) -DBR_TCP_SOLARIS $(TCP_INC_DIR) -c $(LIB_DIR)/br_barrier.c -o $(TCP_SRC_DIR)/br_barrier.o
	gcc -c $(TCP_SRC_DIR)/br_tcpam.c -I. $(TCP_INC_DIR) -Itcp_solaris $(TCP_FLAGS) -o $(TCP_SRC_DIR)/br_tcpam.o
	@echo Creating library $(INSTALL)...
	ar rus $(INSTALL) $(TCP_SRC_DIR)/brahma.o \
			  $(TCP_SRC_DIR)/br_barrier.o \
			  $(TCP_SRC_DIR)/br_tcpam.o 

MYRI_SRC_DIR = myrinet_solaris/src
MYRI_BIN_DIR = myrinet_solaris/bin
MYRI_LIB_DIR = lib
MYRI_HOME = /u/sather/network/myrinet
MYRI_INC_DIRS = -I. -I$(MYRI_SRC_DIR) -I$(MYRI_HOME)/include
MYRI_LIBS = -lLanaiDevice -lbfd -liberty
MYRI_LIB_DIRS = -L$(MYRI_LIB_DIR) -L/usr/ucblib -L${MYRI_HOME}/lib/sparc_solaris
MYRI_FLAGS = -O2 -D_REENTRANT $(DEBUG_FLAG)

solaris_myrinet: beware
	$(MAKE) -C $(MYRI_SRC_DIR) solaris_sparc INSTALL=$(INSTALL) THREAD_FLAGS="-DSOLARIS -DSOLARIS_THREADS -DBR_MYRINET_SOLARIS -D_REENTRANT"

# need at least -O for inilining to work with gcc */
solaris_at_sparc_myrinet: beware
#	$(MAKE) -C $(MYRI_SRC_DIR) solaris_at_sparc INSTALL=$(INSTALL) THREAD_FLAGS="-g -O4 -DSOLARIS -DAT_NO_INLINE -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(AT_COMMON_INC_DIR) "
	$(MAKE) -C $(MYRI_SRC_DIR) solaris_at_sparc INSTALL=$(INSTALL) THREAD_FLAGS="-O4 -DSOLARIS -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(AT_COMMON_INC_DIR) "

smp_lwp:
	@echo The smp_lwp Brahma platform is not yet implemented.

shmem:
	@echo The shmem Brahma platform is not yet implemented.

meiko:
	@echo The meiko Brahma platform is not yet implemented.


###################################

smp_solaris.a: $(SMP_SRC_DIR)/smp_solaris.c \
	       $(SMP_SRC_DIR)/smp_solaris.h \
	       $(COMMON_DIR)/solaris_sync.c \
	       $(COMMON_DIR)/solaris_sync.h \
	       $(COMMON_DIR)/solaris_types.h
	$(MAKE) smp_solaris INSTALL=lib/smp_solaris.a

at_solaris_smp.a: $(AT_SOL_SMP_SRC_DIR)/at_solaris_smp.c \
		  $(AT_SOL_SMP_SRC_DIR)/at_solaris_smp.h \
		  $(AT_COMMON_DIR)/at_sync.c \
		  $(AT_COMMON_DIR)/at_sync.h \
		  $(AT_COMMON_DIR)/at_types.h
	$(MAKE) at_solaris_smp INSTALL=lib/at_solaris_smp.a

at_linux_smp.a: $(AT_LINUX_SMP_SRC_DIR)/at_linux_smp.c \
		$(AT_LINUX_SMP_SRC_DIR)/at_linux_smp.h \
		$(AT_COMMON_DIR)/at_sync.c \
		$(AT_COMMON_DIR)/at_sync.h \
		$(AT_COMMON_DIR)/at_types.h
	$(MAKE) at_linux_smp INSTALL=lib/at_linux_smp.a

at_win32_smp.a: $(AT_WIN32_SMP_SRC_DIR)/at_win32_smp.c \
		$(AT_WIN32_SMP_SRC_DIR)/at_win32_smp.h \
		$(AT_COMMON_DIR)/at_sync.c \
		$(AT_COMMON_DIR)/at_sync.h \
		$(AT_COMMON_DIR)/at_types.h
	$(MAKE) at_win32_smp INSTALL=lib/at_win32_smp.a

osf_at_axp_smp.a: $(OSF_AT_AXP_SMP_SRC_DIR)/osf_at_axp_smp.c \
		  $(AT_SOL_SMP_SRC_DIR)/osf_at_axp_smp.h \
		  $(AT_COMMON_DIR)/at_sync.c \
		  $(AT_COMMON_DIR)/at_sync.h \
		  $(AT_COMMON_DIR)/at_types.h
	$(MAKE) osf_at_axp_smp INSTALL=lib/osf_at_axp_smp.a

hpux_at_hppa_smp.a: $(HPUX_AT_HPPA_SMP_SRC_DIR)/hpux_at_hppa_smp.c \
		    $(AT_SOL_SMP_SRC_DIR)/hpux_at_hppa_smp.h \
		    $(AT_COMMON_DIR)/at_sync.c \
		    $(AT_COMMON_DIR)/at_sync.h \
		    $(AT_COMMON_DIR)/at_types.h
	$(MAKE) hpux_at_hppa_smp INSTALL=lib/hpux_at_hppa_smp.a

tcp_solaris.a: $(TCP_SRC_DIR)/tcp_solaris.c \
	       $(TCP_SRC_DIR)/tcp_solaris.h \
	       $(LIB_DIR)/br_barrier.c \
	       $(COMMON_DIR)/solaris_sync.c \
	       $(COMMON_DIR)/solaris_sync.h \
	       $(COMMON_DIR)/solaris_types.h \
	       $(TCP_SRC_DIR)/br_tcpam.c \
	       $(TCP_SRC_DIR)/br_tcpam.h
	$(MAKE) tcp_solaris INSTALL=lib/tcp_solaris.a

solaris_myrinet.a:
	$(MAKE) solaris_myrinet INSTALL=lib/solaris_myrinet.a

solaris_at_sparc_myrinet.a:
	$(MAKE) solaris_at_sparc_myrinet INSTALL=lib/solaris_at_sparc_myrinet.a ARCH=sparc-solaris

###################################

# Don't use -O3 to avoid interprocedual opts...
benchmark_smp: smp_solaris.a $(BENCHMARK_DIR)/benchmark.c 
	gcc -O2 -DBR_SMP_SOLARIS -I. $(BENCHMARK_DIR)/benchmark.c lib/smp_solaris.a -o benchmark_smp

# Don't use -O3 to avoid interprocedual opts...
benchmark_at_solaris_smp: at_solaris_smp.a $(BENCHMARK_DIR)/benchmark.c 
	gcc $(DEBUG_FLAG) -g -DBR_AT_SOLARIS_SMP $(AT_SMP_INC_DIR) $(BENCHMARK_DIR)/benchmark.c lib/at_solaris_smp.a -o benchmark_at_solaris_smp $(AT_LIB_DIR) $(AT_LIBS)

benchmark_tcp: tcp_solaris.a $(BENCHMARK_DIR)/benchmark.c 
	gcc -O2 -DBR_TCP_SOLARIS -I. $(BENCHMARK_DIR)/benchmark.c tcp_solaris.a -o lib/benchmark_tcp

benchmark_myrinet: solaris_myrinet.a $(BENCHMARK_DIR)/benchmark.c 
	gcc $(MYRI_FLAGS) -DSOLARIS_THREADS -DBR_MYRINET_SOLARIS $(MYRI_INC_DIRS) -I$(COMMON_DIR) $(BENCHMARK_DIR)/benchmark.c -o benchmark_myrinet $(MYRI_LIB_DIRS) lib/solaris_myrinet.a $(MYRI_LIBS) 

benchmark_at_sparc_myrinet: solaris_at_sparc_myrinet.a $(BENCHMARK_DIR)/benchmark.c 
	gcc -g -DAT_NO_INLINE -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) $(BENCHMARK_DIR)/benchmark.c -o benchmark_at_sparc_myrinet $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat -lposix4 

###################################

test_threads_at: myrinet_solaris/tests/test_threads.c
	gcc -O $(DEBUG_FLAG) -DDEBUG -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/test_threads.c -o test_threads_at  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat -lposix4 

test_threads: myrinet_solaris/tests/test_threads.c
	gcc -O $(DEBUG_FLAG) -DDEBUG -DSOLARIS_THREADS -DBR_MYRINET_SOLARIS $(MYRI_INC_DIRS) myrinet_solaris/tests/test_threads.c -o test_threads  $(MYRI_LIB_DIRS) lib/solaris_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) 

test_short_at: myrinet_solaris/tests/test_short.c
	gcc -O $(DEBUG_FLAG) -DDEBUG -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/test_short.c -o test_short_at  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat 

test_migration: myrinet_solaris/tests/test_migration.c myrinet_solaris/src/migration.c
	gcc $(DEBUG_FLAG) -g -DAT_NO_INLINE -DDEBUG -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/test_migration.c myrinet_solaris/src/migration.c -o test_migration  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat 
#	gcc $(DEBUG_FLAG) -O4 -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/test_migration.c myrinet_solaris/src/migration.c -o test_migration  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat 

qsort: myrinet_solaris/tests/qsort.c myrinet_solaris/src/migration.c
#	gcc $(DEBUG_FLAG) -g -O -DAT_NO_INLINE -DDEBUG -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/qsort.c myrinet_solaris/src/migration.c -o qsort  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat 
	gcc $(DEBUG_FLAG) -O4 -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/qsort.c myrinet_solaris/src/migration.c -o qsort  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat 

test_store: myrinet_solaris/tests/test_store.c 
	gcc -O4 $(DEBUG_FLAG) -DAT_THREADS -DBR_MYRINET_SOLARIS_AT $(MYRI_INC_DIRS) $(AT_COMMON_INC_DIR) myrinet_solaris/tests/test_store.c myrinet_solaris/src/migration.c  -o test_store  $(MYRI_LIB_DIRS) lib/solaris_at_sparc_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) -lkstat 

test_short: myrinet_solaris/tests/test_short.c
	gcc -O $(DEBUG_FLAG) -DDEBUG -DSOLARIS_THREADS -DBR_MYRINET_SOLARIS $(MYRI_INC_DIRS) myrinet_solaris/tests/test_short.c -o test_short  $(MYRI_LIB_DIRS) lib/solaris_myrinet.a $(MYRI_LIBS) $(AT_LIB_DIR) $(AT_LIBS) 

###################################

t1:	t1.c brahma.h tcp_solaris.a
	gcc $(DEBUG_FLAG) -c -DBR_TCP_SOLARIS t1.c
	gcc $(DEBUG_FLAG) t1.o tcp_solaris.a -o t1

t2:	t2.c br_barrier.c brahma.h tcp_solaris.a
	gcc -c -DBR_TCP_SOLARIS t2.c
	gcc $(DEBUG_FLAG) br_barrier.o t2.o tcp_solaris.a -o t2

###################################

clean:
	rm -f *.a *.o benchmark_*
	rm -f $(SERIAL_DIR)/*.o $(TCP_SRC_DIR)/*.o $(SMP_SRC_DIR)/*.o 
	rm -f $(MYRI_BIN_DIR)/*.o $(AT_SOL_SMP_SRC_DIR)/*.o

###################################

btest: smp_solaris.a $(TEST_DIR)/btest.c 
	gcc -O2 -DBR_SMP_SOLARIS -I. $(TEST_DIR)/btest.c smp_solaris.a -o btest

###################################

debug: 
	gmake solaris_at_sparc_myrinet.a  
	gmake test_migration ARCH=sparc-solaris
