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
#   serial_boehm         - Built using Brahma "serial"; uses Boehm collector
#   serial_solaris       - Built using Brahma "serial"; ultrasparc/solaris
#   smp_solaris          - Built using Brahma "smp_solaris"; ultrasparc/solaris
#
# The Siva archive file may be created for a given platform "foo" like this:
#
#   make foo INSTALL=xxx.a

# The location of the Brahma directory
BR=../Brahma

all:
	@echo There is no default make rule: read the Siva Makefile.

beware:
	@if [ -z "$(INSTALL)" ]; then \
	   echo "You forgot to set the INSTALL location."; \
	   exit 1; \
	fi

$(BR)/lib/serial.a:
	@echo Making Brahma library "serial"...
	cd $(BR); $(MAKE) serial INSTALL=lib/serial.a

#This is to get around `ar's inability to repack archive files
BOEHM_OBJ= allchblk.o alloc.o blacklst.o checksums.o dbg_mlc.o dyn_load.o finalize.o headers.o mach_dep.o malloc.o mark.o mark_rts.o misc.o new_hblk.o obj_map.o os_dep.o ptr_chck.o reclaim.o solaris_threads.o stubborn.o typd_mlc.o
SERIAL_BR_OBJ= brahma.o

serial_boehm: beware serial_boehm.c serial_boehm.h $(BR)/lib/serial.a
# $(GC_A)
	gcc -O3 -DBR_SERIAL -I$(BR) -DSI_SERIAL_BOEHM -c siva.c
	@echo Creating library $(INSTALL)...
	ar x $(GC_A)
	ar x $(BR)/lib/serial.a
	ar rus $(INSTALL) siva.o $(BOEHM_OBJ) $(SERIAL_BR_OBJ)
	rm -f $(BOEHM_OBJ) $(SERIAL_BR_OBJ)

#This is to get around `ar's inability to repack archive files
SERIAL_SOLARIS_OBJ= brahma.o

serial_solaris: beware serial_solaris.c serial_solaris.h zones.c zones.h $(BR)/lib/serial.a
	gcc -O3 -DBR_SERIAL -I$(BR) -DSI_SERIAL_SOLARIS -c siva.c
	@echo Creating library $(INSTALL)...
	ar x $(BR)/lib/serial.a
	ar rus $(INSTALL) siva.o $(SERIAL_SOLARIS_OBJ)
	rm -f $(SERIAL_SOLARIS_OBJ)

serial_solaris_hyper: beware serial_solaris_hyper.c serial_solaris_hyper.h zones.c zones.h $(BR)/lib/serial.a
	gcc -O3 -DBR_SERIAL -I$(BR) -DSI_SERIAL_SOLARIS_HYPER -c siva.c
	@echo Creating library $(INSTALL)...
	ar x $(BR)/lib/serial.a
	ar rus $(INSTALL) siva.o $(SERIAL_SOLARIS_OBJ)
	rm -f $(SERIAL_SOLARIS_OBJ)

#This is to get around `ar's inability to repack archive files
# Commented the .so files out as they tend to migrate from version to version
# (In particular, this depends on the correct setting of LD_LIBRARY_PATH)
# and no longer compiles with ICSI's current environment
# Boris Wed Oct 15 22:42:20 PDT 1997
#SMP_SOLARIS_OBJ= brahma.o libthread.so libkstat.so
SMP_SOLARIS_OBJ= brahma.o

$(BR)/lib/smp_solaris.a:
	@echo Making Brahma library "smp_solaris"...
	cd $(BR); $(MAKE) smp_solaris INSTALL=lib/smp_solaris.a

smp_solaris: beware smp_solaris.c smp_solaris.h zones.c zones.h $(BR)/lib/smp_solaris.a
	gcc -O3 -DBR_SMP_SOLARIS -I$(BR) -DSI_SMP_SOLARIS -c siva.c
	@echo Creating library $(INSTALL)...
	ar x $(BR)/lib/smp_solaris.a
	ar rus $(INSTALL) siva.o $(SMP_SOLARIS_OBJ)
	rm -f $(SMP_SOLARIS_OBJ)

SMP_WIN32_OBJ= brahma.o
smp_win32: beware smp_win32.c smp_win32.h zones.c zones.h $(BR)/lib/smp_win32.a
	gcc -O3 -DBR_SMP_WIN32 -I$(BR) -DSI_SMP_WIN32 -c siva.c
	@echo Creating library $(INSTALL)...
	ar x $(BR)/lib/smp_win32.a
	ar rus $(INSTALL) siva.o $(SMP_WIN32_OBJ)
	rm -f $(SMP_WIN32_OBJ)

SMP_LINUX_OBJ= brahma.o
smp_linux: beware smp_linux.c smp_linux.h zones.c zones.h $(BR)/lib/smp_linux.a
	gcc -O3 -DBR_SMP_LINUX -I$(BR) -DSI_SMP_LINUX -c siva.c
	@echo Creating library $(INSTALL)...
	ar x $(BR)/lib/smp_linux.a
	ar rus $(INSTALL) siva.o $(SMP_LINUX_OBJ)
	rm -f $(SMP_LINUX_OBJ)

# Don't use -O3 to avoid interprocedual opts...
benchmark: serial_boehm.a benchmark.c
	gcc -O2 -DBR_SERIAL -I$(BR) -DSI_SERIAL_BOEHM benchmark.c smp_solaris.a -o benchmark

clean:
	rm -f *.a *.o *.so benchmark
	cd $(BR); $(MAKE) clean
#	cd $(BOEHM); $(MAKE) clean
