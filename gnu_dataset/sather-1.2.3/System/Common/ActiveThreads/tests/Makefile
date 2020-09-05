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

VPATH    =.:../include
# Tests
INC_DIR  =-I. -I../include -I../md/$(ARCH)
CC       =gcc
#To disable inlining, use -DAT_NO_INLINE
#CFLAGS	  =$(INC_DIR) -O3 -g -DAT_NO_INLINE
CFLAGS   =$(INC_DIR) -O3 -g
SRC_HDRS =at.h at-inline.h
LDFLAGS	 =$(CFLAGS)
LIBS     =../lib/libat.a ../lib/libschedulers.a ../lib/libqt.a
#EXT_LIBS =-lkstat -lm
EXT_LIBS =-lm -lpthread
LINKER   =$(CC)
AT_H     =at.h

test_files =at_create at_null_thread at_rec_create at_test_create at_cswitch \
            at_ping_pong_mutex at_ping_pong_sema at_uncontested_mutex at_uncontested_sema \
            at_mutex_try at_sema_try at_cond at_qsort at_merge at_barrier \
            at_producer at_vm at_mm at_gauss \
#            at_native_barrier at_rw at_photo

tests: $(test_files)
	./at_create
	./at_null_thread
	./at_rec_create
	./at_test_create
	./at_cswitch
	./at_ping_pong_mutex
	./at_ping_pong_sema
	./at_uncontested_mutex
	./at_uncontested_sema
	./at_mutex_try
	./at_sema_try
	./at_cond
#	./at_rw
	./at_qsort 
	./at_merge 
	./at_vm
	./at_mm


at_vm.o: $(AT_H) at_vm.c
at_mm.o: $(AT_H) at_mm.c
at_gauss.o: $(AT_H) at_gauss.c
at_qsort.o: $(AT_H) at_qsort.c
at_merge.o: $(SRC_HDRS) at_merge.c
#at_photo.o: $(SRC_HDRS) at_photo.c
at_tasks.o: $(SRC_HDRS) at_tasks.c
at_flat_merge.o: $(SRC_HDRS) at_flat_merge.c
at_cswitch.o: $(AT_H) at_cswitch.c
at_cond.o: $(AT_H) at_cond.c
#at_rw.o: $(AT_H) at_rw.c
at_barrier.o: $(AT_H) at_barrier.c
#at_native_barrier.o: $(AT_H) at_native_barrier.c
at_create.o: $(AT_H) at_create.c
at_rec_create.o: $(AT_H) at_rec_create.c
at_ping_pong_mutex.o: $(AT_H) at_ping_pong_mutex.c
at_ping_pong_sema.o: $(AT_H) at_ping_pong_sema.c
at_producer.o: $(AT_H) at_producer.c
at_uncontested_mutex.o: $(AT_H) at_uncontested_mutex.c
at_uncontested_sema.o: $(AT_H) at_uncontested_sema.c
at_test_create.o: $(AT_H) at_test_create.c
at_test_create.o: $(AT_H) at_switch_to_self.c
at_affinity.o: $(AT_H) at_affinity.c
at_mutex_try.o: $(AT_H) at_mutex_try.c
at_sema_try.o: $(AT_H) at_sema_try.c
at_grain.o: $(AT_H) at_grain.c

ch.o: $(AT_H) ch.c
ch: ch.o
	$(LINKER) ch.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o ch

michael.o: $(AT_H) michael.c
michael: michael.o
	$(LINKER) michael.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o michael

se.o: $(AT_H) se.c
se: se.o
	$(LINKER) se.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o se

at_merge: at_merge.o
	$(LINKER) at_merge.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_merge 
at_flat_merge: at_flat_merge.o
	$(LINKER) at_flat_merge.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_flat_merge 

#at_photo: at_photo.o
#	$(LINKER) at_photo.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_photo

at_tasks: at_tasks.o
	$(LINKER) at_tasks.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_tasks

at_vm: $(LIBS) at_vm.o
		$(LINKER) at_vm.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_vm 
at_mm: $(LIBS) at_mm.o
	$(LINKER) at_mm.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_mm 
at_gauss:	$(LIBS) at_gauss.o
	$(LINKER) at_gauss.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_gauss 
at_qsort:	$(LIBS) at_qsort.o
	$(LINKER) at_qsort.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_qsort 
at_cswitch:	$(LIBS) at_cswitch.o
	$(LINKER) at_cswitch.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_cswitch 

at_cond:	$(LIBS) at_cond.o
	$(LINKER) at_cond.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_cond 

#at_rw:		$(LIBS) at_rw.o
#	$(LINKER) at_rw.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_rw 

at_create:	$(LIBS) at_create.o
	$(LINKER) at_create.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_create 

at_null_thread:	$(LIBS) at_null_thread.o
	$(LINKER) at_null_thread.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_null_thread 

at_rec_create:	$(LIBS) at_rec_create.o
	$(LINKER) at_rec_create.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_rec_create 

at_mutex_try:	$(LIBS) at_mutex_try.o
	$(LINKER) at_mutex_try.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_mutex_try 

at_sema_try:	$(LIBS) at_sema_try.o
	$(LINKER) at_sema_try.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_sema_try 

at_barrier:	$(LIBS) at_barrier.o
	$(LINKER) at_barrier.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_barrier 

#at_native_barrier:	$(LIBS) at_native_barrier.o
#	$(LINKER) at_native_barrier.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_native_barrier 

at_ping_pong_mutex:	$(LIBS) at_ping_pong_mutex.o
	$(LINKER) at_ping_pong_mutex.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_ping_pong_mutex 

at_ping_pong_sema:	$(LIBS) at_ping_pong_sema.o
	$(LINKER) at_ping_pong_sema.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_ping_pong_sema

at_producer: $(LIBS) at_producer.o
	$(LINKER) at_producer.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_producer

at_uncontested_mutex: $(LIBS) at_uncontested_mutex.o
	$(LINKER) at_uncontested_mutex.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_uncontested_mutex

at_uncontested_sema: $(LIBS) at_uncontested_sema.o
	$(LINKER) at_uncontested_sema.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_uncontested_sema

at_test_create:	$(LIBS) at_test_create.o
	$(LINKER) at_test_create.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_test_create

at_switch_to_self: $(LIBS) at_switch_to_self.o
	$(LINKER) at_switch_to_self.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_switch_to_self

at_affinity: $(LIBS) at_affinity.o
	$(LINKER) at_affinity.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_affinity

at_grain: $(LIBS) at_grain.o
	$(LINKER) at_grain.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_grain 

t1: $(LIBS) t1.o
	$(LINKER) t1.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o t1


# This test was initially written to use Solaris threads.
# Until I have the time to change it to Active Threads, it will stay 
# this way. Feel free to change it - it should be pretty easy ;-)
pool_test: thread.h thread.c pool_test.c pool.o
	gcc -O -g -I. -D_REENTRANT thread.c pool_test.c pool.o -o pool_test -lposix4 -lthread

test_pool.o: $(AT_H) test_pool.c
test_pool: $(LIBS) test_pool.o
	$(LINKER) test_pool.o $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o test_pool 

clean:		
	rm -f $(test_files)
	rm -f *.o 
