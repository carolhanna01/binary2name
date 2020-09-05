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

# Tests
INC_DIR	      = -I. -I../src -I../md/$(ARCH)
CC	      = gcc 
#To disable inlining, use -DAT_NO_INLINE
#CFLAGS	      = $(INC_DIR) -g -DAT_NO_INLINE
CFLAGS	      = $(INC_DIR) -g -O3
SRC_HDRS      = ../src/at.h ../src/at-inline.h
LOCAL_HDRS    = ./at_c_locks.h
ALL_HDRS      = $(SRC_HDRS) $(LOCAL_HDRS)
LDFLAGS	      = $(CFLAGS) 
OBJS          = at_c_locks_man.o at_c_locks.o
LIBS	      = ../lib/libat.a ../lib/libqt.a ../lib/libschedulers.a
/*EXT_LIBS      = -lkstat -lm*/
EXT_LIBS      = -lm 
LINKER	      = $(CC)
AT_H		= ../src/at.h

test_files    = at_c_locks_test_mutex \
                at_c_locks_test_rw \
                at_c_locks_test_c_mutex_8 \
                at_c_locks_benchmark

tests:		$(test_files)
		./at_c_locks_test_mutex
		./at_c_locks_test_rw
		./at_c_locks_test_c_mutex_8
		./at_c_locks_benchmark

at_c_locks.o:             $(ALL_HDRS) at_c_locks.c
at_c_locks_man.o:         $(ALL_HDRS) at_c_locks.c
at_c_locks_test_mutex.o:  $(ALL_HDRS) at_c_locks_test_mutex.c
at_c_locks_test_rw.o:     $(ALL_HDRS) at_c_locks_test_rw.c
at_c_locks_test_mutex.o:  $(ALL_HDRS) at_c_locks_test_mutex.c
at_c_locks_benchmark.o:   $(ALL_HDRS) at_c_locks_benchmark.c


at_c_locks_test_mutex:	$(LIBS) at_c_locks_test_mutex.o $(OBJS)
		$(LINKER) at_c_locks_test_mutex.o $(OBJS) $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_c_locks_test_mutex 

at_c_locks_test_rw:	$(LIBS) at_c_locks_test_rw.o $(OBJS)
		$(LINKER) at_c_locks_test_rw.o $(OBJS) $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_c_locks_test_rw 

at_c_locks_test_c_mutex_8:	$(LIBS) at_c_locks_test_c_mutex_8.o $(OBJS)
		$(LINKER) at_c_locks_test_c_mutex_8.o $(OBJS) $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_c_locks_test_c_mutex_8 

at_c_locks_benchmark:	$(LIBS) at_c_locks_benchmark.o $(OBJS)
		$(LINKER) at_c_locks_benchmark.o $(OBJS) $(LDFLAGS) $(LIBS) $(EXT_LIBS) -o at_c_locks_benchmark 


clean:		
		rm -f $(test_files)
		rm -f *.o 





