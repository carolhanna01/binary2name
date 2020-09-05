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

VPATH	=	.:../include
INC_DIR		= -I. -I../include  -I../md/$(ARCH)
CC		= gcc 
#CFLAGS		= $(INC_DIR) $(DEBUG_FLAGS) -DAT_MIGRATION
CFLAGS		= $(INC_DIR) $(DEBUG_FLAGS) 
AR		= ar

OBJS		= thread_queue.o thread_dqueue.o bundle_queue.o heap.o fifo.o \
		lifo.o mcs.o fifo_lazy.o lifo_lazy.o mcs_lazy.o lff.o crt.o


../lib/libschedulers.a: $(OBJS)
	$(AR) crvs ../lib/libschedulers.a $(OBJS)
#	ranlib ../lib/libschedulers.a

thread_queue.o:	at-int.h thread_queue.h thread_queue.c
thread_dqueue.o: at-int.h thread_dqueue.h thread_dqueue.c
bundle_queue.o:	at-int.h bundle_queue.h bundle_queue.c
heap.o:	at-int.h heap.h heap.c
fifo.o:	at-int.h bundle.h state.h fifo.h fifo.c
lifo.o:	at-int.h bundle.h state.h lifo.h lifo.c
mcs.o:	at-int.h bundle.h state.h mcs.h mcs.c
mig.o:	at-int.h bundle.h state.h mig.h mig.c
lff.o:	at-int.h bundle.h state.h lff.h lff.c
crt.o:	at-int.h bundle.h state.h crt.h crt.c

fifo_lazy.o: at-int.h bundle.h state.h fifo_lazy.h \
		  fifo_lazy.c
lifo_lazy.o: at-int.h bundle.h state.h lifo_lazy.h \
		  lifo_lazy.c
mcs_lazy.o:  at-int.h bundle.h state.h mcs_lazy.h \
		mcs_lazy.c

clean:		
		rm -f *.o
		rm -f ../lib/libschedulers.a
