#------------------------------->  Makefile  <--------------------------------#
#- Copyright (C) 1994 by International Computer Science Institute            -#
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

AR=ar
CFLAGS=-Wall -g  -O2
include ../Make.config

OBJ=		lwp.o sem.o queue.o clk.o sig.o timing.o

all:	timer producer

install: liblwp.a
	cp liblwp.a ../lib
	cp lwp.h ../include

producer: producer.o liblwp.a
	$(CC) $(CFLAGS) -o producer producer.o liblwp.a

timer: timer.o liblwp.a
	$(CC) $(CFLAGS) -o timer timer.o liblwp.a

liblwp.a: $(OBJ) lwp.h
	$(AR) rc liblwp.a $(OBJ)
	$(RANLIB) liblwp.a

.c.o:; $(CC) -DLWP -c $(CFLAGS) $*.c

bm.o: ../Make.config bm.c lwp.h timing.h arch.lh 
clk.o: ../Make.config clk.c lwp.h timing.h arch.lh 
lwp.o: ../Make.config lwp.c lwp.h timing.h arch.lh arch.lc 
producer.o: ../Make.config producer.c lwp.h timing.h arch.lh 
queue.o: ../Make.config queue.c lwp.h timing.h arch.lh 
sem.o: ../Make.config sem.c lwp.h timing.h arch.lh 
sig.o: ../Make.config sig.c lwp.h timing.h arch.lh
timing.o: ../Make.config timing.c timing.h 
timer.o: ../Make.config timer.c lwp.h timing.h arch.lh 
producer.o: ../Make.config producer.c lwp.h timing.h arch.lh 

clean:
	rm -f *.o timer producer liblwp.a