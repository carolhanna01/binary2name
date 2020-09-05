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

#
# Some install variables
#
include ../pSather.options
include ../Make.config
HEADERDIR=../include
LIBDIR=../lib
CFLAGS=$(PCFLAGS) $(PSATHER) -I$(HEADERDIR)

OBJ=alloc.o exceptions.o locks.o psather.o attach.o \
    debug.o trace.o import.o cache.o stat.o fakegc.o parloop.o
HEADER= debug.h exceptions.h local.h locks.h pSather.h

libpSather.a:	$(OBJ)
	$(RM) -f libpSather.a
	$(AR) rc libpSather.a $(OBJ)
	$(RANLIB) libpSather.a

../lib/libam.a:	../am.options
	cd ../am; $(MAKE) install

install: libpSather.a
	$(CP) libpSather.a $(LIBDIR)
	$(CP) pSather.h $(HEADERDIR)
	$(CP) simple-par.h $(HEADERDIR)
	$(RANLIB) $(LIBDIR)/libpSather.a
#	echo "-lpSather -lam $(PLIBS)" > ../libraries
#	echo "$(PCFLAGS)" | sed 's/-Wall//' > ../flags

clean:
	rm -f *.o
	rm -f test
	rm -f libpSather.a
	rm -f .depend

.c.o:;
	$(CC) $(CFLAGS) -c $*.c

.depend:;
	gcc -MM $(CFLAGS) $(OBJ:.o=.c) | sed 's/:/: ..\/Make.config Makefile ..\/pSather.options/' >> .depend

DEPEND = .depend
-include $(DEPEND) /dev/null
