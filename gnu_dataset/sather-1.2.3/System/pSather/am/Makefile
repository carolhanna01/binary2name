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

LIBNAME=libam.a
LIBDIR=../lib
HEADERNAME=am.h
HEADERDIR=../include
#Nobbi:
PCFLAGS=-g -Wall -DAM_THREADS -DLWP -I../lwp
PLIBS=../lwp/liblwp.a

MCFLAGS=$(CFLAGS) $(PCFLAGS) $(AM) -I$(HEADERDIR)
LIBS=-L$(LIBDIR) $(PLIBS)

VERSION=0.08

include ../Make.config

################################################################
## You probably won't need to change anything below this line ##
################################################################

LWPFILES=386bsd.c 386bsd.h Makefile arch.lc arch.lh bm.c clk.c \
         linux.c linux.h lwp.c lwp.h mips.c mips.h producer.c queue.c \
         sem.c sig.c sun3.c sun3.h sun4.c sun4.h timer.c timing.c timing.h
AMMAKEFILE=Makefile
AMREADME=README
AMCFILES=am.c tcpam.c lam.c meiam.c am_add.c am.h am_int.h mesg.c \
         am_nothread.c am_lwp.c am_solaris.c am_linux.c am_win32.c
AMFILES=LICENSE THREADS CHANGES prun pkill tcpam_config
LIC=LICENSE EMAIL

OBJ=am.o tcpam.o am_add.o meiam.o lam.o am_lwp.o am_solaris.o am_linux.o \
    am_win32.o am_nothread.o $(SPECIAL_AM_FILES)

test: mesg Makefile
#	@-echo "*** The Library is now beeing tested."
#	@-echo "*** On a slow system the test will run for several minutes"
#	$(AM_TEST)

install: $(LIBNAME) Makefile
	$(CP) $(LIBNAME) $(LIBDIR)/$(LIBNAME)
	$(CP) am.h $(HEADERDIR)/$(HEADERNAME)

clean:
	$(RM) -f *.o libam.a SPEC mesg

mesg: mesg.o $(LIBNAME) am.h $(OBJ) $(LWP)
	$(CC) $(MCFLAGS) -o mesg mesg.o $(OBJ) $(LIBS)


$(LIBNAME): $(OBJ) Makefile
	$(RM) -f $(LIBNAME)
	$(AR) rc $(LIBNAME) $(OBJ)
	$(RANLIB) $(LIBNAME)
	echo VERSION=$(VERSION) > SPEC
	echo MCFLAGS=$(MCFLAGS) >> SPEC
	echo LIBS=$(LIBS) >> SPEC

am.o:		Makefile am.c am.h am_int.h 
mesg.o:		Makefile mesg.c am.h 
am_nothread.o:	Makefile am_nothread.c am.h am_int.h 
am_lwp.o:	Makefile am_lwp.c am.h am_int.h 
tcpam.o:	Makefile tcpam.c am.h am_int.h 
am_add.o:	Makefile am_add.c am.h am_int.h 
lam.o:		Makefile lam.c am.h am_int.h # ../config/lanai/lcp/ep.h ../config/lanai/lcp/ifc.h 
meiam.o:	Makefile am.h am_int.h 
am_solaris.o:	Makefile am_solaris.c am.h am_int.h 
am_linux.o:	Makefile am_linux.c am.h am_int.h
am_win32.o:	Makefile am_win32.c am.h am_int.h


.c.o:
	$(CC) $(MCFLAGS) -c $*.c
