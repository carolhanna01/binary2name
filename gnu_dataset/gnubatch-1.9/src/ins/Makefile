#  Copyright 2009 Free Software	Foundation, Inc.

#  This	program	is free	software: you can redistribute it and/or modify
#  it under the	terms of the GNU General Public	License	as published by
#  the Free Software Foundation, either	version	3 of the License, or
#  (at your option) any	later version.

#  This	program	is distributed in the hope that	it will	be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with this program.  If	not, see <http://www.gnu.org/licenses/>.  */

SHELL		=	/bin/sh

prefix		=	/usr/local
exec_prefix	=	${prefix}
PARENT		=	..
BASE		=	../..
HDRS		=	$(PARENT)/hdrs
YACC		=	yacc
CC		=	gcc
CCFLAGS		=	-O -g -Wall -fno-stack-protector	
LDFLAGS		=
LIBS		=	
LIBNET		=	
XMLLIB          =       
MYLIBDIR	=	$(PARENT)/lib
LIB		=	$(MYLIBDIR)/.libs/libgnubatch_int.a
LIBCLIENT	=	$(MYLIBDIR)/.libs/libgnubatch_client.a
HDRS		=	$(PARENT)/hdrs
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(BASE) 
RM		=	rm -f
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
BATCHUSER	=	gnubatch
USERMODES	=	-o $(BATCHUSER)	-g root	-m 755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
SBINDIR		=	$(DESTDIR)${exec_prefix}/sbin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnubatch
USERPROGS	=	gbch-atcover
PROGS		=	gbch-btuconv gbch-ciconv gbch-cjlist gbch-cvlist gbch-ripc
STAGED_INSTALL	=	false

all:	gbch-atcover gbch-btuconv gbch-ciconv gbch-cjlist gbch-cjlistx gbch-cvlist gbch-ripc

gbch-atcover:	atcover.o
	$(CC) $(LDFLAGS) -o $@ atcover.o $(LIB)	$(LIBS)

gbch-btuconv:	btuconv.o convcomm.o
	$(CC) $(LDFLAGS) -o $@ btuconv.o convcomm.o $(LIB) $(LIBCLIENT)	$(LIBNET) $(LIBS)

gbch-ciconv:	ciconv.o convcomm.o
	$(CC) $(LDFLAGS) -o $@ ciconv.o	convcomm.o $(LIB) $(LIBCLIENT) $(LIBS)

gbch-cjlist:	cjlist.o convcomm.o
	$(CC) $(LDFLAGS) -o $@ cjlist.o	convcomm.o $(LIB) $(LIBCLIENT) $(LIBNET) $(LIBS)

gbch-cjlistx:	cjlistx.o convcomm.o
	$(CC) $(LDFLAGS) -o $@ cjlistx.o convcomm.o $(LIBCLIENT) $(LIBNET) $(XMLLIB) $(LIB) $(LIBCLIENT) $(LIB) $(LIBS)

gbch-cvlist:	cvlist.o convcomm.o
	$(CC) $(LDFLAGS) -o $@ cvlist.o	convcomm.o $(LIB) $(LIBCLIENT) $(LIBNET) $(LIBS)

gbch-ripc:	ripc.o
	$(CC) $(LDFLAGS) -o $@ ripc.o $(LIB) $(LIBNET) $(LIBS) -lm

install:	all
	$(INSTALL) $(PROGS) $(SBINDIR)
	$(INSTALL) $(USERPROGS)	$(BINDIR)

install-strip:	all
	$(INSTALL) -s $(PROGS) $(SBINDIR)
	$(INSTALL) -s $(USERPROGS) $(BINDIR)

clean:
	$(RM) *.o gbch-*

distclean: clean
	$(RM) Makefile
