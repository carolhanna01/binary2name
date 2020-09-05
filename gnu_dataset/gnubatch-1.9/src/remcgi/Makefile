#
#   Copyright 2009 Free	Software Foundation, Inc.
#
#  This	program	is free	software: you can redistribute it and/or modify
#  it under the	terms of the GNU General Public	License	as published by
#  the Free Software Foundation, either	version	3 of the License, or
#  (at your option) any	later version.
#
#  This	program	is distributed in the hope that	it will	be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
#  GNU General Public License for more details.
#
#   You	should have received a copy of the GNU General Public License
#  along with this program.  If	not, see <http://www.gnu.org/licenses/>.
#
#	Makefile for remote CGI	programs
#

SHELL		=	/bin/sh

prefix		=	/usr/local
exec_prefix	=	${prefix}
CC		=	gcc
CCFLAGS		=	-O -g -Wall -fno-stack-protector	
SHLIBCC		=	libtool	--mode=link $(CC) $(CCFLAGS)
LIBS		=	
LIBDIR		=	${exec_prefix}/lib
LIBNET		=	
MV		=	mv
RM		=	rm -f
PARENT		=	..
BASE		=	../..
HDRS		=	$(PARENT)/hdrs
MYLIBDIR	=	$(PARENT)/lib
APILIBDIR	=	$(PARENT)/apilib
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(PARENT) -I$(APILIBDIR)
LIB		=	$(MYLIBDIR)/.libs/libgnubatch_int.a
LIBCLIENT	=	$(MYLIBDIR)/.libs/libgnubatch_client.a
APISHLIB	=	$(APILIBDIR)/libgnubatch.la
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
RCGIMODES	=	-o root	-g root	-m 6755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnubatch
CGIBINDIR	=	$(INTBINDIR)/cgi-bin

RBTJCGIOBS	=	rbtjcgi.o rcgilib.o
RBTJCCGIOBS	=	rbtjccgi.o rcgilib.o
RBTJDCGIOBS	=	rbtjdcgi.o rcgilib.o
RBTJVCGIOBS	=	rbtjvcgi.o rcgilib.o
RBTJCRCGIOBS	=	rbtjcrcgi.o rcgilib.o
RBTVCGIOBS	=	rbtvcgi.o rcgilib.o
RBTVCCGIOBS	=	rbtvccgi.o rcgilib.o

all:		rbtjcgi	rbtjccgi rbtjdcgi rbtjvcgi rbtjcrcgi rbtvcgi rbtvccgi

rbtjcgi:	$(RBTJCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTJCGIOBS) $(APISHLIB) $(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

rbtjccgi:	$(RBTJCCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTJCCGIOBS) $(APISHLIB) $(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

rbtjdcgi:	$(RBTJDCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTJDCGIOBS) $(APISHLIB) $(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

rbtjvcgi:	$(RBTJVCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTJVCGIOBS) $(APISHLIB) $(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

rbtjcrcgi:	$(RBTJCRCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTJCRCGIOBS) $(APISHLIB)	$(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

rbtvcgi:	$(RBTVCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTVCGIOBS) $(APISHLIB) $(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

rbtvccgi:	$(RBTVCCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RBTVCCGIOBS) $(APISHLIB) $(LIBCLIENT) $(LIB) $(LIBNET) $(LIBS)

install:
	$(INSTALL) -d $(CGIBINDIR)
	libtool	--mode=install $(INSTALL) $(RCGIMODES) rbtjcgi rbtjccgi	rbtjcrcgi rbtjdcgi rbtjvcgi rbtvcgi rbtvccgi $(CGIBINDIR)

install-strip:
	$(INSTALL) -d $(CGIBINDIR)
	libtool	--mode=install $(INSTALL) -s $(RCGIMODES) rbtjcgi rbtjccgi rbtjcrcgi rbtjdcgi rbtjvcgi rbtvcgi rbtvccgi	$(CGIBINDIR)

clean:
	$(RM) *.o rbtjcgi rbtjccgi rbtjcrcgi rbtjdcgi rbtjvcgi rbtvcgi rbtvccgi

distclean:	clean
	$(RM) Makefile
