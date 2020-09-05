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
#	Makefile for local CGI programs
#

SHELL		=	/bin/sh

prefix		=	/usr/local
exec_prefix	=	${prefix}
CC		=	gcc
CCFLAGS		=	-O -g -Wall -fno-stack-protector	
SHLIBCC		=	libtool	--mode=link $(CC) $(CCFLAGS)
LDFLAGS		=
LIBS		=	
LIBDIR		=	${exec_prefix}/lib
LIBNET		=	
MV		=	mv
RM		=	rm -f
PARENT		=	..
BASE		=	../..
INLINE		=	$(PARENT)/inline
HDRS		=	$(PARENT)/hdrs
MYLIBDIR	=	$(PARENT)/lib
LIB		=	$(MYLIBDIR)/.libs/libgnubatch_int.a
SHLIB		=	$(MYLIBDIR)/libgnubatch_int.la
SHLIBCLIENT	=	$(MYLIBDIR)/libgnubatch_client.la
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(PARENT) -I$(BASE)
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
BATCHUSER	=	gnubatch
USERMODES	=	-o $(BATCHUSER)	-g root	-m 4755
CGIMODES	=	-o $(BATCHUSER)	-g root	-m 6755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnubatch
CGIBINDIR	=	$(INTBINDIR)/cgi-bin

PROGS=btjccgi btjcgi btjcrcgi btjdcgi btjvcgi btvccgi btvcgi

all:	$(PROGS)

btjcgi:		btjcgi.o $(LIB)
	$(SHLIBCC) -o btjcgi $(LDFLAGS)	btjcgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

btjccgi:		btjccgi.o $(LIB)
	$(SHLIBCC) -o btjccgi $(LDFLAGS) btjccgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

btjcrcgi:		btjcrcgi.o $(LIB)
	$(SHLIBCC) -o btjcrcgi $(LDFLAGS) btjcrcgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

btjdcgi:		btjdcgi.o $(LIB)
	$(SHLIBCC) -o btjdcgi $(LDFLAGS) btjdcgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

btjvcgi:		btjvcgi.o $(LIB)
	$(SHLIBCC) -o btjvcgi $(LDFLAGS) btjvcgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

btvcgi:	btvcgi.o $(LIB)
	$(SHLIBCC) -o btvcgi $(LDFLAGS)	btvcgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

btvccgi:btvccgi.o $(LIB)
	$(SHLIBCC) -o btvccgi $(LDFLAGS) btvccgi.o $(SHLIB) $(SHLIBCLIENT) $(LIBNET) $(LIBS)

install:
	$(INSTALL) -d $(CGIBINDIR)
	libtool	--mode=install $(INSTALL) $(CGIMODES) $(PROGS) $(CGIBINDIR)

install-strip:
	$(INSTALL) -d $(CGIBINDIR)
	libtool	--mode=install $(INSTALL) -s $(CGIMODES) $(PROGS) $(CGIBINDIR)

#	Clean-ups.

clean:
	$(RM) *.o $(PROGS)

distclean:	clean
	$(RM) Makefile

reallyclean:	distclean
	$(RM) TAGS
