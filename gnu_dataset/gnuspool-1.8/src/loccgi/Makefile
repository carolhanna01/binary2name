#
#   Copyright 2008 Free Software Foundation, Inc.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#	Makefile for local CGI programs
#

SHELL		=	/bin/sh

prefix		=	/usr/local
exec_prefix	=	${prefix}
CC		=	gcc
CCFLAGS		=	-O -g -Wall -fno-stack-protector 
SHLIBCC		=	libtool --mode=link $(CC) $(CCFLAGS)
LDFLAGS		=
LIBS		=	
LIBDIR		=	${exec_prefix}/lib
LIBNET		=	
MV		=	mv
RM		= 	rm -f
PARENT		=	..
BASE		=	../..
INLINE		=	$(PARENT)/inline
HDRS		=	$(PARENT)/hdrs
MYLIBDIR	=	$(PARENT)/lib
SHLIB		=	$(MYLIBDIR)/libgnuspool_int.la
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(PARENT) -I$(BASE)
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
SPOOLUSER	=	gnuspool
USERMODES	=	-o $(SPOOLUSER) -g root -m 4755
CGIMODES	=	-o $(SPOOLUSER) -g root -m 6755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnuspool
CGIBINDIR	=	$(INTBINDIR)/cgi-bin

PROGS=sqcgi sqvcgi sqdcgi sqccgi sqcrcgi spcgi spccgi

all:	$(PROGS)

sqcgi:		sqcgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) sqcgi.o $(SHLIB) $(LIBNET) $(LIBS)

sqvcgi:		sqvcgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) sqvcgi.o $(SHLIB) $(LIBNET) $(LIBS)

sqdcgi:		sqdcgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) sqdcgi.o $(SHLIB) $(LIBNET) $(LIBS)

sqccgi:		sqccgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) sqccgi.o $(SHLIB) $(LIBNET) $(LIBS)

sqcrcgi:	sqcrcgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) sqcrcgi.o $(SHLIB) $(LIBNET) $(LIBS)

spcgi:		spcgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) spcgi.o $(SHLIB) $(LIBNET) $(LIBS)

spccgi:		spccgi.o
	$(SHLIBCC) -o $@ $(LDFLAGS) spccgi.o $(SHLIB) $(LIBNET) $(LIBS)

install:
	$(INSTALL) -d $(CGIBINDIR)
	libtool --mode=install $(INSTALL) $(CGIMODES) $(PROGS) $(CGIBINDIR)

install-strip:
	$(INSTALL) -d $(CGIBINDIR)
	libtool --mode=install $(INSTALL) -s $(CGIMODES) $(PROGS) $(CGIBINDIR)

#
#	Clean-ups.
#

clean:
	$(RM) *.o $(PROGS)

distclean:	clean
	$(RM) Makefile

reallyclean:	distclean
	$(RM) TAGS
