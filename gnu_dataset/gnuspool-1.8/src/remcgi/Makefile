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
#	Makefile for remote CGI programs
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
HDRS		=	$(PARENT)/hdrs
MYLIBDIR	=	$(PARENT)/lib
APILIBDIR	=	$(PARENT)/apilib
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(PARENT) -I$(APILIBDIR)
LIB		=	$(MYLIBDIR)/.libs/libgnuspool_int.a
APISHLIB	=	$(APILIBDIR)/libgnuspool.la
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
RCGIMODES	=	-o root -g root -m 6755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnuspool
CGIBINDIR	=	$(INTBINDIR)/cgi-bin

RSQCGIOBS	=	rsqcgi.o rcgilib.o
RSQCCGIOBS	=	rsqccgi.o rcgilib.o
RSQCRCGIOBS	=	rsqcrcgi.o rcgilib.o
RSQDCGIOBS	=	rsqdcgi.o rcgilib.o
RSQVCGIOBS	=	rsqvcgi.o rcgilib.o
RSPCGIOBS	=	rspcgi.o rcgilib.o
RSPCCGIOBS	=	rspccgi.o rcgilib.o

all:	rsqcgi rsqccgi rsqcrcgi rsqdcgi rsqvcgi rspcgi rspccgi


rsqcgi:	$(RSQCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSQCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

rsqccgi:	$(RSQCCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSQCCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

rsqcrcgi:	$(RSQCRCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSQCRCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

rsqdcgi:	$(RSQDCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSQDCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

rsqvcgi:	$(RSQVCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSQVCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

rspcgi:	$(RSPCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSPCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

rspccgi:	$(RSPCCGIOBS)
	$(SHLIBCC) -o $@ $(LDFLAGS) $(RSPCCGIOBS) $(APISHLIB) $(LIB) $(LIBNET) $(LIBS)

install:
	$(INSTALL) -d $(CGIBINDIR)
	libtool --mode=install $(INSTALL) $(RCGIMODES) rsqcgi rsqccgi rsqcrcgi rsqdcgi rsqvcgi rspcgi rspccgi $(CGIBINDIR)

install-strip:
	$(INSTALL) -d $(CGIBINDIR)
	libtool --mode=install $(INSTALL) -s $(RCGIMODES) rsqcgi rsqccgi rsqcrcgi rsqdcgi rsqvcgi rspcgi rspccgi $(CGIBINDIR)

clean:
	$(RM) *.o rsqcgi rsqccgi rsqcrcgi rsqdcgi rsqvcgi rspcgi rspccgi

distclean:	clean
	$(RM) Makefile
