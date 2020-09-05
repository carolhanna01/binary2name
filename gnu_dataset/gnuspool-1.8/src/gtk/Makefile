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
#	Makefile for GTK routines
#

SHELL		=	/bin/sh
prefix		=	/usr/local
exec_prefix	=	${prefix}
PARENT		=	..
BASE		=	../..
HDRS		=	$(PARENT)/hdrs
CC		=	gcc
CCFLAGS		=	-O -g -Wall -fno-stack-protector 
SHLIBCC		=	libtool --mode=link $(CC) $(CCFLAGS)
LDFLAGS		=	
GTKINCL		=	`pkg-config --cflags gtk+-2.0`
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(BASE) $(GTKINCL)
MYLIBDIR	=	$(PARENT)/lib
LIB		=	$(MYLIBDIR)/.libs/libgnuspool_int.a
SHLIB		=	$(MYLIBDIR)/libgnuspool_int.la
LIBS		=	
LIBNET		=	
GTKLIBS		=	`pkg-config --libs gtk+-2.0`
RM		=	rm -f
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
SPOOLUSER	=	gnuspool
USERMODES	=	-o $(SPOOLUSER) -g root -m 4755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnuspool
SPHELPDIR	=	$(DESTDIR)${datarootdir}/gnuspool/help
USERBINS	=	gspl-xpq gspl-xuser
SYSBINS		=	gtksave
XRES		=	xspq.xpm xspq.menu xspqview.menu xspqsel.menu xspuser.xpm xspuser.menu

XSPUSEROBS=xspuser.o xspu_cbs.o gtk_lib.o
XSPQOBS=xspq.o xsq_cbs.o xsq_jplist.o xsq_jcall.o xsq_pcall.o xsq_prompt.o xsq_view.o gtk_lib.o
GTKSAVEOBS=gtksave.o

all:		gspl-xpq gspl-xuser gtksave

gspl-xuser:	$(XSPUSEROBS)
		$(SHLIBCC) -o $@ $(LDFLAGS) $(XSPUSEROBS) $(SHLIB) $(GTKLIBS) $(LIBNET) $(LIBS)

gspl-xpq:	$(XSPQOBS)
		$(SHLIBCC) -o $@ $(LDFLAGS) $(XSPQOBS) $(SHLIB) $(GTKLIBS) $(LIBNET) $(LIBS)

gtksave:	$(GTKSAVEOBS)
		$(CC) -o $@ $(LDFLAGS) $(GTKSAVEOBS) $(LIB) $(LIBS)

install:	all
		libtool --mode=install $(INSTALL) $(USERMODES) $(USERBINS) $(BINDIR)
		libtool --mode=install $(INSTALL) $(SYSBINS) $(INTBINDIR)
		$(INSTALL_DATA) $(XRES) $(SPHELPDIR)

install-strip:	all
		libtool --mode=install $(INSTALL) -s $(USERMODES) $(USERBINS) $(BINDIR)
		libtool --mode=install $(INSTALL) -s $(SYSBINS) $(INTBINDIR)
		$(INSTALL_DATA) $(XRES) $(SPHELPDIR)

clean:
	$(RM) *.o $(USERBINS) $(SYSBINS) core.*

distclean:	clean
	$(RM) Makefile
