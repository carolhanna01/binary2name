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
#	Makefile for various terminal server routines
#

SHELL		=	/bin/sh
prefix		=	/usr/local
exec_prefix	=	${prefix}
PARENT		=	..
BASE		=	../..
HDRS		=	$(PARENT)/hdrs
CC		=	gcc
CCFLAGS		=	-O -g -Wall -fno-stack-protector 
CFLAGS		=	$(CCFLAGS) -I$(HDRS) -I$(BASE)
LDFLAGS		=
LIBNET		=	
LIBS		=	
MYLIBDIR	=	$(PARENT)/lib
LIB		=	$(MYLIBDIR)/.libs/libgnuspool_int.a
RM		=	rm -f
YACC		=	yacc
YFLAGS		=	-d
LEX		=	:
LEXLIB		=	
RM		=	rm -f
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
SPOOLUSER	=	gnuspool
USERMODES	=	-o $(SPOOLUSER) -g root -m 4755
NOSETMODES	=	-o $(SPOOLUSER) -g root
ROOTMODES	=	-o root -g root -m 4755
BINDIR		=	$(DESTDIR)${exec_prefix}/bin
SBINDIR		=	$(DESTDIR)${exec_prefix}/sbin
INTBINDIR	=	$(DESTDIR)${exec_prefix}/libexec/gnuspool
DATADIR		=	$(DESTDIR)${datarootdir}/gnuspool
HELPFILEDIR	=	$(DESTDIR)${datarootdir}/gnuspool/help
PROGS		=	sendudp xilp xtelnet xtlhp gspl-getsnmp xtftp

# We don't make a distinction between shared and unshared in this case
# as it doesn't seem worth it.

all:	$(PROGS)

sendudp:	sendudp.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB) $(LIBNET) $(LIBS)

xtelnet:	xtelnet.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB) $(LIBNET) $(LIBS)

xilp:	xilp.o common.o
	$(CC) $(LDFLAGS) -o $@ xilp.o common.o $(LIB) $(LIBNET) $(LIBS)

xtlhp:	xtlhp.o xtlhpexec.o xtlhpgram.o xtlhplex.o asn.o prinasn.o snmp.o snmpmacs.o snmpsock.o
	$(CC) $(LDFLAGS) -o $@ xtlhp.o xtlhpexec.o xtlhpgram.o xtlhplex.o asn.o prinasn.o snmp.o snmpmacs.o snmpsock.o $(LEXLIB) -lm $(LIB) $(LIBNET) $(LIBS)

gspl-getsnmp:	getsnmp.o asn.o prinasn.o snmp.o snmpmacs.o snmpsock.o
	$(CC) $(LDFLAGS) -o $@ getsnmp.o asn.o prinasn.o snmp.o snmpmacs.o snmpsock.o -lm $(LIB) $(LIBNET) $(LIBS)

xtftp:	xtftp.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB) $(LIBNET) $(LIBS)

install:	all
	$(INSTALL) $(NOSETMODES) xtlhp xtftp xtelnet $(INTBINDIR)
	$(INSTALL) $(ROOTMODES) xilp $(INTBINDIR)
	$(INSTALL) $(NOSETMODES) gspl-getsnmp $(SBINDIR)
	$(INSTALL) -d $(DATADIR)
	$(INSTALL_DATA) xtsnmpdef xtlhp-ctrl $(DATADIR)

install-strip:	all
	$(INSTALL) -s $(NOSETMODES) xtlhp xtftp xtelnet $(INTBINDIR)
	$(INSTALL) -s $(ROOTMODES) xilp $(INTBINDIR)
	$(INSTALL) -s $(NOSETMODES) gspl-getsnmp $(SBINDIR)
	$(INSTALL) -d $(DATADIR)
	$(INSTALL_DATA) xtsnmpdef xtlhp-ctrl $(DATADIR)

clean:
	$(RM) *.o $(PROGS)

distclean:	clean
	$(RM) Makefile
