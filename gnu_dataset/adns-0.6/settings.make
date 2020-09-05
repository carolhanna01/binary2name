# settings.make.in - main configuration settings for Makefiles,
#  used by autoconf/configure to generate settings.make
#  
#  This file is
#    Copyright (C) 1997-1999 Ian Jackson <ian@davenant.greenend.org.uk>
#
#  It is part of adns, which is
#    Copyright (C) 1997-1999 Ian Jackson <ian@davenant.greenend.org.uk>
#    Copyright (C) 1999 Tony Finch <dot@dotat.at>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
#  any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 

MAJOR=		0
MINOR=		1
SHLIBFILE=	$(SHLIBSONAME).$(MINOR)
SHLIBSONAME=	$(SHLIBFORLINK).$(MAJOR)
SHLIBFORLINK=	libadns.so

CC=		gcc
CFLAGS=		$(AUTOCFLAGS) $(WARNS) $(WERROR) $(DIRCFLAGS) $(XCFLAGS)
LDLIBS=		 $(XLIBS)
AUTOCFLAGS=	-g -O2
WARNS=		-Wall -Wmissing-prototypes -Wwrite-strings -Wstrict-prototypes -Wcast-qual -Wpointer-arith
#WERROR=	-Werror

M4=		m4
RANLIB=		ranlib

SHLIBCC=	$(CC) $(CFLAGS) -fpic

MKSHLIB_1=	$(CC) $(LDFLAGS) -shared -Wl,-soname=$(SHLIBSONAME) -o
MKSHLIB_2=	
MKSHLIB_3=	-lc

prefix=		/usr/local
exec_prefix=	${prefix}
bin_dir=	$(exec_prefix)/bin
lib_dir=	$(exec_prefix)/lib
include_dir=	$(prefix)/include

INSTALL=		install -c
INSTALL_PROGRAM=	$(INSTALL) -m 755 $(INSTALL_PROGRAM_FLAGS)
INSTALL_DATA=		$(INSTALL) -m 644

all:			$(TARGETS)

clean mostlyclean:
		rm -f *.o *.tmp*

distclean:		clean
		rm -f $(TARGETS) *~ ./#*# core *.orig *.rej Makefile

distprep:		$(AUTOCSRCS)

maintainer-clean:	distclean
		-rm -f $(AUTOCSRCS)
