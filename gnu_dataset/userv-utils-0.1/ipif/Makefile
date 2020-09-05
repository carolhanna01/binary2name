# Copyright (C) 1999 Ian Jackson
#
# This is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with userv-utils; if not, write to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# $Id: Makefile,v 1.9 1999/11/09 22:35:41 ian Exp $

OPTIMISE=	-O2
CFLAGS=		-Wall -Wmissing-prototypes -Wstrict-prototypes -Wpointer-arith \
		-Wwrite-strings -g $(OPTIMISE) -D_GNU_SOURCE

etcdir=		/etc
prefix=		/usr/local
bindir=		$(prefix)/bin

libdir=		$(prefix)/lib
libuserv=	$(libdir)/userv

etcuserv=	$(etcdir)/userv
services=	$(etcuserv)/services.d

TARGETS=	service

all:		$(TARGETS)

install:	all
		mkdir -p $(libuserv) $(services)
		cp -b service $(libuserv)/ipif
		cp ipif $(services)/ipif:new
		cp -b udptunnel $(bindir)/.
		set -e; cd $(services); test -f ipif || mv ipif:new ipif
