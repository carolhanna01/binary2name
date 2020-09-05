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
# $Id: Makefile,v 1.3 1999/11/09 23:12:54 ian Exp $

CFLAGS=	-Wall -Wwrite-strings -Wmissing-prototypes -Wstrict-prototypes \
	-Wpointer-arith -O2 -g
LDFLAGS=

TARGETS=	ucgi ucgitarget

all:		$(TARGETS)

OBJS=		ucgi.o ucgitarget.o ucgicommon.o

ucgi:		ucgi.o ucgicommon.o
ucgitarget:	ucgitarget.o ucgicommon.o

$(OBJS):	ucgi.h
