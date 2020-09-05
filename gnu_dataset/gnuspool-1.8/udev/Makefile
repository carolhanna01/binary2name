#
#	UDEV scripts Makefile
#
#   Copyright 2010 Free Software Foundation, Inc.
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

SHELL		=	/bin/sh

BASE		=	..
PARENT		=	..
prefix		=	/usr/local
exec_prefix	=	${prefix}
UDEVDIR		=	$(DESTDIR)/lib/udev
RULESDIR	=	$(UDEVDIR)/rules.d
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
STAGED_INSTALL	=	false
CONFSCRIPT	=	udev-configure-gnuspool
RULE		=	70-gspl-printers.rules

all:	udevscripts

udevscripts:
	# Currently don't do anything

install install-strip: all
	$(INSTALL) -d $(UDEVDIR) $(RULESDIR)
	$(INSTALL) $(CONFSCRIPT) $(UDEVDIR)
	$(INSTALL_DATA) $(RULE) $(RULESDIR)

clean:
	# Currently don't do anything
	:

distclean:	clean
	$(RM) Makefile
