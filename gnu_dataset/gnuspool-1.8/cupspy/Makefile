#
#	CUPSPY Makefile
#
#   Copyright 2009 Free Software Foundation, Inc.
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
CUPSPYDIR	=	$(DESTDIR)${prefix}/etc/cups/cupspy
INSTALL		=	/usr/bin/install -c
INSTALL_DATA	=	${INSTALL} -m 644
MAINPROGS	=	cups.py cupspy-setup.py
PYMODS		=	confdefault.py conf.py filebuf.py ipp.py
CONFIGDATA	=	Default.ppd cupspy.conf
STAGED_INSTALL	=	false

all:	cupspy

cupspy:
	# Currently don't do anything

install install-strip: all
	$(INSTALL) -d $(CUPSPYDIR)
	$(INSTALL) $(MAINPROGS) $(CUPSPYDIR)
	$(INSTALL_DATA) $(PYMODS) $(CONFIGDATA) $(CUPSPYDIR)

clean:
	# Currently don't do anything

distclean:	clean
	$(RM) Makefile
