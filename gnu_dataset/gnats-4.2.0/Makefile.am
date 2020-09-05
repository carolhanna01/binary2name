# Copyright (C) 2005 Free Software Foundation, Inc.
#
# This file is part of GNU GNATS.
# 
# GNU GNATS is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GNU GNATS is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU GNATS; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/common.am

# Distribute these files and directories as well
EXTRA_DIST = TODO TODO.old NEWS.old UPGRADING contrib debian

SUBDIRS = doc gnats

dist-hook:
	  rm -rf `find $(distdir) -name CVS`
	  rm -rf `find $(distdir) -name _MTN`
	  rm -rf `find $(distdir) -name \*~`
