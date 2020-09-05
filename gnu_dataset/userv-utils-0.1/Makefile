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
# $Id: Makefile,v 1.2 1999/11/09 23:21:46 ian Exp $

VERSION=0.1

all:
	echo >&2 'See README.  This is not a unified package.'

distclean:
	find . \( -name '*~' -o -name '#*#' -o -name '*.o' -o -name core \
		-o -name dist_tmp -o -name userv-utils-\*.tar.gz \
		-o -name '.#*' \) \
		-print0 | xargs -0r rm -rf --

dist_tmp=dist_tmp/userv-utils-$(VERSION)
dist_prune=\( -name CVS -o -name 'dist_tmp*' \)
dist:			distclean
	rm -rf dist_tmp*
	mkdir dist_tmp $(dist_tmp)
	find . $(dist_prune) -prune -o -type d -print | \
		sed -e 's#.*#mkdir -p $(dist_tmp)/&#' | sh
	find . $(dist_prune) -prune -o -type f -print | \
		sed -e 's#.*#ln & $(dist_tmp)/&#' | sh
	cd dist_tmp && tar cf ../$(dist_tmp).tar `basename $(dist_tmp)`
	gzip -9 $(dist_tmp).tar
	mv $(dist_tmp).tar.gz .
