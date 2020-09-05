#
# GNUsound documentation Makefile.
# Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
LANGUAGES=C
prefix=/usr/local
docrootdir=/usr/local/share/gnome/help/gnusound
docname=gnusound
xslstylesheet=/usr/share/sgml/docbook/xsl-stylesheets/xhtml/chunk.xsl

owner_user ?= root
owner_group ?= root

dist:
	for lang in $(LANGUAGES); do \
		cd $$lang; \
		xmllint --valid --noent --noout $(docname).xml || exit; \
		xsltproc $(xslstylesheet) $(docname).xml || exit; \
	done

clean: 
	for lang in $(LANGUAGES); do \
		cd $$lang && rm -f *.html; \
	done

veryclean: clean
	for lang in $(LANGUAGES); do \
		cd $$lang && rm -f *~; \
	done
	rm -rf dev/*
	rm -f *~

distclean: veryclean
	rm -f Makefile

uninstall:
	rm -rf $(docrootdir)

install:
	for lang in $(LANGUAGES); do \
		mkdir -p $(docrootdir)/$$lang; \
		install -m 644 -o $(owner_user) -g $(owner_group) \
			$$lang/*.xml $$lang/*.html $$lang/topic.dat \
			$(docrootdir)/$$lang; \
	done

.PHONY: dist clean veryclean distclean install uninstall
