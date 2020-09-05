# vim: set noexpandtab:
#
# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007  Tom Cato Amundsen
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
# Foundation, Inc., 51 Franklin ST, Fifth Floor, Boston, MA  02110-1301  USA

POFILES:=$(wildcard po/*.po)
MOFILES:=$(patsubst %.po,%.mo, $(POFILES))
POLANGUAGES:=$(patsubst po/%.po,%,$(POFILES))
TARGETS += $(MOFILES)
dist_files += $(POFILES) po/Makefile po/solfege.pot

%.mo: %.po
	$(MSGFMT) $< -o $@

local-message-catalogs:=$(patsubst %,share/locale/%/LC_MESSAGES/$(PACKAGE).mo,$(POLANGUAGES))
all: $(local-message-catalogs)

share/locale/%/LC_MESSAGES/$(PACKAGE).mo: po/%.mo
	mkdir -p share/locale/$(patsubst po/%.mo,%,$<)/LC_MESSAGES
	cp $< $@

run-from-srcdir: $(MOFILES)
	# we to this just to make life simpler when running from source dir
	for lang in $(POLANGUAGES); do \
	  mkdir -p share/locale/$$lang/LC_MESSAGES ; \
	  cp po/$$lang.mo share/locale/$$lang/LC_MESSAGES/$(PACKAGE).mo; \
	done

all: po/solfege.pot

dist-po:
	mkdir -p $(DISTNAME)/po
	cp $(po-dist-files) $(DISTNAME)/po

install-po:
	echo $(POLANGUAGES)
	for lang in $(POLANGUAGES); do \
	  mkdir -p $(DESTDIR)/$(datadir)/locale/$$lang/LC_MESSAGES; \
	  $(INSTALL_DATA) po/$$lang.mo $(DESTDIR)/$(datadir)/locale/$$lang/LC_MESSAGES/$(PACKAGE).mo; \
	done

uninstall-po:
	for lang in $(POLANGUAGES); do \
	  rm -f $(DESTDIR)/$(datadir)/locale/$$lang/LC_MESSAGES/$(PACKAGE).mo; \
	  rmdir $(DESTDIR)/$(datadir)/locale/$$lang/LC_MESSAGES || true ; \
	  rmdir $(DESTDIR)/$(datadir)/locale/$$lang|| true ; \
	done
	rmdir $(DESTDIR)/$(datadir)/locale || true

po/solfege.pot:
	$(XGETTEXT) --add-comments=translators -L python -o solfege.pot -p po \
	--copyright-holder="Tom Cato Amundsen" \
	--keyword=_i $(wildcard src/*.py) \
	   $(wildcard mpd/*.py) \
           $(filter-out lesson-files/CVS lesson-files/bin lesson-files/include lesson-files/share,$(wildcard lesson-files/*)) \
		   $(wildcard lesson-files/include/*) \
                             learningtree.txt

.PHONY: rm-pot
rm-pot:
	rm po/solfege.pot

po-update: rm-pot po/solfege.pot
	for pofile in $(POFILES); do \
	  cp $$pofile tmpfile; \
	  $(MSGMERGE) tmpfile po/$(PACKAGE).pot > $$pofile; \
	done
	rm tmpfile

