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

ifdef RSVG
graphics/solfege.png: graphics/solfege.svg
	$(RSVG) $< $@

graphics/%.png: graphics/%.svg
	$(RSVG) -w 32 -h 32 $< $@

TARGETS += $(addsuffix .png, $(basename $(wildcard graphics/*.svg)))
endif

installfiles += $(wildcard graphics/*.png graphics/*.xpm graphics/*.svg)
dist_files += graphics/Makefile $(wildcard graphics/*.png graphics/*.xpm graphics/*.svg)

clean-graphics:
	rm $(addsuffix .png, $(basename $(wildcard graphics/*.svg)))

install-graphics:
	mkdir -p $(DESTDIR)/$(datadir)/pixmaps
	$(INSTALL_DATA) graphics/solfege.png $(DESTDIR)/$(datadir)/pixmaps/

uninstall-graphics:
	rm -f $(DESTDIR)/$(datadir)/pixmaps/solfege.png

