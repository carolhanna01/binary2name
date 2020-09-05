#
# GNUsound modules Makefile.
# Copyright (C) 2002-2005  Pascal Haakmat <a.haakmat@chello.nl>
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

name=gnusound
version=0.7
prefix=/usr/local
sysconfdir=${prefix}/etc
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
libdir=${exec_prefix}/lib/$(name)
datadir=${prefix}/share
moduledir=$(libdir)/modules
gnusounddir=/home/p/src/gnusound

owner_user ?= root
owner_group ?= root

src=$(shell ls *.c)
obj=$(addsuffix .so, $(basename $(src)))
flags=$(addprefix ., $(addsuffix .cflags, $(basename $(src))))
clean_files=$(obj) *.bak 
distclean_files=.*.cflags *~ \#*

module_files=$(obj) $(shell ls *.glade *.xpm)
ifeq (no,yes)
gtk=gtk2
else
gtk=gtk1
endif

all: $(gnusounddir)/.cflags $(flags) $(obj)

$(gnusounddir)/.cflags:
	@echo
	@echo "*** Cannot find .cflags in $(gnusounddir)"
	@echo "*** Please do a ``./configure && make'' in the GNUsound directory."
	@echo
	@exit 1

.%.cflags: 
	@touch $@

%.so : %.c 
	$(CC) `cat $(gnusounddir)/.cflags` -I$(gnusounddir)/src -I$(gnusounddir)/src/$(gtk) -fPIC -shared  $< -o $@  `cat .$(basename $@).cflags`

install:
	mkdir -p $(moduledir)
	install -m 644 -o $(owner_user) -g $(owner_group) $(module_files) $(moduledir)

uninstall:
	(cd $(moduledir) && rm -f $(module_files))
	rmdir --ignore-fail-on-non-empty $(moduledir)

clean:
	rm -f $(clean_files)

veryclean: clean
	rm -f $(distclean_files)

distclean: veryclean
	rm -f Makefile

.PHONY=all clean veryclean distclean install uninstall
