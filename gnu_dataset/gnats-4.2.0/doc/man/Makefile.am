# ------------------------------------------------------------------------------
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
# along with GNU GNATS; see the file COPYING.  If not, write to the
# Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
# 02111, USA.
# ------------------------------------------------------------------------------

include $(top_srcdir)/common.am

EXTRA_DIST = Makefile.in databases.man.in dbconfig.man.in delete-pr.man.in \
	edit-pr.man.in file-pr.man.in gen-index.man.in getclose.man.in \
	gnats.man.in gnatsd.man.in install-sid.man.in mkcat.man.in \
	mkdb.man.in pr-edit.man.in query-pr.man.in queue-pr.man.in \
	rmcat.man.in send-pr.conf.man.in send-pr.man.in

man1_MANS = edit-pr.man query-pr.man send-pr.man
man5_MANS = dbconfig.man databases.man send-pr.conf.man
man7_MANS = gnats.man
man8_MANS = file-pr.man delete-pr.man gen-index.man getclose.man gnatsd.man \
	install-sid.man mkdb.man mkcat.man queue-pr.man rmcat.man pr-edit.man

CLEANFILES = *.man

edit-pr.man: edit-pr.man.in
	$(do_subst) < $< > $@
databases.man: databases.man.in
	$(do_subst) < $< > $@
dbconfig.man: dbconfig.man.in
	$(do_subst) < $< > $@
delete-pr.man: delete-pr.man.in
	$(do_subst) < $< > $@
gnats.man: gnats.man.in
	$(do_subst) < $< > $@
file-pr.man: file-pr.man.in
	$(do_subst) < $< > $@
gen-index.man: gen-index.man.in
	$(do_subst) < $< > $@
getclose.man: getclose.man.in
	$(do_subst) < $< > $@
mkdb.man: mkdb.man.in
	$(do_subst) < $< > $@
gnatsd.man: gnatsd.man.in
	$(do_subst) < $< > $@
install-sid.man: install-sid.man.in
	$(do_subst) < $< > $@
mkcat.man: mkcat.man.in
	$(do_subst) < $< > $@
rmcat.man: rmcat.man.in
	$(do_subst) < $< > $@
pr-edit.man: pr-edit.man.in
	$(do_subst) < $< > $@
query-pr.man: query-pr.man.in
	$(do_subst) < $< > $@
queue-pr.man: queue-pr.man.in
	$(do_subst) < $< > $@
send-pr.conf.man: send-pr.conf.man.in
	$(do_subst) < $< > $@
send-pr.man: send-pr.man.in
	$(do_subst) < $< > $@

# vim:ft=make ts=8 sw=8 noet tw=80:
