# ------------------------------------------------------------------------------
# ds-file.am -- automake fragment file for GNU GNATS flat-file datastore.
# ------------------------------------------------------------------------------
# Copyright (C) 2005,07 Free Software Foundation, Inc.
# Adapted from <topdir>/gnats/Makefile.in by Mel Hatzis <hatzis@wattes.org>.
# Adapted to automake by Chad Walstrom <chewie@wookimus.net>.
#
# This file is part of GNU GNATS.
# 
# GNU GNATS is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.
# 
# GNU GNATS is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# GNU GNATS; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.
# ------------------------------------------------------------------------------
# This file is included in $(top_srcdir)/gnats/Makefile.am.  Construct path
# names accordingly.
# ------------------------------------------------------------------------------
include $(top_srcdir)/common.am

# Include GCC_CFLAGS if specified - supports older builds
AM_CFLAGS = $(GCC_CFLAGS)

# Include -X when flex is used
AM_LFLAGS = $(FLEX_LFLAGS)

AM_CPPFLAGS = -I $(top_srcdir)/gnats

EXTRA_DIST = t-fconfig ds-file.h

# libds-file.a
noinst_LIBRARIES = libds-file.a
libds_file_a_SOURCES = index.c index.h pr.c db.c fld.c \
	$(top_srcdir)/gnats/pr.h $(top_srcdir)/gnats/gnats.h \
	$(top_srcdir)/gnats/ansidecl.h $(top_srcdir)/gnats/pcodes.h \
	$(top_srcdir)/gnats/gnugetopt.h $(top_srcdir)/gnats/regex.h \
	$(top_srcdir)/gnats/query.h $(top_srcdir)/gnats/gnatsd.h \
	$(top_srcdir)/gnats/ds.h

# datastore-specific programs
# The following line is needed by older autoconf versions like 2.59
pkglibexecdir = $(libexecdir)/$(PACKAGE)
LDADD = ../libgnats.a libds-file.a
gen_index_SOURCES = gen-index.c index.h ds-file.h $(top_srcdir)/gnats/gnats.h \
	$(top_srcdir)/gnats/ansidecl.h $(top_srcdir)/gnats/ds.h
gen_closed_date_SOURCES = gen-closed-date.c $(top_srcdir)/gnats/gnats.h \
	$(top_srcdir)/gnats/ansidecl.h $(top_srcdir)/gnats/ds.h
pkglibexec_PROGRAMS = gen-index gen-closed-date

# datastore-specific scripts
pkglibexec_SCRIPTS = check-db mkdb mkcat rmcat

# Apparently, script sources are not automatically included in the dist
EXTRA_DIST += check-db.sh mkdb.sh mkcat.sh rmcat.sh

CLEANFILES = $(pkglibexec_SCRIPTS)

check-db: check-db.sh Makefile
	$(do_subst) < $(srcdir)/check-db.sh > check-db
mkdb: mkdb.sh Makefile
	$(do_subst) < $(srcdir)/mkdb.sh > mkdb
mkcat: mkcat.sh Makefile
	$(do_subst) < $(srcdir)/mkcat.sh > mkcat
rmcat: rmcat.sh Makefile
	$(do_subst) < $(srcdir)/rmcat.sh > rmcat

