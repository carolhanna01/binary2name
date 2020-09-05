# Copyright (C) 2001, 2002 Milan Zamazal
# Copyright (C) 1993,94,95,96,97,99,2000,05,07
# 	Free Software Foundation Inc.
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
# along with GNU GNATS; see the file COPYING. If not, see
# <http://www.gnu.org/licenses/>.
#

# Include the common automake directives
include $(top_srcdir)/common.am

# Include GCC_CFLAGS if specified - supports older builds
AM_CFLAGS = $(GCC_CFLAGS)

# Include -X when flex is used
AM_LFLAGS = $(FLEX_LFLAGS)

BUILT_SOURCES = fconfig.y fconfig.c fconfigl.c getdate.c version.c

# Apparently, the yacc built *.h files don't have corresponding rules
EXTRA_DIST = dbconfig.in gnats-databases.in fconfig.y.in fconfig.h 

# libgnats.a library stuff
LDADD = libgnats.a
noinst_LIBRARIES = libgnats.a

if BUILD_MKAUTH
  MKAUTH = mk_auth.c
endif

libgnats_a_SOURCES = adm.c adm.h ansidecl.h client.c config.h \
	database.c database.h ds.h edit.c fconfig.y fconfigl.l field.c \
	field.h builtin-fields.h getdate.y gnats.h gnatsd.h \
	gnugetopt.c gnugetopt.h gnugetopt1.c internal.c lists.c mail.c \
	mail.h misc.c pcodes.h pr-init.c pr.c pr.h query.c \
	query.h regex.c regex.h version.c $(MKAUTH)

# Set the data store dir based on type -- do this for each type.  This step
# along with AM_CONDITIONAL tells automake to include the potential subdirs
# in DIST_SUBDIRS.
if DS_TYPE_FILE
  LDADD += ds-file/libds-file.a
endif

# Include the DSLIB_DIR for building 
SUBDIRS = . $(DSLIB_DIR)

# End-user appliations
bin_PROGRAMS = query-pr getclose
query_pr_SOURCES = query-pr.c regex.c query.h pr.h ansidecl.h gnats.h ds.h
getclose_SOURCES = getclose.c regex.c gnats.h ansidecl.h ds.h

# Add explicit target in order to build the datastore library at this point
query-pr.o: query-pr.c
	cd $(DSLIB_DIR) && $(MAKE) $(AM_MAKEFLAGS)
	$(COMPILE) -c -o $@ $<

# Server applications
# The following line is needed by older autoconf versions like 2.59
pkglibexecdir = $(libexecdir)/$(PACKAGE)
pkglibexec_PROGRAMS = gnatsd queue-pr pr-age pr-edit gnats-pwconv
gnatsd_SOURCES = gnatsd.c cmds.c regex.c file-pr.c btime.c gnatsd.h gnats.h \
	ansidecl.h
queue_pr_SOURCES = queue-pr.c gnats-dirs.h ansidecl.h pr.h gnats.h ds.h
pr_age_SOURCES = pr-age.c regex.c regex.h query.h gnats.h ansidecl.h ds.h
pr_edit_SOURCES = pr-edit.c file-pr.c btime.c pr.h ansidecl.h gnats.h ds.h
gnats_pwconv_SOURCES = gnats-pwconv.c gnats.h ds.h

# Not being built.
#pr_stat_SOURCES = pr-stat.c query.h regex.c regex.h gnats.h ansidecl.h ds.h

# Turn off byte-compiling
dist_lisp_DATA = gnats.el
ELCFILES = 

# Scripts
bin_SCRIPTS = edit-pr send-pr install-sid
pkglibexec_SCRIPTS = at-pr delete-pr diff-prs file-pr mail-agent mail-query

# Apparently, script sources are not automatically included in the dist
EXTRA_DIST += edit-pr.sh send-pr.sh install-sid.sh at-pr.sh \
	delete-pr.sh diff-prs.sh file-pr.sh mail-agent.sh mail-query.sh

CLEANFILES = $(bin_SCRIPTS) $(pkglibexec_SCRIPTS) version.c fconfig.c \
	fconfig.y fconfigl.c fconfig.h fconfig.tab.c fconfig.tab.h getdate.c \
	dbconfig databases

# Avoid warnings from being fatal for generated code or other libraries
fconfigl.o: fconfigl.c
	$(COMPILE) -Wno-error -c -o $@ $<

gnugetopt.o: gnugetopt.c
	$(COMPILE) -Wno-error -c -o $@ $<

gnugetopt1.o: gnugetopt1.c
	$(COMPILE) -Wno-error -c -o $@ $<

regex.o: regex.c
	$(COMPILE) -Wno-error -c -o $@ $<

version.c: Makefile
	echo 'const char *version_string = "$(VERSION)";' > $@-t
	mv $@-t $@

fconfig.y: fconfig.y.in $(srcdir)/$(DSLIB_DIR)/t-fconfig
	@echo Creating fconfig.y...
	cat $(srcdir)/fconfig.y.in $(srcdir)/$(DSLIB_DIR)/t-fconfig > $@

fconfig.c: AM_YFLAGS=-p fconf -b fconfig -d
fconfig.c: fconfig.y
	$(SHELL) $(YLWRAP) $< fconfig.tab.c $@ fconfig.tab.h $*.h y.output \
		$*.output -- $(YACCCOMPILE)

# Data files
dist_pkgdata_DATA = addresses categories classes gnatsd.host_access \
	gnatsd.user_access responsible states submitters \
	databases send-pr.conf
pkgdata_DATA = dbconfig

dbconfig: dbconfig.in Makefile
	$(do_subst) < $(srcdir)/dbconfig.in > dbconfig

databases: gnats-databases.in Makefile
	$(do_subst) < $(srcdir)/gnats-databases.in > databases

# Shell applications
at-pr: at-pr.sh Makefile
	$(do_subst) < $(srcdir)/at-pr.sh > at-pr
delete-pr: delete-pr.sh Makefile
	$(do_subst) < $(srcdir)/delete-pr.sh > delete-pr
diff-prs: diff-prs.sh Makefile
	$(do_subst) < $(srcdir)/diff-prs.sh > diff-prs
edit-pr: edit-pr.sh Makefile
	$(do_subst) < $(srcdir)/edit-pr.sh > edit-pr
file-pr: file-pr.sh Makefile
	$(do_subst) < $(srcdir)/file-pr.sh > file-pr
install-sid: install-sid.sh Makefile
	$(do_subst) < $(srcdir)/install-sid.sh > install-sid
mail-agent: mail-agent.sh Makefile
	$(do_subst) < $(srcdir)/mail-agent.sh > mail-agent
mail-query: mail-query.sh Makefile
	$(do_subst) < $(srcdir)/mail-query.sh > mail-query
send-pr: send-pr.sh Makefile
	$(do_subst) < $(srcdir)/send-pr.sh > send-pr

# We need to link in defaults database config files for for mkdb
install-data-hook:
	-mkdir -p $(DESTDIR)/$(sysconfdir)/$(PACKAGE)/defaults && \
	for i in $(dist_pkgdata_DATA) $(pkgdata_DATA) ; do \
		ln -s $(pkgdatadir)/$$i \
			$(DESTDIR)/$(sysconfdir)/$(PACKAGE)/defaults || true; \
	done
	for i in databases gnatsd.user_access gnatsd.host_access send-pr.conf ; do \
		if [ ! -e $(DESTDIR)/$(sysconfdir)/$(PACKAGE)/$$i ] ; then \
			${INSTALL_DATA} $(DESTDIR)$(pkgdatadir)/$$i $(DESTDIR)/$(sysconfdir)/$(PACKAGE); \
		fi; \
	done

