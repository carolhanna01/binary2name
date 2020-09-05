# Copyright (C) 2006-2014 Simon Josefsson
#
# This file is part of the Generic Security Service (GSS).
#
# GSS is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# GSS is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GSS; if not, see http://www.gnu.org/licenses or write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

WFLAGS ?= --enable-gcc-warnings
ADDFLAGS ?=
CFGFLAGS ?= --enable-gtk-doc --enable-gtk-doc-pdf $(ADDFLAGS) $(WFLAGS)

INDENT_SOURCES = `find . -name \*.[ch] -o -name gss.h.in | grep -v -e ^./build-aux/ -e ^./lib/gl/ -e ^./gl/ -e ^./src/gl/`

ifeq ($(.DEFAULT_GOAL),abort-due-to-no-makefile)
.DEFAULT_GOAL := bootstrap
endif

local-checks-to-skip = sc_copyright_check sc_immutable_NEWS		\
	sc_makefile_at_at_check sc_prohibit_strcmp sc_require_config_h	\
	sc_require_config_h_first

exclude_file_name_regexp--sc_unmarked_diagnostics = ^src/gss.c

VC_LIST_ALWAYS_EXCLUDE_REGEX = ^maint.mk|GNUmakefile|gtk-doc.make|po/.*.po.in|doc/fdl-1.3.texi|doc/gendocs_template|m4/pkg.m4|build-aux/|((lib/|src/)?gl)/.*$$

update-copyright-env = UPDATE_COPYRIGHT_HOLDER="Simon Josefsson" UPDATE_COPYRIGHT_USE_INTERVALS=2 UPDATE_COPYRIGHT_FORCE=1

gtk-doc.make:
	gtkdocize

doc/Makefile.gdoc:
	printf "gdoc_MANS =\ngdoc_TEXINFOS =\n" > doc/Makefile.gdoc

autoreconf: gtk-doc.make doc/Makefile.gdoc
	for f in po/*.po.in; do \
		cp $$f `echo $$f | sed 's/.in//'`; \
	done
	mv build-aux/config.rpath build-aux/config.rpath-
	test -f ./configure || autoreconf --install
	mv build-aux/config.rpath- build-aux/config.rpath

update-po: refresh-po
	for f in `ls po/*.po | grep -v quot.po`; do \
		cp $$f $$f.in; \
	done
	git add po/*.po.in
	git commit -m "Sync with TP." po/LINGUAS po/*.po.in

bootstrap: autoreconf
	./configure $(CFGFLAGS)

# Code Coverage

web-coverage:
	rm -fv `find $(htmldir)/coverage -type f | grep -v CVS`
	cp -rv doc/coverage/* $(htmldir)/coverage/

upload-web-coverage:
	cd $(htmldir) && \
		cvs commit -m "Update." coverage

# Release

ChangeLog:
	git2cl > ChangeLog
	cat .clcopying >> ChangeLog

tag = $(PACKAGE)-`echo $(VERSION) | sed 's/\./-/g'`
htmldir = ../www-$(PACKAGE)

release: prepare upload2 web upload-web

prepare:
	! git tag -l $(tag) | grep $(PACKAGE) > /dev/null
	rm -f ChangeLog
	$(MAKE) ChangeLog distcheck
	git commit -m Generated. ChangeLog
	git tag -s -m $(VERSION) $(tag)

upload2:
	git push
	git push --tags
	build-aux/gnupload --to ftp.gnu.org:gss $(distdir).tar.gz
	cp $(distdir).tar.gz $(distdir).tar.gz.sig ../releases/$(PACKAGE)/

web:
	cd doc && ../build-aux/gendocs.sh --html "--css-include=texinfo.css" \
		-o ../$(htmldir)/manual/ $(PACKAGE) "$(PACKAGE_NAME)"
	cp -v doc/reference/$(PACKAGE).pdf doc/reference/html/*.html doc/reference/html/*.png doc/reference/html/*.devhelp2 doc/reference/html/*.css $(htmldir)/reference/
	cp -v doc/cyclo/cyclo-$(PACKAGE).html $(htmldir)/cyclo/

upload-web:
	cd $(htmldir) && cvs commit -m "Update." manual/ reference/ cyclo/

review-diff:
	git diff `git describe --abbrev=0`.. \
	| grep -v -e ^index -e '^diff --git' \
	| filterdiff -p 1 -x 'build-aux/*' -x 'gl/*' -x 'lib/gl/*' -x 'src/gl/*' -x 'po/*' -x 'maint.mk' -x '.gitignore' -x '.x-sc*' -x ChangeLog -x GNUmakefile \
	| less
