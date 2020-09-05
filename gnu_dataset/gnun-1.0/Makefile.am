# Copyright (C) 2008, 2009, 2010, 2011, 2012,
#   2013, 2014, 2016, 2017 Free Software Foundation, Inc.

# This file is part of GNUnited Nations.

# GNUnited Nations is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# GNUnited Nations is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNUnited Nations.  If not, see <https://www.gnu.org/licenses/>.

ACLOCAL_AMFLAGS = -I m4 -I m4-external

SUBDIRS = doc dtd

bin_SCRIPTS = gnun-link-diff
tests_available =
tests_enabled =
check_environment = bindir="$(bindir)" \
		    pkglibexecdir="$(pkglibexecdir)" \
		    pkgdatadir="$(pkgdatadir)" \
		    srcdir="$(srcdir)/tests" \
		    AWK="$(AWK)" \
		    MSGATTRIB="$(MSGATTRIB)" \
		    SED="$(SED)" \
		    WDIFF="$(WDIFF)"

tests_available += init-po convert
if HAVE_GETTEXT
bin_SCRIPTS += gnun-report gnun-init-po
tests_enabled += init-po
if HAVE_PO4A
bin_SCRIPTS += gnun-preconvert gnun-merge-preconverted
tests_enabled += convert
endif
endif

validation_tests = validate/1 validate/2 validate/3 validate/4 validate/5
tests_available += $(validation_tests)

if HAVE_MATCH3
validation_tests += validate/match3
else
tests_available += validate/match3
endif

noinst_SCRIPTS = stamp-config.mk
if HAVE_SORT
noinst_SCRIPTS += stamp-i18n.mk
endif
pkglibexec_SCRIPTS = copy-msgid link-diff.awk mailfail update-localized-urls \
 validate-html-notify

if HAVE_VALIDATION
bin_SCRIPTS += gnun-validate-html
tests_enabled += $(validation_tests)
pkglibexec_SCRIPTS += expand-ssi.awk
endif

tests_available += copy-msgid
if HAVE_GETTEXT
tests_enabled += copy-msgid
endif

tests_available += diff-po add-fuzzy-diff

if HAVE_WDIFF
bin_SCRIPTS += gnun-add-fuzzy-diff
pkglibexec_SCRIPTS += add-fuzzy-diff
tests_enabled += add-fuzzy-diff
if HAVE_GETTEXT
bin_SCRIPTS += gnun-diff-po
pkglibexec_SCRIPTS += diff-po.awk
tests_enabled += diff-po
endif
endif
tests_available += collate
if HAVE_SORT
tests_enabled += collate
endif
pkglibexec_SCRIPTS += sort.awk

tests_available += make-prototype localized-urls/1
tests_enabled += make-prototype localized-urls/1

dist_pkglibexec_SCRIPTS = make-prototype.awk

installcheck-local:
	@failed=0; pass=0; skip=0; \
	for i in $(tests_available); do \
	  is=" $$i "; \
	  case " $(tests_enabled) "  in \
            *$$is*) \
	      if $(check_environment) "$(srcdir)/tests/$$i" \
	          2>/dev/null > /dev/null; \
                  then \
	        res="PASS: $$i"; pass=`expr $$pass + 1`; \
	      else \
                $(check_environment) "$(srcdir)/tests/$$i"; \
	        res="FAIL: $$i"; failed=`expr $$failed + 1`; \
	      fi; ;; \
            *) res="SKIP: $$i"; skip=`expr $$skip + 1`; \
	    ;; \
	  esac; \
	  echo $$res; \
	done; \
	echo $$pass tests passed, $$failed tests failed, $$skip tests skipped; \
	test $$failed = 0

# Note that BASE64_ENCODE may contain `|' and `\'', so we substitute it
# in a different way.
edit = $(SED) \
	-e 's|@PACKAGE[@]|$(PACKAGE)|g' \
	-e 's|@PACKAGE_NAME[@]|$(PACKAGE_NAME)|g' \
	-e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
	-e 's|@PACKAGE_BUGREPORT[@]|$(PACKAGE_BUGREPORT)|g' \
	-e 's|@PACKAGE_URL[@]|$(PACKAGE_URL)|g' \
	-e 's|@bindir[@]|$(bindir)|g' \
	-e 's|@datadir[@]|$(pkgdatadir)|g' \
	-e 's|@prefix[@]|$(prefix)|g' \
	-e 's|@pkglibexecdir[@]|$(pkglibexecdir)|g' \
	-e 's|@AWK[@]|$(AWK)|g' \
	-e "s,@BASE64_ENCODE[@],$(BASE64_ENCODE),g" \
	-e 's|@BASH[@]|$(BASH)|g' \
	-e 's|@GREP[@]|$(GREP)|g' \
	-e 's|@MAIL[@]|$(MAIL)|g' \
	-e 's|@MKTEMP[@]|$(MKTEMP) $(MKTEMP_T)|g' \
	-e 's|@MSGATTRIB[@]|$(MSGATTRIB)|g' \
	-e 's|@MSGCAT[@]|$(MSGCAT)|g' \
	-e 's|@MSGEXEC[@]|$(MSGEXEC)|g' \
	-e 's|@MSGFMT[@]|$(MSGFMT)|g' \
	-e 's|@MSGMERGE[@]|$(MSGMERGE)|g' \
	-e 's|@SED[@]|$(SED)|g' \
	-e 's|@SED_BINARY[@]|$(SED_BINARY)|g' \
	-e 's|@SORT[@]|$(SORT)|g' \
	-e 's|@XMLLINT[@]|$(XMLLINT)|g' \
	-e 's|@WDIFF[@]|$(WDIFF)|g'

add-fuzzy-diff copy-msgid diff-po.awk gnun-add-fuzzy-diff \
gnun-diff-po gnun-init-po gnun-link-diff gnun-merge-preconverted gnun-report \
gnun-validate-html mailfail sort.awk update-localized-urls \
validate-html-notify: Makefile
	$(AM_V_at)rm -f $@ $@.tmp
	$(AM_V_GEN)$(edit) '$(srcdir)/$@.in' >$@.tmp
	$(AM_V_at)chmod +x $@.tmp
	$(AM_V_at)mv $@.tmp $@

stamp-config.mk: config.mk
	$(AM_V_GEN)$(edit) config.mk >config.mk.tmp
	$(AM_V_at)mv config.mk.tmp config.mk
	$(AM_V_at)touch $@
if HAVE_SORT
stamp-i18n.mk:
	$(AM_V_GEN)mkdir i18n; \
  $(SED) '/#/d' "$(srcdir)/sort-linguas" | while read line; do \
    lang=`echo $$line | sed "s, .*,,"`; loc=`echo $$line | sed "s,.* ,,"`; \
    echo Generating locale for $$lang...; \
    $(LOCALEDEF) -c -f UTF-8 -i $$loc ./i18n/$$loc || true; \
  done
	$(AM_V_at)touch $@

install-data-hook: 
	$(AM_V_at)$(RM) -r $(DESTDIR)/$(pkgdatadir)/i18n
	$(AM_V_at)cp -R i18n $(DESTDIR)/$(pkgdatadir)

uninstall-hook: 
	$(AM_V_at)$(RM) -r $(DESTDIR)/$(pkgdatadir)/i18n

distclean-local:
	$(AM_V_at)$(RM) -r i18n
endif

add-fuzzy-diff: $(srcdir)/add-fuzzy-diff.in
copy-msgid: $(srcdir)/copy-msgid.in
gnun-add-fuzzy-diff: $(srcdir)/gnun-add-fuzzy-diff.in
gnun-diff-po: $(srcdir)/gnun-diff-po.in
gnun-init-po: $(srcdir)/gnun-init-po.in
gnun-link-diff: $(srcdir)/gnun-link-diff.in
gnun-merge-preconverted: $(srcdir)/gnun-merge-preconverted.in
gnun-report: $(srcdir)/gnun-report.in
gnun-validate-html: $(srcdir)/gnun-validate-html.in
diff-po.awk: $(srcdir)/diff-po.awk.in
mailfail: $(srcdir)/mailfail.in
update-localized-urls: $(srcdir)/update-localized-urls.in
sort.awk: $(srcdir)/sort.awk.in
if !HAVE_SORT
	$(AM_V_GEN)echo '{ print }' > $@ 
endif
validate-html-notify: $(srcdir)/validate-html-notify.in

exampledir = $(docdir)/examples
doc_DATA = TODO
dist_example_DATA = diff-page-head.html diff-page-tail.html \
	       gnun.mk GNUmakefile.team languages.txt \
	       initial-translations-list.html \
	       priorities.mk \
	       translist-head.html translist-tail.html
pkgdata_DATA = config.mk sort-linguas
dist_sysconf_DATA = gnun.conf
dist_pkgdata_DATA = GNUmakefile

EXTRA_DIST = m4-external/README \
	     add-fuzzy-diff.in copy-msgid.in diff-po.awk.in \
	     expand-ssi.awk.in gnun-add-fuzzy-diff.in gnun-diff-po.in \
	     gnun-init-po.in gnun-link-diff.in gnun-merge-preconverted.in \
	     gnun-report.in gnun-validate-html.in link-diff.awk \
	     mailfail.in sort.awk.in sort-linguas update-localized-urls.in \
	     validate-html-notify.in \
	     tests/add-fuzzy-diff tests/fuzzy-diff-0.po tests/fuzzy-diff-1.po \
	     tests/convert tests/conv.html tests/conv.pr.html \
	     tests/preconv.pr.po tests/conv-no-diffs.pr.po tests/conv.pr.po \
	     tests/conv.pot \
	     tests/copy-msgid tests/copy-msgid-0.po tests/copy-msgid-1.po \
	     tests/diff-po tests/diff-po-0.po tests/diff-po-1.po \
	     tests/diff-po.html tests/diff-po-no-common.html \
	     tests/init-po tests/compendium.bg.po tests/init.bg.po \
	     tests/init-no-diffs.bg.po tests/init.pot \
	     tests/make-prototype tests/proto.html tests/proto.proto \
	     tests/collate tests/sort0 tests/sort0-it tests/sort1 \
	     tests/sort1-it \
	     tests/localized-urls/1 \
	     tests/localized-urls/localized-urls.mk \
	     tests/localized-urls/home.html \
	     tests/localized-urls/po/home.pot \
	     tests/validate/1 tests/validate/1.html \
	     tests/validate/2 tests/validate/html5.html \
	     tests/validate/3 tests/validate/3.0.html tests/validate/3.1.html \
	     tests/validate/4 tests/validate/4.0.html tests/validate/4.1.html \
	     tests/validate/5 tests/validate/5.0.html tests/validate/5.1.html \
	     tests/validate/match3 tests/validate/match3.0.html \
	     tests/validate/match3.1.html

CLEANFILES = add-fuzzy-diff copy-msgid diff-po.awk gnun-add-fuzzy-diff \
	     gnun-diff-po gnun-init-po gnun-link-diff gnun-merge-preconverted gnun-report \
	     gnun-validate-html mailfail sort.awk \
	     update-localized-urls validate-html-notify \
	     stamp-config.mk
if HAVE_SORT
CLEANFILES += stamp-i18n.mk
endif
