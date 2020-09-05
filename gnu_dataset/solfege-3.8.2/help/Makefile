USER_MANUAL_LANGUAGES=no fr tr pt_BR
include help/C/Makefile
help/no/solfege.xml: help/no/no.po $(USER_MANUAL_SOURCES)
help/fr/solfege.xml: help/fr/fr.po $(USER_MANUAL_SOURCES)
help/tr/solfege.xml: help/tr/tr.po $(USER_MANUAL_SOURCES)
help/pt_BR/solfege.xml: help/pt_BR/pt_BR.po $(USER_MANUAL_SOURCES)

I18NDEFS:=--stringparam chunker.output.encoding "UTF-8"
XSLTPARAMS:=--param use.id.as.filename 1 --param xref.with.number.and.title 0 --xinclude

dist_files += help/Makefile help/README \
	$(foreach dir,C $(USER_MANUAL_LANGUAGES), \
	$(wildcard help/$(dir)/*.html) \
	$(wildcard help/$(dir)/*.xml) \
	$(wildcard help/$(dir)/$(dir).po) \
	$(wildcard help/$(dir)/figures/*.png) \
	$(wildcard help/$(dir)/ly/*.ly) \
	$(wildcard help/$(dir)/ly/*.png) \
	)

installfiles+=$(foreach dir,$(USER_MANUAL_LANGUAGES), \
	$(wildcard help/$(dir)/*.html) \
	$(wildcard help/$(dir)/figures/*.png) \
	$(wildcard help/$(dir)/ly/*.png) \
	)

all: $(foreach dir,C $(USER_MANUAL_LANGUAGES),help/$(dir)/index.html)
maintainer-clean: rm-generated-user-manual

rm-generated-user-manual:
	for dir in $(USER_MANUAL_LANGUAGES); do \
		rm -f help/$$dir/*.html; \
		rm -f help/$$dir/*.xml; \
	done
	rm -f help/C/*.html

TARGETS += help/C/rhythmtable.xml
help/C/rhythmtable.xml: tools/gen_rhythms_table.py
	$(PYTHON) tools/gen_rhythms_table.py

update-language-po: update-help-C-pot
	for dir in $(USER_MANUAL_LANGUAGES); do \
		echo $$dir ; \
		cp help/$$dir/$$dir.po help/$$dir/$$dir.po.tmp; \
		$(MSGMERGE) -o help/$$dir/$$dir.po help/$$dir/$$dir.po.tmp help/C/solfege.pot; \
		rm help/$$dir/$$dir.po.tmp; \
	done

help/%/solfege.xml:
ifdef XML2PO
ifneq "$(skipmanual)" "yes"
	for filename in $(notdir $(USER_MANUAL_SOURCES)); do \
	$(XML2PO) -e -p $(patsubst help/%/solfege.xml, help/%, $@)/$(patsubst help/%/solfege.xml,%,$@).po -o $(dir $@)/$$filename help/C/$$filename; \
	done
	rm -f .xml2po.mo
endif
endif

%/one-big-page.html: %/solfege.xml
	mv $(dir $<)index.html $(dir $<)/orig-index.html || true
	(cd $(dir $<); $(XSLTPROC) $(I18NDEFS) --xinclude --param onechunk 1 $(STYLESHEET) $(notdir $<) )
	mv $(dir $<)index.html $(dir $<)one-big-page.html
	mv $(dir $<)orig-index.html $(dir $<)index.html || true

%/index.html: %/solfege.xml
ifneq "$(skipmanual)" "yes"
ifeq "$(HAVE_STYLESHEET)" "yes"
 ifneq "$(XSLTPROC)" ""
	(cd $(dir $<); $(XSLTPROC) $(I18NDEFS) $(XSLTPARAMS) $(STYLESHEET) $(notdir $<))
 else
  ifneq "$(skipmanual)" "yes"
	@echo "*** Cannot rebuild the user manual because you don't have xsltproc installed."
	@echo "*** Run make as 'make skipmanual=yes' to ignore the problem and don't build the docs"
	exit 64
  endif
 endif
else
 ifneq "$(skipmanual)" "yes"
	@echo "*** Cannot rebuild html docs because we are missing the docbook stylesheet."
	@echo "*** Try to install the missing software and reconfigure."
	@echo "*** Run make as 'make skipmanual=yes' to ignore the problem and don't build the docs"
	exit 64
 endif
endif
endif
ifeq "$(skipmanual)" "yes"
	@echo "*** Not building manual because nomanual=yes"
endif

ifdef XML2PO
update-manual: $(addsuffix /solfege.xml,$(addprefix help/,$(USER_MANUAL_LANGUAGES)))
else
update-manual:
	@echo "You have to install xml2po to run 'make update-manual'"
endif

%.png:%.ly
	mkdir -p $(dir $<)out
	# This old code dont work any more. Most likely a dist-upgrade to sid
	# cause this.
	# cd $(dir $<) && $(LILYPOND) -o out/ -b eps $(notdir $<)
	#mv $(dir $<)out/$(basename $(notdir $<)).eps $(dir $<)out/$(basename $(notdir $<)).ps
	#cd $(dir $<)out && ps2eps -f $(basename $(notdir $<)).ps
	#$(PYTHON) tools/fix_eps.py $(dir $<)out/$(basename $(notdir $<)).eps
	#gs-gpl -dEPSCrop -dGraphicsAlphaBits=2 -dTextAlphaBits=4 -dNOPAUSE -sDEVICE=png16m -sOutputFile="$@" -r96 "$(dir $<)out/$(basename $(notdir $<)).eps" -c quit
	# The following code works, but it will only include the first line
	# of the music. So always use this for only SHORT music fragments.
	cd $(dir $<) && $(LILYPOND) -o out/ --preview $(notdir $<)
	mv $(dir $<)out/$(basename $(notdir $<)).preview.png $@

uninstall-manual:
	rm -rf /usr/share/omf/solfege
	rm -rf /usr/share/gnome/help/solfege

install-manual: uninstall-manual
	mkdir /usr/share/omf/solfege || true
	cp help/C/solfege.omf /usr/share/omf/solfege
	mkdir -p /usr/share/gnome/help/solfege/C || true
	cp help/C/*.xml /usr/share/gnome/help/solfege/C
