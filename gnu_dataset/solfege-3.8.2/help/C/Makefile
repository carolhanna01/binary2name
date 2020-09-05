installfiles+=$(wildcard help/C/*.html) \
	$(wildcard help/C/ly/*.png) \
	$(wildcard help/C/figures/*.png)

NOT_TRANSLATED_SOURCES=help/C/gpl.xml \
	help/C/lessonfiles.xml \
	help/C/midi-instrument-names.xml \
	help/C/music-theory.xml

USER_MANUAL_SOURCES=$(filter-out $(NOT_TRANSLATED_SOURCES), $(wildcard help/C/*.xml))

ly_gen_png_target = $(patsubst %.ly,%.png,$(filter-out $(wildcard help/C/ly/*common*.ly),$(wildcard help/C/ly/*.ly)))

pngs: $(ly_gen_png_target)

ifneq "$(skipmanual)" "yes"
all: $(ly_gen_png_target)
endif

maintainer-clean:
	rm -f $(ly_gen_png_target)
	rm -f help/C/*.html

dist_files += help/C/solfege.pot \
	help/C/Makefile \
	help/C/solfege.xml.in \
	$(wildcard help/C/scales/*.html) \
	$(wildcard help/C/scales/images/quadriads/*.png) \
	$(wildcard help/C/scales/images/triads/*.png) \
	$(wildcard help/C/scales/images/modes/chords/*.png) \
	$(wildcard help/C/scales/images/modes/scales/*.png) \

po-update: update-help-C-pot update-help-po
update-help-po: $(wildcard help/*/*.po)
.PHONY: update-help-C-pot update-help-po
update-help-C-pot: $(USER_MANUAL_SOURCES)
ifndef XML2PO
	@echo
	@echo "You need to install xml2po to update help/C/solfege.pot"
	@echo
	@exit 64
else
	$(XML2PO) -o help/C/solfege.pot $(USER_MANUAL_SOURCES)
endif

help/C/index.html: $(USER_MANUAL_SOURCES) $(NOT_TRANSLATED_SOURCES)

