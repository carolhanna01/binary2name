## Process this file with automake to produce Makefile.in

bin_PROGRAMS = dominion dom_make dom_add dom_update dom_print

bin_SCRIPTS = addnews.sh make-new-world.sh

info_TEXINFOS = dominion.texi

dominion_SOURCES = dominion.c world.c nation.c user.c ext.c file.c misc.c \
	init.c budget.c techno.c commands.c help.c menus.c diplomacy.c \
	reports.c spy.c army.c armylib.c mail.c diplolib.c maglib.c \
	cur_stuff.c news.c c_news.c economy.c transport.c spells.c \
	spelllib.c root.c cinfo.c construct.c int_mail.c movement.c \
	bonds.c bondslib.c cn_params.c

dom_make_SOURCES = makeworld.c file.c misc.c ext.c armylib.c diplolib.c \
	world.c economy.c movement.c bondslib.c cn_params.c

dom_add_SOURCES = addnation.c file.c misc.c ext.c armylib.c diplolib.c \
	world.c economy.c movement.c bondslib.c cn_params.c

dom_update_SOURCES = update.c file.c misc.c nation.c armylib.c ext.c \
	mail.c techno.c battle.c diplolib.c world.c maglib.c cn_params.c \
	cn.c economy.c spelllib.c news.c movement.c cnlib.c bondslib.c

dom_print_SOURCES = printmap.c file.c misc.c nation.c armylib.c ext.c \
	maglib.c diplolib.c world.c economy.c movement.c spelllib.c \
	bondslib.c cn_params.c

BUILT_SOURCES = config.h

noinst_HEADERS = army.h common.h cur_stuff.h dominion.h proto.h bonds.h \
	decl.h misc.h cn.h costs.h dom_config.h news.h

# The non-source data files are: magic stuff, misc and doc stuff

magic_DATA = mag_Aule mag_Demonology mag_Insects mag_Necromancy mag_Unity \
	mag_Avian mag_Diana mag_Master mag_Neptune mag_Yavanna mag_Chess \
	mag_Inferno mag_Monsters mag_Time mag_orders
magicdir = $(libdir)/dominion/magic

misc_DATA = races cns cns.small army_types spirit_types techno_levels
miscdir = $(libdir)/dominion/misc

# play_doc is the documentation that is used while playing
# play_doc_DATA = dominion.info info_intro refcard
play_doc_DATA = dominion.info info_intro refcard
play_docdir = $(libdir)/dominion/doc

# the next rule is to create certain subdirectories that don't really get
# any files put in them by default
install-data-local:
	-$(mkinstalldirs) $(libdir)/dominion/exec
	-$(mkinstalldirs) $(libdir)/dominion/options
	-$(mkinstalldirs) $(libdir)/dominion/params
	-$(mkinstalldirs) $(libdir)/dominion/mail
	-$(mkinstalldirs) $(libdir)/dominion/news

man_MANS = $(srcdir)/dominion.6
EXTRA_DIST = MACHINES TASKS dominion.tex gm.tex dominion.info gm.info \
	tabular.sty latexinfo.sty addnews.sh make-new-world.sh $(DATA) \
	$(man_MANS) dominion.spec

#CFLAGS = -DVERSION=\"$(VERSION)\" -DDEF_LIBDIR=\"$(libdir)/dominion\" -g
AM_CFLAGS = -DDEF_LIBDIR=\"$(libdir)/dominion\" -DPREFIX=\"@prefix@\" -g 

gm.info: gm.tex
	echo "You cannot make gm.info from this Makefile"
	echo "Please contact the Dominion authors, or get the latexinfo"
	echo "package."
#dominion.info: dominion.tex
#	echo "You cannot make dominion.info from this Makefile"
#	echo "Please contact the Dominion authors, or get the latexinfo"
#	echo "package."

gm.dvi: gm.tex
	latex gm.tex
	latex gm.tex		# run latex twice to get the indices right
#dominion.dvi: dominion.tex
#	latex dominion.tex
#	latex dominion.tex	# run latex twice to get the indices right

gm.ps: gm.dvi
	dvips gm.dvi
#dominion.ps: dominion.dvi
#	dvips dominion.dvi

# this rule makes a new world; [FIXME] it should be replaced with a
# shell script eventually
new-world: all install
	@echo
	@echo 'all data cleared, and lib files installed'
	@echo
	./dom_make -d $(libdir)/dominion
	@echo 'newsgroup ' News ' created automatically'
	-mkdir $(libdir)/dominion/news/News
	@echo News ' 1 0 0' > $(libdir)/dominion/news/groups
	/bin/sh ./addnews.sh $(libdir)/dominion/news groups
