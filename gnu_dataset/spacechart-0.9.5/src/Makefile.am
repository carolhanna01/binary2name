INCLUDES = -I$(top_srcdir) -I$(includedir)                \
	$(GNOME_INCLUDEDIR)                               \
	-DDATADIR=\"$(pkgdatadir)/\"                      \
	-DLOCALEDIR=\"$(datadir)/locale/\"                \
	-DSYSCONFDIR=\"$(sysconfdir)/\"                   \
	-DDOCSDIR=\"$(prefix)/share/doc/spacechart/\"

AM_YFLAGS= -d

bin_PROGRAMS = spacechart

BUILT_SOURCES = rcparser_y.h rcparser_y.c rcparser_l.c

spacechart_SOURCES = main.c gui.c map.c star.c star_catalog.c star_index.c \
	link_index.c star_selection.c star_draw_rules.c link_draw_rules.c \
	common.c star_info_dialog.c settings.c link.c link_selection.c \
	perspective.c map_widget.c star_selection_dialog.c config_dialog.c \
	star_config.c link_config.c rcparser_y.y rcparser_l.l keys.c \
	star_list_dialog.c

EXTRA_DIST = $(BUILT_SOURCES)

spacechart_LDADD = $(GNOMEUI_LIBS) $(GNOME_LIBDIR) $(INTLLIBS)