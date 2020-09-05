INCLUDES = -I$(top_srcdir)/src/include

plugindir = $(libdir)/GNUnet

plugin_LTLIBRARIES = \
  libgnunetqtmodule_stats.la

libgnunetqtmodule_stats_la_SOURCES = \
  statsPlugin.cc \
  statsPlugin.h \
  statsThread.cc \
  statsThread.h

nodist_libgnunetqtmodule_stats_la_SOURCES = \
  moc_statsPlugin.cpp \
  moc_statsThread.cpp \
  ui_stats.h

statsPlugin.cc: ui_stats.h

libgnunetqtmodule_stats_la_LIBADD = \
  $(top_builddir)/src/common/libgnunetqt_common.la

libgnunetqtmodule_stats_la_LDFLAGS = \
  -lgnunetutil -lgnunetstatsapi @LIBS@ -export-dynamic -avoid-version -module

EXTRA_DIST = \
  stats.ui

moc_%.cpp: %.h
	moc $< -o $@

ui_%.h: %.ui
	uic $< -o $@

CLEANFILES = $(filter moc_%.cpp,ui_%.h,$(libgnunetqtmodule_stats_la_SOURCES))
