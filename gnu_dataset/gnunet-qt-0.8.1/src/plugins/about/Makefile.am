INCLUDES = -I$(top_srcdir)/src/include

plugindir = $(libdir)/GNUnet

plugin_LTLIBRARIES = \
  libgnunetqtmodule_about.la

libgnunetqtmodule_about_la_SOURCES = \
  about.cc about.h

nodist_libgnunetqtmodule_about_la_SOURCES = \
  ui_about.h moc_about.cpp

about.cc: ui_about.h

libgnunetqtmodule_about_la_LIBADD = \
  $(top_builddir)/src/common/libgnunetqt_common.la

libgnunetqtmodule_about_la_LDFLAGS = \
  -lgnunetutil @LIBS@ -export-dynamic -avoid-version -module

EXTRA_DIST = \
  about.ui

moc_%.cpp: %.h
	moc $< -o $@

ui_%.h: %.ui
	uic $< -o $@

CLEANFILES = $(filter moc_%.cpp,ui_%.h,$(libgnunetqtmodule_about_la_SOURCES))
