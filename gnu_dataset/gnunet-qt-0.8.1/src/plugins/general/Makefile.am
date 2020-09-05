INCLUDES = -I$(top_srcdir)/src/include

plugindir = $(libdir)/GNUnet

plugin_LTLIBRARIES = \
  libgnunetqtmodule_general.la

libgnunetqtmodule_general_la_SOURCES = \
  general.cc \
  startStopThread.cc \
  checkDaemonThread.cc \
  general.h \
  startStopThread.h \
  checkDaemonThread.h

nodist_libgnunetqtmodule_general_la_SOURCES = \
  moc_general.cpp \
  moc_startStopThread.cpp \
  moc_checkDaemonThread.cpp \
  ui_general.h

general.cc: ui_general.h

libgnunetqtmodule_general_la_LIBADD = \
  $(top_builddir)/src/common/libgnunetqt_common.la

libgnunetqtmodule_general_la_LDFLAGS = \
  -lgnunetutil -lgnunetgetoptionapi @LIBS@ -export-dynamic -avoid-version -module

EXTRA_DIST = \
  general.ui

moc_%.cpp: %.h
	moc $< -o $@

ui_%.h: %.ui
	uic $< -o $@

CLEANFILES = $(filter moc_%.cpp,ui_%.h,$(libgnunetqtmodule_general_la_SOURCES))
