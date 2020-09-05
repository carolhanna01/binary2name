INCLUDES = -I$(top_srcdir)/src/include

lib_LTLIBRARIES = \
  libgnunetqt_common.la

libgnunetqt_common_la_SOURCES = \
  ../include/gnunet_qt_common.h \
  assert.cc \
  desktopServices.cc \
  desktopServices.h \
  pluginLoader.cc \
  gstring.cc \
  event.cc \
  eventDispatcher.cc \
  itemModel.h \
  itemModel.cc \
  menu.h \
  textEditor.cc \
  plugin.h \
  pluginInitParams.h \
  pluginLoader.h \
  event.h \
  eventDispatcher.h \
  textEditor.h

assert.cc: ui_textEditor.h

libgnunetqt_common_la_LDFLAGS = \
  -lgnunetutil -lgnunetnamespace @LIBS@

nodist_libgnunetqt_common_la_SOURCES = \
  ui_textEditor.h \
  moc_plugin.cpp \
  moc_pluginLoader.cpp \
  moc_event.cpp \
  moc_eventDispatcher.cpp \
  moc_itemModel.cpp \
  moc_textEditor.cpp

EXTRA_DIST = \
  textEditor.ui

moc_%.cpp: %.h
	moc $< -o $@

ui_%.h: %.ui
	uic $< -o $@

#ui_%.cpp: %.ui
#  uic -impl $*.h $< -o $@

CLEANFILES = $(filter moc_%.cpp,ui_%.h,$(libgnunetqt_common_la_SOURCES))
