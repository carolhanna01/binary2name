INCLUDES = -I$(top_srcdir)/src/include

bin_PROGRAMS = \
  gnunet-qt

gnunet_qt_SOURCES = \
  aboutDlg.cc \
  main.cc \
  winproc.cc \
  wndMain.cc \
  aboutDlg.h \
  main.h \
  winproc.h \
  wndMain.h

nodist_gnunet_qt_SOURCES = \
  moc_aboutDlg.cpp \
  moc_main.cpp \
  moc_wndMain.cpp \
  pixmaps.cpp \
  ui_about.h \
  ui_wndMain.h

aboutDlg.cc: ui_about.h
main.cc: ui_wndMain.h

gnunet_qt_LDADD = \
  $(top_builddir)/src/common/libgnunetqt_common.la

gnunet_qt_LDFLAGS = -lgnunetutil @LIBS@

EXTRA_DIST = \
  about.ui \
  wndMain.ui \
  ../../pixmaps/pixmaps.qrc \
  ../../pixmaps/choose-file.png \
  ../../pixmaps/clear.png \
  ../../pixmaps/clock.png \
  ../../pixmaps/close.png \
  ../../pixmaps/connected.png \
  ../../pixmaps/debug.png \
  ../../pixmaps/document-open.png \
  ../../pixmaps/document-properties.png \
  ../../pixmaps/download_small.png \
  ../../pixmaps/download.png \
  ../../pixmaps/error.png \
  ../../pixmaps/exit.png \
  ../../pixmaps/fatal.png \
  ../../pixmaps/fs.png \
  ../../pixmaps/general.png \
  ../../pixmaps/gnunet-logo-small.png \
  ../../pixmaps/gnunet-qt.png \
  ../../pixmaps/gnunet-qt-big.png \
  ../../pixmaps/gnunet-qt-logo.png \
  ../../pixmaps/info.png \
  ../../pixmaps/list-add.png \
  ../../pixmaps/list-remove.png \
  ../../pixmaps/network-status-offline.png \
  ../../pixmaps/network-status-online.png \
  ../../pixmaps/not-connected.png \
  ../../pixmaps/refresh.png \
  ../../pixmaps/search.png \
  ../../pixmaps/start.png \
  ../../pixmaps/stats.png \
  ../../pixmaps/stop.png \
  ../../pixmaps/upload_small.png \
  ../../pixmaps/upload.png \
  ../../pixmaps/warning.png \
  ../../pixmaps/welcome.png \
  ../../pixmaps/whatsthis.png

moc_%.cpp: %.h
	moc $< -o $@

pixmaps.cpp : ../../pixmaps/pixmaps.qrc
	rcc ../../pixmaps/pixmaps.qrc -o pixmaps.cpp

ui_%.h: %.ui
	uic $< -o $@

CLEANFILES = $(filter moc_%.cpp,ui_%.h,$(gnunet_qt_SOURCES)) pixmaps.cpp

