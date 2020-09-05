INCLUDES = -I$(top_srcdir)/src/include

plugindir = $(libdir)/GNUnet

plugin_LTLIBRARIES = \
  libgnunetqtmodule_fs.la

libgnunetqtmodule_fs_la_SOURCES = \
  ecrsMetaData.cc \
  ecrsuri.cc \
  downloadController.cc \
  downloadItemDelegate.cc \
  fshelper.cc \
  fs.cc \
  fs-open-uri.cc \
  fs-search.cc \
  searchSummaryController.cc \
  searchSummaryModel.cc \
  uploadController.cc \
  uploadDialog.cc \
  uploadItemDelegate.cc \
  searchController.cc \
  searchItemDelegate.cc \
  ecrsMetaData.h \
  ecrsuri.h \
  downloadController.h \
  downloadItemDelegate.h \
  fshelper.h \
  fs.h \
  fs-open-uri.h \
  fs-search.h \
  searchSummaryController.h \
  searchSummaryModel.h \
  uploadController.h \
  uploadDialog.h \
  uploadItemDelegate.h \
  searchController.h \
  searchItemDelegate.h

nodist_libgnunetqtmodule_fs_la_SOURCES = \
  moc_ecrsMetaData.cpp \
  moc_ecrsuri.cpp \
  moc_downloadController.cpp \
  moc_downloadItemDelegate.cpp \
  moc_fshelper.cpp \
  moc_fs.cpp \
  moc_fs-open-uri.cpp \
  moc_fs-search.cpp \
  moc_searchSummaryController.cpp \
  moc_searchSummaryModel.cpp \
  moc_uploadController.cpp \
  moc_uploadDialog.cpp \
  moc_uploadItemDelegate.cpp \
  moc_searchController.cpp \
  moc_searchItemDelegate.cpp \
  ui_fs.h \
  ui_fs-open-uri.h \
  ui_fs-search-result.h \
  ui_fs-upload.h \
  ui_textEditor.h

downloadController.cc: ui_fs.h
downloadController.cc: ui_fs-search-result.h
downloadController.cc: ui_fs-upload.h
downloadController.cc: ui_fs-open-uri.h

libgnunetqtmodule_fs_la_LIBADD = \
  $(top_builddir)/src/common/libgnunetqt_common.la

libgnunetqtmodule_fs_la_LDFLAGS = \
  -lextractor -lgnunetutil -lgnunetecrs -lgnunetnamespace -lgnunetfsui @LIBS@ -export-dynamic -avoid-version -module

EXTRA_DIST = \
  fs.ui \
  fs-open-uri.ui \
  fs-search-result.ui \
  fs-upload.ui \
  textEditor.ui

moc_%.cpp: %.h
	moc $< -o $@

ui_%.h: %.ui
	uic $< -o $@

CLEANFILES = $(filter moc_%.cpp,ui_%.h,$(libgnunetqtmodule_fs_la_SOURCES))
