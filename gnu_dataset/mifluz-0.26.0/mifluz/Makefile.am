#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
include $(top_srcdir)/Makefile.config

lib_LTLIBRARIES = libmifluz.la

libmifluz_la_SOURCES = \
	unac.c \
	Configuration.cc \
	Dictionary.cc \
	HtMaxMin.cc \
	List.cc \
	Object.cc \
	ParsedString.cc \
	String.cc \
	StringList.cc \
	WordBitCompress.cc \
	WordContext.cc \
	WordCursorOne.cc \
	WordDB.cc \
	WordDBCache.cc \
	WordDBCompress.cc \
	WordDBInfo.cc \
	WordDead.cc \
	WordDict.cc \
	WordKey.cc \
	WordKeyInfo.cc \
	WordList.cc \
	WordListOne.cc \
	WordListMulti.cc \
	WordMeta.cc \
	WordMonitor.cc \
	WordRecord.cc \
	WordRecordInfo.cc \
	WordReference.cc \
	WordType.cc

libmifluz_la_LDFLAGS = -version-info $(MAJOR_VERSION):$(MINOR_VERSION):$(MICRO_VERSION)
libmifluz_la_LIBADD = ../db/libdb.la ../clib/libclib.la

include_HEADERS = \
	mifluz.h

pkginclude_HEADERS = \
	unac.h \
	Configuration.h \
	Dictionary.h \
	HtMaxMin.h \
	HtTime.h \
	List.h \
	Object.h \
	ParsedString.h \
	StringList.h \
	WordBitCompress.h \
	WordContext.h \
	WordCursor.h \
	WordCursorOne.h \
	WordDB.h \
	WordDBCache.h \
	WordDBCompress.h \
	WordDBInfo.h \
	WordDead.h \
	WordDict.h \
	WordKey.h \
	WordKeyInfo.h \
	WordList.h \
	WordListOne.h \
	WordListMulti.h \
	WordMeta.h \
	WordMonitor.h \
	WordRecord.h \
	WordRecordInfo.h \
	WordReference.h \
	WordType.h \
	htString.h \
	ber.h \
	lib.h
