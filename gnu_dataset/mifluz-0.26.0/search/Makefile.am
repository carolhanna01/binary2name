#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
include $(top_srcdir)/Makefile.config

#
# To turn on verbosity use (for instance):
# VERBOSE=-vv make TESTS=t_search check
#
TESTS = t_search 

TESTS_ENVIRONMENT = $(top_srcdir)/test/test_prepare
AM_MAKEFLAGS = MAKE="$(MAKE)"

LOCAL_DEFINES = -I$(top_builddir)/db -I$(top_srcdir)/db $(PROFILING) \
	-DMIFLUZ_CACHE=\"${localstatedir}/cache/mifluz\"

EXTRA_DIST = search_parser.cxx t_search search.txt mifluz-search.conf

bin_PROGRAMS = mifluzsearch

AM_YFLAGS = -l --name-prefix search_

mifluzsearch_SOURCES = mifluzsearch.cc \
	WordExclude.cc \
	WordExcludeMask.cc \
	WordKeySemantic.cc \
	WordResults.cc \
	WordMatch.cc \
	WordSearch.cc \
	WordTree.cc \
	search_parser.yxx
mifluzsearch_DEPENDENCIES = $(MIFLUZLIBS) 
mifluzsearch_LDFLAGS = $(PROFILING)
mifluzsearch_LDADD = $(MIFLUZLIBS) 

noinst_HEADERS = WordExclude.h \
	WordExcludeMask.h \
	WordKeySemantic.h \
	WordResults.h \
	WordMatch.h \
	WordPermute.h \
	WordSearch.h \
	WordTree.h \
	mifluzsearch.h

clean-local:
	rm -fr gmon.out test __db*
	rm -f  tmpfile t_htdb.d? monitor.out 
