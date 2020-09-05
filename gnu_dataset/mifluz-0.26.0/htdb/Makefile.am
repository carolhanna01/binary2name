#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
include $(top_srcdir)/Makefile.config

bin_PROGRAMS = htdb_load htdb_dump htdb_stat mifluzdump mifluzdict mifluzload

htdb_load_SOURCES = htdb_load.cc util_sig.cc
htdb_load_DEPENDENCIES = $(MIFLUZLIBS)
htdb_load_LDFLAGS = $(PROFILING)
htdb_load_LDADD = $(MIFLUZLIBS)

htdb_dump_SOURCES = htdb_dump.cc util_sig.cc
htdb_dump_DEPENDENCIES = $(MIFLUZLIBS)
htdb_dump_LDFLAGS = $(PROFILING)
htdb_dump_LDADD = $(MIFLUZLIBS)

htdb_stat_SOURCES = htdb_stat.cc util_sig.cc
htdb_stat_DEPENDENCIES = $(MIFLUZLIBS)
htdb_stat_LDFLAGS = $(PROFILING)
htdb_stat_LDADD = $(MIFLUZLIBS)

mifluzdump_SOURCES = mifluzdump.cc
mifluzdump_DEPENDENCIES = $(MIFLUZLIBS)
mifluzdump_LDFLAGS = $(PROFILING)
mifluzdump_LDADD = $(MIFLUZLIBS)

mifluzdict_SOURCES = mifluzdict.cc
mifluzdict_DEPENDENCIES = $(MIFLUZLIBS)
mifluzdict_LDFLAGS = $(PROFILING)
mifluzdict_LDADD = $(MIFLUZLIBS)

mifluzload_SOURCES = mifluzload.cc
mifluzload_DEPENDENCIES = $(MIFLUZLIBS)
mifluzload_LDFLAGS = $(PROFILING)
mifluzload_LDADD = $(MIFLUZLIBS)

noinst_HEADERS = util_sig.h

diff:
	rm -fr 1.tmp ; mkdir 1.tmp
	cp *.cc 1.tmp
	cd 1.tmp ; \
	perl -pi -e 's/CDB_//g' *.cc ; \
	for i in stat dump load ; do diff -c ../../../db-3.0.55/*/db_$$i.c htdb_$$i.cc  ; done
