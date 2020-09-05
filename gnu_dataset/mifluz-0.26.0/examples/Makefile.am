#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
include $(top_srcdir)/Makefile.config

EXTRA_DIST = t_example

TESTS = t_example

noinst_PROGRAMS = example0

#
# Standalong program : create + search
#
example0_SOURCES = example0.cc example1.cc
example0_DEPENDENCIES = $(MIFLUZLIBS)
example0_LDADD = $(MIFLUZLIBS)

pkginclude_HEADERS =  example1.h
