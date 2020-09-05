#
# Process this file with automake to produce 'Makefile.in'
#
# Makefile.am: This is a base file to generate makefile for `dionysus'. This
# file is part of the `dionysus' project version 1.0.0
# Copyright (C) 2009 Jean Michel Sellier
#
# `dionysus' is a free software; you can redistribute it and/or modify it under
# the terms of GNU General Public License as published by the Free Software
# Foundation; either version 3 of the license, or(at your option) any later
# version.
#
# `dionysus' is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along
# with `dionysus'; if not, see: <http://www.gnu.org/licenses>.
#
#
# To create the configure script using this file follow these steps:
#
#  1. write Makefile.am
#  2. autoscan
#  3. aclocal
#  4. autoheader
#  5. autoconf
#  6. automake -ac
#

bin_PROGRAMS = dionysus
dionysus_SOURCES = ./src/Tcl_src/dionysus

info_TEXINFOS = doc/dionysus.texi

EXTRA_DIST = $(man_MANS) databases tests src/Makefile README COPYING INSTALL NEWS ChangeLog

dionysusdir=$(pkgdatadir)
# dionysus_DATA = databases/universal.ddb

dionysus: $(dionysus_SOURCES)
	@cp ./src/Tcl_src/dionysus dionysus
