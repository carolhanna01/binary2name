# -*-makefile-gmake-*-
# Copyright (C) 2013, 2016 Free Software Foundation, Inc.

# This file is part of GNUnited Nations.

# GNUnited Nations is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# GNUnited Nations is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNUnited Nations.  If not, see <https://www.gnu.org/licenses/>.

# config.mk.  Generated from config.mk.in by configure.

# This variable is substituted via a Makefile rule.
pkglibexecdir	:= /usr/local/libexec/gnun
pkgdatadir	:= ${datarootdir}

AWK		:= gawk
BASE64_ENCODE	:= /usr/bin/base64 --wrap=0
SED		:= /bin/sed
GREP		:= /bin/grep
EGREP		:= /bin/grep -E
MAIL		:= 
MSGATTRIB	:= /usr/bin/msgattrib
MSGATTRIB_PREV	:= yes
MSGCAT		:= /usr/bin/msgcat
MSGFMT		:= /usr/bin/msgfmt
MSGMERGE	:= /usr/bin/msgmerge
PO4A_GETTEXTIZE	:= 
PO4A_TRANSLATE	:= 
WDIFF		:= 
MULTIVIEWS	:= yes

# Force BASH as the shell.
export SHELL	:= /bin/bash

prefix		:= /usr/local
datarootdir	:= ${prefix}/share
-include ${prefix}/etc/gnun.conf
