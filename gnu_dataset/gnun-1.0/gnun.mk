# Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013 Free Software
#   Foundation, Inc.

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
# along with GNUnited Nations. If not, see <http://www.gnu.org/licenses/>.

# TRANSLATORS: Add here your language code.  Please keep the
# alphabetical order.
TEMPLATE_LINGUAS := af ar bg ca cs de el es fa fr he id it ja ml nb nl pl \
                    pt pt-br ro ru sk sq sr ta tr uk vi zh-cn

# TRANSLATORS: Add here your language code if you want PO files with wdiffs
# to previous msgids.
FUZZY_DIFF_LINGUAS := de es fr it nl pl ru

### The variables below are edited by GNUN maintainers. ###

# List of articles for which GRACE do not apply; i.e. they are
# regenerated even if there are fuzzy strings.
no-grace-articles := home server/takeaction

# List of additional templates
extra-templates := philosophy/philosophy-menu \
		   server/skip-translations \
		   server/top-addendum

# List of articles for which VALIDATE has no full effect; i.e. the
# HTML files are never validated.
# no-validate-articles := fry/happy-birthday-to-gnu
no-validate-articles := 

ALL_DIRS :=	copyleft \
		fry \
		fun/jokes \
		gnu \
		help \
		licenses \
		philosophy \
		server \
		test

ROOT :=		keepingup \
		provide

copyleft :=	copyleft

fry :=		happy-birthday-to-gnu

fun/jokes :=	declarations

gnu :=		gnu \
		gnu-history \
		gnu-users-never-heard-of-gnu \
		initial-announcement \
		linux-and-gnu \
		rms-lisp \
		why-gnu-linux

help :=		help

licenses :=	gpl-faq \
		license-list \
		why-not-lgpl

philosophy := 	bdk \
		can-you-trust \
		categories \
		copyright-and-globalization \
		eldred-amicus \
		free-software-for-freedom \
		free-sw \
		java-trap \
		no-word-attachments \
		not-ipr \
		open-source-misses-the-point \
		philosophy \
		right-to-read \
		software-literary-patents \
		sun-in-night-time \
		why-audio-format-matters \
		why-copyleft \
		why-free \
		words-to-avoid

server :=	sitemap \
		takeaction \
		tasks

test :=	
