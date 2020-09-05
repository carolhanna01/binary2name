################################################################################
##
## Top-level Makefile.am
## Processing Makefile.am with automake produces Makefile.in
##
## Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
## http://savannah.gnu.org/projects/gsegrafix
##
## This file is part of GSEGrafix, a scientific and engineering plotting program.
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
################################################################################

SUBDIRS = src

DOC_FILES = \
   AUTHORS \
   ChangeLog \
   COPYING \
   COPYING-DOCS \
   INSTALL \
   NEWS \
   README

HELP_FILES_1 = \
   gsegrafix.xhtml \
   gsegrafix.css \
   gsegrafix.xml

HELP_FILES_2 = \
   help_files/data_save.txt \
   help_files/data_C_save.txt \
   help_files/keyword_summary.odt \
   help_files/keyword_summary.pdf \
   help_files/gsegraf_plot.txt \
   help_files/gsegraf_C_plot.txt \
   help_files/sinc.txt \
   help_files/sinc_param.txt \
   help_files/sinc3d.txt \
   help_files/sinc3d_param.txt

HELP_FIGURE_FILES = \
   figures/dipole.png \
   figures/gauss.png \
   figures/gsegrafix.png \
   figures/gsegrafix-logo.png \
   figures/loghelix.png \
   figures/loglog.png \
   figures/logzcolor2d.png \
   figures/logzcontour2d.png \
   figures/logzcontour3d.png \
   figures/mandelbrot.png \
   figures/orbit.png \
   figures/sinc.png \
   figures/sinc3d.png \
   figures/symbols.png \
   figures/window.png \
   figures/color_characters.png \
   figures/colors1.png \
   figures/colors2.png \
   figures/colors3.png \
   figures/greek_letters.png \
   figures/math_symbols.png \
   figures/README

TEXINFO_FILES = \
   texinfo/color_specification.texi \
   texinfo/data_files.texi \
   texinfo/examples.texi \
   texinfo/gsegrafix.texi \
   texinfo/introduction.texi \
   texinfo/keyword_summary.texi \
   texinfo/operation.texi \
   texinfo/parameter_file.texi

LICENSE_FILES = \
   COPYING \
   COPYING-DOCS

LOGO_FILE = figures/gsegrafix-logo.png

DESKTOP_FILE = gsegrafix.desktop

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)
doc_DATA = $(DOC_FILES)

helpdir = $(datadir)/gnome/help/$(PACKAGE)/C
help_DATA = $(HELP_FILES_1) $(LICENSE_FILES)

helpfilesdir = $(datadir)/gnome/help/$(PACKAGE)/C/help_files
helpfiles_DATA = $(HELP_FILES_2)

helpfiguresdir = $(datadir)/gnome/help/$(PACKAGE)/C/figures
helpfigures_DATA = $(HELP_FIGURE_FILES)

pixmapsdir = $(datadir)/pixmaps
pixmaps_DATA = $(LOGO_FILE)

desktopdir = $(datadir)/applications
desktop_DATA = $(DESKTOP_FILE)

EXTRA_DIST = \
   COPYING-DOCS \
   gsegrafix.xhtml \
   gsegrafix.css \
   gsegrafix.xml \
   gsegrafix.desktop \
   texinfo_files \
   help_files \
   figures

