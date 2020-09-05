# Process this file with Automake to produce Makefile.in.
# File: Makefile.am for C-Graph/doc
#
# GNU C-Graph Version 2.0.1
# December 2018
# 
# Copyright (c) 2009, 2010, 2011, 2018 Adrienne Gaye Thompson
#
# This file is part of GNU C-Graph.
#
# GNU C-Graph is free software: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License 
# (the GPL) as published by the Free Software Foundation, either 
# version 3 of the License, or (at your option) any later version.
#
# GNU C-Graph is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GPL
# for more details.
#
# GNU C-Graph includes the file COPYING, which contains a copy of the GPL
# version 3.0. You may view the GPL online at
# <http://www.gnu.org/licenses/gpl.html>.
#

info_TEXINFOS = c-graph.texi
c_graph_TEXINFOS = fdl.texi

## install-data-local and uninstall-local targets conflict with Debian.
## Uncomment these lines if desired and run "autoreconf -f -i" on
## non-Debian systems.
##
## Install .png files in system Texinfo directory for viewing in emacs.
## install-data-local:
##	$(INSTALL) c-graph-*.png $(infodir)
##
## uninstall-local:
##	rm -f $(infodir)/c-graph-*.png

# Overwrite dvi target so make distcheck won't look for .eps files.
dvi:

# Automatically make pdf.
# all-local: pdf

#install pdf 
dist_doc_DATA = c-graph.pdf


EXTRA_DIST = \
	c-graph-signals.png \
	c-graph-signals1.png \
	c-graph-signals.txt \
	c-graph-signals1.txt \
	c-graph-transforms.png \
	c-graph-transforms1.png \
	c-graph-transforms.txt \
	c-graph-transforms1.txt	\
	c-graph-convolutions.png \
	c-graph-convolutions1.png \
	c-graph-convolutions.txt \
	c-graph-convolutions1.txt

