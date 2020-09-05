# Process this file with Automake to produce Makefile.in.
# File: Makefile.am for C-Graph/src
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


bin_PROGRAMS = c-graph

c_graph_SOURCES = get-cgopt.f90 fftmod.f90 c-graph.F90 

c_graph_CPPFLAGS=-DPKGDATADIR="\"$(pkgdatadir)\""

clean-local: 
	-rm -f *.mod  *~

## There was unpredictable behaviour with parallel build of
## FORTRAN source, so disable parallel make in this directory.
.NOTPARALLEL:

