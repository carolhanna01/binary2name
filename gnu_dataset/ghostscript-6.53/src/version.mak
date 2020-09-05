#    Copyright (C) 1997, 2002 artofcode LLC.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU General Public License for full details.
# 
# Everyone is granted permission to copy, modify and redistribute GNU
# Ghostscript, but only under the conditions described in the GNU General
# Public License.  A copy of this license is supposed to have been given
# to you along with GNU Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# $Id: version.mak,v 1.22.2.12 2002/02/13 06:09:54 giles Exp $
# Makefile fragment containing the current revision identification.

# Major and minor version numbers.
# MINOR0 is different from MINOR only if MINOR is a single digit.
GS_VERSION_MAJOR=6
GS_VERSION_MINOR=53
GS_VERSION_MINOR0=53
# Revision date: year x 10000 + month x 100 + day.
GS_REVISIONDATE=20020213
# Derived values
GS_VERSION=$(GS_VERSION_MAJOR)$(GS_VERSION_MINOR0)
GS_DOT_VERSION=$(GS_VERSION_MAJOR).$(GS_VERSION_MINOR)
GS_REVISION=$(GS_VERSION)
