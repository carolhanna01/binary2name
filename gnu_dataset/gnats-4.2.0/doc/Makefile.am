# Automake template file
# ------------------------------------------------------------------------------
# Copyright (C) 2005 Free Software Foundation, Inc.
#
# This file is part of GNU GNATS.
# 
# GNU GNATS is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.
# 
# GNU GNATS is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# GNU GNATS; see the file COPYING.  If not, write to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111, USA.
# ------------------------------------------------------------------------------
# Notes: version.texi is computed automatically by automake.
# Additionally, texinfo.tex is included when you run ``automake --add-missing''
# ------------------------------------------------------------------------------
include $(top_srcdir)/common.am

SUBDIRS = man

EXTRA_DIST = ChangeLog README.send-pr \
	flowchart.eps flowchart.txt flowchart.fig flowchart.png flowchart.pdf

info_TEXINFOS = gnats.texi gnats-faq.texi

gnats_TEXINFOS = emacs.texi fields.texi p-admin.texi p-inst.texi p-usage.texi \
	s-usage.texi states.texi fdl.texi

gnats_faq_TEXINFOS = gnats-faq.texi
