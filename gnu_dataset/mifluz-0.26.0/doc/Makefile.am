#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
# $Header: /sources/mifluz/mifluz/doc/Makefile.am,v 1.22 2002/04/29 19:14:14 loic Exp $
#

EXTRA_DIST = mifluz.html mifluz.txt \
	cmprinfo-1.png dbcompress-1.png dbcompress-2.png dbcompress-3.png \
	dbcompress-4.png drawarchi1.png drawdoc1.png drawdoc2.png drawdoc3.png \
	cmprinfo-1.eps dbcompress-4.eps drawdoc1.eps cmprinfo.obj \
	dbcompress.obj drawdoc2.dia dbcompress-1.eps drawarchi1.dia \
	drawdoc2.eps dbcompress-2.eps drawarchi1.eps drawdoc3.dia \
	dbcompress-3.eps drawdoc1.dia drawdoc3.eps

info_TEXINFOS = mifluz.texinfo

SUFFIXES = .txt

.texinfo.txt:
	( cd $(srcdir) \
	  && $(MAKEINFO)  --no-headers `echo $< | sed 's,.*/,,'` ) > $@
