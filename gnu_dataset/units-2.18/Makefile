# Makefile.  Generated from Makefile.in by configure.

# Makefile for units, a program for units conversion
#
# Copyright (C) 1996, 1997, 1999, 2005, 2006, 2012, 2014, 2017, 2018
# Free Software Foundation, Inc
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
#    
#
# This program was written by Adrian Mariano (adrianm@gnu.org)
#



SHELL = /bin/sh
prefix = /usr/local
exec_prefix = ${prefix}
srcdir = .

CC = gcc

INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
MKDIR_P = /bin/mkdir -p
LDFLAGS = 
LIBS= -lreadline  -lm 

HAVE_PYTHON = yes
PYTHON = /home/carolhanna/miniconda3/bin/python
HAVE_MKS = no

datarootdir = ${prefix}/share
datadir = ${datarootdir}
sharedstatedir = ${prefix}/com
bindir = ${exec_prefix}/bin
infodir = ${datarootdir}/info
mandir = ${datarootdir}/man

transform=s,x,x,

DEFS=-DUNITSFILE=\"${datarootdir}/units/definitions.units\" -DLOCALEMAP=\"${datarootdir}/units/locale_map.txt\" \
                     -DREADLINE -DSUPPORT_UTF8 -DPACKAGE_NAME=\"GNU\ units\" -DPACKAGE_TARNAME=\"units\" -DPACKAGE_VERSION=\"2.18\" -DPACKAGE_STRING=\"GNU\ units\ 2.18\" -DPACKAGE_BUGREPORT=\"adrianm@gnu.org\" -DPACKAGE_URL=\"http://www.gnu.org/software/units/\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1
CFLAGS = -g -O2 -fpic
CPPFLAGS = 
OBJECTS = units.o parse.tab.o getopt.o getopt1.o 

.PHONY: currency-units-update

.SUFFIXES:
.SUFFIXES: .c .o .rc .res .texinfo .pdf

RC = rc.exe
RES = units.res

.rc.res:
	$(RC) -fo $@ $<

.texinfo.pdf:
	$(TEXI2PDF) $<

.c.o:
	$(CC) $(DEFS) $(CPPFLAGS) $(CFLAGS) -I$(srcdir) -c $<

# %.o: %.c
#     $(CC) $(DEFS) $(CFLAGS) -I$(srcdir) -c %.c


DISTFILES = README ChangeLog units.info units.txt getopt1.c units.dvi \
   Makefile.in units.c getopt.c getopt.h definitions.units units.texinfo \
   configure.ac configure strfunc.c COPYING install-sh \
   units.man NEWS texi2man INSTALL units.pdf units_cur \
   parse.tab.c parse.y units.h locale_map.txt fdl-1.3.texi currency.units \
   unitsfile.ico unitsprog.ico units.rc icon_ms.png \
   Makefile.OS2 makeobjs.cmd README.OS2 \
   UnitsMKS.texinfo UnitsMKS.pdf setvcvars.sh \
   UnitsWin.texinfo UnitsWin.pdf winmkdirs.bat Makefile.Win


all: units units.1 units.info units_cur_inst

units.o: units.c units.h

parse.tab.c: parse.y
	bison parse.y

parse.tab.o: parse.tab.c

units: $(OBJECTS) 
	$(CC) $(CFLAGS) $(LDFLAGS)  -o units $(OBJECTS)  $(LIBS)

units_cur_inst: units_cur
	sed -e "s@outfile_name = 'currency.units'@outfile_name='${prefix}/com/units/currency.units'@"\
            -e "s@/usr/bin/python@$(PYTHON)@" \
             $(srcdir)/units_cur > units_cur_inst

install-support: definitions.units units_cur_inst currency-units-update
	$(MKDIR_P) $(DESTDIR)${datarootdir}/units/ $(DESTDIR)$(bindir) $(DESTDIR)${prefix}/com/units/
	$(INSTALL_DATA) $(srcdir)/definitions.units $(DESTDIR)${datarootdir}/units/definitions.units
	-rm -f $(DESTDIR)${datarootdir}/units/currency.units
	$(INSTALL_DATA) currency.units $(DESTDIR)${prefix}/com/units/currency.units
	if [ ${prefix}/com/units/ != ${datarootdir}/units/ ];then ln -sf ${prefix}/com/units/currency.units $(DESTDIR)${datarootdir}/units/currency.units;fi
	$(INSTALL_DATA) $(srcdir)/locale_map.txt $(DESTDIR)${datarootdir}/units/locale_map.txt
	if test $(HAVE_PYTHON) = yes; then \
	  $(INSTALL_PROGRAM) units_cur_inst $(DESTDIR)$(bindir)/`echo units_cur|sed '$(transform)'`;fi
	if test $(HAVE_MKS) = yes; then make install-mks; fi


install: units install-doc install-support
	$(MKDIR_P) $(DESTDIR)$(bindir) 
	$(INSTALL_PROGRAM) units $(DESTDIR)$(bindir)/`echo units|sed '$(transform)'`

install-strip: units install-doc install-support
	$(MKDIR_P) $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) -s units $(DESTDIR)$(bindir)/`echo units|sed '$(transform)'`

install-doc: install-man install-info

install-man: units.1
	$(MKDIR_P) $(DESTDIR)$(mandir)/man1
	$(INSTALL_DATA) units.1 $(DESTDIR)$(mandir)/man1/`echo units|sed '$(transform)'`.1

# file association and DefaultIcon
# assume that if we're running the MKS Toolkit, we're running the Korn
# shell, and can use pattern replacement
install-mks:
	$(INSTALL_DATA) $(srcdir)/unitsfile.ico $(DESTDIR)${datarootdir}/units/unitsfile.ico
	$(INSTALL_DATA) $(srcdir)/unitsprog.ico $(DESTDIR)${datarootdir}/units/unitsprog.ico
	assoc .units Units.DataFile
	vidir="$${ROOTDIR//\//\\}"; \
	    ftype Units.DataFile "\"$$vidir"'\mksnt\viw.exe" -Uc8p8 "%1"'
	registry -s -k "HKCR\\Units.DataFile" -v 'GNU units data file'
	iconfile=$(DESTDIR)${datarootdir}/units/unitsfile.ico; \
	    registry -s -k "HKCR\\Units.DataFile\\DefaultIcon" -n '' -v "$${iconfile//\//\\}"

units.man: units.texinfo texi2man
	./texi2man units.texinfo > units.man

units.1: units.man
	sed s!@DATAFILE@!${datarootdir}/units/definitions.units! $(srcdir)/units.man > units.1

install-info: units.info
	$(POST_INSTALL)
	$(MKDIR_P) $(DESTDIR)$(infodir)
	$(INSTALL_DATA) units.info $(DESTDIR)$(infodir)
# Run install-info only if it exists.
# Use `if' instead of just prepending `-' to the
# line so we notice real errors from install-info.
# We use `$(SHELL) -c' because some shells do not
# fail gracefully when there is an unknown command.
	if test -f $(DESTDIR)$(infodir)/dir; then \
	  if $(SHELL) -c 'install-info --version' >/dev/null 2>&1; then \
	    install-info --dir-file=$(DESTDIR)$(infodir)/dir \
	               $(DESTDIR)$(infodir)/units.info; \
	  else true; fi \
	else true; fi

Makefile: Makefile.in config.status
	./config.status

uninstall: 
	-rm -f $(DESTDIR)$(bindir)/`echo units|sed '$(transform)'` \
	       $(DESTDIR)$(mandir)/man1/`echo units|sed '$(transform)'`.1 \
	       $(DESTDIR)$(infodir)/units.info \
	       $(DESTDIR)${datarootdir}/units/definitions.units \
	       $(DESTDIR)${prefix}/com/units/currency.units \
	       $(DESTDIR)${datarootdir}/units/currency.units \
	       $(DESTDIR)${datarootdir}/units/locale_map.txt \
	       $(DESTDIR)${datarootdir}/units/unitsfile.ico \
	       $(DESTDIR)${datarootdir}/units/unitsprog.ico \
	       $(DESTDIR)$(bindir)/`echo units_cur|sed '$(transform)'`
	-rmdir $(DESTDIR)${datarootdir}/units/
	@if test -f $(DESTDIR)$(infodir)/dir; then \
	  echo You may need to remove units.info from $(DESTDIR)$(infodir)/dir; \
	else true; fi

clean mostlyclean: texclean
	-rm -f *.o *.res units units.dvi units.1 distname .chk units_cur_inst
	-rm -rf wwwold wwwnew

distclean: clean
	-rm -f config.* Makefile TAGS

maintainer-clean: clean
	-rm -f units.txt units.info units.pdf units.dvi \
	       UnitsMKS.pdf UnitsWin.pdf \
               units-*.tar.gz parse.tab.c currency.units

# get rid of the TeX auxilliary files
texclean:
	-rm -f units.log UnitsMKS.log UnitsWin.log \
	    *.aux *.cp *.fn *.ky *.op *.pg *.toc *.tp *.vr

currency-units-update: 
	@echo "Trying to update currency.units (will use existing file if this fails)"
	-$(srcdir)/units_cur currency.units
	if [ ! -s currency.units ]; then cp $(srcdir)/currency.units currency.units;fi

sig:
	echo units-`sed -n -e '/\#.*VERSION/s/.*"\(.*\)"/\1/gp' \
	    $(srcdir)/units.c`.tar.gz > distname
	echo 'version: 1.2' > `cat distname`.directive
	echo 'directory: units' >> `cat distname`.directive
	echo 'filename: '`cat distname` >> `cat distname`.directive
	gpg --clearsign `cat distname`.directive
	gpg -b `cat distname`
	-rm -f `cat distname`.directive distname


dist: $(DISTFILES) units
	@./units_cur
	@echo Updated currencies, running check:
	./units -f definitions.units -c
	@echo ''
	@chmod g+rw,o+r *
	@echo units-`sed -n -e '/\#.*VERSION/s/.*"\(.*\)"/\1/gp' \
	    $(srcdir)/units.c` > distname
	-@if [ -d `cat distname` ]; then rm -r `cat distname`; fi
	-@if [ -e `cat distname`.tar.gz ]; then rm `cat distname`.tar.gz; fi
	@mkdir `cat distname`
	@dst=`cat distname`; for f in $(DISTFILES); do \
	   ln $(srcdir)/$$f $$dst/$$f || { echo copying $$f; \
	          cp -p $(srcdir)/$$f $$dst/$$f ; } \
	done
	@chmod 777 `cat distname`
	@tar czf `cat distname`.tar.gz `cat distname`
	@echo `cat distname`.tar.gz created
	-@rm -rf `cat distname` distname

dvi: units.dvi

pdf: units.pdf winpdf

winpdf: UnitsWin.pdf UnitsMKS.pdf

units.dvi: units.texinfo
	$(TEXI2DVI) $(srcdir)/units.texinfo

info: units.info

units.info: units.texinfo
	$(MAKEINFO) $(srcdir)/units.texinfo

units.txt: units.1
	nroff -Tascii -man $(srcdir)/units.1 | col -b > units.txt

doc: units.dvi units.info units.txt units.pdf UnitsMKS.pdf UnitsWin.pdf

check: all
	@echo Checking units
	@./units -f $(srcdir)/definitions.units \
	      '(((square(kiloinch)+2.84m2) /0.5) meters^2)^(1|4)' m \
	    | sed -n -e 's/	\* //p' > .chk
	@if [ "`cat .chk`" = 6 ]; then echo Units seems to work; \
	   else echo Something is wrong: units failed the check: ;cat .chk; fi
	@rm -f .chk

configure: configure.ac
	autoconf

TAGS: units.c
	etags $(srcdir)/units.c $(srcdir)/parse.y

tags: units.c
	ctags $(srcdir)/units.c

smalldist: units.c units.h parse.y parse.tab.c
	echo units-`sed -n -e '/\#.*VERSION/s/.*"\(.*\)"/\1/gp' \
	    $(srcdir)/units.c` > distname
	-rm -r `cat distname` `cat distname`.tar `cat distname`.tar.gz
	tar cf `cat distname`.tar units.c units.h  parse.y  parse.tab.c\
	   getopt1.c getopt.c getopt.h
	gzip `cat distname`.tar


#
# This section attempts to update the manual on the GNU web server
# If all works correctly, it will add/remove files from the CVS tree as required.
# It assumes there are only files in the manual and manual/html_node directories. 
#

www: doc
	-rm -r wwwold wwwnew
	mkdir wwwnew
	./gendocs.sh -o wwwnew/units/manual units "GNU Units Manual"
	mkdir wwwold
	cd wwwold;CVS_RSH=ssh cvs -z3 -d:ext:adrianm@cvs.savannah.gnu.org:/webcvs/units co units
	cd wwwold/units/manual;ls > /tmp/wwwunits.listold
	cd wwwnew/units/manual;ls > /tmp/wwwunits.listnew
	cd wwwold/units/manual/html_node;ls > /tmp/wwwunits.hlistold
	cd wwwnew/units/manual/html_node;ls > /tmp/wwwunits.hlistnew
	-grep -vf /tmp/wwwunits.listold /tmp/wwwunits.listnew > /tmp/wwwunits.add
	-grep -vf /tmp/wwwunits.listnew /tmp/wwwunits.listold | grep -v CVS > /tmp/wwwunits.rm
	-grep -vf /tmp/wwwunits.hlistold /tmp/wwwunits.hlistnew > /tmp/wwwunits.html.add
	-grep -vf /tmp/wwwunits.hlistnew /tmp/wwwunits.hlistold | grep -v CVS > /tmp/wwwunits.html.rm
	rsync -Cr --del -v wwwnew/units/manual wwwold/units/
	@if [ -s /tmp/wwwunits.add ]; then cd wwwold/units/manual; cvs add `cat /tmp/wwwunits.add`; \
	       cvs commit `cat /tmp/wwwunits.add`; fi
	@if [ -s /tmp/wwwunits.rm ]; then cd wwwold/units/manual; cvs remove `cat /tmp/wwwunits.rm`; \
	       cvs commit `cat /tmp/wwwunits.rm`; fi
	@if [ -s /tmp/wwwunits.html.add ]; then cd wwwold/units/manual/html_node; \
	       cvs add `cat /tmp/wwwunits.add`; cvs commit `cat /tmp/wwwunits.add`; fi
	@if [ -s /tmp/wwwunits.html.rm ]; then cd wwwold/units/manual/html_node; \
	cvs remove `cat /tmp/wwwunits.htmlrm`; cvs commit `cat /tmp/wwwunits.rm`; fi
	cd wwwold/units; cvs commit
	-rm /tmp/wwwunits.hlistnew /tmp/wwwunits.hlistold /tmp/wwwunits.listnew /tmp/wwwunits.listold
	-rm /tmp/wwwunits.html.rm /tmp/wwwunits.html.add /tmp/wwwunits.add /tmp/wwwunits.rm
	-rm -r wwwold wwwnew


#
# Not very portable code to compile into library (written for Linux)
#

libunits.a: $(OBJECTS)
	ar -r libunits.a $^

libunits.so: $(OBJECTS)
	$(CC) $(LDFLAGS) -shared -o $@ $^

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

