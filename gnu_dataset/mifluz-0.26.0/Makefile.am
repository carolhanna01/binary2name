#
# Part of the ht://Dig package   <http://www.htdig.org/>
# Copyright (c) 1999, 2000, 2001 The ht://Dig Group
# For copyright details, see the file COPYING in your distribution
# or the GNU General Public License version 2 or later
# <http://www.gnu.org/copyleft/gpl.html>
#
include $(top_srcdir)/Makefile.config

SUBDIRS = clib db mifluz htdb search man doc test examples

DISTCLEANFILES = autom4te.cache

EXTRA_DIST = .version acconfig.h Makefile.config mifluz.conf acinclude.m4

bin_SCRIPTS = mifluz-config

pkginclude_HEADERS = config.h

dist-hook:
	find $(distdir) -depth -name CVS -print | xargs rm -fr 

install-data-local:
	mkdir -p ${DESTDIR}${localstatedir}/cache/mifluz
	@if [ ! -d ${DESTDIR}${sysconfdir} ] ; then mkdir -p ${DESTDIR}${sysconfdir} ; fi
	@if [ ! -f ${DESTDIR}${sysconfdir}/mifluz.conf ] ; \
	then a="cp ${srcdir}/mifluz.conf ${DESTDIR}${sysconfdir}/mifluz.conf" ; echo $$a ; $$a ; \
	else echo "${DESTDIR}${sysconfdir}/mifluz.conf exists, not overwritten" ; \
	fi

#
# Build source and binary rpms. As of rpm-3.0, the ~/.rpmmacros
# must contain the following line:
# %_topdir /home/loic/local/rpm
# and that /home/loic/local/rpm contains the directory SOURCES, BUILD etc.
#
# If additional configure flags are needed to build the package, add the
# following in ~/.rpmmacros
# %configure CFLAGS="%{optflags}" ./configure %{_target_platform} --prefix=%{_prefix} ${AM_CONFIGFLAGS}
# and run make rpm in the following way:
# AM_CONFIGFLAGS='--with-uri=/home/users/loic/local/RedHat-6.2' make rpm
#
VERSION = $(MAJOR_VERSION).$(MINOR_VERSION).$(MICRO_VERSION)

rpm:
	RPM_TOPDIR=`rpm --showrc | $(PERL) -n -e 'print if(s/.*_topdir\s+(.*)/$$1/)'` ; \
	cp $(srcdir)/$(PACKAGE).spec $$RPM_TOPDIR/SPECS ; \
	cp $(PACKAGE)-$(VERSION).tar.gz $$RPM_TOPDIR/SOURCES ; \
	rpm -ba --clean --rmsource $$RPM_TOPDIR/SPECS/$(PACKAGE).spec ; \
	mv $$RPM_TOPDIR/RPMS/i386/$(PACKAGE)-$(VERSION)-1.i386.rpm . ; \
	mv $$RPM_TOPDIR/SRPMS/$(PACKAGE)-$(VERSION)-1.src.rpm .

