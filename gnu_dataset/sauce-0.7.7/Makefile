########### Makefile
# Edit the settings below, if you like, and say `make install'.
#
# This file is part of SAUCE, a very picky anti-spam receiver-SMTP.
# SAUCE is Copyright (C) 1997-2001 Ian Jackson
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
#
# $Id: Makefile,v 1.22 2001/03/21 23:53:14 ian Exp $

# Edit these variables, if you want to:

prefix=/usr/local
var_base=/var
var_dir=$(var_base)/lib/sauce
log_dir=/var/log/sauce
etc_dir=/etc
tclsh=/usr/bin/tclsh8.2
var_run_dir=$(var_base)/run
fw_lockfile=$(var_run_dir)/sauce-firewall.lock

username=mail
userlist=/etc/userlist

bin_dir=$(prefix)/bin
sbin_dir=$(prefix)/sbin
share_dir=$(prefix)/share

sharelib_dir=$(share_dir)/sauce
doc_dir=$(share_dir)/doc/sauce

config_dir=$(etc_dir)/sauce
userv_dir=$(etc_dir)/userv
uservserv_dir=$(userv_dir)/services.d

INSTALL=/usr/bin/install
INSTALL_DATA=$(INSTALL) -c
INSTALL_PROGRAM=$(INSTALL) -c

# You should not need to edit anything beyond this point.
#
#

VERSION=	0.1

USERV_SERVS=	sauce-userblacklist sauce-firewall sauce-rcptpolicy

TCL_LIBS=	readlibs readconf library thread dns avf avfpool \
		smtp msgdata notifybl datastate yesmaster sconfig

TCL_USEREXECS=	sauce-bwlist sauce-setuserpolicy
TCL_SYSEXECS=	sauce sauceadmin sauce-setsyspolicy
TCL_LIBEXECS=	firewall rcpt-policy

TARGETS=	$(TCL_USEREXECS) $(TCL_SYSEXECS) $(TCL_LIBEXECS) \
		$(TCL_LIBS) $(USERV_SERVS)
DOC_FILES=	BLURB.text README.text ERRORS.text EXIM-CONFIG.text \
		INSTALL.text POLICY-CONFIG.text

all:		$(TARGETS)

install:	$(TARGETS)
		$(INSTALL_DATA) $(TCL_LIBS) $(sharelib_dir)
		$(INSTALL_PROGRAM) $(TCL_LIBEXECS) $(sharelib_dir)
		$(INSTALL_PROGRAM) $(TCL_USEREXECS) $(bin_dir)
		$(INSTALL_PROGRAM) $(TCL_SYSEXECS) $(sbin_dir)
		set -e; for f in $(USERV_SERVS); do \
			$(INSTALL_DATA) $$f $(uservserv_dir)/$$f:example; \
		done

install_doc:	$(DOC_FILES)
		$(INSTALL_DATA) $(DOC_FILES) $(doc_dir)

settings:	Makefile
		$(tclsh) setsettings.tcl >$@-n 				\
			_tclsh		"$(tclsh)"			\
			var_dir		"$(var_dir)"			\
			log_dir		"$(log_dir)"			\
			sbin_dir	"$(sbin_dir)"			\
			config_dir	"$(config_dir)"			\
			share_dir	"$(sharelib_dir)"		\
			readlibs	"$(sharelib_dir)/readlibs"	\
			username	"$(username)"			\
			userlist	"$(userlist)"			\
			fw_lockfile	"$(fw_lockfile)"		\
			tcl_lib_ext?	""
		@mv -f $@-n $@

%:		%.tcl settings
		$(tclsh) subst.tcl settings $<

%:		%.in settings
		$(tclsh) subst.tcl settings $<

clean distclean:
		rm -f settings $(TARGETS) *.new *~ ./#*# *.bak
		rm -f debian/*~ debian/#*# debian/*.bak
		rm -f db.addr-seen.main db.addr-list.log
		rm -f db.site-seen.main db.site-list.log
