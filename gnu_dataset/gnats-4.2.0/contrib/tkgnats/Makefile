#################################################
#################################################
#### Section 1. Manual Configuration section ####
#################################################
#################################################

# WISHPROG can be a full path or just program name.
WISHPROG	= wish

# Set GNATS=network if you _are_ using the GNATS daemon "gnatsd".
# Set GNATS=local   if you're going to access disk files directly.
GNATS		= network

# Set CHECK_CFG=no if GNATS=local but GNATS is not actually installed yet.
CHECK_CFG	= yes

########## TkGnats installation locations

prefix		= /usr/local

########## tkgnats executable location
BINDIR		= $(prefix)/bin
########## tkgnats.1 man directory
MANDIR		= $(prefix)/man
########## TkGnats is installed here
TKGNATSLIB	= $(prefix)/share/tkgnats4

########## Existing GNATS installation locations (required if _not_ using GNATS daemon)

########## location of GNATS pr-edit/npr-edit programs
LIBEXECDIR	= $(prefix)/libexec
########## location of GNATS database
GNATS_ROOT	= $(prefix)/share/gnats/gnats-db

################################################################################
### If GNATS=network, the following variables are not used.                  ###
################################################################################

#SUBMITTER	= YourSubmitterSite
#GNATS_ADDR	= bugs@bugsrus.com
#GNATS_USER	= gnats


# include local overrides so that CVS stays pure.
-include Makefile.local

#########################################################################
#########################################################################
### Section 2. You shouldn't have to change anything below this point ###
#########################################################################
#########################################################################

VERSION		= tkgnats-4.0-beta1

MAKEPATH	= mkdir -p
SED		= sed

GNATS_ROOT_REPL =set TkGnats(GNATS_ROOT) $(GNATS_ROOT); \#\#GNATS_ROOT\#\#
GNATS_ROOT_SED  =-e "s,^.*\#\#GNATS_ROOT\#\#,$(GNATS_ROOT_REPL),"

SUBMITTER_REPL  =set TkGnats(SUBMITTER) $(SUBMITTER); \#\#SUBMITTER\#\#
SUBMITTER_SED	=-e "s,^.*\#\#SUBMITTER\#\#,$(SUBMITTER_REPL),"

GNATS_ADDR_REPL =set TkGnats(GNATS_ADDR) $(GNATS_ADDR); \#\#GNATS_ADDR\#\#
GNATS_ADDR_SED	=-e "s,^.*\#\#GNATS_ADDR\#\#,$(GNATS_ADDR_REPL),"

GNATS_USER_REPL =set TkGnats(GNATS_USER) $(GNATS_USER); \#\#GNATS_USER\#\#
GNATS_USER_SED	=-e "s,^.*\#\#GNATS_USER\#\#,$(GNATS_USER_REPL),"

LIBEXECDIR_REPL	=set TkGnats(GNATS_LIBEXECDIR) $(LIBEXECDIR); \#\#LIBEXECDIR\#\#
LIBEXECDIR_SED	=-e "s,^.*\#\#LIBEXECDIR\#\#,$(LIBEXECDIR_REPL),"

TKGNATSLIB_REPL =set TkGnats(lib) $(TKGNATSLIB); \#\#TKGNATSLIB\#\#
TKGNATSLIB_SED	=-e "s,^.*\#\#TKGNATSLIB\#\#,$(TKGNATSLIB_REPL),"

WISH_SED        =-e "3s,^exec wish ,exec $(WISHPROG) ,"

T_LIB	= _tlib
T_BIN	= _tbin

tkgnatsbin= \
 $(T_BIN)/tkgnats

xbmfiles= \
 $(T_LIB)/tkeditpr.xbm \
 $(T_LIB)/tkgnats.xbm \
 $(T_LIB)/tkquerypr.xbm \
 $(T_LIB)/tksendpr.xbm

tkgnatslib= \
 $(T_LIB)/tkgnats.config \
 $(T_LIB)/tkeditpr.tcl \
 $(T_LIB)/tkgnats.doc \
 $(T_LIB)/tkprdatesel.tcl \
 $(T_LIB)/tkpr_library.tcl \
 $(T_LIB)/tkprfolder.tcl \
 $(T_LIB)/tkprfont.tcl \
 $(T_LIB)/tkprhelp.tcl \
 $(T_LIB)/tkprprint.tcl \
 $(T_LIB)/tkprsort.tcl \
 $(T_LIB)/tkquerypr.tcl \
 $(T_LIB)/tksendpr.tcl \
 $(T_LIB)/tkviewpr.tcl \
 $(T_LIB)/CHANGES \
 $(T_LIB)/VERSION \
 $(xbmfiles)

manfiles= tkgnats.1

all: check_cfg dirs $(tkgnatslib) $(tkgnatsbin)

# WARNING: the tests below involving GNATS_ADDR and SUBMITTER
#          are very sensitive to having quotes (or not) with various
#          bourne/bash shells. Don't mess with them unless they
#          fail. They are the way they are to make it work for
#          /bin/sh on Solaris 2.5 and IRIX64 6.2 and bash on Linux.
#          This is all due to the nested backticks and single and double quotes.

check_cfg:
	@echo " "
	@cat VERSION
	@echo " "
	@if [ ! $(GNATS) = network -a $(CHECK_CFG) = yes ]; then \
	  rm -f /tmp/tkgnats.errors; \
	  echo "Checking the Makefile configuration variables for your GNATS setup..."; \
	  echo "---------------------------------------------------------------------"; \
	  echo " "; \
	  if [ ! -d $(LIBEXECDIR)/gnats ]; then \
	    echo "*** Error: Cannot find '$(LIBEXECDIR)/gnats'."; \
	    touch /tmp/tkgnats.errors; \
	  else \
	    echo \'$(LIBEXECDIR)\' is the configured path for GNATS LIBEXECDIR.; \
	  fi; \
	  if [ x$(SUBMITTER) = "x" ]; then \
	    echo "    Set SUBMITTER in this file to the name of your site. "; \
	    echo "    GNATS and TkGnats use it as your Submitter-Id."; \
	    touch /tmp/tkgnats.errors; \
	  else \
	    echo \'$(SUBMITTER)\' is the configured name for SUBMITTER.; \
	  fi; \
	  if [ x$(GNATS_ADDR) = "x" ]; then \
	    echo "    Set GNATS_ADDR in this file to the email address that gnats"; \
	    echo "    bugs are supposed to be sent to. It's probably missing due to"; \
	    echo "    an old bug in the GNATS installation Makefile."; \
	    touch /tmp/tkgnats.errors; \
	  else \
	    echo \'$(GNATS_ADDR)\' is the configured mail address for GNATS_ADDR.; \
	  fi; \
	  if [ x$(GNATS_USER) = "x" ]; then \
	    echo "    Set GNATS_USER in this file to the gnats administrator user id."; \
	    touch /tmp/tkgnats.errors; \
	  else \
	    echo \'$(GNATS_USER)\' is the configured user id for GNATS_USER.; \
	  fi; \
	  if [ -f /tmp/tkgnats.errors ]; then \
	    echo "*** Errors have been detected. Make aborted."; \
	    echo "*** If you don't have GNATS installed locally, run make with CHECK_CFG=no."; \
	    rm -f /tmp/tkgnats.errors; \
	    exit 1; \
	  fi; \
	  echo " "; \
	  echo "Done checking GNATS configuration."; \
	  echo "----------------------------------"; \
	  echo " "; \
	elif [ ! $(GNATS) = network ]; then \
	  echo "By-passed checking GNATS configuration."; \
	  echo "---------------------------------------"; \
	  echo " "; \
	  echo \'$(LIBEXECDIR)\' is the configured path for GNATS LIBEXECDIR.; \
	  echo \'$(SUBMITTER)\' is the configured name for SUBMITTER.; \
	  echo \'$(GNATS_ADDR)\' is the configured mail address for GNATS_ADDR.; \
	  echo \'$(GNATS_USER)\' is the configured user id for GNATS_USER.; \
	  echo " "; \
	fi

uninstall: uninstall_tkgnatsbin uninstall_tkgnatslib uninstall_man

uninstall_tkgnatsbin:
	for f in $(tkgnatsbin); do \
	    rm -if "$(BINDIR)/`basename $$f`"; \
	done

uninstall_tkgnatslib:
	for f in $(tkgnatslib); do \
	    rm -if "$(TKGNATSLIB)/`basename $$f`"; \
	done

uninstall_man:
	for f in $(manfiles); do \
	    rm -if $(MANDIR)/man1/$$f; \
	done

clean:
	rm -rf $(T_LIB) $(T_BIN) *~

dirs:
	@-$(MAKEPATH) $(T_LIB)
	@-$(MAKEPATH) $(T_BIN)

install: all install_tkgnatsbin install_tkgnatslib

install_tkgnatsbin:
	-$(MAKEPATH) $(BINDIR)
	cp -p $(T_BIN)/* $(BINDIR)/

install_tkgnatslib:
	-$(MAKEPATH) $(TKGNATSLIB)
	cp -p $(T_LIB)/* $(TKGNATSLIB)/
	-$(MAKEPATH) $(TKGNATSLIB)/print
	-$(MAKEPATH) $(TKGNATSLIB)/query
	-$(MAKEPATH) $(TKGNATSLIB)/sort
	cp -p `find print \( -type f -print \) -or \( -name CVS -prune \)` $(TKGNATSLIB)/print/
	cp -p `find query \( -type f -print \) -or \( -name CVS -prune \)` $(TKGNATSLIB)/query/
	cp -p `find sort \( -type f -print \) -or \( -name CVS -prune \)`  $(TKGNATSLIB)/sort/

install_man:
	-$(MAKEPATH) $(MANDIR)
	-$(MAKEPATH) $(MANDIR)/man1
	@chmod 664 $(manfiles)
	for f in $(manfiles); do \
	    cp -p $$f $(MANDIR)/man1/$$f; \
	done

$(T_BIN)/tkgnats: tkgnats Makefile
	$(SED) $(WISH_SED) $(TKGNATSLIB_SED) <tkgnats >$@
	@chmod 775 $@

$(T_LIB)/CHANGES: CHANGES
	cp -p CHANGES $@
	@chmod 664 $@

$(T_LIB)/VERSION: VERSION
	cp -p VERSION $@
	@chmod 664 $@

$(T_LIB)/tkeditpr.tcl: tkeditpr.tcl Makefile
	cp -p tkeditpr.tcl $@
	@chmod 664 $@

$(T_LIB)/tkviewpr.tcl: tkviewpr.tcl Makefile
	cp -p tkviewpr.tcl $@
	@chmod 664 $@

$(T_LIB)/tksendpr.tcl: tksendpr.tcl Makefile
	cp -p tksendpr.tcl $@
	@chmod 664 $@

$(T_LIB)/tkquerypr.tcl: tkquerypr.tcl Makefile
	cp -p tkquerypr.tcl $@
	@chmod 664 $@

$(T_LIB)/tkprdatesel.tcl: tkprdatesel.tcl Makefile
	cp -p tkprdatesel.tcl $@
	@chmod 664 $@

$(T_LIB)/tkprfolder.tcl: tkprfolder.tcl Makefile
	cp -p tkprfolder.tcl $@
	@chmod 664 $@

$(T_LIB)/tkprfont.tcl: tkprfont.tcl Makefile
	cp -p tkprfont.tcl $@
	@chmod 664 $@

$(T_LIB)/tkpr_library.tcl: tkpr_library.tcl Makefile
	cp -p tkpr_library.tcl $@
	@chmod 664 $@

$(T_LIB)/tkprprint.tcl: tkprprint.tcl Makefile
	cp -p tkprprint.tcl $@
	@chmod 664 $@

$(T_LIB)/tkprsort.tcl: tkprsort.tcl Makefile
	cp -p tkprsort.tcl $@
	@chmod 664 $@

$(T_LIB)/tkprhelp.tcl: tkprhelp.tcl Makefile
	cp -p tkprhelp.tcl $@
	@chmod 664 $@

$(T_LIB)/tkgnats.doc: tkgnats.doc Makefile
	cp -p tkgnats.doc $@
	@chmod 664 $@

# Again, the following ugliness is intentional and works on Linux, Solaris 2.5 and IRIX64 6.2.

$(T_LIB)/tkgnats.config: tkgnats.config Makefile
	@if [ ! $(GNATS) = network ]; then \
	  tempsubm=$(SUBMITTER); \
	  tempsubmrepl="set TkGnats(SUBMITTER) $$tempsubm; \#\#SUBMITTER\#\#"; \
	  tempsubmsed='s,^.*\#\#SUBMITTER\#\#,'$$tempsubmrepl,; \
	  tempaddr=$(GNATS_ADDR); \
	  tempaddrrepl="set TkGnats(GNATS_ADDR) $$tempaddr; \#\#GNATS_ADDR\#\#"; \
	  tempaddrsed='s,^.*\#\#GNATS_ADDR\#\#,'$$tempaddrrepl,; \
	  tempuser=$(GNATS_USER); \
	  tempuserrepl="set TkGnats(GNATS_USER) $$tempuser; \#\#GNATS_USER\#\#"; \
	  tempusersed='s,^.*\#\#GNATS_USER\#\#,'$$tempuserrepl,; \
	  $(SED) $(LIBEXECDIR_SED) \
	      $(GNATS_ROOT_SED) \
	      -e "$$tempsubmsed" \
	      -e "$$tempaddrsed" \
	      -e "$$tempusersed" \
	     <tkgnats.config >$@; \
	  echo $(SED) $(LIBEXECDIR_SED) \
	      $(GNATS_ROOT_SED) \
	      -e "$$tempsubmsed" \
	      -e "$$tempaddrsed" \
	      -e "$$tempusersed" \
	     \<tkgnats.config \>$@; \
	else \
	  echo cp -p tkgnats.config $@; \
	  cp -p tkgnats.config $@; \
	fi;
	@chmod 664 $@

# The following was OK on my Linux system but failed on Solaris and IRIX64.

#$(T_LIB)/tkgnats.config: tkgnats.config Makefile
#	$(SED) $(LIBEXECDIR_SED) \
#	    $(GNATS_ROOT_SED) \
#	    $(SUBMITTER_SED) \
#	    $(GNATS_ADDR_SED) \
#	    $(GNATS_USER_SED) \
#	   <tkgnats.config >$@

$(T_LIB)/tkgnats.xbm: tkgnats.xbm Makefile
	cp -p tkgnats.xbm $@
	@chmod 664 $@

$(T_LIB)/tkquerypr.xbm: tkquerypr.xbm Makefile
	cp -p tkquerypr.xbm $@
	@chmod 664 $@

$(T_LIB)/tksendpr.xbm: tksendpr.xbm Makefile
	cp -p tksendpr.xbm $@
	@chmod 664 $@

$(T_LIB)/tkeditpr.xbm: tkeditpr.xbm Makefile
	cp -p tkeditpr.xbm $@
	@chmod 664 $@

package: clean
	rm -f VERSION
	echo "This is version ${VERSION} and was made by $$LOGNAME@`hostname` on" > VERSION
	date >> VERSION
	echo untarit > /tmp/tkgnpkg.exclude
	dir=`pwd | sed -e "s:.*/::g"`; \
	fil=$$dir.tar.gz; \
	rm -f $$fil; \
	rm -rf _tbin _tlib $$dir.zip; \
	cd ..; \
	tar -cvf - -X /tmp/tkgnpkg.exclude $$dir | gzip -9c > $$fil; \
	rm -rf /tmp/tkgnpkg.exclude; \
	mv $$fil $$dir; \
	cd $$dir; \
	zip -rl9 $$dir.zip * -x *.gz untarit; \
	ls  -l $$fil $$dir.zip

sed_check:
	(cd $(T_BIN); fgrep usr *) >/tmp/cfg_check.bin
	(cd $(T_LIB); fgrep usr *) >/tmp/cfg_check.lib
	more /tmp/cfg_check.bin /tmp/cfg_check.lib

#
# do not use these rules !!! these are for tkgnats development only !!
#
__all:
	make 'SUBMITTER=calgary' 'GNATS_ADDR=bugs' all
__check:
	make 'SUBMITTER=calgary' 'GNATS_ADDR=bugs' check_cfg
__install:
	make 'SUBMITTER=calgary' 'GNATS_ADDR=bugs' install
