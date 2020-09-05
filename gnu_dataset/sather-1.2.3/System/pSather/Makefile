#------------------------------->  Makefile  <--------------------------------#
#- Copyright (C) 199x by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

# Toplevel Makefile for pSather

include pSather.options 

CFLAGS=-g -Wall 
MM=$(MAKE) PSATHER='$(PSATHER)'

include Make.config

all:	$(ARCH)

Make.config:
	touch Make.config

linux: dirs
	./bin/mkconfig Make.Linux
	cd lwp ; $(MM) install
	cd am ; $(MM) install
	cd pSather ; $(MM) install

win32: dirs
	./bin/mkconfig Make.Win32
	cd lwp ; $(MM) install
	cd am ; $(MM) install
	cd pSather ; $(MM) install

sun: dirs
	./bin/mkconfig Make.SunOS4
	cd lwp ; $(MM) install
	cd am ; $(MM) install
	cd pSather ; $(MM) install

sun-no-threads: dirs
	@echo "*** This configuration does not work for pSather"
	@echo "*** but is useful for debugging reasons."
	./bin/mkconfig Make.SunOS4-no-threads
	cd lwp ; $(MM) install
	cd am ; $(MM) install

solaris: dirs
	./bin/mkconfig Make.Solaris2.4
#	cd am ; $(MM) install
	cp lib/libam_solaris.a lib/libam.a
	cd pSather ; $(MM) install

solaris-tcpip: dirs
	./bin/mkconfig Make.Solaris2.4-tcpip
#	cd am ; $(MM) install
	cp lib/libam_solaris_tcp.a lib/libam.a
	cd pSather ; $(MM) install

solaris-tcpip-no-threads: dirs
	@echo "*** This configuration does not work for pSather"
	@echo "*** but is useful for debugging reasons."
	./bin/mkconfig Make.Solaris2.4-tcpip-no-threads
	cd am ; $(MM) install ; cd ..

solaris-myrinet: dirs
	./bin/mkconfig Make.Solaris2.4-myrinet
#	$(MM) lcp.dat
#	cd am ; $(MM) install
	cp lib/libam_myrinet.a lib/libam.a
	cd pSather ; $(MM) install

solaris-myrinet-no-threads: dirs
	@echo "*** This configuration does not work for pSather"
	@echo "*** but is useful for debugging reasons."
	./bin/mkconfig Make.Solaris2.4-myrinet-no-threads
#	$(MM) lcp.dat
	cd am ; $(MM) install

solaris-no-threads: dirs
	@echo "*** This configuration does not work for pSather"
	@echo "*** but is useful for debugging reasons."
	./bin/mkconfig Make.Solaris2.4-no-threads
	cd am ; $(MM) install

lcp.dat:
	cd config/lanai/lcp ; sh -c $(COMPILE_LANAI)

meiko: dirs
	./bin/mkconfig Make.Meiko
#	cd am ; $(MM) install
	cp lib/libam_meiko.a lib/libam.a
	cd pSather ; $(MM) install

meiko-no-threads: dirs
	./bin/mkconfig Make.Meiko-no-threads
	cd am ; $(MM) install
	cd pSather ; $(MM) install

# Make a copy of the directory in "DIR". Dir has to be passed
# as an argument. Example:  make DIR=/u/fleiner/pSather/c.code copy
# DIR must already exist. It will create the directory pSather in
# DIR. It assumes that it is running in $SATHER_CONFIG/System/pSather
# 
# To safe time and space, this rule checks first to see if
# the pSather install directory is writable. If so, it will
# only install a link to it. This avoids the copy and possibly 
# recompilation too.

copy:
	@if [ ! -d "${DIR}" ]; then \
		echo "Sorry, ${DIR} does not exist, aborting" ; \
		exit 1 ; \
	fi 
	@if [ -d "${DIR}/pSather" ]; then \
		$(RM) -rf ${DIR}/pSather ; \
	fi
	@if [ -h "${DIR}/pSather" ]; then \
		$(RM) -f ${DIR}/pSather ; \
	fi
	@if [ -w Makefile ]; then \
		ln -sf ${SATHER_HOME}/System/pSather ${DIR}/pSather ; \
		if cmp ${DIR}/sather.h include/sather.h 2>&1 > /dev/null; then :; else touch Makefile; fi; \
		cp ${DIR}/*.h include ; \
	else \
		mkdir ${DIR}/pSather ; \
		mkdir ${DIR}/pSather/am ; \
		mkdir ${DIR}/pSather/lwp ; \
		mkdir ${DIR}/pSather/lib ; \
		mkdir ${DIR}/pSather/include ; \
		mkdir ${DIR}/pSather/pSather ; \
		ln -sf ${SATHER_HOME}/System/pSather/lib/libam_* ${DIR}/pSather/lib ; \
		cp ${SATHER_HOME}/System/pSather/include/am.h ${DIR}/pSather/include ; \
		cp *.options Make.config Makefile ${DIR}/pSather ; \
		cp lwp/*.[ch] lwp/Makefile ${DIR}/pSather/lwp ; \
		cp am/*.[ch] am/Makefile ${DIR}/pSather/am ; \
		cp pSather/*.[ch] pSather/Makefile ${DIR}/pSather/pSather ; \
		ln -sf ${SATHER_HOME}/System/pSather/config ${DIR}/pSather/config ; \
		ln -sf ${SATHER_HOME}/System/pSather/bin ${DIR}/pSather/bin ; \
	fi

dirs:
	[ -d lib ] || mkdir lib
	[ -d include ] || mkdir include

clean:
	rm -rf lib
	rm -rf include
#	rm -f libraries flags
	cd lwp ; $(MM) clean
	cd am ;  $(MM) clean
	cd pSather ;  $(MM) clean
