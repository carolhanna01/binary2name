#
# Build both the Solaris and POSIX versions of tgrep.
#
include ./makefile.master
#
SUBDIRS= \
	Common \
	AT 

#	Posix
#

all: 
	@for file in ${SUBDIRS}; do \
		echo "--- Working in $$file (building ${ARCH} version)---"; \
		(cd $$file; $(MAKECMD)) ; \
	done
	@echo "---------------- DONE ----------------------"

clean: 
	@for file in ${SUBDIRS}; do \
		echo "--- Working in $$file (building $(ARCH) version)---"; \
		(cd $$file; $(MAKECMD) clean ) ; \
	done
	@echo "---------------- DONE ----------------------"
	@rm -f *~ *%

clobber: 
	@for file in ${SUBDIRS}; do \
		echo "--- Working in $$file (building $(ARCH) version)---"; \
		(cd $$file; $(MAKECMD) clobber ) ; \
	done
	@echo "---------------- DONE ----------------------"
	@rm -f *~ *%

install: 
	@-cp Solaris/tgrep ./bin/tgrep.Solaris.$(ARCH)
	@-cp Posix/tgrep   ./bin/tgrep.Posix.$(ARCH)
	@-cp Common/tgrep.1 ./man/man1


