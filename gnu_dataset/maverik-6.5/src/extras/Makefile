
SHELL=/bin/sh

all:
	cd TDM; make
	cd avatar; make
	cd vrad; make
	cd TR; make
	cd VRML97; make; 
	if [ -d beta ] ; then cd beta ; make ; fi
	cd lwo; make
	cd hud; make
	cd SR; make


clean:
	rm -f *.bck *.*~ core
	cd TDM; make clean
	cd avatar; make clean
	cd vrad; make clean
	cd TR; make clean
	cd VRML97; make clean
	if [ -d beta ] ; then cd beta ; make clean ; fi
	cd lwo; make clean
	cd hud; make clean
	cd SR; make clean

distclean: clean
	cd TDM; make distclean
	cd avatar; make distclean
	cd vrad; make distclean
	cd TR; make distclean
	cd VRML97; make distclean
	if [ -d beta ] ; then cd beta ; make distclean ; fi
	cd lwo; make distclean
	cd hud; make distclean
	cd SR; make distclean
