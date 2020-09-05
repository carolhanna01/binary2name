
SHELL=/bin/sh

all:	examples

examples:
	cd MPG; make
	cd misc; make
	cd kernel; make

.PHONY: clean distclean

clean:
	rm -f *.bck *.*~ core
	cd MPG; make clean
	cd misc; make clean
	cd kernel; make clean

distclean: clean
	cd MPG; make distclean
	cd misc; make distclean
	cd kernel; make distclean
