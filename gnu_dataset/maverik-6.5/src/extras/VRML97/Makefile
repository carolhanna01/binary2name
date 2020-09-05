
SHELL=/bin/sh

all:
	cd dummy; make
	if [ "${VRML97COMP}" = "1" ] ; then cd CyberVRML97; make; fi


clean:
	rm -f *.bck *.*~ core
	cd dummy; make clean
	cd CyberVRML97; make clean

distclean: clean
	cd dummy; make distclean
	cd CyberVRML97; make distclean

