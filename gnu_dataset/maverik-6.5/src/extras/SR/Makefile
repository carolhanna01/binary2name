
SHELL=/bin/sh

all:
	cd dummy; make
	if [ "${VVCOMP}" = "1" ] ; then cd ViaVoice; make; fi


clean:
	rm -f *.bck *.*~ core
	cd dummy; make clean
	cd ViaVoice; make clean

distclean: clean
	cd dummy; make distclean
	cd ViaVoice; make distclean
