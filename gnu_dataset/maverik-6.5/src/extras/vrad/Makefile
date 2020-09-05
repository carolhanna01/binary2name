
VRAD_OBJS = mav_vradinit.o mav_vrad.o mav_element.o

../../../lib/libmav_vrad.so: ${VRAD_OBJS}
	${LD} -o libmav_vrad.so ${VRAD_OBJS}
	mv libmav_vrad.so ../../../lib

CFLAGS= -I../../../incl ${CFLAG} ${OPENGLINCL}

.PHONY: clean distclean

clean:
	rm -f ${VRAD_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../../lib/libmav_vrad.so
