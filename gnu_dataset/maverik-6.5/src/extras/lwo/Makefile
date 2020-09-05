
LWO_OBJS = mav_lwo.o mav_lwoinit.o

../../../lib/libmav_lwo.so: ${LWO_OBJS}
	${LD} -o libmav_lwo.so ${LWO_OBJS}
	mv libmav_lwo.so ../../../lib

CFLAGS= -I../../../incl ${CFLAG} ${OPENGLINCL}

.PHONY: clean distclean

clean:
	rm -f ${LWO_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../../lib/libmav_lwo.so
