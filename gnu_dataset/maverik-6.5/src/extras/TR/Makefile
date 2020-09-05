
TR_OBJS = mav_tr.o

../../../lib/libmav_tr.so: ${TR_OBJS}
	${LD} -o libmav_tr.so ${TR_OBJS} ${TRLIBS}
	mv libmav_tr.so ../../../lib

CFLAGS= -I../../../incl ${TRINCL} ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${TR_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../../lib/libmav_tr.so
