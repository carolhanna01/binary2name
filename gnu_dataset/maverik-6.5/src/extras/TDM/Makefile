
TDM_OBJS = mav_tdm.o mav_cursor.o mav_view.o mav_tdmevent.o

../../../lib/libmav_tdm.so: ${TDM_OBJS}
	${LD} -o libmav_tdm.so ${TDM_OBJS}
	mv libmav_tdm.so ../../../lib

CFLAGS= -I../../../incl ${TDMINCL} ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${TDM_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../../lib/libmav_tdm.so
