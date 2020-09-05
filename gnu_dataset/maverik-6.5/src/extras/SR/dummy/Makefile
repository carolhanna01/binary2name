
SR_OBJS = mav_sr.o

../../../../lib/libmav_sr.so:	${SR_OBJS}
	${LD} -o libmav_sr.so ${SR_OBJS}
	cd ../../../../lib/; \
	mv ../src/extras/SR/dummy/libmav_sr.so libmav_sr-dummy.so; \
	rm -f libmav_sr.so; \
	ln -s libmav_sr-dummy.so libmav_sr.so

CFLAGS= -I../../../../incl ${CFLAG}

clean:
	rm -f ${SR_OBJS} *.bck *.*~ core

distclean: clean
	rm -f ../../../../lib/libmav_sr.so
	rm -f ../../../../lib/libmav_sr-dummy.so

