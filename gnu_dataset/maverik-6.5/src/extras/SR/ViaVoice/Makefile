
SR_OBJS = mav_sr.o

../../../../lib/libmav_sr.so:	${SR_OBJS}
	${LD} -o libmav_sr.so ${SR_OBJS} -lsmapi
	cd ../../../../lib/; \
	mv ../src/extras/SR/ViaVoice/libmav_sr.so libmav_sr-ViaVoice.so; \
	rm -f libmav_sr.so; \
	ln -s libmav_sr-ViaVoice.so libmav_sr.so

CFLAGS= -I../../../../incl -I/usr/lib/ViaVoice/include ${CFLAG}

clean:
	rm -f ${SR_OBJS} *.bck *.*~ core

distclean: clean
	rm -f ../../../../lib/libmav_sr-ViaVoice.so

