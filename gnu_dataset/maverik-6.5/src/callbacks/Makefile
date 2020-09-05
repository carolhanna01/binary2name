
CALLBACKS_OBJS = mav_callbacks.o mav_draw.o mav_bb.o mav_intersect.o mav_id.o mav_dump.o mav_get.o mav_clip.o

../../lib/libmav_callbacks.so: ${CALLBACKS_OBJS}
	${LD} -o libmav_callbacks.so ${CALLBACKS_OBJS}
	mv libmav_callbacks.so ../../lib

CFLAGS= -I../../incl ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${CALLBACKS_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../lib/libmav_callbacks.so
