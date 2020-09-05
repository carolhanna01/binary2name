
AVATAR_OBJS = mav_avatar.o mav_avatarinit.o mav_animate.o mav_build.o

../../../lib/libmav_avatar.so: ${AVATAR_OBJS}
	${LD} -o libmav_avatar.so ${AVATAR_OBJS}
	mv libmav_avatar.so ../../../lib

CFLAGS= -I../../../incl ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${AVATAR_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../../lib/libmav_avatar.so
