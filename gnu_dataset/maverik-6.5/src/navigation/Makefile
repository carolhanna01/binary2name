
NAV_OBJS= mav_navigation.o mav_navigators.o mav_mousenav.o mav_keyboardnav.o

../../lib/libmav_navigation.so: ${NAV_OBJS}
	${LD} -o libmav_navigation.so ${NAV_OBJS}
	mv libmav_navigation.so ../../lib

CFLAGS= -I../../incl ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${NAV_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../lib/libmav_navigation.so
