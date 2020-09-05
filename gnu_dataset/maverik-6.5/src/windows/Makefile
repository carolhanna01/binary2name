
WINDOWS_OBJS = mav_windows.o mav_mouse.o mav_keyboard.o mav_resize.o mav_map.o mav_crossing.o mav_expose.o

../../lib/libmav_windows.so: ${WINDOWS_OBJS}
	${LD} -o libmav_windows.so ${WINDOWS_OBJS}
	mv libmav_windows.so ../../lib

CFLAGS= -I../../incl ${CFLAG}

clean:
	rm -f ${WINDOWS_OBJS} *.bck *.*~ core

distclean: clean
	rm -f ../../lib/libmav_windows.so
