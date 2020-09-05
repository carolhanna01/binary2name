
HUD_OBJS = mav_hud.o

../../../lib/libmav_hud.so: ${HUD_OBJS}
	${LD} -o libmav_hud.so ${HUD_OBJS}
	mv libmav_hud.so ../../../lib

CFLAGS= -I../../../incl ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${HUD_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../../lib/libmav_hud.so
