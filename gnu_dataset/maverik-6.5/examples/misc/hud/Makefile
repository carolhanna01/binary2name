
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmav_hud -lmaverik -lm

all:	hud

hud:	hud.o
	${CC} hud.o -o hud ${LIBS}

.PHONY: clean distclean

clean:
	rm -f *.o *.bck core

distclean: clean
	rm -f hud
