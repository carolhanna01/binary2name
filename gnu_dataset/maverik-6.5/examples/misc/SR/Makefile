
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmav_sr -lmaverik

all:		sr

sr:		sr.o
		${CC} sr.o -o sr ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f sr

