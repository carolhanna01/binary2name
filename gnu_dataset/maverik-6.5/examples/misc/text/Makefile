
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik

all:		2Dtext 3Dtext

2Dtext:		2Dtext.o
		${CC} 2Dtext.o -o 2Dtext ${LIBS}

3Dtext:		3Dtext.o
		${CC} 3Dtext.o -o 3Dtext ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f 2Dtext 3Dtext
