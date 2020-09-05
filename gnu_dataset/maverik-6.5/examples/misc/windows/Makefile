
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik

all:		wins

wins:		wins.o
		${CC} wins.o -o wins ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck *.*~ core

distclean: clean
		rm -f wins
