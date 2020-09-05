
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik

all:		net

net:		net.o
		${CC} net.o -o net ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f net
