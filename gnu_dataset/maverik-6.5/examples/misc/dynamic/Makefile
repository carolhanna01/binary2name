
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik -lm

all:		dynamic

dynamic:	dynamic.o
		${CC} dynamic.o -o dynamic ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f dynamic

