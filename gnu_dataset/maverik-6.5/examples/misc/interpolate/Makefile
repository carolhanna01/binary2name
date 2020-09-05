
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik -lm

all:		interpolate

interpolate:	interpolate.o
		${CC} interpolate.o -o interpolate ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f interpolate

