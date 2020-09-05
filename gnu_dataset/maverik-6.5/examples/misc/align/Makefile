
CFLAGS = -I${MAV_HOME}/incl
LIBS   = -L${MAV_HOME}/lib -lmaverik -lm

all:		align

align:		align.o
		${CC} align.o -o align ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f align
