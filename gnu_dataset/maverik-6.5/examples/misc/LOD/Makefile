
CFLAGS= -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik -lm

all:		lod

lod:		lod.o
		${CC} lod.o -o lod ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f lod
