
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik -lm

all:		allobjs

allobjs:	allobjs.o prims.o callbacks.o misc.o
		${CC} allobjs.o prims.o callbacks.o misc.o -o allobjs ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f allobjs

