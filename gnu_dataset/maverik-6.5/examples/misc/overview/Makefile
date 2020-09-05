
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik

all:		overview

overview:	overview.o
		${CC} overview.o -o overview ${LIBS} -lm

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f overview

