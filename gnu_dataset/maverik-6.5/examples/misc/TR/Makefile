
CFLAGS = -I${MAV_HOME}/incl
LIBDIR= -L${MAV_HOME}/lib
LIBS= -lmav_tr -lmav_avatar -lmaverik -lm

all:	tr

tr:	tr.o
	${CC} tr.o -o tr ${LIBDIR} ${LIBS}

.PHONY: clean distclean

clean:
	rm -f *.o *.bck core

distclean: clean
	rm -f tr
