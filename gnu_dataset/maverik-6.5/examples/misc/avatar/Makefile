
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmav_avatar -lmaverik -lm

all:	avatar

avatar:	avatar.o
	${CC} avatar.o -o avatar ${LIBS}

.PHONY: clean distclean

clean:
	rm -f *.o *.bck core

distclean: clean
	rm -f avatar
