
CFLAGS = -I${MAV_HOME}/incl
LIBDIR= -L${MAV_HOME}/lib
LIBS= -lmav_tdm -lmaverik

all:	tdm tdmavatar

tdm:	tdm.o
	${CC} tdm.o -o tdm ${LIBDIR} ${LIBS}

tdmavatar:	tdmavatar.o
		${CC} tdmavatar.o -o tdmavatar ${LIBDIR} -lmav_avatar ${LIBS} -lm

.PHONY: clean distclean

clean:
	rm -f *.o *.bck core

distclean: clean
	rm -f tdm tdmavatar
