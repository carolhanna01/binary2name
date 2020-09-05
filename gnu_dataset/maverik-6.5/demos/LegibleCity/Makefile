
CFLAGS = -I${MAV_HOME}/incl ${OPENGLINCL}
LIBS= -L${MAV_HOME}/lib -lmaverik ${OPENGLLIBS} -lm

all:	legiblecity legiblecity2

legiblecity:	main.o lcprims.o build.o
		${CC} main.o lcprims.o build.o -o legiblecity ${LIBS}

legiblecity2:	lcity2.o
		${CC} lcity2.o -o legiblecity2 ${LIBS}


clean:
		rm -f *.o *~ *.bck

distclean: clean
		rm -f legiblecity legiblecity2


