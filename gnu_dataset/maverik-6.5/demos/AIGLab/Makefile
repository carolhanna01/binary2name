
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmav_vrad -lmaverik -lm

all:	lab 

lab:	lab.o
	${CC} lab.o -o lab ${LIBS}


clean:
	rm -f *.o *~ *.bck

distclean: clean
	rm -f lab
