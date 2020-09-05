
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmav_vrml97 -lmav_lwo -lmaverik 

all:		viewer

viewer:		viewer.o
		${CC} viewer.o -o viewer ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f viewer

