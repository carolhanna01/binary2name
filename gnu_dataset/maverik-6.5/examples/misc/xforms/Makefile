
#XFORMSINC= /home/people/gibsons/xforms/FORMS
#XFORMSLIB= /home/people/gibsons/xforms/FORMS

XFORMSINC= /usr/X11R6/include
XFORMSLIB= /usr/X11R6/lib

CFLAGS = -I${MAV_HOME}/incl -I${XFORMSINC}
LIBS= -L${XFORMSLIB} -lforms -L${MAV_HOME}/lib -lmaverik

all:		xforms thread

xforms:		xforms.o
		${CC} xforms.o -o xforms ${LIBS}

thread:		thread.o
		${CC} thread.o -o thread ${LIBS} -lpthread

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f xforms thread
