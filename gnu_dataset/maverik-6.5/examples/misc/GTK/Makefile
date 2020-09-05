
CFLAGS= -I${MAV_HOME}/incl `gtk-config --cflags`
LIBS= -L${MAV_HOME}/lib -lmaverik `gtk-config --libs`

all:		gtk

gtk:	gtk.o
	${CC} gtk.o -o gtk ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f gtk
