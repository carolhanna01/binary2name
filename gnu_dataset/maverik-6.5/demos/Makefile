
SHELL=/bin/sh

all:	demos

demos:
	if [ -d LegibleCity ] ; then cd LegibleCity ; make ; fi
	if [ -d EscapeCity ]; then cd EscapeCity ; make ; fi
	if [ -d AIGLab ]; then cd AIGLab ; make ; fi

clean:
	rm -f *.bck *.*~ core
	if [ -d LegibleCity ] ; then cd LegibleCity ; make clean ; fi
	if [ -d EscapeCity ]; then cd EscapeCity ; make clean ; fi
	if [ -d AIGLab ]; then cd AIGLab ; make clean ; fi

distclean: clean
	if [ -d LegibleCity ] ; then cd LegibleCity ; make distclean ; fi
	if [ -d EscapeCity ]; then cd EscapeCity ; make distclean ; fi
	if [ -d AIGLab ]; then cd AIGLab ; make distclean ; fi
