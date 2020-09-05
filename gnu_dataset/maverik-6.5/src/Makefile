
SHELL=/bin/sh

all:	comp libs

comp:
	cd kernel; make
	cd gfx; make
	cd callbacks; make
	cd SMS; make
	cd windows; make
	cd navigation; make
	cd objects; make
	cd extras; make



../lib/libmaverikOpenGLX11.so: ${COBJS} ${GOBJS}
	${LD} -o libmaverik.so  ${COBJS} ${GOBJS} ${DLLIBS} ${OPENGLLIBS} ${PNGLIBS} -lm
	cd ../lib; \
	mv ../src/libmaverik.so libmaverikOpenGLX11.so; \
	rm -f libmaverik.so; \
	ln -s libmaverikOpenGLX11.so libmaverik.so	

../lib/libmaverikOpenGLGTK.so: ${COBJS} ${GOBJS}
	${LD} -o libmaverik.so  ${COBJS} ${GOBJS} ${DLLIBS} ${GTKLIBS} ${OPENGLLIBS} ${PNGLIBS} -lm
	cd ../lib; \
	mv ../src/libmaverik.so libmaverikOpenGLGTK.so; 
#	rm -f libmaverik.so; \
#	ln -s libmaverikOpenGLGTK.so libmaverik.so	

../lib/libmaverikOpenGLQt.so: ${COBJS} ${GOBJS}
	${LD} -o libmaverik.so  ${COBJS} ${GOBJS} ${DLLIBS} ${QTLIBS} ${OPENGLLIBS} ${PNGLIBS} -lm
	cd ../lib; \
	mv ../src/libmaverik.so libmaverikOpenGLQt.so;
#	rm -f libmaverik.so; \
#	ln -s libmaverikOpenGLQt.so libmaverik.so	

../lib/libmaverikIrisGLX11.so: ${COBJS} ${GOBJS}
	${LD} -o libmaverik.so  ${COBJS} ${GOBJS} ${DLLIBS} ${IRISGLLIBS} ${PNGLIBS} -lm
	cd ../lib; \
	mv ../src/libmaverik.so libmaverikIrisGLX11.so;
#	rm -f libmaverik.so; \
#	ln -s libmaverikIrisGLX11.so libmaverik.so	

../lib/libmaverikNone.so: ${COBJS} ${GOBJS}
	${LD} -o libmaverik.so  ${COBJS} ${GOBJS} ${DLLIBS} ${PNGLIBS} -lm
	mv libmaverik.so ../lib/libmaverikNone.so

libs:
	make ../lib/libmaverikOpenGLX11.so "COBJS= `find . -name "*.o" -print | grep -v "/gfx/" | grep -v "/extras/" | paste -s -`" GOBJS="gfx/mav_gfxOpenGL.o gfx/mav_gfxWMOpenGLX11.o"
	if [ -f gfx/mav_gfxWMOpenGLGTK.o ] ; then make ../lib/libmaverikOpenGLGTK.so "COBJS= `find . -name "*.o" -print | grep -v "/gfx/" | grep -v "/extras/" | paste -s -`" GOBJS="gfx/mav_gfxOpenGL.o gfx/mav_gfxWMOpenGLGTK.o" ; fi
	if [ -f gfx/mav_gfxWMOpenGLQt.o ] ; then make ../lib/libmaverikOpenGLQt.so "COBJS= `find . -name "*.o" -print | grep -v "/gfx/" | grep -v "/extras/" | paste -s -`" GOBJS="gfx/mav_gfxOpenGL.o gfx/mav_gfxWMOpenGLQt.o" ; fi
	if [ -f gfx/mav_gfxWMIrisGLX11.o ] ; then make ../lib/libmaverikIrisGLX11.so "COBJS= `find . -name "*.o" -print | grep -v "/gfx/" | grep -v "/extras/" | paste -s -`" GOBJS="gfx/mav_gfxIrisGL.o gfx/mav_gfxWMIrisGLX11.o" ; fi
	make ../lib/libmaverikNone.so "COBJS= `find . -name "*.o" -print | grep -v "/gfx/" | grep -v "/extras/" | paste -s -`" GOBJS="gfx/mav_gfxNone.o gfx/mav_gfxWMNone.o"

.PHONY: clean distclean

clean:
	rm -f *.bck *.*~ core
	cd kernel; make clean
	cd gfx; make clean	
	cd callbacks; make clean
	cd SMS; make clean
	cd windows; make clean
	cd navigation; make clean
	cd objects; make clean
	cd extras; make clean

distclean: clean
	echo "making distclean"
	rm -f ../lib/libmaverikOpenGLX11.so
	rm -f ../lib/libmaverikOpenGLGTK.so
	rm -f ../lib/libmaverikOpenGLQt.so
	rm -f ../lib/libmaverikIrisGLX11.so
	rm -f ../lib/libmaverikNone.so
	cd kernel; make distclean
	cd gfx; make distclean	
	cd callbacks; make distclean
	cd SMS; make distclean
	cd windows; make distclean
	cd navigation; make distclean
	cd objects; make distclean
	cd extras; make distclean
