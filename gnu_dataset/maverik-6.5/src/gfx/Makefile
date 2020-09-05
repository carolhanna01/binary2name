
GFX_OGLX11_OBJS= mav_gfxOpenGL.o mav_gfxWMOpenGLX11.o
GFX_OGLGTK_OBJS= mav_gfxOpenGL.o mav_gfxWMOpenGLGTK.o
GFX_OGLQT_OBJS= mav_gfxOpenGL.o mav_gfxWMOpenGLQt.o
GFX_IGLX11_OBJS= mav_gfxIrisGL.o mav_gfxWMIrisGLX11.o
GFX_NONE_OBJS= mav_gfxNone.o mav_gfxWMNone.o

CXXFLAGS= -I../../../../incl ${CXXFLAG}

all:	../../lib/libmav_gfxOpenGLX11.so ${GTKLIB} ${QTLIB} ${IRISGLLIB} ../../lib/libmav_gfxNone.so

../../lib/libmav_gfxOpenGLX11.so: ${GFX_OGLX11_OBJS}
	${LD} -o libmav_gfx.so ${GFX_OGLX11_OBJS} ${OPENGLLIBS}
	cd ../../lib; \
	mv ../src/gfx/libmav_gfx.so libmav_gfxOpenGLX11.so; \
	rm -f libmav_gfx.so; \
	ln -s libmav_gfxOpenGLX11.so libmav_gfx.so

../../lib/libmav_gfxOpenGLGTK.so: ${GFX_OGLGTK_OBJS}
	${LD} -o libmav_gfx.so ${GFX_OGLGTK_OBJS} ${GTKLIBS} ${OPENGLLIBS} 
	cd ../../lib; \
	mv ../src/gfx/libmav_gfx.so libmav_gfxOpenGLGTK.so; \
	rm -f libmav_gfx.so; \
	ln -s libmav_gfxOpenGLGTK.so libmav_gfx.so

../../lib/libmav_gfxOpenGLQt.so: ${GFX_OGLQT_OBJS}
	${LD} -o libmav_gfx.so ${GFX_OGLQT_OBJS} ${QTLIBS} ${OPENGLLIBS} 
	cd ../../lib; \
	mv ../src/gfx/libmav_gfx.so libmav_gfxOpenGLQt.so; \
	rm -f libmav_gfx.so; \
	ln -s libmav_gfxOpenGLQt.so libmav_gfx.so

../../lib/libmav_gfxIrisGLX11.so: ${GFX_IGLX11_OBJS}
	${LD} -o libmav_gfx.so ${GFX_IGLX11_OBJS} ${IRISGLLIBS} 
	cd ../../lib; \
	mv ../src/gfx/libmav_gfx.so libmav_gfxIrisGLX11.so; \
	rm -f libmav_gfx.so; \
	ln -s libmav_gfxIrisGLX11.so libmav_gfx.so

../../lib/libmav_gfxNone.so: ${GFX_NONE_OBJS}
	${LD} -o libmav_gfx.so ${GFX_NONE_OBJS} ${NONEGFXLIBS}
	mv libmav_gfx.so ../../lib/libmav_gfxNone.so



mav_gfxOpenGL.o:	mav_gfxOpenGL.c
	${CC} -I../../incl ${OPENGLINCL} ${CFLAG} -c mav_gfxOpenGL.c
mav_gfxIrisGL.o:	mav_gfxIrisGL.c
	${CC} -I../../incl ${IRISGLINCL} ${CFLAG} -c mav_gfxIrisGL.c
mav_gfxNone.o:		mav_gfxNone.c
	${CC} -I../../incl ${CFLAG} -c mav_gfxNone.c
mav_gfxWMOpenGLX11.o:	mav_gfxWMOpenGLX11.c
	${CC} -I../../incl ${OPENGLINCL} ${CFLAG} -c mav_gfxWMOpenGLX11.c
mav_gfxWMOpenGLGTK.o:	mav_gfxWMOpenGLGTK.c
	${CC} -I../../incl ${GTKINCL} ${OPENGLINCL} ${CFLAG} -c mav_gfxWMOpenGLGTK.c
mav_gfxWMOpenGLQt.o:	mav_gfxWMOpenGLQt.cxx
	${CXX} -I../../incl ${QTINCL} ${OPENGLINCL} ${CXXFLAG} -c mav_gfxWMOpenGLQt.cxx
mav_gfxWMIrisGLX11.o:	mav_gfxWMIrisGLX11.c
	${CC} -I../../incl ${IRISGLINCL} ${CFLAG} -c mav_gfxWMIrisGLX11.c
mav_gfxWMNone.o:	mav_gfxWMNone.c
	${CC} -I../../incl ${CFLAG} -c mav_gfxWMNone.c

.PHONY: clean distclean

clean:
	rm -f ${GFX_OGLX11_OBJS}
	rm -f ${GFX_OGLGTK_OBJS}
	rm -f ${GFX_OGLQT_OBJS}
	rm -f ${GFX_IGLX11_OBJS}
	rm -f ${GFX_NONE_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f   ../../lib/libmav_gfxOpenGLX11.so
	rm -f	../../lib/libmav_gfxNone.so
	rm -f   ../../lib/libmav_gfxOpenGLGTK.so
	rm -f   ../../lib/libmav_gfxOpenGLQt.so
	rm -f   ../../lib/libmav_gfxIrisGLX11.so
	rm -f 	../../lib/libmaverik.so
	rm -f 	../../lib/libmav_gfx.so


		
