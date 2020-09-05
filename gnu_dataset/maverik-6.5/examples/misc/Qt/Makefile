
CXXFLAGS= -I${MAV_HOME}/incl -I${QTDIR}/include
LIBS= -L${MAV_HOME}/lib -L${QTDIR}/lib -lmaverik -lqt

OBJS= qt.o qt_moc.o

.SUFFIXES:.cxx

.cxx.o:
	${CXX} ${CXXFLAGS} -c $<

.h.cxx:
	${QTDIR}/bin/moc -o $@ $<

all: 	qt

qt:	${OBJS}
	${CXX} ${OBJS} -o qt ${LIBS} 

.PHONY: clean distclean

clean:
	rm -f ${OBJS} qt_moc.cpp
	rm -f *.bck *.*~ core

distclean: clean
	rm -f qt
