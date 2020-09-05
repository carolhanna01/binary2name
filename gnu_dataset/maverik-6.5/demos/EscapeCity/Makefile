
CFLAGS = -I${MAV_HOME}/incl ${OPENGLINCL}
LIBS= -L${MAV_HOME}/lib -lmaverik ${OPENGLLIBS} -lm

OBJS=	mav_escapeavatar.o city_build.o city_composite.o city_culling.o \
	city_main.o city_navigate.o city_prims.o \
	city_vcb.o city_warp.o

all:	city

city:	${OBJS}
	${CC} ${OBJS} -o city ${LIBS}


clean:
	rm -f *.o *~ *.bck

distclean: clean
	rm -f city

