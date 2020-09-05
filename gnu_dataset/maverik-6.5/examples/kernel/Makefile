
CFLAGS = -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmav_kernel -lmav_gfx

all:	kernel1 kernel2 kernel3 kernel4 kernel5 kernel6

kernel1:	kernel1.o
		${CC} kernel1.o -o kernel1 ${LIBS}

kernel2:	kernel2.o
		${CC} kernel2.o -o kernel2 ${LIBS}

kernel3:	kernel3.o
		${CC} kernel3.o -o kernel3 ${LIBS}

kernel4:	kernel4.o
		${CC} kernel4.o -o kernel4 ${LIBS}

kernel5:	kernel5.o
		${CC} kernel5.o -o kernel5 ${LIBS}

kernel6:	kernel6.o
		${CC} kernel6.o -o kernel6 ${LIBS}


clean:
		rm -f *.o *.bck core

distclean: clean
		rm -f kernel1 kernel2 kernel3 kernel4 kernel5 kernel6
