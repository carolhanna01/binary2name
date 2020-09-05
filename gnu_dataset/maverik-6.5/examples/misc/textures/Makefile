
CFLAGS= -ggdb -I${MAV_HOME}/incl
LIBS= -L${MAV_HOME}/lib -lmaverik

all:		textures mipmap trans

textures:	textures.o
		${CC} textures.o -o textures ${LIBS}

mipmap:		mipmap.o
		${CC} mipmap.o -o mipmap ${LIBS}

trans:		trans.o
		${CC} trans.o -o trans ${LIBS}

.PHONY: clean distclean

clean:
		rm -f *.o *bck core

distclean: clean
		rm -f mipmap textures trans
