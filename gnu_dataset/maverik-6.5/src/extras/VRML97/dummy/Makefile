
VRML97_OBJS = mav_vrml97.o

../../../../lib/libmav_vrml97.so:	${VRML97_OBJS}
	${LD} -o libmav_vrml97.so ${VRML97_OBJS}
	cd ../../../../lib/; \
	mv ../src/extras/VRML97/dummy/libmav_vrml97.so libmav_vrml97-dummy.so; \
	rm -f libmav_vrml97.so; \
	ln -s libmav_vrml97-dummy.so libmav_vrml97.so

CFLAGS= -I../../../../incl ${CFLAG}

clean:
	rm -f ${VRML97_OBJS} *.bck *.*~ core

distclean: clean
	rm -f libmav_gfx.solibmav_vrml97.so
	rm -f ../../../../lib/libmav_vrml97-dummy.so
	
