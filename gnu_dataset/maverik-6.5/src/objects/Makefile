
OBJECTS_OBJS = 	mav_objects.o mav_box.o mav_pyramid.o mav_cylinder.o mav_cone.o \
		mav_sphere.o mav_hsphere.o mav_ellipse.o mav_hellipse.o mav_ctorus.o mav_rtorus.o \
		mav_polygon.o mav_polygonGrp.o mav_facet.o mav_rectangle.o mav_polyline.o \
		mav_text.o mav_composite.o mav_ac3d.o mav_jif.o mav_smsobj.o mav_teapot.o mav_splashDef.o

../../lib/libmav_objects.so: ${OBJECTS_OBJS}
	${LD} -o libmav_objects.so ${OBJECTS_OBJS}
	mv libmav_objects.so ../../lib

CFLAGS= -I../../incl ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${OBJECTS_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../lib/libmav_objects.so
