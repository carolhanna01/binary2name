
KERNEL_OBJS =	mav_kernel.o mav_callback.o mav_class.o mav_sms.o \
		mav_object.o mav_window.o mav_device.o mav_lists.o \
		mav_frame.o mav_palette.o mav_maths.o mav_textures.o

../../lib/libmav_kernel.so:	${KERNEL_OBJS}
	${LD} -o libmav_kernel.so ${KERNEL_OBJS} ${DLLIBS} ${PNGLIBS} -lm
	mv libmav_kernel${OPT_EXT}.so ../../lib

CFLAGS= -I../../incl ${PNGINCL} ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${KERNEL_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../lib/libmav_kernel.so
