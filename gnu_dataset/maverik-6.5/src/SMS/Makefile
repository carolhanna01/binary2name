
SMS_OBJS = mav_smss.o mav_smscallbacks.o mav_objList.o mav_hbb.o mav_hbbConstruct.o

../../lib/libmav_sms.so: ${SMS_OBJS}
	${LD} -o libmav_sms.so ${SMS_OBJS}
	mv libmav_sms.so ../../lib

CFLAGS= -I../../incl ${CFLAG}

.PHONY: clean distclean

clean:
	rm -f ${SMS_OBJS}
	rm -f *.bck *.*~ core

distclean: clean
	rm -f ../../lib/libmav_sms.so
