srcdir = .

  
  OBJS = \
  	disx.o \
	event.o \
	entity.o \
	sitename.o \
	register.o \
	lookup.o \
	simmgr_xdr.o \
	basic.o \
	parse.o \
	io.o \
	free.o \
	sizes.o \
	globe.o\
	datum.o \
	dr.o \
	datum_table.o \
	dis_xdr.o \
	dis_xdr1.o \
	dis_xdr2.o \
	xdr_umem.o \
	xdr_var_array.o

  SRCS = \
  	$(srcdir)/disx.c \
	$(srcdir)/event.c \
	$(srcdir)/entity.c \
	$(srcdir)/sitename.c \
	$(srcdir)/register.c \
	$(srcdir)/lookup.c \
	simmgr_xdr.c \
	$(srcdir)/basic.c \
	$(srcdir)/parse.c \
	$(srcdir)/io.c \
	$(srcdir)/free.c \
	$(srcdir)/sizes.c \
	$(srcdir)/globe.c \
	$(srcdir)/datum.c \
	$(srcdir)/dr.c \
	$(srcdir)/datum_table.c \
	dis_xdr.c \
	$(srcdir)/dis_xdr1.c \
	$(srcdir)/dis_xdr2.c \
	$(srcdir)/xdr_umem.c \
	$(srcdir)/xdr_var_array.c
 DISGEN = ../disgen/disgen
 RPCGEN = rpcgen
#CFLAGS = -I$(srcdir)/.. -I. -I.. -I../dis -DNPSNET_COMPAT -g -O2 -DPACKAGE_NAME=\"FULL-PACKAGE-NAME\" -DPACKAGE_TARNAME=\"full-package-name\" -DPACKAGE_VERSION=\"VERSION\" -DPACKAGE_STRING=\"FULL-PACKAGE-NAME\ VERSION\" -DPACKAGE_BUGREPORT=\"BUG-REPORT-ADDRESS\" -DPACKAGE_URL=\"\" -DPACKAGE=\"full-package-name\" -DVERSION=\"VERSION\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DHAVE_ARPA_INET_H=1 -DHAVE_FCNTL_H=1 -DHAVE_FLOAT_H=1 -DHAVE_MEMORY_H=1 -DHAVE_NETDB_H=1 -DHAVE_NETINET_IN_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_STRINGS_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_IOCTL_H=1 -DHAVE_SYS_PARAM_H=1 -DHAVE_SYS_SOCKET_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_TERMIOS_H=1 -DHAVE_UNISTD_H=1 -DHAVE_FORK=1 -DHAVE_VFORK=1 -DHAVE_WORKING_VFORK=1 -DHAVE_WORKING_FORK=1 -DHAVE_STDLIB_H=1 -DHAVE_MALLOC=1 -DHAVE_STDLIB_H=1 -DHAVE_REALLOC=1 -DHAVE_ALARM=1 -DHAVE_BZERO=1 -DHAVE_GETHOSTBYNAME=1 -DHAVE_GETHOSTNAME=1 -DHAVE_GETTIMEOFDAY=1 -DHAVE_ISASCII=1 -DHAVE_MEMSET=1 -DHAVE_POW=1 -DHAVE_RINT=1 -DHAVE_SELECT=1 -DHAVE_SOCKET=1 -DHAVE_SQRT=1 -DHAVE_STRDUP=1 -DHAVE_STRPBRK=1 -DHAVE_STRRCHR=1 -DHAVE_STRTOL=1 -DHAVE_RAND=1 -DHAVE_RANDOM=1
 CFLAGS = -I$(srcdir)/.. -I. -I.. -I../dis -g -O2 -DPACKAGE_NAME=\"FULL-PACKAGE-NAME\" -DPACKAGE_TARNAME=\"full-package-name\" -DPACKAGE_VERSION=\"VERSION\" -DPACKAGE_STRING=\"FULL-PACKAGE-NAME\ VERSION\" -DPACKAGE_BUGREPORT=\"BUG-REPORT-ADDRESS\" -DPACKAGE_URL=\"\" -DPACKAGE=\"full-package-name\" -DVERSION=\"VERSION\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DHAVE_ARPA_INET_H=1 -DHAVE_FCNTL_H=1 -DHAVE_FLOAT_H=1 -DHAVE_MEMORY_H=1 -DHAVE_NETDB_H=1 -DHAVE_NETINET_IN_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_STRINGS_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_IOCTL_H=1 -DHAVE_SYS_PARAM_H=1 -DHAVE_SYS_SOCKET_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_TERMIOS_H=1 -DHAVE_UNISTD_H=1 -DHAVE_FORK=1 -DHAVE_VFORK=1 -DHAVE_WORKING_VFORK=1 -DHAVE_WORKING_FORK=1 -DHAVE_STDLIB_H=1 -DHAVE_MALLOC=1 -DHAVE_STDLIB_H=1 -DHAVE_REALLOC=1 -DHAVE_ALARM=1 -DHAVE_BZERO=1 -DHAVE_GETHOSTBYNAME=1 -DHAVE_GETHOSTNAME=1 -DHAVE_GETTIMEOFDAY=1 -DHAVE_ISASCII=1 -DHAVE_MEMSET=1 -DHAVE_POW=1 -DHAVE_RINT=1 -DHAVE_SELECT=1 -DHAVE_SOCKET=1 -DHAVE_SQRT=1 -DHAVE_STRDUP=1 -DHAVE_STRPBRK=1 -DHAVE_STRRCHR=1 -DHAVE_STRTOL=1 -DHAVE_RAND=1 -DHAVE_RANDOM=1
     CC = gcc
   PERL = perl
 RANLIB = ranlib
     AR = ar
ARFLAGS = cq

all:	libdis.a

libdis.a: ../dis/disp.h simmgr.h $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

../dis/disp.h dis_xdr.c: dis.x
	$(DISGEN) $(srcdir)/dis.x
	mv dis.h ../dis/disp.h
	mv dis_xdr.c tmp.c
	perl -p -e 's/\#include "dis.h"/#include <disp.h>/' <tmp.c >dis_xdr.c
	rm -f tmp.c

../dis/dis_xdr.h: dis_xdr.c dis_xdr1.c xdrhdr.pl
	cat dis_xdr.c $(srcdir)/dis_xdr1.c | $(PERL) $(srcdir)/xdrhdr.pl $@

simmgr.h: simmgr.x
	rm -f $@
	$(RPCGEN) -h -o $@ $(srcdir)/simmgr.x

simmgr_xdr.c: simmgr.x
	rm -f $@
	$(RPCGEN) -c -o $@ $(srcdir)/simmgr.x
	mv $@ tmp.c
	perl -p -e 's/\#include .*simmgr.*$$/#include "simmgr.h"/' <tmp.c >$@
	rm -f tmp.c

dis_xdr.o: ../dis/dis_xdr.h

datum_table.o: datumtbl.h

datumtbl.h: datumtbl.pl
	$(PERL) $(srcdir)/datumtbl.pl <$(srcdir)/../dis/datum.h >datumtbl.h

clean:
	rm -rf *.o core datumtbl.h libdis.a \
		../dis/disp.h ../dis/dis_xdr.h *.bak \
		dis_xdr.c simmgr.h simmgr_xdr.c

depend: $(SRCS)
	makedepend -- $(CFLAGS) -- $(SRCS)

distclean: clean
	rm -f Makefile

tags TAGS check test:

# DO NOT DELETE THIS LINE -- make depend depends on it.
