# src/Makefile.  Generated from Makefile.in by configure.

      srcdir = .
      prefix = /usr/local
      bindir = $(prefix)/bin

       SHELL = /bin/sh
          CC = gcc
       SLIBS =  -lX11 -lm -lXext
     INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
STD_INCLUDES = \
	-I. -I$(srcdir) \
	-I$(srcdir)/../V/lib \
	-I$(srcdir)/../dis \
	-I../dis
      TOPDIR = ..
 CLEAN_THESE = flight-state black_box_input black_box_output bitmap.h TAGS tags
  MAKEDEPEND = makedepend

   PROGRAMS = acm
    DEFINES = -DSRCDIR=\"$(srcdir)\" -DACM_LIBRARY=\"$(prefix)/share/acm/\"
     CFLAGS = $(DEFINES) -g -O2 -DPACKAGE_NAME=\"FULL-PACKAGE-NAME\" -DPACKAGE_TARNAME=\"full-package-name\" -DPACKAGE_VERSION=\"VERSION\" -DPACKAGE_STRING=\"FULL-PACKAGE-NAME\ VERSION\" -DPACKAGE_BUGREPORT=\"BUG-REPORT-ADDRESS\" -DPACKAGE_URL=\"\" -DPACKAGE=\"full-package-name\" -DVERSION=\"VERSION\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DHAVE_ARPA_INET_H=1 -DHAVE_FCNTL_H=1 -DHAVE_FLOAT_H=1 -DHAVE_MEMORY_H=1 -DHAVE_NETDB_H=1 -DHAVE_NETINET_IN_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_STRINGS_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_IOCTL_H=1 -DHAVE_SYS_PARAM_H=1 -DHAVE_SYS_SOCKET_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_TERMIOS_H=1 -DHAVE_UNISTD_H=1 -DHAVE_FORK=1 -DHAVE_VFORK=1 -DHAVE_WORKING_VFORK=1 -DHAVE_WORKING_FORK=1 -DHAVE_STDLIB_H=1 -DHAVE_MALLOC=1 -DHAVE_STDLIB_H=1 -DHAVE_REALLOC=1 -DHAVE_ALARM=1 -DHAVE_BZERO=1 -DHAVE_GETHOSTBYNAME=1 -DHAVE_GETHOSTNAME=1 -DHAVE_GETTIMEOFDAY=1 -DHAVE_ISASCII=1 -DHAVE_MEMSET=1 -DHAVE_POW=1 -DHAVE_RINT=1 -DHAVE_SELECT=1 -DHAVE_SOCKET=1 -DHAVE_SQRT=1 -DHAVE_STRDUP=1 -DHAVE_STRPBRK=1 -DHAVE_STRRCHR=1 -DHAVE_STRTOL=1 -DHAVE_RAND=1 -DHAVE_RANDOM=1 $(STD_INCLUDES) -DHAVE_DIS
    LDFLAGS = 
       SRCS = main.c pm.c gear.c flaps.c drone.c init.c aim9m.c box.c \
	    aim120.c ccip.c \
		m61a1.c weapon.c players.c planes.c inventory.c missile.c\
		chaser.c missilec.c update.c events.c mouse.c \
		joystick.c damage.c panel.c render.c hud.c \
		hsi.c navaid.c windows.c \
		radar.c place.c transpose.c scale.c \
		interpolate.c resupply.c air.c effects.c redraw.c \
		imath.c alarm.c audio.c dis_if.c transfer.c terrain.c \
		runway.c fpage.c vlist.c browse.c

       OBJS = main.o pm.o gear.o flaps.o drone.o init.o aim9m.o box.o \
	    aim120.o ccip.o \
		m61a1.o weapon.o players.o planes.o inventory.o missile.o\
		chaser.o missilec.o update.o events.o mouse.o \
		joystick.o damage.o panel.o render.o hud.o \
		hsi.o navaid.o windows.o \
		radar.o place.o transpose.o scale.o \
		interpolate.o resupply.o air.o effects.o redraw.o \
		imath.o alarm.o audio.o dis_if.o transfer.o terrain.o \
		runway.o fpage.o vlist.o browse.o

       VLIB = ../V/lib/libV.a
     DISLIB = ../dis/lib/libdis.a
BITMAP_FILES = $(srcdir)/eng.xbm $(srcdir)/flaps0.xbm \
		$(srcdir)/flaps1.xbm $(srcdir)/flaps2.xbm \
		$(srcdir)/flaps3.xbm $(srcdir)/gearDown.xbm \
		$(srcdir)/gearTran.xbm $(srcdir)/gearUp.xbm \
		$(srcdir)/handleDn.xbm $(srcdir)/handleUp.xbm


# Programs to make tags files.
  ETAGS = etags -tw
  CTAGS = ctags -tw
tagsrcs = $(SRCS) $(srcdir)/../V/lib/*.c $(srcdir)/../dis/lib/*.c

all:	$(PROGRAMS)

acm:	$(OBJS) $(VLIB) $(DISLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(VLIB) $(DISLIB) $(SLIBS)

../V/lib/libV.a:
	cd ../V/lib; $(MAKE) -e

mostlyclean clean:
	rm -f $(PROGRAMS) *.o core *~

distclean: clean
	rm -f Makefile $(CLEAN_THESE)

realclean: distclean
	rm -f TAGS tags

install: $(PROGRAMS)
	$(INSTALL_PROGRAM) acm $(bindir)/acm

uninstall:
	rm -f $(bindir)/acm

render.o panel.o: bitmap.h

bitmap.h: $(BITMAP_FILES)
	grep '^#define' $(BITMAP_FILES) | sed -e 's/^.*://g' > $@

depend: bitmap.h
	$(MAKEDEPEND) -- $(CFLAGS) -- $(SRCS)

check test tar shar:

TAGS:	$(tagsrcs)
	$(ETAGS) $(tagsrcs)

tags:	$(tagsrcs)
	$(CTAGS) $(tagsrcs)

# DO NOT DELETE THIS LINE -- make depend depends on it.
