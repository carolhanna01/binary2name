# Generated automatically from Makefile.in by configure.
CC     = gcc
CFLAGS = -Wall -fstrength-reduce -ffast-math -pipe -fno-exceptions -Os -I/usr/include/ncurses  -I/home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/src/include
LIBS   = -lpng -lz -lm -lncurses      -lX11   -lX11   -lX11 
LFLAGS = -O
AR     = ar
RANLIB = ranlib


TLIBS= \
       lib/libui.a \
       lib/libengine.a \
       lib/libutil.a \
       lib/libfilter.a \
        lib/libuicurses.a 

DIRS= \
       filter \
       engine \
       ui \
       util \
        ui/ui-drv/curses 

PROG= /home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/bin/xaos
OBJS = $(SRCS:.c=.o)

TLIB = 


all default:	$(PROG)

/home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/bin:
	mkdir $@

/home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/src/lib:
	mkdir $@

libs:
	@for name in $(DIRS) ; do sh -c "cd $$name ; $(MAKE) all"; done

$(PROG): /home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/bin /home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/src/lib include/config.h libs
	$(CC) $(CFLAGS) $(LFLAGS) -o $@ $(TLIBS) $(LIBS)

include/config.h:
	cp include/config/config.autoconf include/config.h

clean:
	@for name in $(DIRS) ; do sh -c "cd $$name ; $(MAKE) $@"; done
	rm -f include/config.h
	rm -f $(PROG)

distclean:clean
	rm  -f Makefile

#dep:
#	rm -f .depend
#	make .depend
#
#.depend:
#	echo '# Program dependencies' >.depend
#	gcc -I svgalib $(DEFINES) -MM $(patsubst %.o,%.c,$(OBJS)) >>.depend
#
#include .depend
