# Generated automatically from Makefile.in by configure.
CC     = gcc
CFLAGS = -Wall -fstrength-reduce -ffast-math -pipe -fno-exceptions -Os -I/usr/include/ncurses  -I/home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/src/include
LIBS   = -lpng -lz -lm -lncurses      -lX11   -lX11   -lX11 -lm
LFLAGS = -O
AR     = ar
RANLIB = ranlib

SRCS = ui_aa.c 

OBJS = $(SRCS:.c=.o)

TLIB = /home/carolhanna/Security_Project/gnu_dataset/xaos-3.0/src/lib/libuiaa.a


all:	$(TLIB)

$(TLIB):$(OBJS)
	rm -f $@
	$(AR) rc $@ $(OBJS)
	$(RANLIB) $@ 

clean:
	rm -f $(TLIB)
	rm -f *.[oas]
	rm -f *~
	rm -f core 

distclean:clean
	rm  Makefile

#dep:
#	rm -f .depend
#	make .depend
#
#.depend:
#	echo '# Program dependencies' >.depend
#	gcc -I svgalib $(DEFINES) -MM $(patsubst %.o,%.c,$(OBJS)) >>.depend
#
#include .depend
