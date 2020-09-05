srcdir = .
topdir = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib


include ../Makefile.inc
topdir=..
topincdir=../../include

RPMLIBOBJECTS=

DO_DEBUG = 

CC = gcc
CXX = g++
CPP = gcc -E
CPPFLAGS = 

ifeq ($(DO_DEBUG),)
DEBUGLIBOBJECTS = 
.c.o:
	$(CC) $(CFLAGS) -c -o $*.o  $<;
else
DEBUGLIBOBJECTS = 
.c.o:
	$(CC) $(CFLAGS) -c -o $*.o  $<; $(CC) $(CFLAGS) -S -gstabs+ -o $*.S $<
endif

LIBOBJECTS = \
	swi.o swi_common.o swi_debug.o swi_distdata.o swi_xfile.o swi_afile.o swi_base.o

LIB = swi.a

ifeq (.depend,$(wildcard .depend))
TARGET=everything
else
TARGET=depend everything
endif

all: $(TARGET)

everything: $(LIB)

$(LIB): $(LIBOBJECTS)
	$(AR) r $@ $(LIBOBJECTS)
	$(RANLIB) $@

$(PROGS): $(LIBOBJECTS)

distdir:
install-data:
install-exec:
install-html:
install-dvi:
install-pdf:
install-ps:
install-strip:
install-info:
installdirs:
installcheck:
install:
uninstall:
info:
dvi:
html:
pdf:
ps:
dist:
check:
tags:
ctags:
maintainer-clean: distclean
mostlyclean: clean

clean:
	rm -f *.S *.a *.o *~ $(PROGS) a.out core

distclean: clean
	rm -f .depend Makefile

depend:
	( > .depend )

#	($(CPP) $(CFLAGS) -M $(srcdir)/*.c > .depend; exit 0)

ifeq (.depend,$(wildcard .depend))
include .depend
endif
