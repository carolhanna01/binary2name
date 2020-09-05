srcdir = .
topdir = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib


include ../Makefile.inc
topdir=..
topincdir=../../include

DO_DEBUG = 
LIBTAR = compression.a
COMPRESSION_DIR=minilzo.108

CC = gcc
CXX = g++
CPP = gcc -E
CPPFLAGS = 

LOPT=-DSWBISBINDIR=\"${prefix}/bin\"

ifeq ($(DO_DEBUG),)
.c.o:
	$(CC) $(CFLAGS) $(LOPT) -c -o $*.o  $<
DEBUGLIBOBJECTS =
else
.c.o:
	$(CC) $(CFLAGS) $(LOPT) -c -o $*.o  $<; $(CC) $(CFLAGS) $(LOPT) -S -gstabs+ -o $*.S $<
DEBUGLIBOBJECTS = 
endif

LIBOBJECTS = $(DEBUGLIBOBJECTS) minilzo.o

ifeq (.depend,$(wildcard .depend))
TARGET=everything
else
TARGET=depend everything
endif

all: $(TARGET)

everything: $(LIBTAR)

$(LIBTAR): $(LIBOBJECTS) 
	$(AR) r $@ $(LIBOBJECTS)
	$(RANLIB) $@

$(PROGS): $(LIBOBJECTS)

minilzo.o: $(COMPRESSION_DIR)/minilzo.c
	$(CC) $(CFLAGS) -I ./$(COMPRESSION_DIR) $(LOPT) -c -o $@ $<
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
maintainer-clean: clean
distclean: clean
mostlyclean: clean
clean:
	rm -f *.S *.a *.o *~ $(PROGS)

distclean: clean
	rm -f .depend Makefile

depend:
	( > .depend )

#	($(CPP) $(CFLAGS) -M $(srcdir)/*.c > .depend; exit 0)

ifeq (.depend,$(wildcard .depend))
include .depend
endif
