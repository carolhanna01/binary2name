srcdir = .
topdir = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swstructlib
#VPATH = $(srcdir)

# GNU make targets
#all
#install
#install-html
#install-dvi
#install-pdf
#install-ps
#uninstall
#install-strip
#clean
#distclean
#mostlyclean
#maintainer-clean
#info
#dvi
#html
#pdf
#ps
#dist
#check
#
prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
sbindir=${exec_prefix}/sbin
libexecdir=${exec_prefix}/libexec
datadir=${prefix}/share
mandir=${prefix}/share/man
mansubdir=@mansubdir@

include $(topdir)/Makefile.inc
DO_DEBUG = 
CC = gcc
CXX = g++
CPP = gcc -E
CPPFLAGS = 

LIB =
SWSTRUCTLIB = swstructlib.a
LOADLIBS = 
LIBS = $(SWSTRUCTLIB)
PROGS =
OUT_PATH = .

SUBDIRS = src
ifeq (.depend,$(wildcard .depend))
TARGET=everything
else
TARGET=depend everything
endif

.SUFFIXES:
.SUFFIXES: .cxx .o

.cxx.o:
	$(CXX) $(CFLAGS) -c -o $(OUT_PATH)/$*.o  $< 

all: $(TARGET)

everything: $(SWSTRUCTLIB)  $(PROGS)

$(SWSTRUCTLIB): NOFILE
	cd src && $(MAKE) installlib

$(PROGS): $(LIBS)

distdir:
install:
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
uninstall:
# clean:
mostlyclean: clean
# distclean:
maintainer-clean: distclean
info:
dvi:
html:
pdf:
ps:
dist:
check:
tags:
ctags:

.PHONY:		clean
clean:
	for d in $(SUBDIRS); do \
		(cd $$d; $(MAKE) $@) ;\
	done
	rm -f *.S *.a *.o *~ $(PROGS) a.out
	rm -f lib/*.a
	rm -fr autom4te.cache

.PHONY:		distclean
distclean: clean
	for d in $(SUBDIRS); do \
		(cd $$d; $(MAKE) $@) ;\
	done
	rm -f aclocal.m4 
	rm -fr autom4te.cache
	rm -f .depend Makefile Makefile.inc stamp-h1
	rm -f config.cache \
		config.status \
		config.log

atarfile: tarfile

tarfile: NOFILE
	false

NOFILE:

.PHONY:		depend
depend:
#	for d in $(SUBDIRS); do \
#		(cd $$d; $(MAKE) $@) ;\
#	done

ifeq (.depend,$(wildcard .depend))
include .depend
endif



