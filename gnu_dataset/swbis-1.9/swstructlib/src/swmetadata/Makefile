srcdir = .
topdir = /home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swstructlib/src

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
include ../Makefile.inc
topdir=..
topincdir=../../../include

DO_DEBUG = 

CC = gcc
CXX = g++
CPP = gcc -E
CPPFLAGS = 

SWLIB = swmetadata.a

LIBS=$(SWLIB)

LOADLIBS = 

ifeq ($(DO_DEBUG),)
DEBUGLIBOBJECTS = 
DO_CPP_ASSEM = 
DO_C_ASSEM = 
else
DEBUGLIBOBJECTS = 
DO_CPP_ASSEM = $(CXX) $(CFLAGS) -S -gstabs+ -Wno-unused $(CPPFLAGS) -c -o $(OUT_PATH)/$*.S $<
DO_C_ASSEM = $(CC) $(CFLAGS) -S -gstabs+ -c -o $(OUT_PATH)/$*.S $<
endif


OUT_PATH = .
LDFLAGS =

ifeq (.depend,$(wildcard .depend))
TARGET=everything
else
TARGET=depend everything
endif

.SUFFIXES:
.SUFFIXES: .cxx .o

.cxx.o:
	$(CXX) $(CFLAGS) -c -o $(OUT_PATH)/$*.o  $<; $(DO_CPP_ASSEM)

SWDEBUGOBJS= 	\
	swdefinition_debug.o \
	swpackagefile_debug.o \
	swdefinitionfile_debug.o

SWOBJS= 	\
	swattribute.o \
	swfilemap.o \
	swmemfile.o \
	swcatalogfile.o \
	swcontrolfile.o \
	swcontrolscript.o \
	swdefbundle.o \
	swdefcontrolfile.o \
	swdefdistribution.o \
	swdeffile.o \
	swdeffileset.o \
	swdefhost.o \
	swdefinition.o \
	swdefinitionfile.o \
	swattributefile.o \
	swdefinstalled_software.o \
	swdefmedia.o \
	swdefproduct.o \
	swdefsubproduct.o \
	swdefvendor.o \
	swpackagefile.o \
	swpackagedir.o \
	swinstalled.o \
	swindex.o \
	swinfo.o \
	swoption.o \
	swmetadata.o \
	swpsf.o \
	swpsfi.o \
	swextendeddef.o \
	swdefcategory.o


all: $(TARGET)

everything: $(LIBS)

$(SWLIB): $(SWOBJS)
	ar rc $(SWLIB) $(SWOBJS)
	ranlib $(SWLIB)
	cp -p $(SWLIB) ../lib

clean:
	rm -f *.S *.a *.o *~ a.out
	rm -f lib/*.a ../lib/*.a

install:

distclean: clean
	rm -f .depend Makefile

atarfile: tarfile

tarfile: NOFILE
	false
NOFILE:

depend:
	( > .depend )

#	($(CPP) $(CFLAGS) -M $(srcdir)/*.cxx > .depend; exit 0)

ifeq (.depend,$(wildcard .depend))
include .depend
endif


