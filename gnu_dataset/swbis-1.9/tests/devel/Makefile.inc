

topincdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/include
topdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9
CPP = gcc -E
prefix= /usr/local
exec_prefix = ${prefix}
libdir= $(prefix)/lib

AR = ar
RANLIB = ranlib
OPTS = -DHAVE_CONFIG_H
LIBRPMRC_FILENAME=
LIBRPMALIAS_FILENAME=
RPMNLSDIR=
RPMNLSPACKAGE=
topdir1=swstructlib/src

CFLAGS = -W -Wall -Wno-unused -g -O2 $(WARNINGS) $(OPTS) -I. -I$(topdir) -I$(topincdir) -I/usr/include/rpm  

LDFLAGS =  -L$(topdir)/lib \
		-L$(topdir)/swsupplib/tarb -L$(topdir)/swsupplib/misc -L$(topdir)/swsupplib/taru \
		-L$(topdir)/swsupplib/uxfio -L$(topdir)/swsupplib/strob -L$(topdir)/swsupplib/uinfile
VERSION = 0.14

