topincdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/include
topdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swstructlib
topdirswsupplib=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib
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

CFLAGS = -W -Wall -g -O2 $(WARNINGS) $(OPTS) -I. -I$(topincdir) -I$(topdir)/include 

LDFLAGS =  -L$(topdir)/lib \
		-L$(topdir)/swsupplib/tarb -L$(topdir)/swsupplib/misc -L$(topdir)/swsupplib/taru \
		-L$(topdir)/swsupplib/uxfio -L$(topdir)/swsupplib/strob -L$(topdir)/swsupplib/uinfile

