topincdir=
topdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib/um_rpm/rpm-3.0.5
prefix = /usr/local
exec_prefix = ${prefix}
libdir = $(prefix)/lib
AR= ar
RANLIB= ranlib
OPTS= -DHAVE_CONFIG_H -DSWBISLIBEXECDIR=\"${exec_prefix}/libexec\"
LIBRPMRC_FILENAME=
LIBRPMALIAS_FILENAME=
RPMNLSDIR=
RPMNLSPACKAGE=
CFLAGS = -g -O2 -D_GNU_SOURCE -Wall -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wno-char-subscripts -D_GNU_SOURCE -D_REENTRANT $(WARNINGS) $(OPTS) -I. -I../build -I../popt -I..
LDFLAGS =  
