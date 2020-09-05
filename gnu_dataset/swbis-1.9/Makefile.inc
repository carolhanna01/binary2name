topincdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/include
topdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9
CPP = gcc -E
prefix= /usr/local
exec_prefix = ${prefix}
libdir= $(prefix)/lib
AR = ar
RANLIB = ranlib
OPTS = -DHAVE_CONFIG_H -DSWBISLIBEXECDIR=\"${exec_prefix}/libexec\" -DSWBISLIBDIR=\"${exec_prefix}/lib\"
LIBRPMRC_FILENAME=
LIBRPMALIAS_FILENAME=
RPMNLSDIR=
RPMNLSPACKAGE=
CFLAGS = -W -Wall -Wno-unused -g -O2 $(WARNINGS) $(OPTS) -I. -I$(topincdir)
LDFLAGS =  -L$(topdir)/lib
