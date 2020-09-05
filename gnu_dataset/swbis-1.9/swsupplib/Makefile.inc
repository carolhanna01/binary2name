topincdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/include
topdir=/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib
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
RPMINCOPT=

DO_DEBUG = 
DO_UM_RPM = um_rpm
#CCDEPMODE = depmode=none

CFLAGS = -pedantic -W -Wall -Wstrict-prototypes -Wno-unused -g -O2 $(WARNINGS) $(OPTS) -I. -I$(topincdir) -I$(topdir) $(RPMINCOPT) -D_POSIX_PTHREAD_SEMANTICS
LDFLAGS =  -L$(topdir)/lib \
		-L$(topdir)/tarb -L$(topdir)/misc -L$(topdir)/taru -L$(topdir)/uxfio \
		-L$(topdir)/strob -L$(topdir)/uinfile

