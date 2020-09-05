# Makefile for mkpass is in -*- text -*- mode.
#

# The real locations of programs.
AR	= ar
RANLIB	= ranlib
MKDIR	= mkdir
TAR	= tar
LN	= ln
CP	= cp
RM	= rm -f
MYINSTALL = cp

MHTML_SRCDIR = $(TOPDIR)

CC		= gcc $(GCC_WARN)
GCC_WARN	= -Wall -Wstrict-prototypes -Wshadow
# PROFILE_FLAGS	= -pg
# DEBUG_FLAGS	= -g $(PROFILE_FLAGS)
OPTIMIZE_FLAGS	= -O69 -finline-functions
INCLUDE_FLAGS	= -I$(MHTML_SRCDIR)libutils
REQLIBS		= $(MHTML_SRCDIR)libutils/bprintf/libbprintf.a \
		  $(MHTML_SRCDIR)libutils/xmalloc/libxmalloc.a $(OS_LIBS)
CFLAGS		= $(OPTIMIZE_FLAGS) $(DEBUG_FLAGS) $(INCLUDE_FLAGS)
LDFLAGS		= $(DEBUG_FLAGS)

OBJS = mkpass.o
PROG = mkpass$(EXE)
VERSION = 1.0
TARDIR = $(PROG)-$(VERSION)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDE_FLAGS) -c $<

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(REQLIBS) $(NTLIBS)

install: $(PROG)
	$(MYINSTALL) $(PROG) $(bindir)/$(PROG)

clean: FORCE
	$(RM) *.o *.a $(PROG)

distclean: clean

dist: FORCE
	rm -rf $(TARDIR)
	mkdir $(TARDIR)
	ln *.c Makefile $(TARDIR)
	tar -zcf $(TARDIR).tar.gz $(TARDIR)
	rm -rf $(TARDIR)

FORCE:
