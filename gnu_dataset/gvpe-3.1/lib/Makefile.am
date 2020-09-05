noinst_LIBRARIES = libgvpe.a

AM_CPPFLAGS = @AM_CPPFLAGS@ -I. -I$(top_builddir)

libgvpe_a_SOURCES = pidfile.c getopt1.c dropin.c

libgvpe_a_LIBADD = @LIBOBJS@ @ALLOCA@
libgvpe_a_DEPENDENCIES = $(libgvpe_a_LIBADD)

noinst_HEADERS = pidfile.h dropin.h gettext.h

