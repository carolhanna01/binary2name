#AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = -I m4
LIB_VERSION=1:8:0
lib_LTLIBRARIES = libgnudos.la
libgnudos_la_SOURCES = kbd.c ukbd.c screen.c dialogs.c strings.c
library_includedir = $(includedir)/console
library_include_HEADERS = kbd.h ukbd.h screen.h dialogs.h strings.h
libgnudos_la_LDFLAGS = -version-info $(LIB_VERSION)
