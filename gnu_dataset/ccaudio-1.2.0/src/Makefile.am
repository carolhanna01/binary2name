# Copyright (C) 2006 Free Software Foundation.
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AUTOMAKE_OPTIONS = no-dependencies dist-shar
KDOC_DIR = @KDOC_DIR@
RELEASE = -version-info @LT_VERSION@ -release @LT_RELEASE@
ccxxincludedir=$(includedir)/cc++2/cc++

if MODULES
AM_CPPFLAGS = -DAUDIO_MODULE_PATH=\"$(libdir)/ccaudio1\"
endif

lib_LTLIBRARIES = libccaudio1.la

libccaudio1_la_SOURCES = audiofile.cpp friends.cpp codec.cpp \
	cdalinux.cpp cdawin32.cpp samples.cpp fileio.cpp \
	dtmf.cpp
libccaudio1_la_LDFLAGS = -lm $(RELEASE)

noinst_PROGRAMS = wavetest1 wavetest2
wavetest1_SOURCES = wavetest1.cpp
wavetest1_LDADD = libccaudio1.la
wavetest2_SOURCES = wavetest2.cpp
wavetest2_LDADD = libccaudio1.la

ccxxinclude_HEADERS = audio.h

TESTS = wavetest1
kdoc_headers = audio.h

kdoc:	
	kdoc -f html -d $(KDOC_DIR) -L $(KDOC_DIR) -n ostaudio  $(kdoc_headers)

view:
	lynx ../doc/index.html

macosx: libccaudio1.la
	rm -rf .libs/*.dylib
	@LD@ -r -o ccaudio1.lo $(libccaudio1_la_OBJECTS)
	../libtool --mode=link c++ -dynamic -dynamiclib -o libccaudio1.la \
		-rpath $(libdir) ccaudio1.lo $(LDFLAGS) -lpthread
	touch macosx
                                                                                

