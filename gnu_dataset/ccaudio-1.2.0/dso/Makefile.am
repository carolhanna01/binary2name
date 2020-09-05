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
pkglibdir = $(libdir)/ccaudio1

if MODULES
pkglib_PROGRAMS = g.711
AM_CPPFLAGS = -I../src -I$(top_srcdir)/src

g_711_SOURCES = g711.cpp
g_711_LDFLAGS = @MODULE_FLAGS@
g_711_CXXFLAGS = -fPIC
endif

