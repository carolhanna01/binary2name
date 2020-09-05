# vim: set noexpandtab:
#
# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007  Tom Cato Amundsen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin ST, Fifth Floor, Boston, MA  02110-1301  USA

installfiles += $(wildcard soundcard/*.py)
dist_files += soundcard/Makefile \
	$(wildcard soundcard/*.py) \
	$(wildcard soundcard/*.c soundcard/*.i soundcard/*.h) \
	soundcard/winmidi.dsp soundcard/winmidi.dsw soundcard/winmidi.def

easy-dist-files += soundcard/solfege_c_midi_wrap.c

CFLAGS:=$(CFLAGS) -I/usr/src/linux/include -Wall -pedantic -fPIC
CC:=$(CC) $(CFLAGS) -DHAVE_CONFIG_H 

ifeq ($(ENABLE_OSS_SOUND),yes)
 ifeq ($(SWIG_VERSION),1.1)
  TARGETS += soundcard/solfege_c_midimodule.so
 endif
 ifeq ($(SWIG_VERSION),1.3)
  TARGETS += soundcard/_solfege_c_midi.so
 endif
 ifeq ($(SWIG_VERSION),unknown)
  TARGETS += soundcard/_solfege_c_midi.so
 endif
endif

OBJECTS = soundcard/macro_to_function.o \
	soundcard/solfege_c_midi.o \
	soundcard/solfege_c_midi_wrap.o

ifeq ($(ENABLE_TUNER),yes)
OBJECTS += fft.o dsp.o gate.o calc.o xmalloc.o
endif

soundcard/solfege_c_midimodule.so: $(OBJECTS)
	$(CC) -fPIC -shared $(OBJECTS) -o $@ -lc

soundcard/_solfege_c_midi.so: $(OBJECTS)
	$(CC) -fPIC -shared $(OBJECTS) -o $@

soundcard/solfege_c_midi_wrap.o: soundcard/solfege_c_midi_wrap.c soundcard/solfege_c_midi.i
	(cd soundcard; $(CC) $(CFLAGS) -c solfege_c_midi_wrap.c $(PYTHON_INCLUDES))

soundcard/solfege_c_midi_wrap.c: soundcard/solfege_c_midi.c soundcard/macro_to_function.c soundcard/solfege_c_midi.i
	(cd soundcard; $(SWIG) -python solfege_c_midi.i)

# win32

ifeq ($(ENABLE_WINMIDI),yes)
TARGETS += soundcard/winmidi.pyd
endif

PYDLL=/c/Windows/system32/python24.dll
DLLWRAP=dllwrap

%/winmidi.pyd: %/winmidi.o
	$(DLLWRAP) --dllname=$@ --def=soundcard/winmidi.def $< -o $@ -s --entry=_DllMain@12 -L/usr/i586-mingw32msvc/lib/ -lwinmm $(PYDLL)

%/winmidi.o: %/winmidi.c
	$(CC) $(CFLAGS) $(PYTHON_INCLUDES) -c $< -o $@

# end win32

install-soundcard:
ifeq ($(ENABLE_OSS_SOUND),yes)
	mkdir -p $(DESTDIR)/$(libdir)/$(PACKAGE)
ifeq ($(SWIG_VERSION),1.1)
	$(INSTALL_DATA) soundcard/solfege_c_midimodule.so $(DESTDIR)/$(libdir)/$(PACKAGE)/
else
	$(INSTALL_DATA) soundcard/_solfege_c_midi.so $(DESTDIR)/$(libdir)/$(PACKAGE)/
endif
endif

uninstall-soundcard:
ifeq ($(SWIG_VERSION),1.1)
	rm -f $(DESTDIR)/$(libdir)/$(PACKAGE)/solfege_c_midimodule.so
else
	rm -f $(DESTDIR)/$(libdir)/$(PACKAGE)/_solfege_c_midi.so
endif
	rmdir $(DESTDIR)/$(libdir)/$(PACKAGE) || true
	rmdir $(DESTDIR)/$(libdir)/$(PACKAGE) || true

clean-files += \
	$(wildcard soundcard/*.o soundcard/*.so soundcard/*.pyc soundcard/*.pyd soundcard/*.pyo)

