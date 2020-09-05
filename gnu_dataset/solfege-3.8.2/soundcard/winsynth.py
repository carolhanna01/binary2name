# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007  Tom Cato Amundsen
# Copyright (C) 2001 Joe Lee
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

from mpd.track import MidiEventStream
import winmidi

class WinSynth(object):
    NUM_CHANNELS = 16
    def __init__(self, devnum, verbose_init):
        try:
            self.__driver = winmidi.Winmidi(devnum)
        except RuntimeError, e:
            devnum = 0
            self.__driver = winmidi.Winmidi(devnum)
        self.m_type_major = "win32" #FIXME
        self.m_devnum = devnum
        if verbose_init:
            print "Solfege will use Windows multimedia output."
    def close(self):
        self.__driver = None
    def stop(self):
        # dummy function
        pass
    def play_track(self, *tracks):
        self.play_midieventstream(MidiEventStream(*tracks))
    def play_midieventstream(self, midieventstream):
        if self.__driver is None:
            raise RuntimeError, "Attempted to use synth after closing."
        self.__driver.reset()
        # bigger magic plays slower
        magic = 1440000
        self.__driver.set_tempo(int(magic * 4 / 60))
        v = []
        notelen = 0
        for e in midieventstream:
            if e[0] == midieventstream.TEMPO:
                self.__driver.set_tempo(int(magic * e[2] / e[1]))
            elif e[0] == midieventstream.NOTELEN_TIME:
                notelen = e[1]
                #print "notelen: ", notelen
            elif e[0] == midieventstream.NOTE_ON:
                self.__driver.note_on(int(1000 * notelen), e[1], e[2], e[3])
                notelen = 0
            elif e[0] == midieventstream.NOTE_OFF:
                self.__driver.note_off(int(1000 * notelen), e[1], e[2], e[3])
                notelen = 0
            elif e[0] == midieventstream.SET_PATCH:
                self.__driver.program_change(e[1], e[2])
            elif e[0] == midieventstream.BENDER:
                print "ugh todo: seq_bender for play_with_drvmidi"
                #m.seq_bender(DEV, e[1], e[2])
            else:
                raise error
        self.__driver.play()
