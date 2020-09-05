# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2006, 2007  Tom Cato Amundsen
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
class Synth:
    NUM_CHANNELS = 16
    def __init__(self, verbose_init):
        print "Solfege will use fakesynth"
        self.m_type_major = "Fake"
    def close(self):
        pass
    def stop(self):
        pass
    def play_track(self, *tracks):
        print "FakeSynth.play_track()"
        last = None
        for e in MidiEventStream(*tracks):
            if e[0] != last:
                print
            print e,
            last = e[0]
        print

