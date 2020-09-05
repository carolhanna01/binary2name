# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
from mpd.track import Track, MidiEventStream
from mpd.rat import Rat

class TestTrack(unittest.TestCase):
    def test_simple1(self):
        t = Track()
        t.note(4, 90, 127)
        self.assertEquals(list(MidiEventStream(t)),
          [('program-change', 0, 0),
           ('note-on', 0, 90, 127),
           ('notelen-time', Rat(1, 4)),
           ('note-off', 0, 90, 127)])
    def test_1voice_setpatch(self):
        t = Track()
        t.note(4, 90, 127)
        t.set_patch(3)
        t.note(4, 91, 127)
        self.assertEquals(list(MidiEventStream(t)),
          [('program-change', 0, 0),
           ('note-on', 0, 90, 127),
           ('notelen-time', Rat(1, 4)),
           ('note-off', 0, 90, 127),
           ('program-change', 0, 3),
           ('note-on', 0, 91, 127),
           ('notelen-time', Rat(1, 4)),
           ('note-off', 0, 91, 127),
           ])

suite = unittest.makeSuite(TestTrack)
