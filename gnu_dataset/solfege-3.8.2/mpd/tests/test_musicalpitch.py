# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
import doctest
import mpd.musicalpitch
from mpd.musicalpitch import MusicalPitch

class TestMusicalPitch(unittest.TestCase):
    def test_normalize_double_accidental(self):
        for a, b in (("c", "c"),
                     ("cisis", "d"),
                     ("disis", "e"),
                     ("eisis", "fis"),
                     ("fisis", "g"),
                     ("gisis", "a"),
                     ("aisis", "b"),
                     ("bisis", "cis'"),
                     ("ceses", "bes,"),
                     ("deses", "c"),
                     ("eses", "d"),
                     ("feses", "ees"),
                     ("geses", "f"),
                     ("ases", "g"),
                     ("beses", "a"),
                     ):
            n = MusicalPitch.new_from_notename(a)
            n.normalize_double_accidental()
            self.assertEquals(n.get_octave_notename(), b)

suite = unittest.makeSuite(TestMusicalPitch)
suite.addTest(doctest.DocTestSuite(mpd.musicalpitch))
