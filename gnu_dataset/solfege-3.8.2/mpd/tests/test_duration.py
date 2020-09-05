# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
import doctest
import mpd.duration
from mpd.duration import Duration
import sys

class TestDuration(unittest.TestCase):
    def test_constructor(self):
        for a, b, f in ((1, 0, 1.0), (2, 0, 0.5), (2, 1, 0.75)):
            d = Duration(a, b)
            r = d.get_rat_value()
            self.assertEquals(float(r), f)

suite = unittest.makeSuite(TestDuration)
suite.addTest(doctest.DocTestSuite(mpd.duration))
