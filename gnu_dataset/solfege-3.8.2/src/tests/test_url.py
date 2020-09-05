# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
from src.utils import Url

class TestUrl(unittest.TestCase):
    def setUp(self):
        pass
    def test_all(self):
        url = Url('solfege:practise/lesson-files/melodic-intervals?ask_for_names=[1, 2, 3]')
        self.assert_(url.protocol == 'solfege')
        self.assert_(url.action == 'practise')
        self.assert_(url.lessonfile == 'lesson-files/melodic-intervals')
        self.assert_(url.config == {'ask_for_names': '[1, 2, 3]'})
        url = Url('solfege://home/tom/lessonfiles/melodic-intervals?ask_for_names=[1, 2, 3]')
        self.assert_(url.protocol == 'solfege')
        self.assert_(url.lessonfile == '/home/tom/lessonfiles/melodic-intervals')
        self.assert_(url.config == {'ask_for_names': '[1, 2, 3]'})

suite = unittest.makeSuite(TestUrl)

