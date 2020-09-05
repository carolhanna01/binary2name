# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
from src.lessonfile import LessonfileCommon, UnknownMusicType
from src import dataparser

class TestParser(unittest.TestCase):
    def test_bad_musictype(self):
        s = 'question {\n' \
            'music = music("c e g", "chor")\n' \
            '}'
        p = LessonfileCommon("<STRING>")
        try:
            p.parse_string(s)
        except UnknownMusicType, e:
            self.assertEquals(e.m_token[dataparser.TOKEN_STRING], "chor")
            self.assertEquals('(line 1): question {\n'
                              '(line 2): music = music("c e g", "chor")\n'
                              '                                 ^',
                              e.m_nonwrapped_text)

suite = unittest.makeSuite(TestParser)

