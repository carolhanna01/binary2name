# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
import doctest
import src.cfg
from src.cfg import parse_file_into_dict

class Test_parse_file_into_dict(unittest.TestCase):
    def test_fail_on_non_utf8(self):
        d = {}
        try:
            parse_file_into_dict(d, "test-files/test1.cfg")
        except UnicodeDecodeError, e:
            self.assertRaises(NameError, lambda: data)
    def test_parse_utf8(self):
        d = {}
        parse_file_into_dict(d, "test-files/test2.cfg")
        self.assertEquals(d['sound']['i'], u'3')
        self.assertEquals(d['sound']['f'], u'1.1')
        self.assertEquals(d['sound']['s'], u'/home/Us\xe9r/bin/prog')
        self.assert_(isinstance(d['sound']['s'], basestring))
        self.assert_(isinstance(d['sound']['s'], unicode))

suite = unittest.makeSuite(Test_parse_file_into_dict)
suite.addTest(doctest.DocTestSuite(src.cfg))
