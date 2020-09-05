# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
import doctest
import traceback

import src.dataparser
from src.dataparser import *

class TestLexer(unittest.TestCase):
    def test_get_line(self):
        l = Lexer("""#comment1
#comment2
#comment3

var = 3
""", None)
        self.assertEquals(l.get_line(0), "#comment1")
        self.assertEquals(l.get_line(1), "#comment2")
        self.assertEquals(l.get_line(2), "#comment3")
        self.assertEquals(l.get_line(3), "")
        self.assertEquals(l.get_line(4), "var = 3")
    def test_scan(self):
        p = Dataparser({}, {})
        p._lexer = Lexer("\"string\" name 1.2 2 (", p)
        self.assertEquals(p._lexer.scan(STRING), "string")
        self.assertEquals(p._lexer.scan(NAME), "name")
        self.assertEquals(p._lexer.scan(FLOAT), "1.2")
        self.assertEquals(p._lexer.scan(INTEGER), "2")
        p._lexer = Lexer("1 2 3", p)
        try:
            p._lexer.scan(STRING)
        except DataparserException, e:
            self.assertEquals(u"(line 1): 1 2 3\n"
                              u"          ^",
                              e.m_nonwrapped_text)
    def test_unable_to_tokenize(self):
        p = Dataparser({}, {})
        try:
            p._lexer = Lexer("question { a = 3} |!", p)
        except UnableToTokenizeException, e:
            self.assertEquals("(line 1): question { a = 3} |!\n"
                              "                            ^",
                              e.m_nonwrapped_text)
        try:
            p._lexer = Lexer("x = 4\n"
                             "question { a = 3} |!", p)
        except UnableToTokenizeException, e:
            self.assertEquals("(line 1): x = 4\n"
                              "(line 2): question { a = 3} |!\n"
                              "                            ^",
                              e.m_nonwrapped_text)

class TestDataParser(unittest.TestCase):
    def test_for_trainingset(self):
        p = Dataparser({}, {})
        p.parse_string("""fileformat_version = 1
        lesson {
        lesson_id = "lkalskdj alskdj lkj "
        count = 3
        repeat=2
        delay = 2}""")
        self.assertEqual(p.globals['fileformat_version'], 1)
        self.assertEqual(len(p.blocklists), 1)
        self.assertEqual(len(p.blocklists['lesson']), 1)
        l = p.blocklists['lesson'][0]
        self.assertEqual(l['count'], 3)
    def assertRaisedIn(self, methodname):
        t = traceback.extract_tb(sys.exc_info()[2])
        self.assertEquals(t[-1][2], methodname)
    def test_exception_statement_1(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("b")
        except DataparserSyntaxError, e:
            self.assertRaisedIn('statement')
            self.assertEquals(u"(line 1): b\n"+
                              u"           ^",
                              e.m_nonwrapped_text)
            self.assertEquals(e.m_token, ('EOF', None, 1, 0))
    def test_exception_statement_2(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("a)")
        except DataparserSyntaxError, e:
            self.assertRaisedIn('statement')
            self.assertEquals(u"(line 1): a)\n"+
                              u"           ^",
                              e.m_nonwrapped_text)
            self.assertEquals(e.m_token, (')', ')', 1, 0))
    def test_exception_statement_3(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("""#comment
  XYZ
""")
        except DataparserSyntaxError, e:
            self.assertRaisedIn('statement')
            self.assertEquals(u"(line 1): #comment\n"+
                              "(line 2):   XYZ\n"+
                              "(line 3): \n"+
                              "          ^",
                              e.m_nonwrapped_text)
    def test_exception_statement_4(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("""#comment
  A)
""")

        except DataparserSyntaxError, e:
            self.assertRaisedIn('statement')
            self.assertEquals(u"(line 1): #comment\n"+
                              "(line 2):   A)\n"+
                              "             ^",
                              e.m_nonwrapped_text)
    def test_exception_functioncall(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("a=1\n"
                           "b = func() c=3")
        except NameLookupException, e:
            self.assertEquals(e.m_token[TOKEN_STRING], 'func')
            self.assertEquals("(line 1): a=1\n"
                              "(line 2): b = func() c=3\n"
                              "              ^",
                              e.m_nonwrapped_text)
        p = Dataparser({}, {})
        try:
            p.parse_string("a=1\n"
                           "b = func(3, 4) c=3")
        except NameLookupException, e:
            self.assertEquals(e.m_token[TOKEN_STRING], 'func')
            self.assertEquals("(line 1): a=1\n"
                              "(line 2): b = func(3, 4) c=3\n"
                              "              ^",
                              e.m_nonwrapped_text)
    def test_exception_atom(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("a=b")
        except NameLookupException, e:
            self.assertEquals(e.m_token[TOKEN_STRING], "b")
            self.assertEquals("(line 1): a=b\n"
                              "            ^",
                              e.m_nonwrapped_text)
    def test_exception_assignment(self):
        p = Dataparser({}, {})
        try:
            p.parse_string("question = 3")
        except AssignmentToReservedWordException, e:
            self.assertRaisedIn('assignment')
            self.assertEquals(u"(line 1): question = 3\n" +
                              u"          ^",
                              e.m_nonwrapped_text)

suite = unittest.makeSuite(TestLexer)
suite.addTest(unittest.makeSuite(TestDataParser))
