# -*- coding: iso-8859-1 -*-
# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
from src.htmlwidget import NewHTMLParser, tree

class TestHtmlParser(unittest.TestCase):
    def setUp(self):
        self.m_parser = NewHTMLParser()
    def test_simple(self):
        self.m_parser.feed("<html><p>test</p></html>")
        self.m_parser.calc_bounds()
        x = self.m_parser.m_tree.get_as_string()
        self.assertEquals(x, """
html-None-None
 p-0-3
  ::Text::-0-3""")
    def test_links(self):
        self.m_parser.feed("""
<html>
<body>
<p>hei
<a href="h.html">h.html</a>end</p>
</body>
</html>
        """)
        self.m_parser.calc_bounds()
        x = self.m_parser.m_tree.get_as_string()
        self.assertEquals(x, """
html-None-None
 body-0-12
  p-0-12
   ::Text::-0-3
   a-4-9
    ::Text::-4-9
   ::Text::-10-12""")
    def test_img(self):
        self.m_parser.feed("<html><p><img src='png/mainwin.png'/></p></html>")
        self.m_parser.calc_bounds()#FIXME not complete
        #x = self.m_parser.m_tree.get_as_string()
        #print "'%s'" % x
    def test_trademark(self):
        self.m_parser.feed("""
<html>
<head>
<title>TITTEL</title>
</head>
<body>
<p>123<trademark class="copyright"/>456</p>
</body>
</html>""")
        self.m_parser.calc_bounds()
    def test_title(self):
        self.m_parser.feed("<html><head><title>tittel</title></head><body><p>123</p></body></html>")
        self.m_parser.calc_bounds()
    def test_ignore_style(self):
        self.m_parser.feed("<html><head><style>bla bla</style><title>test_ignore_style</title></head><body><p>123</p></body></html>")
        self.m_parser.calc_bounds()
    def test_linkrel_trouble(self):
        self.m_parser.feed("""<html>
<head>
<link rel="next" href="solfege-intro.html" title="chapterâ 1.â introduction."></head><body><span class="application">gnu solfege</span> manual v3.0.0
</body></html>""")
        debug_flag = ()
    def test_linkrel(self):
        self.m_parser.feed("""<html>
<head>
<link rel="next" href="solfege-intro.html" title="chaption.">
<link rel="next" href="solfege-intro.html" title="chaption.">
</head>
<body>test
</body></html>""")
        c = tree.Cursor(self.m_parser.m_tree)
        self.assertEqual(c.get().m_name, 'html')
        self.assertEqual(c.get().m_children[0].m_name, 'head')
        c.go_next()
        self.assertEqual(c.get().m_children[0].m_name, 'link')
        self.assertEqual(c.get().m_children[1].m_name, 'link')
        c.go_next_sibling()
        self.assertEqual(c.get().m_name, 'body')
        c.go_next()
        self.assertEqual(c.get().m_name, 'p')
        c.go_next()
        self.assertEqual(c.get().m_name, '::Text::')
    def test_table(self):
        global debug_flag
        #debug_flag = ('start', 'end')
        self.m_parser.feed("""<html>
<table border="1">
<tr>
  <td>C1</td>
  <td>C2</td>
</tr>
<tr><td>D1</td></tr>
</table>
<p>end</p>
</html>""")
        self.m_parser.pop_head_out_of_tree()
        self.m_parser.strip_whitespace()
        self.m_parser.insert_newline_before_blocks()
        self.m_parser.do_list_items()
        self.m_parser.do_table()
        #self.m_parser.calc_bounds()
        #self.m_parser.m_tree.show()
        debug_flag = ()
    def test_table_colgroup(self):
        self.m_parser.feed("""<html><body><table><colgroup><col><col></colgroup><tbody><tr><td>cell</td></tr></tbody></table></body></html>""")
    def test_parse_all_user_manual(self):
        import glob
        for n in glob.glob("help/*/*.html"):
            try:
                p = NewHTMLParser()
                p.feed(file(n, 'r').read())
                p.pop_head_out_of_tree()
               # p.post_process(p)
            except Exception, e:
                print "Failed:", n
                raise e


suite = unittest.makeSuite(TestHtmlParser)
