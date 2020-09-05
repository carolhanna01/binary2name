# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
import os
from src.reportlib import *

class TestReport(unittest.TestCase):
    def create_report(self):
        r = Report()
        r.append(Heading(1, "Heading1"))
        r.append(Paragraph("Text"))
        t = Table()
        r.append(t)
        row = TableRow()
        row.append("Cell1 text")
        row.append("Cell2 text")
        t.append(row)
        t.append_row("cell1", "cell2")
        return r
    def test_html(self):
        if not os.path.exists("reportlib-test"):
            os.mkdir("reportlib-test")
        r = self.create_report()
        HtmlReport(r, "reportlib-test/t1.html")
    def test_latex(self):
        if not os.path.exists("reportlib-test"):
            os.mkdir("reportlib-test")
        r = self.create_report()
        LatexReport(r, "reportlib-test/t1.tex")

suite = unittest.makeSuite(TestReport)

