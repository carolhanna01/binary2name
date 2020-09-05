# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest

import glob
import os.path
modules = [os.path.splitext(os.path.basename(x))[0] \
           for x in glob.glob("src/tests/test_*.py")]

for m in modules:
    exec "import %s" % m
suite = unittest.TestSuite([globals()[m].suite for m in modules])
