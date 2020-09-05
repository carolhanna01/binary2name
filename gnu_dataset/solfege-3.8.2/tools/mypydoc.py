#!/usr/bin/python

"""
This is a customized pydoc script. It is necessary to use this on
most of the files in solfege because they require i18n setup.

Example usage:
./tools/mypydoc.py src.dataparser
./tools/mypydoc.py src/dataparser.py
"""

import sys
import os

import pydoc
sys.path.insert(0, ".")
import src.i18n
src.i18n.setup(".")

def test_module2(fn):
    subdir, filename = os.path.split(fn)
    modulename = filename.split(".")[0]
    module_str = "%s.%s" % (subdir, modulename)
    exec "import %s" % module_str
    module_var = eval(module_str)
    sys.path.insert(0, subdir)

    pydoc.cli()


def test_module(fn):
    subdir, modulename = fn.split(".")

    module_str = "%s.%s" % (subdir, modulename)
    exec "import %s" % module_str
    module_var = eval(module_str)

    pydoc.cli()

if '/' in sys.argv[1]:
    test_module2(sys.argv[1])
else:
    test_module(sys.argv[1])
