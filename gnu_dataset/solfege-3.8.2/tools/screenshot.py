#!/usr/bin/python

import os
import sys
import re
from subprocess import *

def run(cmd):
    print "run:", cmd
    os.system(cmd)

def get_image_dim(fn):
    output = Popen(["file", fn], stdout=PIPE).communicate()[0]
    r = re.compile("(\d+)\s*x+\s*(\d+)")
    m = r.search(output)
    if m:
        return int(m.groups()[0]), int(m.groups()[1])
    else:
        return None, None

def do_file(fn):
    f, ext = os.path.splitext(fn)
    run("import -frame %s" % fn)
    x, y = get_image_dim(fn)
    if x > 510:
        run("convert -scale 510 %s %s-resized.png" % (fn, f))
        run("mv %s-resized.png %s" % (f, fn))
    run("pngquant -nofs 8 %s" % fn)
    run("mv %s-or8.png %s" % (f, fn))

help = """
Usage: ./tools/screenshot path/to/image.png

Make a screenshot using "import". Run this script, and then
click on the window you want to make a screenshot of.
"""
if len(sys.argv) != 2:
    print help
    sys.exit()
if sys.argv[1] in ('-h', '--help'):
    print help
    sys.exit()

do_file(sys.argv[1])

