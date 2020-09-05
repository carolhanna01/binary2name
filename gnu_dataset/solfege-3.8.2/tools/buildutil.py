#/usr/bin/python
# GNU Solfege - free ear training software
# Copyright (C) 2006, 2007  Tom Cato Amundsen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin ST, Fifth Floor, Boston, MA  02110-1301  USA

import glob
import os.path
import sys
sys.path.append(".")
import src.configureoutput
import re

def create_languages_py():
    f = file("src/languages.py", "w")

    print >> f, "# Generated at build time by tools/buildscript.py"
    print >> f, "# Do not edit. Changes will be lost."
    print >> f, "languages = ["
    print >> f, "  'system default',"
    print >> f, "  'C (english)',"
    for fn in glob.glob("po/*.po"):
        print >> f, "   '%s'," % os.path.splitext(os.path.basename(fn))[0]
    print >> f, "]"
    f.close()

