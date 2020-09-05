#!/usr/bin/python

# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007  Tom Cato Amundsen
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

# This script is used to launch Solfege when running
# it from the source dir without installing.

import sys
import os
import src
import src.cfg

import src.gethomedir
if not os.path.exists(src.gethomedir.get_home_dir()):
    os.mkdir(src.gethomedir.get_home_dir())

if sys.platform == 'win32':
    print "Using as home dir:", src.gethomedir.get_home_dir()

try:
    src.cfg.initialise("default.config", None,
                   os.path.join(src.gethomedir.get_home_dir(), ".solfegerc"))
except UnicodeDecodeError, e:
    import traceback
    traceback.print_exc()
    print "\n\tYou .solfegerc file is not properly utf8 encoded. Most likely"
    print "\tit is the path to some external program that contain non-ascii"
    print "\tcharacters. Please edit or delete the file. Or email it to"
    print "\ttca@gnu.org, and he will tell you what the problem is.\n"
    sys.exit(-1)


# i18n should be imported very early in program init because it setup
# the _ and _i functions for the whole program.
import src.i18n
src.i18n.setup(".", src.cfg.get_string("app/lc_messages"))

import src.mainwin

src.mainwin.start_app(".", ".")
