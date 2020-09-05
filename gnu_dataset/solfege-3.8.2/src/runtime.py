# GNU Solfege - free ear training software
# Copyright (C) 2005, 2007  Tom Cato Amundsen
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

"""
This file does the checking for optional features that depend on
python modules the user can have installed.

It also does sanity check on the python and pygtk versions and some
other initial setup tasks.
"""

import sys
import os

_has_gtkhtml2 = None
use_cairo_widgets = None

def assert_python_version(required_version):
    if sys.version_info < required_version:
        print "*"* 67
        print "Solfege need Python %s or newer. The configure script told you so!" % (".".join([str(i) for i in required_version]))
        print "*"* 67
        sys.exit(-1)

def setup_pygtk(required_version):
    import pygtk
    # this is needed for py2exe
    if sys.platform == 'win32':
        os.environ['PATH'] += ";lib;bin;"
    else:
        pygtk.require("2.0")
    import gtk
    gtk.rc_parse("solfege.gtkrc")
    # The rest of this function is just for sanity check. Not really required.
    if gtk.pygtk_version < required_version:
        print "-"*55
        print " GNU Solfege requires pygtk version %s or newer." % (".".join([str(i) for i in required_version]))
        print " The version installed appears to be %s" % (".".join([str(i) for i in gtk.pygtk_version]))
        print " Exiting program."
        print "-"*55+"\n"
        sys.exit(-1)


def init(options):
    global _has_gtkhtml2
    global use_cairo_widgets
    assert_python_version((2, 3))
    setup_pygtk((2, 4))
    if options.without_gtkhtml:
        _has_gtkhtml2 = False
        print "gtkhtml2: disabled by runtime option"
    if options.no_cairo_widgets:
        use_cairo_widgets = False
    else:
        import gtk
        if gtk.pygtk_version < (2, 8, 0):
            use_cairo_widgets = False
        else:
            use_cairo_widgets = True



def has_gtkhtml2():
    global _has_gtkhtml2
    if _has_gtkhtml2 is not None:
        return _has_gtkhtml2
    print "Checking for gtkhtml2...",
    try:
        import gtkhtml2
        print "ok"
        _has_gtkhtml2 = True
    except ImportError, e:
        print e
        _has_gtkhtml2 = False
    return _has_gtkhtml2

