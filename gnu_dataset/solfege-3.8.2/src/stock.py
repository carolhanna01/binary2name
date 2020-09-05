# GNU Solfege - free ear training software
# Copyright (C) 2004, 2005, 2007 Tom Cato Amundsen
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

import gtk
import os
import sys
import cfg

class BaseIconFactory(gtk.IconFactory):
    def __init__(self, widget, datadir):
        gtk.IconFactory.__init__(self)
        self.datadir = datadir
        self.add_default()

    def add_icons(self, icons):
        for stock_id, filename in icons.items():
            if os.path.isfile(os.path.join(self.datadir, filename)):
                iconset = gtk.IconSet(gtk.gdk.pixbuf_new_from_file(os.path.join(self.datadir, filename)))
                self.add(stock_id, iconset)
            else:
                print >> sys.stderr, "File not found: %s" % filename

class EditorIconFactory(BaseIconFactory):
    """
    This class is used by lessonfile_editor.py
    """
    def __init__(self, widget, datadir):
        BaseIconFactory.__init__(self, widget, datadir)
        icons = {'solfege-icon': "graphics/solfege.png", #FIXME new icon
            'solfege-sharp': "graphics/sharp.png",
            'solfege-double-sharp': "graphics/double-sharp.png",
            'solfege-flat': "graphics/flat.png",
            'solfege-double-flat': "graphics/double-flat.png",
            'solfege-natural': "graphics/natural.png",
            'solfege-erase': "graphics/erase.png",
            'solfege-notehead': "graphics/notehead.png"}
        self.add_icons(icons)


class SolfegeIconFactory(BaseIconFactory):
    def __init__(self, widget, datadir):
        BaseIconFactory.__init__(self, widget, datadir)
        icon_list = ['happyface', 'sadface', 
            'chord', 'chord-voicing', 'harmonic-interval',
            'melodic-interval', 'sing-interval', 'sing-chord', 'id-by-name',
            'identify-scale', 'identify-bpm', 'twelve-tone',
            'rhythm', 'id-tone', 'harmonic-progression-dictation',
            'compare-intervals', 'dictation',
            'rhythm-c12c12c12', 'rhythm-c12c12r12', 'rhythm-c12r12c12',
            'rhythm-c16c16c16c16', 'rhythm-c16c16c8', 'rhythm-c16c8c16',
            'rhythm-c16c8.', 'rhythm-c4', 'rhythm-c8c16c16', 'rhythm-c8.c16',
            'rhythm-c8c8', 'rhythm-r12c12c12', 'rhythm-r12c12r12',
            'rhythm-r12r12c12', 'rhythm-r16c16c16c16', 'rhythm-r16c16c8',
            'rhythm-r16c8c16', 'rhythm-r16c8.', 'rhythm-r4',
            'rhythm-r8c16c16', 'rhythm-r8c8', 'rhythm-r8r16c16',
            'rhythm-wrong']
        d = {}
        if cfg.get_string('gui/theme'):
            theme_dir = os.path.join("themes", cfg.get_string("gui/theme"))
            if not os.path.isdir(theme_dir):
                print >> sys.stderr,  "Theme '%s' not found." % cfg.get_string("gui/theme")
            path_ext = [
                (theme_dir, ".svg"),
                (theme_dir, ".png"),
                ("graphics", ".png")]
            d['solfege-icon'] = os.path.join(theme_dir, 'solfege.svg')
        else:
            path_ext = [("graphics", ".png"),]
            d['solfege-icon'] = 'graphics/solfege.png'
        for iname in icon_list:
            for path, ext in path_ext:
                if os.path.isfile(os.path.join(path, iname)+ext):
                    d['solfege-%s' % iname] = os.path.join(path, iname)+ext
                    #d['solfege-%s' % iname] = "graphics/solfege.svg"
                    break
        self.add_icons(d)

