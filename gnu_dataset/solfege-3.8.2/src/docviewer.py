# GNU Solfege - free ear training software
# Copyright (C) 2005, 2006, 2007 Tom Cato Amundsen
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
import i18n
from htmlwidget import HtmlWidget

class DocViewer(gtk.VBox):
    """
    The purpose of this class is to:
        - provide the status bar
        - select the correct translation of a document
    """
    def __init__(self, activate_cb):
        gtk.VBox.__init__(self)
        self.m_htmlwidget = HtmlWidget(activate_cb, self.on_anchor_track)
        self.m_htmlwidget.show_all()
        self.pack_start(self.m_htmlwidget)
        self.set_size_request(500, 300)
        self.get_vadjustment = self.m_htmlwidget.get_vadjustment
        self.on_key_press_event = self.m_htmlwidget.on_key_press_event
        self.m_statusbar = gtk.Statusbar()
        self.m_statusbar.show()
        self.pack_start(self.m_statusbar, False)
        self.source = self.m_htmlwidget.source
        self.grab_focus = self.m_htmlwidget.grab_focus
        self.m_language = "C"
        for lang in i18n.langs():
            if os.path.isdir(os.path.join('help', lang)):
                self.m_language = lang
                break
        self.m_htmlwidget.m_document_wd = os.path.join("help", self.m_language)
    def read_docfile(self, fn, anchor):
        """
        We reread the docfile even if we are already displaying this file,
        just in case it has changed on disk.

        fn is one of two:
        1. A absolute path to a file that we should read. Display 'File
           not found' if exactly this file is not found.
        2. Relative path to a file relative to the file currently displayed
           in m_htmlwidget.

        Set self.m_loaded_file.
        """
        try:
            if fn:
                if os.path.isabs(fn):
                    self.m_htmlwidget.read_file_abs(fn)
                else:
                    if not os.path.exists(os.path.join(self.m_htmlwidget.m_document_wd, fn)):
                        fn = os.path.normpath(os.path.join("../C", fn))
                    self.m_htmlwidget.read_file_rel(fn)
                self.m_loaded_file = fn
            if anchor and 'g_view' in dir(self.m_htmlwidget):
                self.m_htmlwidget.g_view.jump_to_anchor(anchor)
        except IOError:
            self.m_htmlwidget.source("<html>File not found: '%s'</html>" % fn)
    def on_anchor_track(self, url):
        if url:
            # remove newlines in url because it make the window resize
            s = url.replace("\n", "")
            self.m_statusbar.pop(1)
            self.m_statusbar.push(1, s)
        else:
            self.m_statusbar.pop(1)


