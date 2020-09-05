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
from docviewer import DocViewer
import history
import utils
import os

class HelpBrowser(gtk.Window):
    def __init__(self, app):
        gtk.Window.__init__(self)
        self.m_app = app
        self.m_history = history.History()
        self.g_action_group = gtk.ActionGroup('HelpBrowser')
        self.g_action_group.add_actions([
          ('HelpToolbar', None, 'help'),
          ('HelpBrowserBack', 'gtk-go-back', None, None, None,
            self.history_back),
          ('HelpBrowserForward', 'gtk-go-forward', None, None, None,
            self.history_forward),
          ('HelpBrowserRefresh', 'gtk-refresh', None, None, None, self.refresh),
          ('HelpBrowserHome', 'gtk-home', None, None, None, self.go_home),
          ('HelpBrowserClose', 'gtk-close', None, None, _("Close the window"), self.hide_on_delete),
          ])
        self.g_ui_manager = gtk.UIManager()
        self.g_ui_manager.insert_action_group(self.g_action_group, 0)
        self.g_ui_manager.add_ui_from_file("helpbrowser.xml")
        self.add_accel_group(self.g_ui_manager.get_accel_group())
        self.g_ui_manager.get_accel_group().connect_group(gtk.keysyms.W,
           gtk.gdk.CONTROL_MASK, 0,
           self.hide_on_delete)
        self.vbox = gtk.VBox()
        self.add(self.vbox)
        self.vbox.pack_start(self.g_ui_manager.get_widget("/HelpToolbar"), False)
        #self.add_accel_group(self.g_ui_manager.get_accel_group())
        self.set_default_size(550, 500)
        self.g_docviewer = DocViewer(self.handle_href)
        self.vbox.pack_start(self.g_docviewer)
        self.connect('delete_event', self.hide_on_delete)
        self.show_all()
    def go_home(self, action):
        self.show_docfile('index.html')
    def refresh(self, action):
        self.m_history.set_adj_of_current(self.g_docviewer.get_vadjustment().get_value())
        self.show_docfile(*self.m_history.get_current()[0].split("#"))
        while gtk.events_pending(): gtk.main_iteration()
        adj = self.m_history.get_current()[1]
        if not adj:
            adj = 0.0
        self.g_docviewer.get_vadjustment().set_value(adj)
    def history_back(self, action):
        self.m_history.set_adj_of_current(self.g_docviewer.get_vadjustment().get_value())
        self.m_history.back()
        self.m_history.lock()
        self.show_docfile(*self.m_history.get_current()[0].split("#"))
        self.m_history.unlock()
        while gtk.events_pending(): gtk.main_iteration()
        adj = self.m_history.get_current()[1]
        if not adj:
            adj = 0.0
        self.g_docviewer.get_vadjustment().set_value(adj)
    def history_forward(self, action):
        self.m_history.set_adj_of_current(self.g_docviewer.get_vadjustment().get_value())
        self.m_history.forward()
        self.m_history.lock()
        self.show_docfile(*self.m_history.get_current()[0].split("#"))
        self.m_history.unlock()
        while gtk.events_pending(): gtk.main_iteration()
        adj = self.m_history.get_current()[1]
        if not adj:
            adj = 0.0
        self.g_docviewer.get_vadjustment().set_value(adj)
    def handle_href(self, href):
        """
        If href is the name of a HTML file, then we display it.
        If not, we call m_app.handle_href to take care of it.
        """
        urlobj = utils.Url(href)
        if not(not urlobj.protocol and urlobj.filename.endswith(".html")):
            self.m_app.handle_href(href)
            return
        fn = href.split("#")[0]
        anchor = "#".join(href.split("#")[1:])
        # If history is locked, then we are probably called from the back
        # or forward functions, and they handle adjustment themselves.
        if not self.m_history.m_lock:
            self.m_history.set_adj_of_current(self.g_docviewer.get_vadjustment().get_value())
        saved_document_wd = self.g_docviewer.m_htmlwidget.m_document_wd
        self.g_docviewer.read_docfile(fn, anchor)
        # Find the absolute path to the file filename
        absname = os.path.join(os.getcwd(),
            saved_document_wd, self.g_docviewer.m_loaded_file)
        self.m_history.add(absname)
    def show_docfile(self, filename, anchor=None):
        """
        Load a html file relative to the help directory for the current
        language, for example help/C or help/no. But try the same file
        in help/C if the file does not exist for the language we are using.
        """
        absname = os.path.join(os.getcwd(), "help",
            self.g_docviewer.m_language, filename)
        if not os.path.exists(absname):
            absname = os.path.join(os.getcwd(), "help", "C", filename)
        self.m_history.add(absname)
        self.g_docviewer.read_docfile(absname, anchor)

if __name__ == '__main__':
    import sys
    sys.path.insert(0, ".")
    import src
    import src.i18n
    src.i18n.setup(".")
    w = HelpBrowser()
    w.connect('delete_event', gtk.main_quit)
    w.show()
    w.show_docfile("toc.html")
    gtk.main()
