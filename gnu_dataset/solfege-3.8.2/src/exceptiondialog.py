# GNU Solfege - free ear training software
# Copyright (C) 2007  Tom Cato Amundsen
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


import sys
import locale
import traceback
import gtk

class ExceptionDialog(gtk.Dialog):
    def __init__(self, exception):
        gtk.Dialog.__init__(self, sys.exc_info()[0].__name__)
        self.set_resizable(False)
        self.set_border_width(6)
        self.set_has_separator(False)
        self.vbox.set_spacing(0)
        hbox = gtk.HBox()
        hbox.set_spacing(12)
        hbox.set_border_width(6)
        self.vbox.pack_start(hbox)
        vbox = gtk.VBox()
        hbox.pack_start(vbox, False, False)
        img = gtk.image_new_from_stock(gtk.STOCK_DIALOG_ERROR, gtk.ICON_SIZE_DIALOG)
        vbox.pack_start(img, False, False)
        vbox = gtk.VBox()
        hbox.pack_start(vbox)
        self.msg_vbox = gtk.VBox()
        vbox.pack_start(self.msg_vbox)
        try:
            # The exceptions defined in Solfege will nicely create an unicode
            # string
            estr = unicode(exception)
        except UnicodeDecodeError, e:
            # Standard python exceptions are more difficult.
            estr = str(exception).decode(locale.getpreferredencoding(), 'replace')
        self.g_primary = gtk.Label(estr)
        self.g_primary.set_alignment(0.0, 0.5)
        self.g_primary.set_line_wrap(True)
        self.m_primary_bold = False
        self.msg_vbox.pack_start(self.g_primary)
        # This label is here just for spacing
        l = gtk.Label("")
        vbox.pack_start(l)
        expander = gtk.Expander("Traceback")
        self.vbox.pack_start(expander)
        l = gtk.Label("".join(traceback.format_exception(
            sys.exc_type, sys.exc_value, sys.exc_traceback)))
        l.set_alignment(0.0, 0.5)
        sc = gtk.ScrolledWindow()
        sc.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        sc.set_size_request(-1, 100)
        expander.add(sc)
        sc.add_with_viewport(l)
        w = self.add_button(gtk.STOCK_CLOSE, gtk.RESPONSE_CLOSE)
        self.set_default_response(gtk.RESPONSE_CLOSE)
        self.set_focus(w)
        self.show_all()
    def _make_primary_bold(self):
        if not self.m_primary_bold:
            self.m_primary_bold = True
            self.g_primary.set_markup('<span weight="bold" size="larger">%s</span>' %
                self.g_primary.get_text())
    def _parsep(self):
        l = gtk.Label("")
        l.show()
        self.msg_vbox.pack_start(l)
    def add_text(self, text):
        self._make_primary_bold()
        self._parsep()
        # We add a empty string with a newline to get the spacing
        l = gtk.Label(text)
        l.set_line_wrap(True)
        l.set_alignment(0.0, 0.5)
        l.show()
        self.msg_vbox.pack_start(l)
    def add_nonwrapped_text(self, text):
        self._make_primary_bold()
        self._parsep()
        l = gtk.Label()
        l.set_markup('<span font_family="monospace">%s</span>' % text)
        l.set_line_wrap(False)
        l.set_alignment(0.0, 0.5)
        l.show()
        self.msg_vbox.pack_start(l)

if __name__ == '__main__':
    v = []
    try:
        print v[1]
    except Exception, e:
        d = ExceptionDialog(e)
        d.add_text("Para2 " * 20)
        d.add_nonwrapped_text("""(line 3) bla bla bla bla bla bla bla bla bla bla bla bla
(line 4) bla bla bla bla bla bla kjkkjkjkjkj kjkjkkkj bla bla bla bla bla bla
(line 5) bla bla bla bla bla bla bla bla bla bla bla bla""")
        d.run()
        d.destroy()
