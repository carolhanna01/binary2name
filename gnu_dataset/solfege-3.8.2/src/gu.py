# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2006, 2007  Tom Cato Amundsen
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
import gobject
import re
import soundcard
import cfg
import os

PAD = 8
PAD_SMALL = 4

#  Prefixes used in this module:
#  t  pack into a table, the first five parameters are table, x1, x2, y1, y2
#  n  a widget that get its state stored in ~/.solfegerc
#  b  the widget is packed into the first argument


def tLabel(table, x1, x2, y1, y2, text="", xalign=0.0, yalign=0.5, xoptions=gtk.EXPAND|gtk.FILL, yoptions=gtk.EXPAND|gtk.FILL, xpadding=0, ypadding=0):
    label = gtk.Label(text)
    label.set_alignment(xalign, yalign)
    table.attach(label, x1, x2, y1, y2, xoptions=xoptions, yoptions=yoptions, xpadding=xpadding, ypadding=ypadding)
    return label

def bLabel(pack_into, label, expand=True, fill=True):
    b = gtk.Label(label)
    b.show()
    pack_into.pack_start(b, expand, fill)
    return b

def bButton(pack_into, label, callback=None, expand=True, fill=True):
    b = gtk.Button(label)
    b.show()
    if callback:
        b.connect('clicked', callback)
    pack_into.pack_start(b, expand, fill)
    return b

class nSpinButton(gtk.SpinButton, cfg.ConfigUtils):#FIXME (??what is there to fix???)
    def __init__(self, exname, name, adj, climb_rate=1, digits=1):
        gtk.SpinButton.__init__(self, adj, climb_rate, digits)
        cfg.ConfigUtils.__init__(self, exname)
        self.m_name = name
        self.set_digits(0)
        self.show()
        self.set_value(self.get_float(self.m_name))
        if self.get_value() != self.get_float(self.m_name):
            self.set_float(self.m_name, self.get_value())
        self.connect('value-changed', self.on_changed)
        self._watch_id = self.add_watch(self.m_name, self._watch_cb)
        self.m_stop_watch = 0
    def _watch_cb(self, name):
        if not self.m_stop_watch:
            gtk.SpinButton.set_value(self, self.get_float(name))
    def set_value(self, value):
        gtk.SpinButton.set_value(self, value)
        self.set_float(self.m_name, value)
    def on_changed(self, _o):
        self.m_stop_watch = 1
        self.set_float(self.m_name, self.get_value())
        self.m_stop_watch = 0

def tSpinButton(table, x1, x2, y1, y2,
                value, lower, upper, step_incr=1, page_incr=10, callback=None):
    adj = gtk.Adjustment(value, lower, upper, step_incr, page_incr)
    spin = gtk.SpinButton(adj, digits=0)
    if callback:
        spin.connect('value-changed', callback)
    table.attach(spin, x1, x2, y1, y2)
    return spin

def bHBox(pack_into, expand=True, fill=True, padding=0):
    b = gtk.HBox()
    b.show()
    pack_into.pack_start(b, expand, fill, padding)
    return b

def bVBox(pack_into, expand=True, fill=True, padding=0):
    b = gtk.VBox()
    pack_into.pack_start(b, expand, fill, padding)
    return b

class nCheckButton(gtk.CheckButton, cfg.ConfigUtils):
    def __init__(self, exname, name, label=None, default_value=0, callback=None):
        gtk.CheckButton.__init__(self, label)
        #cfg.ConfigUtils.__init__(self, exname)
        cfg.ConfigUtils.__dict__['__init__'](self, exname)
        self.m_name = name
        self.m_callback = callback
        self.show()
        if default_value:
            s = "true"
        else:
            s = "false"
        self.set_bool(self.m_name, self.get_bool(self.m_name+"="+s))
        if self.get_bool(self.m_name):
            self.set_active(1)
        self._clicked_id = self.connect('toggled', self.on_clicked)
        self._watch_id = self.add_watch(self.m_name, self._watch_cb)
    def _watch_cb(self, name):
        self.set_active(self.get_bool(name))
    def on_clicked(self, _o):
        self.set_bool(self.m_name, self.get_active())
        if self.m_callback:
            self.m_callback(_o)
 
def RadioButton(group, label, callback=None):
    rdb = gtk.RadioButton(group, label)
    if callback:
        rdb.connect('toggled', callback)
    rdb.show()
    return rdb

class nCombo(gtk.Combo, cfg.ConfigUtils):
    def __init__(self, exname, name, default, popdown_strings):
        """
        Be aware that the value of the entry, is stored as an integer
        popdown_strings.index(entry.get_text()), so if popdown_strings
        changes when upgrading the program, the value of the combo
        might change.

        Despite this problems, I do it this way, because if we store
        the actual value of the entry, we get into trouble when running
        the program with other locale settings.
        """
        gtk.Combo.__init__(self)
        #cfg.ConfigUtils.__init__(self, exname)
        cfg.ConfigUtils.__dict__['__init__'](self, exname)
        self.popdown_strings = popdown_strings
        self.m_name = name
        self.set_value_in_list(True, False)
        self.set_popdown_strings(popdown_strings)
        i = self.get_int_with_default(name, -1)
        if i == -1:
            i = popdown_strings.index(default)
        self.entry.set_text(popdown_strings[i])
        self.entry.connect("changed", self.entry_changed)
        self.entry.set_editable(False)
        self.show()
    def entry_changed(self, entry):
        self.set_int(self.m_name, self.popdown_strings.index(entry.get_text()))

class PercussionNameComboBoxEntry(gtk.ComboBoxEntry, cfg.ConfigUtils):
    def __init__(self, exname, name, default):
        liststore = gtk.ListStore(gobject.TYPE_STRING)
        for pn in soundcard.percussion_names:
            liststore.append((pn,))
        gtk.ComboBoxEntry.__init__(self, liststore)
        cfg.ConfigUtils.__init__(self, exname)
        self.m_name = name
        i = self.get_int(name)
        if not i:
            i = soundcard.percussionname_to_int(default)
            self.set_int(name, i)
        self.child.set_text(soundcard.int_to_percussionname(i))
        self.connect("changed", self.entry_changed)
    def entry_changed(self, widget):
        #FIXME value 35 should be taken from soundcard module.
        self.set_int(self.m_name, widget.get_active()+35)


class FlashBar(gtk.Frame):
    def __init__(self):
        gtk.Frame.__init__(self)
        self.set_shadow_type(gtk.SHADOW_IN)
        #FIXME different gtk themes can make these values wrong
        self.set_size_request(-1, 40)
        self.__stack = []
        self.__label = HarmonicProgressionLabel('')
        self.add(self.__label)
        self.__timeout = None
        # The allocated size
        self.m_sx, self.m_sy = 0, 0
    def require_size(self, stringlist):
        """
        stringlist is a list of the strings believed to be widest.
        require_size will make sure that the flashbar is at least large enough to
        show all the strings in stringlist.
        """
        for s in stringlist:
            self.__label.set_text(s)
            x, y =  self.__label.size_request()
            self.m_sx = max(x, self.m_sx)
            self.m_sy = max(y, self.m_sy)
        self.__label.set_text("")
        self.__label.set_size_request(self.m_sx, self.m_sy)
    def flash(self, txt):
        """Display a message that is automatically removed after some time.
        If we flash a new message before the old flashed message are removed,
        we old flashed message are removed.
        """
        if self.__timeout:
            gobject.source_remove(self.__timeout)
        self.__label.set_size_request(-1, -1)
        self.__label.set_text(txt)
        sx, sy = self.__label.size_request()
        self.m_sx = max(sx, self.m_sx)
        self.m_sy = max(sy, self.m_sy)
        self.__label.set_size_request(self.m_sx, self.m_sy)
        def f(self=self):
            self.__timeout = None
            if self.__stack:
                self.__label.set_text(self.__stack[-1])
            else:
                self.__label.set_text('')
        self.__timeout = gobject.timeout_add(2000, f)
    def push(self, txt):
        # stop any flashing before we push
        if self.__timeout:
            gobject.source_remove(self.__timeout)
            self.__timeout = None
        self.__stack.append(txt)
        self.__label.set_text(txt, '')
    def pop(self):
        """If a message is being flashed right now, that flashing is
        not affected, but the message below the flashed message is removed.
        """
        if self.__stack:
            self.__stack.pop()
        if not self.__timeout:
            if self.__stack:
                self.__label.set_text(self.__stack[-1])
            else:
                self.__label.set_text('')
    def clear(self):
        self.__stack = []
        if not self.__timeout:
            self.__label.set_text('')
    def set(self, txt):
        """
        Empty the stack of messages and display the string in txt.
        """
        self.__stack = [txt]
        self.__label.set_text(txt)


def hig_dlg_vbox():
    """a GtkVBox containing as many rows as you wish to have categories
    inside the control area of the GtkDialog.
    """
    vbox = gtk.VBox()
    vbox.set_spacing(18)
    vbox.set_border_width(12)
    return vbox

def hig_category_vbox(title):
    """
    Return a tuple of two boxes:
    box1 -- a box containing everything including the title. Useful
            if you have to hide a category.
    box2    The box you should pack your stuff in.
    """
    vbox = gtk.VBox()
    vbox.set_spacing(6)
    label = gtk.Label('<span weight="bold">%s</span>' % title)
    label.set_use_markup(True)
    label.set_alignment(0.0, 0.0)
    vbox.pack_start(label, False)
    hbox = gtk.HBox()
    vbox.pack_start(hbox, False)
    fill = gtk.Label("    ")
    hbox.pack_start(fill, False)
    category_content_vbox = gtk.VBox()
    hbox.pack_start(category_content_vbox, True)
    category_content_vbox.set_spacing(6)
    vbox.show_all()
    return vbox, category_content_vbox

def hig_label_widget(txt, widget, sizegroup):
    """
    Return a box containing a label and a widget, aligned nice
    as the HIG say we should.
    """
    hbox = gtk.HBox()
    hbox.set_spacing(6)
    label = gtk.Label(txt)
    label.set_alignment(0.0, 0.5)
    if sizegroup:
        sizegroup.add_widget(label)
    hbox.pack_start(label, False)
    if type(widget) != type([]):
        widget = [widget]
    for w in widget:
        hbox.pack_start(w,False)
    label.set_use_underline(True)
    label.set_mnemonic_widget(widget[0])
    return hbox

class SpinButtonRangeController(object):
    def __init__(self, spin_low, spin_high, lowest_value, highest_value):
        self.g_spin_low = spin_low
        self.g_spin_low.connect('value-changed', self.on_low_changed)
        self.g_spin_high = spin_high
        self.g_spin_high.connect('value-changed', self.on_high_changed)
        self.m_lowest_value = lowest_value
        self.m_highest_value = highest_value
    def on_low_changed(self, widget, *v):
        if widget.get_value() > self.g_spin_high.get_value():
            self.g_spin_low.set_value(self.g_spin_high.get_value())
        elif widget.get_value() < self.m_lowest_value:
            self.g_spin_low.set_value(self.m_lowest_value)
    def on_high_changed(self, widget, *v):
        if widget.get_value() < self.g_spin_low.get_value():
            self.g_spin_high.set_value(self.g_spin_low.get_value())
        elif widget.get_value() > self.m_highest_value:
            self.g_spin_high.set_value(self.m_highest_value)

def create_stock_menu_item(stock, txt, callback, ag, accel_key, accel_mod):
    box = gtk.HBox()
    box.set_spacing(gu.PAD_SMALL)
    im = gtk.Image()
    im.set_from_stock(stock, gtk.ICON_SIZE_MENU)
    item = gtk.ImageMenuItem(txt)
    item.set_image(im)
    if accel_key != 0:
        item.add_accelerator('activate', ag, accel_key, accel_mod, gtk.ACCEL_VISIBLE)
    item.connect('activate', callback)
    return item

class HarmonicProgressionLabel(gtk.HBox):
    """
    This class can parse strings like I-(6,4)V(5,3)-I and can be used
    as button labels.
    """
    def __init__(self, str, align=''):
        gtk.HBox.__init__(self)
        self.show()
        self.set_text(str, align)
    def set_text(self, str, align=''):
        for o in self.get_children():
            o.destroy()
        self.m_str = str
        if align == 'center':
            self.pack_start(gtk.HBox())
        while self.m_str:
            T, A, B = self.get_next_token()
            if T == 'big' or T == 'err':
                self.bigchar(A)
            elif T == 'two':
                self.twoline(A, B)
            else:
                assert T == 'one'
                self.oneline(A)
        if align == 'center':
            self.pack_start(gtk.HBox())
        self.show_all()
    def get_next_token(self):
        m_re1 = re.compile("([^\(]+)", re.UNICODE)
        m_re2 = re.compile("\((\w*),\s*(\w*)\)", re.UNICODE)
        m_re3 = re.compile("\((\w*)\)", re.UNICODE)
        m1 = m_re1.match(self.m_str)
        m2 = m_re2.match(self.m_str)
        m3 = m_re3.match(self.m_str)
        if m1:
            self.m_str = self.m_str[len(m1.group()):]
            return "big", m1.groups()[0], None
        if m2:
            self.m_str = self.m_str[len(m2.group()):]
            return "two", m2.groups()[0], m2.groups()[1]
        if m3:
            self.m_str = self.m_str[len(m3.group()):]
            return "one", m3.groups()[0], None
        return "err"
    def twoline(self, A, B):
        vbox = gtk.VBox()
        t1 = gtk.Label(A)
        t1.set_name("ProgressionLabelNumber")
        t1.show();vbox.pack_start(t1);
        t1.set_alignment(0, 0);
        t2 = gtk.Label(B)
        t2.set_name("ProgressionLabelNumber")
        t2.show();vbox.pack_start(t2);
        t2.set_alignment(0, 0);
        self.pack_start(vbox, False)
    def oneline(self, A):
        vbox = gtk.VBox()
        t = gtk.Label(A)
        t.set_name("ProgressionLabelNumber")
        t.show();vbox.pack_start(t);
        t.set_alignment(0, 0);
        self.pack_start(vbox, False)
    def bigchar(self, A):
        t1 = gtk.Label(A)
        t1.set_name("ProgressionNameLabel")
        t1.show()
        self.pack_start(t1, False)

def dialog_yesno(text, parent=None):
    """Return True if the answer is yes, False if the answer is no.
    """
    m = gtk.MessageDialog(parent, gtk.DIALOG_MODAL, gtk.MESSAGE_QUESTION,
            gtk.BUTTONS_YES_NO, text)
    ret = m.run()
    m.destroy()
    return ret == gtk.RESPONSE_YES

def dialog_ok(text, parent=None, secondary_text=None):
    """"
    Return the gtk.RESPONSE_XXXX returned by .run()
    """
    m = gtk.MessageDialog(parent, gtk.DIALOG_MODAL, gtk.MESSAGE_INFO,
            gtk.BUTTONS_OK, text)
    if secondary_text:
        m.format_secondary_text(secondary_text)
    ret = m.run()
    m.destroy()
    return ret

class NewLineBox(gtk.VBox):
    def __init__(self):
        gtk.VBox.__init__(self)
        self.m_todo_widgets = []
    def add_widget(self, widget):
        self.m_todo_widgets.append(widget)
    def show_widgets(self):
        if 'newline' in self.m_todo_widgets:
            self._newline_show_widgets()
        else:
            self._flow_show_widgets()
    def newline(self):
        self.m_todo_widgets.append('newline')
    def _newline_show_widgets(self):
        sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)
        hbox = bHBox(self, True)
        for n in self.m_todo_widgets:
            if n == 'newline':
                hbox = bHBox(self, False)
            else:
                hbox.pack_start(n, False)
                sizegroup.add_widget(n)
    def _flow_show_widgets(self):
        w = 8
        num_lines = len(self.m_todo_widgets) // w + 1
        w = len(self.m_todo_widgets) // num_lines + 1
        sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)
        c = 0
        for n in self.m_todo_widgets:
            sizegroup.add_widget(n)
            if c % w == 0:
                hbox = bHBox(self, True, True)
            hbox.pack_start(n, False)
            c += 1
        self.show_all()
    def empty(self):
        self.foreach(lambda w: w.destroy())
        self.m_todo_widgets = []
    def get_max_child_height(self):
        return max([c.size_request()[1] for c in self.m_todo_widgets])

def create_png_image(fn):
    """
    Create an image by loading a png file from graphics dir
    """
    im = gtk.Image()
    im.set_from_file(os.path.join('graphics', fn)+'.png')
    im.show()
    return im

def create_rhythm_image(rhythm):
    """
    rhythm : a string like 'c8 c8' or 'c8 c16 c16'
    The image returned is shown.
    """
    im = gtk.Image()
    im.set_from_file(os.path.join('graphics', 'rhythm-%s.png' % (rhythm.replace(" ", ""))))
    im.show()
    return im

