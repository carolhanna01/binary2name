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

import gtk, soundcard
import gu, os
import cfg

class MidiInstrumentMenu(gtk.Menu):
    def __init__(self, callback):
        gtk.Menu.__init__(self)
        self.m_callback = callback
        for x in range(len(soundcard.instrument_names)):
            if x % 8 == 0:
                menuitem = gtk.MenuItem(soundcard.instrument_sections[x/8])
                submenu = gtk.Menu()
                self.append(menuitem)
                menuitem.set_submenu(submenu)
                menuitem.show()
            item = gtk.MenuItem(soundcard.instrument_names[x])
            item.connect('activate', self.on_activate)
            item.set_data('instrument', x)
            submenu.append(item)
            item.show()
        self.show()
    def on_activate(self, menuitem):
        self.m_callback(menuitem.get_data('instrument'))


class nInstrumentSelector(gtk.VBox, cfg.ConfigUtils):
    def __init__(self, exname, name, sizegroup):
        gtk.VBox.__init__(self)
        cfg.ConfigUtils.__dict__['__init__'](self, exname)
        self.m_name = name
        hbox = gu.bHBox(self)
        hbox.set_spacing(gu.PAD_SMALL)
        label = gtk.Label(_('_Instrument:'))
        label.set_alignment(0.0, 0.5)
        sizegroup.add_widget(label)
        hbox.pack_start(label, False, False)

        self.g_button = gtk.Button(
              soundcard.instrument_names[self.get_int(self.m_name)])
        self.g_button.connect('clicked', self.on_btnclick)
        label.set_use_underline(True)
        label.set_mnemonic_widget(self.g_button)
        hbox.pack_start(self.g_button)

        self.g_menu = MidiInstrumentMenu(self.on_instrument_selected)
        self.m_instrument = self.get_int('config/preferred_instrument')
        if self.get_int(self.m_name+'_velocity')==0:
            self.set_int(self.m_name+'_velocity',
                      self.get_int('config/preferred_instrument_velocity'))

        hbox = gtk.HBox()
        hbox.set_spacing(6)
        self.pack_start(hbox)

        label = gtk.Label(_("_Velocity:"))
        label.set_use_underline(True)
        label.set_alignment(0.0, 0.5)
        sizegroup.add_widget(label)
        hbox.pack_start(label, False)
        self.g_velocity = gu.nSpinButton(exname, self.m_name+'_velocity', gtk.Adjustment(self.get_int(self.m_name+'_velocity'), 0, 127, 1, 10), digits=0)
        label.set_mnemonic_widget(self.g_velocity)
        hbox.pack_start(self.g_velocity, True)

    def on_btnclick(self, *argv):
        self.g_menu.popup(None, None, None, 1, 0)
    def on_instrument_selected(self, instrument=None):
        self.set_int(self.m_name, instrument)
        self.g_button.get_children()[0].set_text(soundcard.instrument_names[instrument])
        self.m_instrument = instrument
        self.play_selected_instrument()
    def play_selected_instrument(self, _o=None):
        soundcard.play_note(self.m_instrument, 4, 60,
                            self.g_velocity.get_value_as_int())

    def show(self):
        self.show_all()

class ReadOnlyInstrumentSelector(gtk.Frame, cfg.ConfigUtils):
    def __init__(self, txt, section, name, on_change_callback=None):
        """
        ReadOnlyInstrumentSelector is used by ConfigWindow, because
        the config data should not be changed before the user press
        'apply' or 'ok'. The exercises' config page should use
        the InstrumentSelector class
        
        section: what section in config file to use, typically an exercise name
        name: name and name_velocity are used in configfile
        """
        gtk.Frame.__init__(self, txt)
        #cfg.ConfigUtils.__init__(self, section)
        cfg.ConfigUtils.__dict__['__init__'](self, section)
        self.m_name = name
        self.m_on_change_callback = on_change_callback
        vbox = gtk.VBox()
        vbox.set_border_width(gu.PAD_SMALL)
        self.add(vbox)
        self.g_button = gu.bButton(vbox,
              soundcard.instrument_names[self.get_int(self.m_name)],
              self.on_btnclick)
        self.g_menu = MidiInstrumentMenu(self.on_instrument_selected)
        self.m_instrument = self.get_int(name)
        if self.get_int(self.m_name+'_velocity')==0:
            self.set_int(self.m_name+'_velocity',
                      self.get_int('config/preferred_instrument_velocity'))
        hbox = gu.bHBox(vbox)
        hbox.set_spacing(gu.PAD_SMALL)
        label = gtk.Label(_("Velocity:"))
        label.set_alignment(1.0, 0.5)
        hbox.pack_start(label)
        self.g_velocity = gtk.SpinButton(gtk.Adjustment(self.get_int(self.m_name+'_velocity'), 0, 127, 1, 10), digits=0)
        self.g_velocity.connect('value-changed', self.spin_callback)
        ihbox = gu.bHBox(hbox, False)
        ihbox.pack_start(self.g_velocity, False)
        btn = gu.bButton(ihbox, None, self.play_selected_instrument, False)
        im = gtk.Image()
        im.set_from_file(os.path.join("graphics", "test-sound.png"))
        btn.add(im)
        self.show_all()
    def on_btnclick(self, *argv):
        self.g_menu.popup(None, None, None, 1, 0)
    def on_instrument_selected(self, instrument=None):
        self.g_button.get_children()[0].set_text(soundcard.instrument_names[instrument])
        self.m_instrument = instrument
        self.play_selected_instrument()
        if self.m_on_change_callback:
            self.m_on_change_callback()
    def spin_callback(self, spin):
        if self.m_on_change_callback:
            self.m_on_change_callback()
    def play_selected_instrument(self, _o=None):
        soundcard.play_note(self.m_instrument, 4, 60,
                            self.g_velocity.get_value_as_int())

class InstrumentSelector(ReadOnlyInstrumentSelector):
    def __init__(self, txt, section, name, on_change_callback=None):
        ReadOnlyInstrumentSelector.__init__(self, txt, section, name, on_change_callback)
        self.add_watch(self.m_name, self._instr_cb)
        self.add_watch(self.m_name+'_velocity', self._velocity_cb)
    def _instr_cb(self, name):
        self.g_button.get_children()[0].set_text(
              soundcard.instrument_names[self.get_int(name)])
    def _velocity_cb(self, name):
        self.g_velocity.set_value(self.get_float(name))
    def on_instrument_selected(self, instrument=None):
        ReadOnlyInstrumentSelector.on_instrument_selected(self, instrument)
        self.set_int(self.m_name, instrument)
    def spin_callback(self, p=None):
        ReadOnlyInstrumentSelector.spin_callback(self, p)
        self.set_int(self.m_name+'_velocity', p.get_value_as_int())

class InstrumentConfigurator(gtk.VBox, cfg.ConfigUtils):
    def __init__(self, exname, num, labeltext):
        gtk.VBox.__init__(self)
        #cfg.ConfigUtils.__init__(self, exname)
        cfg.ConfigUtils.__dict__['__init__'](self, exname)
        assert num in (2, 3)
        self.m_num = num
        self.g_override_default_instrument_checkbutton \
            = gu.nCheckButton(exname, 'override_default_instrument',
                labeltext,
                 callback=self.update_instrument_override)
        self.pack_start(self.g_override_default_instrument_checkbutton,
                        False)
        hbox = gu.bHBox(self)
        hbox.set_spacing(gu.PAD_SMALL)

        self.g_instrsel_high = InstrumentSelector(_("Highest instrument"),
                                                 exname, 'highest_instrument')
        hbox.pack_start(self.g_instrsel_high, False)
        if num == 3:
            self.g_instrsel_middle = InstrumentSelector(_("Middle instrument"),
                                            exname, 'middle_instrument')
            hbox.pack_start(self.g_instrsel_middle, False)
        else:
            self.g_instrsel_middle = None
        self.g_instrsel_low = InstrumentSelector(_("Lowest instrument"),
                                                exname, 'lowest_instrument')
        hbox.pack_start(self.g_instrsel_low, False)
        self.update_instrument_override()
    def update(self):
        self.update_instrument_override()
        self.g_instrsel_high.update()
        if self.m_num == 3:
            self.g_instrsel_middle.update()
        self.g_instrsel_low.update()
    def update_instrument_override(self, _o=None):
        self.g_override_default_instrument_checkbutton.set_active(
                self.get_bool('override_default_instrument'))
        self.g_instrsel_high.set_sensitive(
               self.g_override_default_instrument_checkbutton.get_active())
        self.g_instrsel_low.set_sensitive(
               self.g_override_default_instrument_checkbutton.get_active())
        if self.g_instrsel_middle:
            self.g_instrsel_middle.set_sensitive(
               self.g_override_default_instrument_checkbutton.get_active())
    def show(self):
        self.show_all()

