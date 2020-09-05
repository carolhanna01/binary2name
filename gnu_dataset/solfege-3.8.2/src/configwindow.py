# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007  Tom Cato Amundsen
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
import os
import gobject, gtk
import gu, mpd
import configureoutput
import notenamespinbutton
from instrumentselector import nInstrumentSelector, InstrumentConfigurator
import cfg
import osutils
import soundcard
import languages

def find_wav_player_programs():
    ret = []
    for p, cmd in (('/usr/bin/play', '/usr/bin/play %s'),
                   ('/usr/bin/aplay', '/usr/bin/aplay %s'),
                   ('/usr/bin/esdplay', '/usr/bin/esdplay %s')):
        if os.path.exists(p):
            ret.append(cmd)
    return ret

def find_midi_player_programs():
    for p, cmd in (('/usr/bin/timidity', '/usr/bin/timidity -idqq %s'),
                   ('/usr/bin/drvmidi', '/usr/bin/drvmidi'),
                   ('/usr/bin/playmidi', '/usr/bin/playmidi')):
        if os.path.exists(p):
            yield cmd

class ConfigWindow(gtk.Dialog, cfg.ConfigUtils):
    def on_destroy(self, widget, e):
        self.m_app.m_ui.g_config_window.destroy()
        self.m_app.m_ui.g_config_window = None
    def __init__(self, app):
        gtk.Dialog.__init__(self, _("GNU Solfege Preferences"),
             app.m_ui, 0,
             (gtk.STOCK_CLOSE, gtk.RESPONSE_CLOSE))
        cfg.ConfigUtils.__init__(self, 'configwindow')
        self.connect('response', self.apply_and_close)
        # We do this so that the window is only hidden when the
        # user click on the close button provided by the window manager.
        self.connect('delete-event', self.on_destroy)#lambda w, e: True)
        self.set_default_size(400, 400)
        self.m_app = app
        self.g_notebook = gtk.Notebook()
        self.vbox.pack_start(self.g_notebook)
        ########
        # midi #
        ########
        page_vbox = gu.hig_dlg_vbox()
        self.g_notebook.append_page(page_vbox, gtk.Label(_("Midi stuff")))

        vbox, category_vbox = gu.hig_category_vbox(_("Tempo"))
        page_vbox.pack_start(vbox, False)
        sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)

        self.g_default_bpm = gu.nSpinButton('config', 'default_bpm',
            gtk.Adjustment(self.get_int('config/default_bpm'), 10, 500, 1, 10))
        box = gu.hig_label_widget(_("_Default bpm:"), self.g_default_bpm, sizegroup)
        category_vbox.pack_start(box, False)

        self.g_arpeggio_bpm = gu.nSpinButton('config', 'arpeggio_bpm',
            gtk.Adjustment(self.get_int('config/arpeggio_bpm'), 10, 500, 1, 10))
        box = gu.hig_label_widget(_("A_rpeggio bpm:"), self.g_arpeggio_bpm, sizegroup)
        category_vbox.pack_start(box, False)

        box, category_vbox = gu.hig_category_vbox(_("Preferred instrument"))
        page_vbox.pack_start(box, False)
        self.g_instrsel = nInstrumentSelector('config',
                        'preferred_instrument', sizegroup)
        category_vbox.pack_start(self.g_instrsel, False)

        box, category_vbox = gu.hig_category_vbox(_("Chord instruments"))
        page_vbox.pack_start(box, False)
        self.g_instrument_configurator  \
              = InstrumentConfigurator("config", 3,
                    _("Use different instruments for chords and harmonic intervals."))
        category_vbox.pack_start(self.g_instrument_configurator, False)

        vbox, category_box = gu.hig_category_vbox(_("Preferred percussion instruments"))
        page_vbox.pack_start(vbox, False)
        category_box.pack_start(gu.hig_label_widget(
            _("Count in:"),
            gu.PercussionNameComboBoxEntry("config", "countin_perc", "Claves"),
            sizegroup))
        category_box.pack_start(gu.hig_label_widget(
            _("Rhythm:"),
            gu.PercussionNameComboBoxEntry("config", "rhythm_perc", "Side Stick"),
            sizegroup))
        ########
        # user #
        ########
        page_vbox = gu.hig_dlg_vbox()
        self.g_notebook.append_page(page_vbox, gtk.Label(_("User")))

        box, category_vbox = gu.hig_category_vbox(_("User's voice"))
        page_vbox.pack_start(box, False)
        sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)

        self.g_highest_singable = notenamespinbutton.NotenameSpinButton(
            self.get_string('user/highest_pitch'))
        box = gu.hig_label_widget(_("Highest note user can sing:"),
                                  self.g_highest_singable,
                                  sizegroup)
        category_vbox.pack_start(box)

        self.g_lowest_singable = notenamespinbutton.NotenameSpinButton(
            self.get_string('user/lowest_pitch'))
        box = gu.hig_label_widget(_("Lowest note user can sing:"),
                                  self.g_lowest_singable,
                                  sizegroup)
        category_vbox.pack_start(box)
        notenamespinbutton.nNotenameRangeController(
                  self.g_lowest_singable, self.g_highest_singable,
                  mpd.LOWEST_NOTENAME, mpd.HIGHEST_NOTENAME,
                  'user', 'lowest_pitch', 'highest_pitch')


        box, category_vbox = gu.hig_category_vbox(_("Sex"))
        page_vbox.pack_start(box, False)
        self.g_sex_male = gtk.RadioButton(None, _("_Male"))
        self.g_sex_male.connect('toggled', lambda w: self.set_string('user/sex', 'male'))
        category_vbox.pack_start(self.g_sex_male, False)
        self.g_sex_female = gtk.RadioButton(self.g_sex_male, _("_Female or child"))
        self.g_sex_female.connect('toggled', lambda w: self.set_string('user/sex', 'female'))
        category_vbox.pack_start(self.g_sex_female, False)
        if self.get_string('user/sex') == 'female':
            self.g_sex_female.set_active(True)
        #######
        # gui #
        #######
        page_vbox = gu.hig_dlg_vbox()
        self.g_notebook.append_page(page_vbox, gtk.Label(_("Gui")))

        box, category_vbox = gu.hig_category_vbox(_("External programs"))
        page_vbox.pack_start(box, False)
        sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)

        self.g_mail_program = gtk.Entry()
        self.g_mail_program.set_text(self.get_string("config/mua"))
        self.g_mail_program.connect('changed', lambda w: self.set_string('config/mua', self.g_mail_program.get_text()))
        box = gu.hig_label_widget(_("_Mail program:"), self.g_mail_program, sizegroup)
        category_vbox.pack_start(box)
        # midi_to_wav
        liststore = gtk.ListStore(gobject.TYPE_STRING)
        liststore.append(("/usr/bin/timidity -Ow %(in)s -o %(out)s",))
        self.g_wav_convertor = gtk.ComboBoxEntry(liststore)
        self.g_wav_convertor.child.set_text(
            self.get_string("app/midi_to_wav_cmd"))
        self.g_wav_convertor.connect('changed', 
            lambda w: self.set_string('app/midi_to_wav_cmd', w.child.get_text()))
        category_vbox.pack_start(
            gu.hig_label_widget(_("MIDI to WAV convertor:"),
            self.g_wav_convertor, sizegroup))
        # wav_to_mp3
        liststore = gtk.ListStore(gobject.TYPE_STRING)
        liststore.append(("/usr/bin/lame %(in)s %(out)s",))
        self.g_mp3_convertor = gtk.ComboBoxEntry(liststore)
        self.g_mp3_convertor.child.set_text(
            self.get_string("app/wav_to_mp3_cmd"))
        self.g_mp3_convertor.connect('changed', 
            lambda w: self.set_string('app/wav_to_mp3_cmd', w.child.get_text()))
        category_vbox.pack_start(
            gu.hig_label_widget(_("WAV to MP3 convertor:"),
            self.g_mp3_convertor, sizegroup))
        # wav_to_ogg
        liststore = gtk.ListStore(gobject.TYPE_STRING)
        liststore.append(("/usr/bin/oggenc %(in)s",))
        self.g_ogg_convertor = gtk.ComboBoxEntry(liststore)
        self.g_ogg_convertor.child.set_text(
            self.get_string("app/wav_to_ogg_cmd"))
        self.g_ogg_convertor.connect('changed', 
            lambda w: self.set_string('app/wav_to_ogg_cmd', w.child.get_text()))
        category_vbox.pack_start(
            gu.hig_label_widget(_("WAV to OGG convertor:"),
            self.g_ogg_convertor, sizegroup))

        box, category_vbox = gu.hig_category_vbox(_("Misc"))
        page_vbox.pack_start(box, False)

        self.g_mainwin_user_resizeable = gu.nCheckButton('gui',
          'mainwin_user_resizeable', _("_User resizeable main window"))
        category_vbox.pack_start(self.g_mainwin_user_resizeable, False)

        self.g_expert_mode = gu.nCheckButton('gui', 
                'expert_mode', _("E_xpert mode"))
        self.g_expert_mode.connect('toggled', self.m_app.reset_exercise)
        category_vbox.pack_start(self.g_expert_mode, False)
        # Combobox to select language
        hbox = gtk.HBox()
        hbox.set_spacing(6)
        label = gtk.Label()
        label.set_text_with_mnemonic(_("Select _language:"))
        hbox.pack_start(label, False)
        self.g_language = gtk.combo_box_new_text()
        for n in languages.languages:
            self.g_language.append_text(n)
        label.set_mnemonic_widget(self.g_language)
        if cfg.get_string('app/lc_messages') in languages.languages:
            idx = languages.languages.index(cfg.get_string('app/lc_messages'))
            self.g_language.set_active(idx)
        else:
            self.g_language.set_active(0)
        def f(combobox):
            cfg.set_string('app/lc_messages', languages.languages[combobox.get_active()])
        self.g_language.connect_after('changed', f)
        hbox.pack_start(self.g_language, False)
        category_vbox.pack_start(hbox)
        l = gtk.Label(_("You have to restart the program for the language change to take effect."))
        l.set_alignment(0.0, 0.5)
        category_vbox.pack_start(l)
        ############
        # Practise #
        ############
        page_vbox = gu.hig_dlg_vbox()
        self.g_notebook.append_page(page_vbox, gtk.Label(_("Practise")))

        box, category_vbox = gu.hig_category_vbox(_("Practise"))
        page_vbox.pack_start(box, False)

        self.g_picky_on_new_question = gu.nCheckButton('config', 'picky_on_new_question', _("_Not allow new question before the old is solved"))
        category_vbox.pack_start(self.g_picky_on_new_question, False)

        self.g_autorepeat_if_wrong = gu.nCheckButton('config', 'auto_repeat_question_if_wrong_answer', _("_Repeat question if the answer was wrong"))
        category_vbox.pack_start(self.g_autorepeat_if_wrong, False)


        #########
        # sound #
        #########
        if sys.platform == 'win32':
            self.create_win32_sound_page()
        else:
            self.create_linux_sound_page()
        self.show_all()
    def create_linux_sound_page(self):
        page_vbox = gu.hig_dlg_vbox()
        self.g_notebook.append_page(page_vbox, gtk.Label(_("Sound setup")))

        box, category_vbox = gu.hig_category_vbox(_("External programs"))
        page_vbox.pack_start(box, False)

        sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)
        liststore = gtk.ListStore(gobject.TYPE_STRING)
        for n in find_wav_player_programs():
            liststore.append((n,))
        self.g_wav_player = gtk.ComboBoxEntry(liststore)
        self.g_wav_player.child.set_text(cfg.get_string('sound/wav_player'))
        self.g_wav_player.connect('changed',
            lambda w: self.set_string('sound/wav_player', w.child.get_text()))
        button = gtk.Button(_("Test"))
        button.connect('clicked', self.test_wav_player)
        box = gu.hig_label_widget(_(".wav file player"),
                                  [self.g_wav_player, button],
                                  sizegroup)
        category_vbox.pack_start(box)

        liststore = gtk.ListStore(gobject.TYPE_STRING)
        for s in find_midi_player_programs():
            liststore.append((s,))
        self.g_midi_player = gtk.ComboBoxEntry(liststore)
        self.g_midi_player.child.set_text(cfg.get_string('sound/midi_player'))
        self.g_midi_player.connect('changed',
            lambda w: self.set_string('sound/midi_player', w.child.get_text()))
        button = gtk.Button(_("Test"))
        button.connect('clicked', self.test_midi_player)
        box = gu.hig_label_widget(_(".midi file player"),
                                  [self.g_midi_player, button],
                                  sizegroup)
        category_vbox.pack_start(box)

        #############
        # midi setup
        #############
        box, category_vbox = gu.hig_category_vbox(_("Midi setup"))
        page_vbox.pack_start(box)

        self.g_fakesynth_radio = gu.RadioButton(None, _("_No sound"), None)
        category_vbox.pack_start(self.g_fakesynth_radio, False)

        hbox = gu.bHBox(category_vbox, False)
        self.g_device_radio = gu.RadioButton(self.g_fakesynth_radio,
              _("Use _device"), None)
        hbox.pack_start(self.g_device_radio, False)

        liststore = gtk.ListStore(gobject.TYPE_STRING)
        for s in ('/dev/sequencer', '/dev/sequencer2', '/dev/music'):
            liststore.append((s,))
        self.g_device_file = gtk.ComboBoxEntry(liststore)
        self.g_device_file.child.set_text(self.get_string('sound/device_file'))
        self.g_synth_num = gtk.SpinButton(gtk.Adjustment(0, 0, 100, 1, 1),
                             digits=0)
        self.g_synth_num.set_value(self.get_int('sound/synth_number'))
        hbox.pack_start(self.g_device_file, False)
        hbox.pack_start(self.g_synth_num, False)

        # checkbox to enable awe support
        hbox = gtk.HBox()
        category_vbox.pack_start(hbox, False)
        hbox.pack_start(gtk.Label("    "))
        self.g_awe_checkbutton = gtk.CheckButton(_("_My sound card is Sound Blaster AWE32, AWE64 or pnp32"))
        self.g_awe_checkbutton.set_active(self.get_string('sound/card_info') == 'awe')
        if not configureoutput.HAVE_LINUX_AWE_VOICE_H:
            self.g_awe_checkbutton.set_sensitive(False)
        else:
            self.g_awe_checkbutton.set_sensitive(self.get_string("sound/type") == "sequencer-device")
        hbox.pack_start(self.g_awe_checkbutton, False)
        ###
        hbox = gu.bHBox(category_vbox, False)
        self.g_midiplayer_radio = gu.RadioButton(self.g_fakesynth_radio,
             _("Use _external midiplayer"), None)
        hbox.pack_start(self.g_midiplayer_radio, False)

        if self.get_string("sound/type") == "external-midiplayer":
            self.g_midiplayer_radio.set_active(True)
        elif self.get_string("sound/type") == "sequencer-device":
            self.g_device_radio.set_active(True)
        else:
            self.g_fakesynth_radio.set_active(True)

        hbox = gtk.HButtonBox()
        category_vbox.pack_start(hbox)
        gu.bButton(category_vbox, _("_Apply changes and play test sound"), self.on_apply_and_play_test_sound)

        self.g_device_radio.connect('toggled', lambda w: self.g_awe_checkbutton.set_sensitive(w.get_active()))
    def create_win32_sound_page(self):
        page_vbox = gu.hig_dlg_vbox()
        self.g_notebook.append_page(page_vbox, gtk.Label(_("Midi setup")))

        box, category_vbox = gu.hig_category_vbox(_("External programs"))
        page_vbox.pack_start(box, False)

        liststore = gtk.ListStore(gobject.TYPE_STRING)
        for s in ('/usr/bin/timidity -idqq %s', '/usr/bin/playmidi -f %s'):
            liststore.append((s, ))
        self.g_midi_player = gtk.ComboBoxEntry(liststore)
        self.g_midi_player.child.set_text(cfg.get_string('sound/midi_player'))
        self.g_midi_player.connect('changed',
            lambda w: self.set_string('sound/midi_player', w.child.get_text()))
        button = gtk.Button(_("Test"))
        button.connect('clicked', self.test_midi_player)
        box = gu.hig_label_widget(".midi file player",
                                  [self.g_midi_player, button],
                                  None)
        category_vbox.pack_start(box)

        #############
        # midi setup
        #############
        box, category_vbox = gu.hig_category_vbox(_("Sound setup"))
        page_vbox.pack_start(box)
        txt = gtk.Label(_("""Solfege has two ways to play midi files. It is recommended to use Windows multimedia output. An external midiplayer can be useful if your soundcard lacks a hardware synth, in which case you have to use a program like timidity to play the music."""))
        txt.set_line_wrap(1)
        txt.set_justify(gtk.JUSTIFY_FILL)
        txt.set_alignment(0.0, 0.0)
        category_vbox.pack_start(txt, False)

        self.g_fakesynth_radio = gu.RadioButton(None, _("_No sound"), None)
        category_vbox.pack_start(self.g_fakesynth_radio, False)

        hbox = gu.bHBox(category_vbox, False)
        self.g_device_radio = gu.RadioButton(self.g_fakesynth_radio,
              _("_Windows multimedia output, synth number:"), None)
        self.g_synth_num = gtk.SpinButton(gtk.Adjustment(0, 0, 100, 1, 1),
                             digits=0)
        self.g_synth_num.set_value(self.get_int('sound/synth_number'))
        hbox.pack_start(self.g_device_radio, False)
        hbox.pack_start(self.g_synth_num, False)

        hbox = gu.bHBox(category_vbox, False)
        self.g_midiplayer_radio = gu.RadioButton(self.g_fakesynth_radio,
             _("Use _external midiplayer"), None)
        hbox.pack_start(self.g_midiplayer_radio, False)

        if self.get_string("sound/type") == "external-midiplayer":
            self.g_midiplayer_radio.set_active(True)
        elif self.get_string("sound/type") == "winsynth":
            self.g_device_radio.set_active(True)
        else:
            self.g_fakesynth_radio.set_active(True)

        gu.bButton(category_vbox, _("_Apply changes and play test sound"), self.on_apply_and_play_test_sound)

    def test_wav_player(self, w):
        osutils.run_external_program(self.g_wav_player.child.get_text(),
                 'lesson-files/share', 'fifth-small-220.00.wav')
    def test_midi_player(self, w):
        osutils.run_external_program(self.g_midi_player.child.get_text(),
                        'lesson-files/share', 'fanfare.midi')
    def set_gui_from_config(self):
        if self.get_string("sound/type") == "fake-synth":
            self.g_fakesynth_radio.set_active(True)
        elif self.get_string("sound/type") == "external-midiplayer":
            self.g_midiplayer_radio.set_active(True)
        else:
            assert self.get_string("sound/type") in ("winsynth", "sequencer-device")
            self.g_device_radio.set_active(True)
        return -1
    def apply_and_close(self, w, response):
        if response ==  gtk.RESPONSE_DELETE_EVENT:
            self.set_gui_from_config()
        else:
            if self.on_apply() == -1:
                self.set_gui_from_config()
                return
        self.hide()
    def on_apply_and_play_test_sound(self, *w):
        if self.on_apply() != -1:
            self.set_gui_from_config()
            self.play_midi_test_sound()
    def play_midi_test_sound(self):
        mpd.play_music(r"""
        \staff\relative c{
          c16 e g c e, g c e g, c e g c4
        }
        \staff{
          c4 e g8 e c4
        }
        """, 130, 0, 100)
    def on_help(self, *v):
        #FIXME It is stupid to display this help in the main window.
        self.m_app.handle_href("preferences-window.html")
    def on_apply(self, *v):
        """Returns -1 if sound init fails."""
        card_info = ""
        if soundcard.synth:
            soundcard.synth.close()
        if self.g_midiplayer_radio.get_active():
            soundcard.initialise_external_midiplayer(
                  self.g_midi_player.child.get_text())
            soundcard.synth.error_report_cb = self.m_app.m_ui.display_error_message
        elif self.g_device_radio.get_active():
            if sys.platform != 'win32' and configureoutput.HAVE_LINUX_AWE_VOICE_H and \
                self.g_awe_checkbutton.get_active():
                card_info = "awe"
            else:
                card_info = ""
            try:
                if sys.platform == 'win32':
                    soundcard.initialise_winsynth(self.g_synth_num.get_value_as_int())
                else:
                    soundcard.initialise_devicefile(
                        self.g_device_file.child.get_text(),
                        self.g_synth_num.get_value_as_int(),
                        card_info)
            except (soundcard.SoundInitException, OSError, ImportError), e:
                self.m_app.display_sound_init_error_message(e)
                return -1
        else: # no sound
            assert self.g_fakesynth_radio.get_active()
            soundcard.initialise_using_fake_synth(0)
        if self.g_midiplayer_radio.get_active():
            self.set_string("sound/type", "external-midiplayer")
        elif self.g_device_radio.get_active():
            if sys.platform == "win32":
                self.set_string("sound/type", "winsynth")
            else:
                self.set_string("sound/type", "sequencer-device")
        else:
            assert self.g_fakesynth_radio.get_active()
            self.set_string("sound/type", "fake-synth")
        if sys.platform != 'win32':
            self.set_string("sound/device_file", self.g_device_file.child.get_text())
            self.set_string("sound/card_info", card_info)
        if soundcard.synth.m_type_major not in ('Midifile', 'Fake'):
            self.set_int("sound/synth_number", soundcard.synth.m_devnum)
            # we set the spin just in case m_devnum was changed by the
            # soundcard setup code, if it was out of range
            self.g_synth_num.set_value(soundcard.synth.m_devnum)
        if sys.platform != 'win32':
            self.set_string("sound/wav_player",
                            self.g_wav_player.child.get_text())
        self.set_string("sound/midi_player",
                        self.g_midi_player.child.get_text())

