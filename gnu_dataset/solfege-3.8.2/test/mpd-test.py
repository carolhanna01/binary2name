#!/usr/bin/python

import pygtk
pygtk.require("2.0")
import sys, os
sys.path.append(".")
import src.i18n
src.i18n.setup(".")

import src


import gnome.ui
import mpd, mpd.musicdisplayer, soundcard
import gtk
from src import gu, cfg
cfg.initialise(None, None, 'mpd-test')
import src.utils

class DisplaytestWindow(gtk.Window):
    def on_quit(self, w):
        cfg.sync()
    def __init__(self):
        gtk.Window.__init__(self)
        self.connect('destroy', self.on_quit)
        self.vbox = vbox = gtk.VBox()
        vbox.show()
        self.add(vbox)
        self.g_text = gtk.TextView()
        self.g_text.set_size_request(-1, 100)
        self.g_text.show()
        self.g_text.set_editable(True)
        s = cfg.get_string('config/score_displayer')
        if not s:
            s = r"\staff{c' d' e'}"
        self.m_buf = self.g_text.get_buffer()
        self.m_buf.insert(self.m_buf.get_end_iter(), s)
        vbox.pack_start(self.g_text)
        self.g_displayer = mpd.musicdisplayer.MusicDisplayer(src.utils.play_tone)
        self.g_displayer.set_size_request(200, 200)
        self.g_displayer.show()
        self.vbox.pack_start(self.g_displayer)
        gu.bButton(vbox, "Parse", self.on_parse)
        gu.bButton(vbox, "Display", self.on_display)
        gu.bButton(vbox, "Display first notes", self.on_display_first_notes)
        #gu.bButton(vbox, "Display last notes", self.on_display_last_notes)
        gu.bButton(vbox, "Play", self.on_play)
        gu.bButton(vbox, "Play first", self.on_play_first)
    def on_parse(self, _o):
        t = self.m_buf.get_text(self.m_buf.get_start_iter(),
                                self.m_buf.get_end_iter(), True)
        score = mpd.parser.parse_to_score_object(t)
    def on_display(self, _o):
        t = self.m_buf.get_text(self.m_buf.get_start_iter(),
                                self.m_buf.get_end_iter(), True)
        cfg.set_string('config/score_displayer', t)
        self.g_displayer.display(t, 20)
    def on_display_first_notes(self, _o):
        t = self.m_buf.get_text(self.m_buf.get_start_iter(),
                                self.m_buf.get_end_iter(), True)
        cfg.set_string('config/score_displayer', t)
        self.g_displayer.display(t, 20, mpd.FIRST)
    def on_display_last_notes(self, _o):
        t = self.m_buf.get_text(self.m_buf.get_start_iter(),
                                self.m_buf.get_end_iter(), True)
        cfg.set_string('config/score_displayer', t)
        self.g_displayer.display(t, 20, mpd.LAST)
    def on_play(self, _o):
        t = self.m_buf.get_text(self.m_buf.get_start_iter(),
                                self.m_buf.get_end_iter(), True)
        cfg.set_string('config/score_displayer', t)
        mpd.play_music(t, (120, 4), 0, 100)
    def on_play_first(self, _o):
        t = self.m_buf.get_text(self.m_buf.get_start_iter(),
                                self.m_buf.get_end_iter(), True)
        tr = mpd.music_to_track(t, 100, mpd.Rat(0, 1), mpd.Rat(1, 8))
        soundcard.synth.play_track(tr)


#soundcard.initialise_devicefile("/dev/sequencer", 0)
#soundcard.initialise_devicefile("/dev/music", 0)
#soundcard.initialise_using_fake_synth()
soundcard.initialise_external_midiplayer("/usr/bin/timidity %s")

w = DisplaytestWindow()
w.connect('destroy', gtk.main_quit)
w.show()
gtk.main()
