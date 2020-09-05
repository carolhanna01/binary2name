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

import gtk
import gu, const, soundcard, mpd, mpd.musicdisplayer, abstract
import lessonfile
import utils
import dataparser

class Teacher(abstract.Teacher):
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.SingChordLessonfile
    def new_question(self):
        self.q_status = const.QSTATUS_NEW
        self.m_P.select_random_question()
    def play_answer(self):
        self.play_question()
    def play_440hz(self):
        soundcard.play_note(self.get_int('config/preferred_instrument'),
                       4, mpd.notename_to_int("a'"),
                       self.get_int('config/preferred_instrument_velocity'))

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window)
        ################
        # practise_box #
        ################
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.practise_box.pack_start(self.g_music_displayer)

        self.g_new = gu.bButton(self.action_area, _("_New"), self.new_question)
        gu.bButton(self.action_area, _("440h_z"), lambda f, s=self: s.m_t.play_440hz())
        self.g_play_answer = gu.bButton(self.action_area, _("_Play answer"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question_musicformat_satb_arpeggio))
        self.practise_box.show_all()
        ##############
        # config_box #
        ##############
        self.add_random_transpose_gui()
    def new_question(self, widget=None):
        def exception_cleanup():
            soundcard.synth.stop()
            self.g_play_answer.set_sensitive(False)
            self.g_music_displayer.clear()
        try:
            self.m_t.new_question()
            fontsize = self.get_int('config/feta_font_size=20')
            self.g_music_displayer.display(self.m_t.m_P.get_music(), fontsize)
            self.g_play_answer.set_sensitive(True)
            self.m_t.play_440hz()
        except Exception, e:
            e.m_mpd_varname = 'music'
            e.m_mpd_badcode = self.m_t.m_P.get_question()['music'].m_musicdata
            if not self.standard_exception_handler(e, __file__, exception_cleanup):
                raise
    def on_start_practise(self):
        self.update_gui_after_lessonfile_change()
        # if we have not m_P, then parsing of the lessonfile failed,
        # and then we have no questions.
        if not self.m_t.m_P:
            self.g_new.set_sensitive(False)
        else:
            self.g_new.set_sensitive(True)
        self.g_new.grab_focus()
    def on_end_practise(self):
        self.m_t.end_practise()
        self.g_play_answer.set_sensitive(False)
        self.g_music_displayer.clear(2)
    def update_gui_after_lessonfile_change(self):
        self.g_music_displayer.clear(2)
        self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        self.g_new.set_sensitive(True)
        self.g_play_answer.set_sensitive(False)

