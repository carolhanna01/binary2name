# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007  Tom Cato Amundsen
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
import abstract, idbyname, const, gu, statisticsviewer
import soundcard, mpd, mpd.musicdisplayer
import utils
import lessonfile

class Teacher(idbyname.Teacher):
    def __init__(self, exname, app):
        idbyname.Teacher.__init__(self, exname, app)
    def play_question(self):
        if self.q_status == const.QSTATUS_NO:
            return
        self.m_P.play_question_as_choral(self.m_P.get_question())


class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window)
        ################
        # practise_box #
        ################
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.practise_box.pack_start(self.g_music_displayer)

        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False)
        self.practise_box.set_spacing(gu.PAD)

        self.g_entry = gtk.Entry()
        self.g_entry.set_activates_default(True)
        self.practise_box.pack_start(self.g_entry, False)
        self.g_new = gu.bButton(self.action_area, _("_New"), self.new_question)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
                                   lambda _o, self=self: self.m_t.play_question())
        self.g_repeat.set_sensitive(False)

        self.g_play_tonic = gu.bButton(self.action_area, _("Play _tonic"),
                                 lambda w: self.m_t.play_tonic())

        self.g_guess_answer = gu.bButton(self.action_area, _("Guess _answer"),
                                         self.guess_answer)
        self.g_guess_answer.set_sensitive(False)
        self.g_guess_answer.set_flags(gtk.CAN_DEFAULT)
        self.g_show = gu.bButton(self.action_area, _("_Show"), self.show_answer)
        self.g_show.set_sensitive(False)
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"), self.give_up)
        self.g_give_up.set_sensitive(False)
        self.practise_box.show_all()
        ##############
        # config_box #
        ##############
        self.add_random_transpose_gui()
        self._add_auto_new_question_gui(self.config_box)
        # ----------------------------------------------

        ###############
        # statistics
        ###############
        self.setup_statisticsviewer(statisticsviewer.StatisticsViewer,
                                   _("Harmonic progression dictation"))

    def guess_answer(self, widget=None):
        if self.m_t.q_status == const.QSTATUS_NO:
            return
        if self.m_t.q_status == const.QSTATUS_SOLVED:
            if self.m_t.guess_answer(self.g_entry.get_text()):
                self.g_flashbar.flash(_("Correct, but you have already solved this question"))
            else:
                self.g_flashbar.flash(_("Wrong, but you have already solved this question"))
        else:
            if self.m_t.guess_answer(self.g_entry.get_text()):
                self.g_flashbar.flash(_("Correct"))
                self.g_give_up.set_sensitive(False)
                self.g_new.set_sensitive(True)
                self.g_new.grab_focus()
            else:
                self.g_flashbar.flash(_("Wrong"))
                self.g_give_up.set_sensitive(True)
    def show_answer(self, widget=None):#FIXME rename to show_music??
        if self.m_t.q_status != const.QSTATUS_NO:
            self.g_music_displayer.display(self.m_t.m_P.get_music(),
                               self.get_int('config/feta_font_size=20'))
    def new_question(self, widget=None):
        def exception_cleanup():
            soundcard.synth.stop()
            self.g_repeat.set_sensitive(False)
            self.g_show.set_sensitive(False)
            self.g_guess_answer.set_sensitive(False)
            self.g_entry.set_text("")
            self.g_music_displayer.clear(2)
        if not self.m_t.m_P:
            return
        # pop just in case there is something in the stack.
        self.g_flashbar.pop()
        try:
            g = self.m_t.new_question()
            if g == self.m_t.OK:
                self.g_music_displayer.display(self.m_t.m_P.get_music(),
                            self.get_int('config/feta_font_size=20'), mpd.FIRST)
                self.g_repeat.set_sensitive(True)
                self.g_play_tonic.set_sensitive(
                      'tonic' in self.m_t.m_P.get_question())
                self.g_show.set_sensitive(True)
                self.g_guess_answer.set_sensitive(True)
                self.g_entry.set_text("")
                self.g_new.set_sensitive(
                     not self.get_bool('config/picky_on_new_question'))
                self.m_t.play_question()
                self.g_entry.grab_focus()
        except mpd.MpdException, e:
            if not self.standard_exception_handler(e, __file__, exception_cleanup):
                raise
        except lessonfile.LessonfileException, e:
            if not self.standard_exception_handler(e, __file__, exception_cleanup):
                raise
    def give_up(self, widget=None):
        self.m_t.give_up()
        self.g_new.set_sensitive(True)
        self.g_guess_answer.set_sensitive(False)
        self.g_show.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.g_flashbar.push(self.m_t.m_P.get_cname())
        self.show_answer()
    def on_start_practise(self):
        self.m_t.m_custom_mode = self.get_bool('gui/expert_mode')
        self.update_gui_after_lessonfile_change()
        self.g_guess_answer.grab_default()
        self.g_new.grab_focus()
    def on_end_practise(self):
        self.g_new.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_guess_answer.set_sensitive(False)
        self.g_show.set_sensitive(False)
        self.g_music_displayer.clear(2)
        self.m_t.end_practise()
    def update_gui_after_lessonfile_change(self):
        self.g_music_displayer.clear(2)
        self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        self.g_new.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_guess_answer.set_sensitive(False)
        self.g_play_tonic.set_sensitive(False)
        self.g_show.set_sensitive(False)
