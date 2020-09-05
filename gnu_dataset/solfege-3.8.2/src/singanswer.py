# GNU Solfege - free ear training software
# Copyright (C) 2004, 2005, 2007  Tom Cato Amundsen
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

import gobject
import gtk
import gu
import abstract
import lessonfile
import dataparser
import mpd
import utils
import const
import soundcard

class Teacher(abstract.Teacher):
    OK = 1
    ERR_NO_QUESTION = 2
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.SingAnswerLessonfile
    def new_question(self):
        # TODO This function is very similar to the one in src/idbyname.py
        self.q_status = const.QSTATUS_NO
        assert self.m_P
        try:
            self.m_P.select_random_question()
            self.q_status = const.QSTATUS_NEW
            return self.OK
        except lessonfile.NoQuestionConfiguredException:
            return self.ERR_NO_QUESTION

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.Gui.__init__(self, teacher, window)
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.practise_box.pack_start(self.g_music_displayer)
        self.g_flashbar = gu.FlashBar()
        self.practise_box.pack_start(self.g_flashbar)
        self.g_music_displayer.clear()
        self.g_new = gu.bButton(self.action_area, _("_New"), self.new_question)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question))
        self.g_repeat_arpeggio = gu.bButton(self.action_area, 
            _("Repeat _arpeggio"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question_arpeggio))
        self.g_play_answer = gu.bButton(self.action_area, 
                    _("_Play answer"), self.hear_answer)
        ##############
        # config_box #
        ##############
        self.add_random_transpose_gui()
        self.practise_box.show_all()
    def new_question(self, widget):
        def exception_cleanup():
            soundcard.synth.stop()
            self.g_music_displayer.clear()
            self.g_repeat.set_sensitive(False)
            self.g_repeat_arpeggio.set_sensitive(False)
            self.g_play_answer.set_sensitive(False)
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.clear()
        try:
            g = self.m_t.new_question()
            if self.m_t.m_P.get_question()['music'].is_displayable():
                self.g_music_displayer.display(self.m_t.m_P.get_music(), 20)
            else:
                self.g_music_displayer.clear()
            self.g_flashbar.push(self.m_t.m_P.get_question()['question_text'])
            self.m_t.m_P.play_question()
            self.g_repeat.set_sensitive(True)
            self.g_repeat_arpeggio.set_sensitive(True)
            self.g_play_answer.set_sensitive(True)
            self.g_play_answer.grab_focus()
        except Exception, e:
            if not self.standard_exception_handler(e, __file__,
                    exception_cleanup):
                raise
    def hear_answer(self, widget):
        try:
            self.m_t.m_P.play_question(varname='answer')
        except Exception, e:
            if not self.standard_exception_handler(e, __file__):
                raise
        self.g_new.grab_focus()
    def update_gui_after_lessonfile_change(self):
        self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        if self.m_t.m_P.header.have_repeat_arpeggio_button:
            self.g_repeat_arpeggio.show()
        else:
            self.g_repeat_arpeggio.hide()
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.show()
        else:
            self.g_music_displayer.hide()
        self.g_repeat.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        self.g_play_answer.set_sensitive(False)
        self.g_flashbar.clear()
        self.g_new.set_sensitive(bool(self.m_t.m_P))
    def on_start_practise(self):
        self.update_gui_after_lessonfile_change()
        self.g_new.grab_focus()
        gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
            self.g_flashbar.flash(_("Click 'New' to begin.")))
        self.g_flashbar.require_size(
            [q['question_text'] for q in self.m_t.m_P.m_questions]
            + [_("Click 'New' to begin.")])
