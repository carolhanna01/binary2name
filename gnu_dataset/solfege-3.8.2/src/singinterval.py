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
import gu
from multipleintervalconfigwidget import MultipleIntervalConfigWidget
import soundcard, mpd, mpd.musicdisplayer
import abstract, const
import statistics, statisticsviewer
import utils
import lessonfile

class Teacher(abstract.MelodicIntervalTeacher):
    def __init__(self, exname, app):
        abstract.MelodicIntervalTeacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.IntervalsLessonfile
        self._ignore_picky = 1
        self.m_statistics = statistics.IntervalStatistics(self)
    def enter_test_mode(self):
        self.m_custom_mode = False
        self.m_statistics.enter_test_mode()
        self.m_P.enter_test_mode()
    def get_question(self):
        """
        Return a string that can be used to display the intervals.
        """
        s = self.m_tonika.get_octave_notename()
        last = self.m_tonika
        for i in self.m_question:
            n = mpd.Interval()
            n.set_from_int(i)
            last = last + n
            if abs(last.m_accidental_i) > 1:
                last.normalize_double_accidental()
            s = s + " " + last.get_octave_notename()
        return self.m_tonika, r"\staff{ \clef %s %s}" % (mpd.select_clef(s), s)
    def guessed_correct(self):
        if self.get_int('number_of_intervals=1') == 1 \
                and not self.m_custom_mode:
            self.m_statistics.add_correct(self.m_question[0])
        self.q_status = const.QSTATUS_SOLVED
    def guessed_wrong(self):
        if self.get_int('number_of_intervals=1') == 1 \
                and not self.m_custom_mode:
            self.m_statistics.add_wrong(self.m_question[0], None)
        # we set q_status to WRONG, just to trick the MelodicIntervalTeacher
        # .new_interval to do the work for us.
        self.q_status = const.QSTATUS_SOLVED
    def play_first_tone(self):
        if self.q_status == const.QSTATUS_NO:
            return
        assert self.m_question
        soundcard.play_note(self.get_int('config/preferred_instrument'),
                  4, self.m_tonika.semitone_pitch(),
                  self.get_int('config/preferred_instrument_velocity'))
    def play_last_tone(self):
        if self.q_status == const.QSTATUS_NO:
            return
        assert self.m_question
        t = self.m_tonika
        for i in self.m_question:
            t = t + i
        soundcard.play_note(self.get_int('config/preferred_instrument'),
                  4, t.semitone_pitch(),
                  self.get_int('config/preferred_instrument_velocity'))


class Gui(abstract.Gui):
    def __init__(self, teacher, window):
        abstract.Gui.__init__(self, teacher, window)
        ################
        # practise_box #
        ################
        self.g_score_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.practise_box.pack_start(self.g_score_displayer)
        self.g_score_displayer.clear()
        b1 = gu.bHBox(self.practise_box, False)
        self.g_new_interval_correct = gu.bButton(b1,
                _("_New interval,\nlast was correct"), self.new_question)
        self.g_new_interval_wrong = gu.bButton(b1,
                _("New interval,\nlast was _wrong"), self.new_last_was_wrong)
        self.g_new_interval = gu.bButton(b1,
               _("_New interval"), self.new_question)
        self.g_new_interval_wrong.set_sensitive(False)
        self.g_repeat_tonika = gu.bButton(self.action_area,
                     _("_Repeat first tone"),
                     lambda _o, self=self: self.m_t.play_first_tone())
        self.g_repeat_tonika.set_sensitive(False)
        self.g_play_answer = gu.bButton(self.action_area,
                     _("_Play answer"),
                     lambda _o, self=self: self.m_t.play_question())
        self.g_play_answer.set_sensitive(False)
        self.g_repeat_last_tone = gu.bButton(self.action_area,
           _("Play _last tone"), lambda _o, self=self: self.m_t.play_last_tone())
        self.g_repeat_last_tone.set_sensitive(False)
        self.practise_box.show_all()
        self.g_new_interval_correct.hide()
        self.g_new_interval_wrong.hide()
        ##############
        # config_box #
        ##############
        self.g_mici = MultipleIntervalConfigWidget(self.m_exname)
        self.config_box.pack_start(self.g_mici, False)
        self.config_box.show_all()
        ###############
        # statistics
        ###############
        self.setup_statisticsviewer(
           statisticsviewer.PercentagesStatisticsViewer, _("Sing interval"))
    def _new_question(self):
        r = self.m_t.new_question(self.get_string('user/lowest_pitch'),
                                  self.get_string('user/highest_pitch'))
        if r == Teacher.OK:
            self.m_t.play_first_tone()
            q = self.m_t.get_question()
            fontsize = self.get_int('config/feta_font_size=20')
            self.g_score_displayer.display(q[1], fontsize)
            self.g_new_interval_wrong.set_sensitive(True)
            self.g_repeat_tonika.set_sensitive(True)
            self.g_play_answer.set_sensitive(True)
            self.g_repeat_last_tone.set_sensitive(True)
            self.g_new_interval_correct.grab_focus()
        elif r == Teacher.ERR_CONFIGURE:
            self.g_repeat_tonika.set_sensitive(False)
            self.g_play_answer.set_sensitive(False)
            self.g_repeat_last_tone.set_sensitive(False)
            self.g_win.display_error_message(_("The exercise has to be better configured.\nClick 'Reset to default values' on the config\npage if you don't know how to do this."))
            self.g_score_displayer.clear()
        return r
    def on_start_practise(self):
        # First, we have to empty the cfg database because we will
        # copy the values from the lesson header.
        for i in range(self.get_int('maximum_number_of_intervals')):
            self.set_list('ask_for_intervals_%i' % i, [])
        # If ask_for_intervals_0 is not set, then we run in custom_mode
        # where the user configure the exercise on her own.
        if self.m_t.m_P.header.ask_for_intervals_0:
            for i in range(self.get_int('maximum_number_of_intervals')):
                if 'ask_for_intervals_%i' % i in self.m_t.m_P.header:
                    self.set_list('ask_for_intervals_%i' % i,
                      self.m_t.m_P.header['ask_for_intervals_%i' % i])
                else:
                    break
            self.set_int('number_of_intervals', i)
        self.m_t.m_custom_mode = bool(
                      not self.m_t.m_P.header.ask_for_intervals_0)
        if self.m_t.m_P.header.lesson_heading:
            self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        else:
            self.set_lesson_heading(_("Sing the interval"))
        if self.m_t.m_custom_mode:
            self.g_mici.show()
        else:
            self.g_mici.hide()
            self.m_t.m_statistics.reset_session()
        self.g_statview.g_heading.set_text("%s - %s" % (_("Sing interval"), self.m_t.m_P.header.title))
        self.handle_config_box_visibility()
        self.g_new_interval.grab_focus()
    def on_end_practise(self):
        self.g_new_interval.show()
        self.g_new_interval_correct.hide()
        self.g_new_interval_wrong.hide()
        self.g_repeat_tonika.set_sensitive(False)
        self.g_play_answer.set_sensitive(False)
        self.g_repeat_last_tone.set_sensitive(False)
        self.m_t.end_practise()
        self.g_score_displayer.clear()
    def new_last_was_wrong(self, widget=None):
        if self.m_t.q_status == const.QSTATUS_NEW:
            self.m_t.guessed_wrong()
            if self.m_t.m_app.m_test_mode and self.m_t.m_P.is_test_complete():
                self.do_test_complete()
                return
        self._new_question()
    def new_question(self, widget=None):#called by 'new last was correct' and from
                 # teacher when we get a new question automatically.
        if self.m_t.q_status == const.QSTATUS_NEW:
            self.m_t.guessed_correct()
            if self.m_t.m_app.m_test_mode and self.m_t.m_P.is_test_complete():
                self.do_test_complete()
                return
        r = self._new_question()
        if r == Teacher.OK:
            self.g_new_interval.hide()
            self.g_new_interval_correct.show()
            self.g_new_interval_wrong.show()
    def enter_test_mode(self):
        self.m_saved_q_auto = self.get_bool('new_question_automatically')
        self.m_saved_s_new = self.get_float('seconds_before_new_question')
        self.set_bool('new_question_automatically', True)
        self.set_float('seconds_before_new_question', 0.5)
        self.m_t.enter_test_mode()
        self.g_new_interval.set_label(_("_Start test"))
        self.g_cancel_test.show()
    def exit_test_mode(self):
        self.set_bool('new_question_automatically', self.m_saved_q_auto)
        self.set_float('seconds_before_new_question', self.m_saved_s_new)
        self.m_t.exit_test_mode()
        self.g_new_interval.show()
        self.g_new_interval.set_label(_("_New interval"))
