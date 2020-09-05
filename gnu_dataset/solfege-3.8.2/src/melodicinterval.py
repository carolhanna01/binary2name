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

import gtk, gobject
import gu, inputwidgets
from multipleintervalconfigwidget import MultipleIntervalConfigWidget
import abstract, const
import statistics, statisticsviewer
import soundcard, utils, mpd
import lessonfile

class Teacher(abstract.MelodicIntervalTeacher):
    def __init__(self, exname, app):
        abstract.MelodicIntervalTeacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.IntervalsLessonfile
        self.m_statistics = statistics.IntervalStatistics(self)
        self.m_timeout_handle = None
        self.m_custom_mode = False
    def enter_test_mode(self):
        self.m_custom_mode = False
        self.m_statistics.enter_test_mode()
        self.m_P.enter_test_mode()
    def give_up(self):
        """This function is only called *after* the user already has
        answered wrong once, so the statistics are already updated.
        """
        self.q_status = const.QSTATUS_GIVE_UP
    def guess_answer(self, answer, directions=None):
        """
        return TRUE value if correct
        """
        assert self.q_status not in (const.QSTATUS_NO, const.QSTATUS_GIVE_UP)
        if directions:
            q = self.m_question
        else:
            q = map(abs, self.m_question)
        if q == answer:
            if self.q_status == const.QSTATUS_NEW \
                and not self.m_custom_mode \
                and self.get_int('number_of_intervals=1') == 1:
                    self.m_statistics.add_correct(self.m_question[0])
            self.maybe_auto_new_question()
            self.q_status = const.QSTATUS_SOLVED
            return 1
        else:
            if self.q_status == const.QSTATUS_NEW \
                and not self.m_custom_mode \
                and self.get_int('number_of_intervals=1') == 1:
                    self.m_statistics.add_wrong(self.m_question[0], answer[0])
            if self.m_app.m_test_mode:
                self.maybe_auto_new_question()
            self.q_status = const.QSTATUS_WRONG
    def start_practise(self):
        # First, we have to empty the cfg database because we will
        # copy the values from the lesson header.
        for i in range(self.get_int('maximum_number_of_intervals')):
            self.set_list('ask_for_intervals_%i' % i, [])
        # If ask_for_intervals_0 is not set, then we run in custom_mode
        # where the user configure the exercise on her own.
        if 'ask_for_intervals_0' in self.m_P.header:
            self.set_bool('disable_unused_intervals', self.m_P.header.disable_unused_intervals)
            for i in range(self.get_int('maximum_number_of_intervals')):
                if 'ask_for_intervals_%i' % i in self.m_P.header:
                    self.set_list('ask_for_intervals_%i' % i,
                      self.m_P.header['ask_for_intervals_%i' % i])
                else:
                    break
            self.set_int('number_of_intervals', i)
        else:
            self.set_list('ask_for_intervals_0', [])
            self.set_int('number_of_intervals', 1)
        self.m_custom_mode = bool(not self.m_P.header.ask_for_intervals_0)

class Gui(abstract.IntervalGui):
    def __init__(self, teacher, window):
        abstract.IntervalGui.__init__(self, teacher, window)
        self.g_test_stat_dlg = None
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"), self.give_up)
        self.g_give_up.set_sensitive(False)
        ##############
        # config_box #
        ##############
        self.g_mici = MultipleIntervalConfigWidget(self.m_exname)
        self.config_box.pack_start(self.g_mici, False, False)
        self.config_box.pack_start(gtk.HBox(), False,
                                   padding=gu.PAD_SMALL)
        self._add_auto_new_question_gui(self.config_box)
        # ----------------------------------
        self._create_select_inputwidget_gui()
        self.config_box.show_all()
        ##############
        # statistics #
        ##############
        self.setup_statisticsviewer(statisticsviewer.StatisticsViewer,
                                    _("Melodic interval"))
        self.select_inputwidget()
        def _f(watchvar):
            # The variables being watched by this function will change
            # when we switch lesson files or when we are in expert mode and
            # the user configures the exercise manually. We only have to run
            # end|start_exercise here in expert mode because it is called
            # automatically when we change lesson file.
            if self.m_t.m_custom_mode:
                self.on_end_practise()
                #self.on_start_practise()
        for i in range(self.get_int('maximum_number_of_intervals')):
            self.add_watch('ask_for_intervals_%i' % i, _f)
        self.add_watch('number_of_intervals', _f)
    def give_up(self, _o=None):
        if self.m_t.q_status == const.QSTATUS_WRONG:
            s = utils.int_to_intervalname(self.m_t.m_question[0],
                       len(self.m_t.m_question) > 1, 1)
            for n in self.m_t.m_question[1:]:
                s = s + "+" + utils.int_to_intervalname(n,
                       len(self.m_t.m_question) > 1, 1)
            self.g_flashbar.push(_("The answer is: %s") % s)
            self.m_t.give_up()
            self.g_new_interval.set_sensitive(True)
            self.g_new_interval.grab_focus()
            self.g_give_up.set_sensitive(False)
    def get_interval_input_list(self):
        v = []
        for x in range(self.get_int('number_of_intervals')):
            for i in self.get_list('ask_for_intervals_%i' % x):
                if not (abs(i) in v):
                    v.append(abs(i))
        v.sort()
        return v
    def click_on_interval(self, mouse_button, interval, midi_int):
        """The key bindings are also directed here.
        """
        if mouse_button == 3 and not self.m_t.m_app.m_test_mode \
                and self.get_int('number_of_intervals') == 1:
            if not self.m_t.m_tonika:
                return
            i = mpd.Interval()
            i.set_from_int(interval)
            n = self.m_t.m_tonika + i
            track = soundcard.Track()
            track.set_bpm(self.get_int('config/default_bpm'))
            track.set_patch(self.get_int('config/preferred_instrument'))
            track.note(4, self.m_t.m_tonika.semitone_pitch(),
                    self.get_int('config/preferred_instrument_velocity'))
            track.note(4, n.semitone_pitch(),
                    self.get_int('config/preferred_instrument_velocity'))
            soundcard.synth.play_track(track)
            return
        if mouse_button != 1:
            return
        if self.m_t.m_app.m_test_mode and self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'Start test' to begin."))
            return
        if self.m_t.m_app.m_test_mode:
            self.g_new_interval.hide()
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New interval' to begin."))
            return
        if midi_int:
            # midi_int is only set when we use some of the instrument widgets,
            # not when we use the buttons interface.
            soundcard.play_note(self.get_int('config/preferred_instrument'),
                        4, midi_int,
                        self.get_int('config/preferred_instrument_velocity'))
        if self.m_t.q_status == const.QSTATUS_GIVE_UP:
            return
        if self.m_t.q_status == const.QSTATUS_SOLVED:
            self.g_flashbar.flash(_("You have already identified this interval"))
            return
        if not (-17 < interval < 17):
            self.g_flashbar.flash(_("Ignoring intervals greater than decim."))
            self.g_input.forget_last_tone()
            return
        self.m_answer.append(interval)
        d = self.m_t.m_question[len(self.m_answer)-1]
        d = d / abs(d)
        md = d
        if self.g_input.know_directions():
            md = 1
        if not self.msg:
            self.msg = utils.int_to_intervalname(interval * md, 1, 1) + "+ ..."
        else:
            self.msg = self.msg[:-4]
            self.msg = self.msg + utils.int_to_intervalname(interval * md, 1, 1) + "+ ..."
        self.g_flashbar.push(self.msg)
        if len(self.m_answer) == self.m_number_of_intervals_in_question:
            if self.m_t.guess_answer(self.m_answer,
                                     self.g_input.know_directions()):
                self.g_flashbar.clear()
                self.g_flashbar.flash(_("Correct"))
                self.g_new_interval.set_sensitive(True)
                self.g_new_interval.grab_focus()
                self.g_give_up.set_sensitive(False)
            else:
                self.g_flashbar.clear()
                self.g_flashbar.flash(_("Wrong"))
                if self.get_bool("config/auto_repeat_question_if_wrong_answer"):
                    self.m_t.play_question()
                self.g_give_up.set_sensitive(True)
            self.m_answer = []
            self.g_input.set_first_note(self.m_t.m_tonika)
            self.msg = ""
        if self.m_t.m_app.m_test_mode and self.m_t.m_P.is_test_complete():
            self.do_test_complete()
            return
    def new_question(self, _o=None):
        self.msg = ""
        self.m_number_of_intervals_in_question \
               = self.get_int('number_of_intervals')
        self.m_answer = []
        if isinstance(self.g_input, inputwidgets.IntervalButtonsWidget):
            ret = self.m_t.new_question(self.get_string('user/lowest_pitch'),
                                    self.get_string('user/highest_pitch'))
        else:
            ret = self.m_t.new_question(self.g_input.m_lowest_tone,
                                    self.g_input.m_highest_tone)
        if ret == Teacher.ERR_CONFIGURE:
            self.g_flashbar.clear()
            self.g_flashbar.flash(_("The exercise has to be better configured."))
            self.g_repeat.set_sensitive(False)
        elif ret == Teacher.OK:
            self.g_repeat.set_sensitive(True)
            self.g_give_up.set_sensitive(False)
            self.g_input.set_first_note(self.m_t.m_tonika)
            self.m_t.play_question()
            self.g_new_interval.set_sensitive(
                  not self.get_bool('config/picky_on_new_question'))
            self.g_flashbar.clear()
            #inputwidget 0 is always the buttons.
            if self.get_int('inputwidget') == 0:
                self.g_input.grab_focus_first_sensitive_button()
        elif ret == Teacher.ERR_PICKY:
            self.g_flashbar.flash(_("You have to solve this question first."))
    def on_start_practise(self):
        self.m_t.start_practise()
        if self.m_t.m_P.header.lesson_heading:
            self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        else:
            self.set_lesson_heading(_("Identify the interval"))
        if self.m_t.m_custom_mode:
            self.g_mici.show()
        else:
            self.m_t.m_statistics.reset_session()
            self.g_mici.hide()
        self.g_statview.g_heading.set_text("%s - %s" % (_("Melodic interval"), self.m_t.m_P.header.title))
        self.g_new_interval.grab_focus()
        if self.m_t.m_app.m_test_mode:
            gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
                self.g_flashbar.flash(_("Click 'Start test' to begin.")))
        else:
            gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
                self.g_flashbar.flash(_("Click 'New interval' to begin.")))
    def on_end_practise(self):
        self.m_t.end_practise()
        self.g_new_interval.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.g_input.clear()
        self.g_flashbar.clear()
    def enter_test_mode(self):
        self.m_saved_q_auto = self.get_bool('new_question_automatically')
        self.m_saved_s_new = self.get_float('seconds_before_new_question')
        self.set_bool('new_question_automatically', True)
        self.set_float('seconds_before_new_question', 0.5)
        self.m_t.enter_test_mode()
        self.g_new_interval.set_label(_("_Start test"))
        self.g_cancel_test.show()
        self.g_give_up.hide()
    def exit_test_mode(self):
        self.set_bool('new_question_automatically', self.m_saved_q_auto)
        self.set_float('seconds_before_new_question', self.m_saved_s_new)
        self.m_t.exit_test_mode()
        self.g_new_interval.show()
        self.g_new_interval.set_label(_("_New interval"))
        self.g_give_up.show()
