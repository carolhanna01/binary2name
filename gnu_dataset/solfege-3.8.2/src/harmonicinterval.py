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

from __future__ import division

import os
import pickle
import time
import gtk, gobject
import gu, inputwidgets
from multipleintervalconfigwidget import IntervalCheckBox
import soundcard, mpd
import abstract, const, utils
import statistics, statisticsviewer
import htmlwidget
import cfg
import random
import gethomedir
import lessonfile

class Teacher(abstract.Teacher):
    OK = 0
    ERR_PICKY = 1
    ERR_NO_INTERVALLS = 2
    ERR_NOTERANGE = 3
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.IntervalsLessonfile
        self.m_statistics = statistics.HarmonicIntervalStatistics(self)
        self.m_tonika = None
        self.m_interval = None
    def enter_test_mode(self):
        self.m_custom_mode = False
        self.m_statistics.enter_test_mode()
        self.m_P.enter_test_mode()
    def new_question(self, L, H):
        """
        Return values:
        OK: new question created, all ok
        ERR_NO_INTERVALLS: no new question because no intervals are selected
        ERR_PICKY: you have to solve this question before you are allowed to create new
        """
        if self.m_timeout_handle:
            gobject.source_remove(self.m_timeout_handle)
            self.m_timeout_handle = None

        if self.m_app.m_test_mode:
            self.m_P.next_test_question()
            self.m_interval = self.m_P.m_test_questions[self.m_P.m_test_idx]
            #FIXME use tone pitch range from preferences window.
            self.m_tonika = mpd.MusicalPitch()
            self.m_tonika.randomize("f", "f'")
            self.q_status = const.QSTATUS_NEW
            return self.OK

        if self.get_bool('config/picky_on_new_question') \
                 and self.q_status in [const.QSTATUS_NEW, const.QSTATUS_WRONG]:
            return Teacher.ERR_PICKY

        if self.get_list('intervals') == []:
            self.q_status = const.QSTATUS_NO
            return Teacher.ERR_NO_INTERVALLS
        last_question = self.m_interval
        last_tonika = self.m_tonika
        L, H = utils.adjust_low_high_to_irange(L, H, self.get_list('intervals'))
        while 1:
            # in this loop we will try to make a question that is not the
            # same that the last one.
            self.m_tonika, self.m_interval = \
                  utils.random_tonika_and_interval(L, H,
                    self.get_list('intervals'))
            if last_question is None:
                break
            if (self.m_interval == last_question
                and self.m_tonika == last_tonika) \
                and (len(self.get_list('intervals')) > 1 or (H - L) > 1):
                continue
            break
        assert self.m_tonika
        self.q_status = const.QSTATUS_NEW
        return Teacher.OK
    def guess_answer(self, answer):
        """
        Return: 1 if correct, None if wrong
        """
        assert self.q_status not in (const.QSTATUS_NO, const.QSTATUS_GIVE_UP)
        if self.m_interval == answer:
            if self.q_status == const.QSTATUS_NEW \
                    and not self.m_custom_mode:
                self.m_statistics.add_correct(self.m_interval)
            self.maybe_auto_new_question()
            self.q_status = const.QSTATUS_SOLVED
            return 1
        else:
            if self.q_status == const.QSTATUS_NEW:
                if not self.m_custom_mode:
                    self.m_statistics.add_wrong(self.m_interval, answer)
                self.q_status = const.QSTATUS_WRONG
            if self.m_app.m_test_mode:
                self.maybe_auto_new_question()
    def give_up(self):
        """This function is only called *after* the user already has
        answered wrong once, so the statistics are already updated.
        """
        self.q_status = const.QSTATUS_GIVE_UP
    def play_question(self):
        if self.q_status == const.QSTATUS_NO:
            return
        instr_low, instr_low_vel, instr_high, instr_high_vel = self.get_instrument_config(2)
        low_tone = self.m_tonika.semitone_pitch()
        high_tone = (self.m_tonika+self.m_interval).semitone_pitch()

        t1 = soundcard.Track()
        t1.set_bpm(self.get_int('config/default_bpm'))
        t1.set_patch(instr_low)
        t1.start_note(low_tone, instr_low_vel)
        t1.notelen_time(4)
        t1.stop_note(low_tone, instr_low_vel)

        t2 = soundcard.Track()
        t1.set_bpm(self.get_int('config/default_bpm'))
        t1.set_patch(instr_high)
        t2.start_note(high_tone, instr_high_vel)
        t2.notelen_time(4)
        t2.stop_note(high_tone, instr_high_vel)
        soundcard.synth.play_track(t1, t2)
        return 1
    def play_melodic(self):
        if self.q_status == const.QSTATUS_NO:
            return
        if self.get_bool('config/override_default_instrument'):
            instr_low = self.get_int('config/lowest_instrument')
            instr_low_vel = self.get_int('config/lowest_instrument_velocity')
            instr_high = self.get_int('config/highest_instrument')
            instr_high_vel = self.get_int('config/highest_instrument_velocity')
        else:
            instr_low = instr_high = self.get_int('config/preferred_instrument')
            instr_low_vel = instr_high_vel \
                    = self.get_int('config/preferred_instrument_velocity')
        low_tone = self.m_tonika.semitone_pitch()
        high_tone = (self.m_tonika+self.m_interval).semitone_pitch()
        m = soundcard.Track()
        m.set_bpm(self.get_int('config/default_bpm'))
        m.set_patch(instr_low)
        m.note(4, low_tone, instr_low_vel)
        m.set_patch(instr_high)
        m.note(4, high_tone, instr_high_vel)
        soundcard.synth.play_track(m)
    def start_practise(self):
        self.m_custom_mode = bool(not self.m_P.header.intervals)
        if self.m_P.header.intervals:
            self.set_list('intervals', self.m_P.header.intervals)
        self.set_bool('disable_unused_intervals', self.m_P.header.disable_unused_intervals)

class Gui(abstract.IntervalGui):
    def __init__(self, teacher, window):
        abstract.IntervalGui.__init__(self, teacher, window)
        self.g_test_stat_dlg = None
        ################
        # practice_box #
        ################
        self.g_repeat_melodic = gu.bButton(self.action_area,
            _("Repeat _melodic"), self.repeat_melodic)

        self.g_repeat_melodic.set_sensitive(False)

        self.g_give_up = gu.bButton(self.action_area, _("_Give up"), self.give_up)
        self.g_give_up.set_sensitive(False)
        ##############
        # config_box #        
        ##############
        self.g_ask_for_frame = frame = gtk.Frame(_("Ask for these intervals"))
        self.config_box.pack_start(frame, False)

        self.g_interval_selector = IntervalCheckBox(self.m_exname, 'intervals')

        def _ff(var):
            if self.m_t.m_custom_mode:
                # If we are running in custom mode, then the user can
                # select himself what intervals to practise. And then
                # we have to reset the exercise.
                self.on_end_practise()
                self.on_start_practise()
        self.add_watch('intervals', _ff)
        self.g_interval_selector.set_border_width(gu.PAD)
        frame.add(self.g_interval_selector)
        #------we need some space
        self.config_box.pack_start(gtk.HBox(), False,
                                   padding=gu.PAD_SMALL)
        # ------------------------------------------
        self._add_auto_new_question_gui(self.config_box)
        # ----------------------------------------------
        self._create_select_inputwidget_gui()
        # ------------ frame -------------------
        self.config_box.set_spacing(0)
        self.config_box.show_all()
        ##############
        # statistics #
        ##############
        self.setup_statisticsviewer(statisticsviewer.StatisticsViewer,
                                   _("Harmonic interval"))
        self.select_inputwidget()
    def give_up(self, _o=None):
        if self.m_t.q_status == const.QSTATUS_WRONG:
            self.g_flashbar.push(_("The answer is: %s")
                 % const.int_interval[self.m_t.m_interval])
            self.m_t.give_up()
            self.g_new_interval.set_sensitive(True)
            self.g_new_interval.grab_focus()
            self.g_give_up.set_sensitive(False)
    def get_interval_input_list(self):
        return self.get_list('intervals')
    def click_on_interval(self, mouse_button, interval, midi_int):
        if mouse_button == 1:
            # Do nothing if the user clicks on the anwer buttons or on
            # another input widget before the test has started.
            if self.m_t.m_app.m_test_mode and self.m_t.q_status == const.QSTATUS_NO:
                self.g_flashbar.flash(_("Click 'Start test' to begin."))
                return
            # we have to ignore intervals greater than decime, because
            # the statistics viewer can't handle it. It is no big deal
            # to fix it, but then the statistics can become almost unreadable
            # because of so many intervals.
            if not -17 < interval < 17:
                self.g_flashbar.flash(_("Ignoring intervals greater than decim."))
                self.g_input.forget_last_tone()
                return
            if self.m_t.q_status == const.QSTATUS_NO:
                self.g_flashbar.flash(_("Click 'New interval' to begin."))
                return
            if self.m_t.q_status == const.QSTATUS_SOLVED:
                if self.m_t.guess_answer(interval):
                    self.g_flashbar.flash(_("Correct, but you have already solved this question"))
                else:
                    self.g_flashbar.flash(_("Wrong, but you have already solved this question"))
            elif self.m_t.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG):
                if self.m_t.guess_answer(interval):
                    self.g_flashbar.flash(_("Correct"))
                    self.g_new_interval.set_sensitive(True)
                    self.g_new_interval.grab_focus()
                    self.g_give_up.set_sensitive(False)
                else:
                    self.g_flashbar.flash(_("Wrong"))
                    self.g_give_up.set_sensitive(True)
                    if self.get_bool("config/auto_repeat_question_if_wrong_answer"):
                        self.m_t.play_question()
            self.g_input.set_first_note(self.m_t.m_tonika)
        elif mouse_button == 2:
            # we only get here if you use one if the instrument widgets, not
            # with the buttons interface.
            soundcard.play_note(self.get_int('config/preferred_instrument'),
                          4, midi_int,
                          self.get_int('config/preferred_instrument_velocity'))
        elif mouse_button == 3 and self.m_t.q_status != const.QSTATUS_NO:
            instr_low, instr_low_vel, instr_high, instr_high_vel = self.m_t.get_instrument_config(2)
            n2 = self.m_t.m_tonika + interval
            t1 = self.m_t.m_tonika + interval
            t1 = soundcard.Track()
            t1.set_patch(instr_low)
            t1.start_note(self.m_t.m_tonika.semitone_pitch(), instr_low_vel)
            t1.notelen_time(4)
            t1.stop_note(self.m_t.m_tonika.semitone_pitch(), instr_high_vel)
            t2 = soundcard.Track()
            t2.set_patch(instr_high)
            t2.start_note(n2.semitone_pitch(), instr_low_vel)
            t2.notelen_time(4)
            t2.stop_note(n2.semitone_pitch(), instr_high_vel)
            soundcard.synth.play_track(t1, t2)
    def new_question(self, _o=None):
        """This function is called when you click on the 'New interval'
        button, or when you use the key bindings. So it can be called even
        if the 'New interval' button is insensitive.
        """
        if self.m_t.m_app.m_test_mode and self.m_t.m_P.is_test_complete():
            self.do_test_complete()
            return
        if self.m_t.m_app.m_test_mode:
            self.g_new_interval.hide()
        if isinstance(self.g_input, inputwidgets.IntervalButtonsWidget):
            g = self.m_t.new_question(self.get_string('user/lowest_pitch'),
                                      self.get_string('user/highest_pitch'))
        else:
            g = self.m_t.new_question(self.g_input.m_lowest_tone,
                                      self.g_input.m_highest_tone)

        if g == Teacher.OK: # new question, everything is OK
            self.g_repeat.set_sensitive(True)
            self.g_repeat_melodic.set_sensitive(True)
            self.g_input.set_first_note(self.m_t.m_tonika)
            self.g_new_interval.set_sensitive(
              not self.get_bool('config/picky_on_new_question'))
            self.m_t.play_question()
            self.g_flashbar.clear()
            #inputwidget 0 is always the buttons.
            if self.get_int('inputwidget') == 0:
                self.g_input.grab_focus_first_sensitive_button()
        elif g == Teacher.ERR_PICKY:
            self.g_flashbar.flash(_("You have to solve this question first."))
        elif g == Teacher.ERR_NO_INTERVALLS:
            self.g_repeat.set_sensitive(False)
            self.g_repeat_melodic.set_sensitive(False)
            self.g_flashbar.clear()
            self.g_flashbar.flash(
                    _("You have to select some intervals to practise."))
    def repeat_melodic(self, *w):
        self.m_t.play_melodic()
        self.g_input.grab_focus_first_sensitive_button()
    def on_start_practise(self):
        self.m_t.start_practise()
        if self.m_t.m_P.header.lesson_heading:
            self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        else:
            self.set_lesson_heading(_("Identify the interval"))
        if self.m_t.m_custom_mode:
            self.g_ask_for_frame.show()
        else:
            self.m_t.m_statistics.reset_session()
            self.g_ask_for_frame.hide()
        self.g_statview.g_heading.set_text("%s - %s" % (_("Harmonic interval"), self.m_t.m_P.header.title))
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
        self.g_repeat_melodic.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.g_flashbar.clear()
        self.g_input.clear()
    def enter_test_mode(self):
        self.m_saved_q_auto = self.get_bool('new_question_automatically')
        self.m_saved_s_new = self.get_float('seconds_before_new_question')
        self.set_bool('new_question_automatically', True)
        self.set_float('seconds_before_new_question', 0.5)
        self.m_t.enter_test_mode()
        self.g_new_interval.set_label(_("_Start test"))
        self.g_repeat_melodic.hide()
        self.g_cancel_test.show()
        self.g_give_up.hide()
    def exit_test_mode(self):
        self.set_bool('new_question_automatically', self.m_saved_q_auto)
        self.set_float('seconds_before_new_question', self.m_saved_s_new)
        self.m_t.exit_test_mode()
        self.g_new_interval.show()
        self.g_new_interval.set_label(_("_New interval"))
        self.g_repeat_melodic.show()
        self.g_give_up.show()
