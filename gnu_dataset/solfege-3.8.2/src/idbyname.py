# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007 Tom Cato Amundsen
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
import gu
from specialwidgets import QuestionNameButtonTable, QuestionNameCheckButtonTable
import abstract, const, mpd, mpd.musicdisplayer
import utils
import statistics, statisticsviewer
import lessonfile
import dataparser
import soundcard
import osutils


class Teacher(abstract.Teacher):
    OK = 0
    ERR_PICKY = 1
    ERR_NO_QUESTION = 2
    def __init__(self, exname,  app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.IdByNameLessonfile
        self.m_statistics = statistics.LessonStatistics(self)
    def enter_test_mode(self):
        self.m_custom_mode = False
        self.m_statistics.enter_test_mode()
        self.m_P.enter_test_mode()
    def exit_test_mode(self):
        self.m_statistics.exit_test_mode()
    def give_up(self):
        self.q_status = const.QSTATUS_GIVE_UP
    def new_question(self):
        """
        UI will never call this function unless we have a usable lessonfile.
        """
        if self.m_timeout_handle:
            gobject.source_remove(self.m_timeout_handle)
            self.m_timeout_handle = None

        if self.m_app.m_test_mode:
            self.m_P.next_test_question()
            self.q_status = const.QSTATUS_NEW
            return self.OK

        if self.get_bool('config/picky_on_new_question') \
                 and self.q_status in [const.QSTATUS_NEW, const.QSTATUS_WRONG]:
            return Teacher.ERR_PICKY

        self.q_status = const.QSTATUS_NO

        assert self.m_P
        self.m_P.select_random_question()
        self.q_status = const.QSTATUS_NEW
        return self.OK
    def guess_answer(self, answer):
        """
        Return: 1 if correct, None if wrong
        """
        if answer == self.m_P.get_cname():
            if self.q_status == const.QSTATUS_NEW \
                    and not self.m_custom_mode:
                self.m_statistics.add_correct(answer)
            self.maybe_auto_new_question()
            self.q_status = const.QSTATUS_SOLVED
            return 1
        else:
            if self.q_status == const.QSTATUS_NEW:
                if not self.m_custom_mode:
                    self.m_statistics.add_wrong(self.m_P.get_cname(), answer)
                self.q_status = const.QSTATUS_WRONG
            if self.m_app.m_test_mode:
                self.maybe_auto_new_question()

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window)
        ################
        # practise_box #
        ################
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.practise_box.pack_start(self.g_music_displayer, False)
        self.g_bb = QuestionNameButtonTable(self.m_t.m_exname)
        self.practise_box.pack_start(self.g_bb, False)

        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False)
        self.practise_box.set_spacing(gu.PAD)

        self.g_new = gu.bButton(self.action_area, _("_New"), self.new_question)
        self.g_play_music = gu.bButton(self.action_area, _("P_lay music"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question))
        self.g_display_music = gu.bButton(self.action_area, _("_Display music"),
            self.show_answer)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question))
        self.g_repeat_arpeggio = gu.bButton(self.action_area, 
            _("Repeat _arpeggio"), 
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question_arpeggio))
        self.g_repeat_slowly = gu.bButton(self.action_area, _("Repeat _slowly"),
              lambda w: self.run_exception_handled(self.m_t.m_P.play_question_slowly))
        self.g_give_up = gu.bButton(self.action_area, 
              _("_Give up"), self.give_up)
        self.g_give_up.set_sensitive(False)

        # This button exist mostly for historical reasons, but I think
        # it an be usedful, even thoug it is not much used today (as in
        # Solfege 3.6). It will present a Show button for exercises that
        # does not have a music displayer. And the button will be insensitive
        # until the question has been solved.
        self.g_show = gu.bButton(self.action_area, _("_Show"), self.show_answer)
        self.g_show.set_sensitive(False)
        self.practise_box.show_all()
        ##############
        # config_box #
        ##############
        self.config_box.set_border_width(12)
        self.config_box.set_spacing(18)
        self.add_random_transpose_gui()
        # -----------------------------------------
        self.g_select_questions_category_box, category_box= gu.hig_category_vbox(
            _("Questions to ask"))
        self.config_box.pack_start(self.g_select_questions_category_box, True)
        self.g_select_questions = QuestionNameCheckButtonTable(self.m_t)
        self.g_select_questions.initialize(4, 0)
        category_box.pack_start(self.g_select_questions, False)
        self.g_select_questions.show()
        # ------------------------------------------
        self._add_auto_new_question_gui(self.config_box)
        # ----------------------------------------------
        ##############
        # statistics #
        ##############
        self.setup_statisticsviewer(statisticsviewer.StatisticsViewer,
                                   _("Identify by name"))
        def _f(varname):
            self.setup_action_area_buttons()
        self.add_watch('ask_for_names', _f)
    def update_answer_buttons(self):
        if self.m_t.m_P:
            self.g_bb.initialize(self.m_t.m_P.header.fillnum,
                                 self.m_t.m_P.header.filldir)
            for question in self.m_t.m_P.iterate_questions_with_unique_names():
                self.g_bb.add(question,
                      self.m_t.m_P.header.labelformat, self.on_click)
        else:
            self.g_bb.initialize(0, 0)
        if self.m_t.m_P and self.m_t.m_custom_mode:
            self.g_bb.ask_for_names_changed()
    def update_select_question_buttons(self):
        #FIXME duplicate code in src/chord.py
        if self.m_t.m_custom_mode:
            self.g_select_questions_category_box.show()
            self.g_select_questions.initialize(self.m_t.m_P.header.fillnum,
                                 self.m_t.m_P.header.filldir)
            self.m_t.check_askfor()
            for question in self.m_t.m_P.iterate_questions_with_unique_names():
                self.g_select_questions.add(question,
                      self.m_t.m_statistics.get_label_style())
        else:
            self.g_select_questions_category_box.hide()
            self.g_select_questions.initialize(0, 0)
    def setup_action_area_buttons(self):
        """
        Make the buttons visible or invisible depending
        on the lesson file, and make the right set of buttons
        sensitive for the first question.
        """
        self.g_new.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_repeat_slowly.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        self.g_show.set_sensitive(False)
        self.g_display_music.set_sensitive(False)
        self.g_play_music.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        if filter(lambda q: q.is_transposable(), self.m_t.m_P.m_questions):
            self.g_random_transpose_box.show()
        else:
            self.g_random_transpose_box.hide()
        # If one or more of the questions is of musictype 'chord', then
        # we need the "Repeat arpeggio" button.
        if filter(lambda q: q['music'].m_musictype == 'chord',
                  self.m_t.m_P.m_questions):
            self.g_repeat_arpeggio.show()
        else:
            self.g_repeat_arpeggio.hide()
        if self.m_t.m_P.header.have_repeat_slowly_button:
            self.g_repeat_slowly.show()
        else:
            self.g_repeat_slowly.hide()
        self.show_hide_at_question_start_buttons()
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.show()
            self.g_music_displayer.clear()
            self.g_show.hide()
        else:
            self.g_music_displayer.hide()
            self.g_show.show()
    def give_up(self, _o=None):
        if self.m_t.q_status == const.QSTATUS_WRONG:
            self.g_flashbar.push(_("The answer is: %s") % self.m_t.m_P.get_name())
            self.m_t.give_up()
            self.g_show.set_sensitive(True)
            self.g_new.set_sensitive(True)
            self.g_new.grab_focus()
            self.g_give_up.set_sensitive(False)
            if self.m_t.m_P.header.have_music_displayer:
                self.run_exception_handled(self.show_answer)
    def on_click(self, button, event=None):
        if not event:
            self.on_left_click(button)
        elif event.button == 3:
            if self.m_t.m_P and self.m_t.m_P.header.enable_right_click:
                self.on_right_click(button)
            else:
                self.g_flashbar.flash(_("Right click is not allowed for this lesson file."))
    def on_right_click(self, button):
        if self.m_t.m_app.m_test_mode:
            return
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New' to begin."))
            return
        if self.m_t.q_status == const.QSTATUS_NEW:
            self.g_flashbar.flash(_("You should try to guess before right-clicking."))
            return
        if self.m_t.q_status not in (const.QSTATUS_GIVE_UP, const.QSTATUS_WRONG,
                    const.QSTATUS_SOLVED):
            self.g_flashbar.flash(_("You should try to guess before right-clicking."))
            return
        try:
            if 'set' in self.m_t.m_P.get_question():
                for question in self.m_t.m_P.m_questions:
                    if question['set'] == self.m_t.m_P.get_question()['set'] \
                        and question.get_cname() == button.get_data('cname'):
                        self.m_t.m_P.play_question(question)
                        return
            for question in self.m_t.m_P.m_questions:
                if question.get_cname() == button.get_data('cname'):
                    self.m_t.m_P.play_question(question)
                    return
        except Exception, e:
            if not self.standard_exception_handler(e, __file__):
                raise
    def on_left_click(self, button):
        if self.m_t.q_status == const.QSTATUS_NO:
            if self.m_t.m_app.m_test_mode:
                self.g_flashbar.flash(_("Click 'Start test' to begin."))
            else:
                self.g_flashbar.flash(_("Click 'New' to begin."))
            return
        try:
            if self.m_t.q_status == const.QSTATUS_SOLVED:
                if self.m_t.guess_answer(button.get_data('cname')):
                    self.g_flashbar.flash(_("Correct, but you have already solved this question"))
                else:
                    self.g_flashbar.flash(_("Wrong, but you have already solved this question"))
            elif self.m_t.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG):
                if self.m_t.guess_answer(button.get_data('cname')):
                    self.g_flashbar.flash(_("Correct"))
                    self.g_new.set_sensitive(True)
                    self.g_new.grab_focus()
                    self.g_show.set_sensitive(True)
                    self.g_give_up.set_sensitive(False)
                    if self.m_t.m_P.header.have_music_displayer:
                        self.show_answer()
                else:
                    self.g_flashbar.flash(_("Wrong"))
                    if self.get_bool("config/auto_repeat_question_if_wrong_answer"):
                        self.m_t.m_P.play_question()
                    self.g_give_up.set_sensitive(True)
        except Exception, e:
            if not self.standard_excpetion_handler(e, __file__):
                raise
    def new_question(self, widget=None):
        """
        The new button should be insensitive if we have no lesson file.
        """
        if self.m_t.m_app.m_test_mode and self.m_t.m_P.is_test_complete():
            self.do_test_complete()
            return
        if self.m_t.m_app.m_test_mode:
            self.g_new.hide()
        def exception_cleanup():
            self.m_t.q_status = const.QSTATUS_NO
            soundcard.synth.stop()
            self.g_repeat.set_sensitive(False)
            self.g_repeat_slowly.set_sensitive(False)
            self.g_repeat_arpeggio.set_sensitive(False)
            self.g_show.set_sensitive(False)
            self.g_give_up.set_sensitive(False)
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.clear()
        try:
            g = self.m_t.new_question()
            if g == self.m_t.OK:
                self.do_at_question_start_show_play()
                self.g_repeat.set_sensitive(True)
                self.g_repeat_slowly.set_sensitive(True)
                self.g_repeat_arpeggio.set_sensitive(True)
                self.g_display_music.set_sensitive(True)
                self.g_play_music.set_sensitive(True)
                self.g_new.set_sensitive(
                    not self.get_bool('config/picky_on_new_question'))
                self.g_bb.grab_focus_first_button()
                self.g_give_up.set_sensitive(False)
                self.g_show.set_sensitive(False)
                self.g_flashbar.clear()
        except lessonfile.NoQuestionConfiguredException, e:
            self.g_flashbar.clear()
            self.g_flashbar.flash(_("You have to select some questions to practise."))
        except Exception, e:
            if not self.standard_exception_handler(e, __file__, exception_cleanup):
                raise
    def on_start_practise(self):
        self.m_t.m_custom_mode = self.get_bool('gui/expert_mode')
        for question in self.m_t.m_P.m_questions:
            question['active'] = 1
        self.setup_action_area_buttons()
        self.update_answer_buttons()
        self.update_select_question_buttons()
        self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        self.g_flashbar.require_size([
            _("Right click is not allowed for this lesson file."),
            _("You should try to guess before right-clicking."),
            _("You should try to guess before right-clicking."),
            _("Correct, but you have already solved this question"),
            _("Wrong, but you have already solved this question"),
            _("You have to select some questions to practise."),
        ])
        self.g_new.grab_focus()
        if not self.m_t.m_custom_mode:
            self.m_t.m_statistics.reset_session()
        self.g_statview.g_heading.set_text(self.m_t.m_P.header.title)
        self.g_music_displayer.clear()
        # We only want the Show button if there are any questions that can
        # be displayed.
        if [q for q in self.m_t.m_P.m_questions if q['music'].is_displayable()] and not self.m_t.m_P.header.have_music_displayer:
            self.g_show.show()
        else:
            self.g_show.hide()
        if self.m_t.m_app.m_test_mode:
            gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
                self.g_flashbar.flash(_("Click 'Start test' to begin.")))
        else:
            gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
                self.g_flashbar.flash(_("Click 'New' to begin.")))
    def on_end_practise(self):
        self.m_t.end_practise()
        self.g_repeat.set_sensitive(False)
        self.g_repeat_slowly.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        if self.m_t.m_P and self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.clear()
        self.g_show.set_sensitive(False)
        self.g_new.set_sensitive(True)
        self.g_give_up.set_sensitive(False)
        self.g_flashbar.clear()
    def enter_test_mode(self):
        self.m_saved_q_auto = self.get_bool('new_question_automatically')
        self.m_saved_s_new = self.get_float('seconds_before_new_question')
        self.set_bool('new_question_automatically', True)
        self.set_float('seconds_before_new_question', 0.5)
        self.m_t.enter_test_mode()
        self.g_give_up.hide()
        self.g_new.set_label(_("_Start test"))
        self.g_show.hide()
        self.g_repeat_arpeggio.hide()
        self.g_repeat_slowly.hide()
        self.g_cancel_test.show()
    def exit_test_mode(self):
        self.set_bool('new_question_automatically', self.m_saved_q_auto)
        self.set_float('seconds_before_new_question', self.m_saved_s_new)
        self.m_t.exit_test_mode()
        self.g_new.set_label(_("_New"))
        self.g_new.show()
        self.on_start_practise()
        self.g_repeat_slowly.show()
