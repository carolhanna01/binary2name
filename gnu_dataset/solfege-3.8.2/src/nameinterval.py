# GNU Solfege - free ear training software
# Copyright (C) 2006, 2007 Tom Cato Amundsen
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
import gobject
import random

import abstract
import lessonfile
import gu
import mpd
import utils
import const

class Teacher(abstract.Teacher):
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.NameIntervalLessonfile
        self.m_answered_quality = None
        self.m_answered_number = None
    def new_question(self):
        self.m_interval = random.choice(self.m_P.header.intervals)
        c = 0
        while 1:
            c += 1
            if self.m_P.header.tones[1] - self.m_P.header.tones[0] < self.m_interval.get_intvalue():
                return False
            self.m_low_pitch = mpd.MusicalPitch().randomize(
                self.m_P.header.tones[0],
                self.m_P.header.tones[1] - self.m_interval.get_intvalue())
            #
            if self.m_interval.steps() == (self.m_low_pitch + self.m_interval).steps() - self.m_low_pitch.steps():
                # this can happen for cases like d# + aug 3
                continue
            if abs(self.m_low_pitch.m_accidental_i) <= self.m_P.header.accidentals and\
                    abs((self.m_low_pitch + self.m_interval).m_accidental_i) <= self.m_P.header.accidentals:
                break
            if c > 1000:
                return False
        self.m_answered_quality = None
        self.m_answered_number = None
        self.q_status = const.QSTATUS_NEW
        return True
    def get_music_string(self):
        return r"\staff{ \clef %(clef)s \stemUp %(low)s %(high)s"% {
            'clef': self.m_P.header.clef,
            'high': (self.m_low_pitch + self.m_interval).get_octave_notename(),
            'low': self.m_low_pitch.get_octave_notename(),
        }
    def _check_answer(self):
        """
        Set qstatus and return True if the question is answered correctly.
        """
        assert self.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG)
        if self.answer_complete():
            if self.answered_correctly():
                self.q_status = const.QSTATUS_SOLVED
            else:
                self.q_status = const.QSTATUS_WRONG
    def answer_complete(self):
        """
        Return True if both interval quality and interval number
        has been guessed. If there is only one choice, then we don't
        have to guess it.
        """
        return ((self.m_answered_quality is not None
                 or len(self.m_P.header.interval_quality) == 1)
            and (self.m_answered_number is not None
                 or (len(self.m_P.header.interval_number) == 1)))
    def answer_quality(self, n):
        """
        Set q_status according to how we answer.
        """
        assert self.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG)
        self.m_answered_quality = n
        self._check_answer()
    def answer_number(self, n):
        """
        Set q_status according to how we answer.
        """
        assert self.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG)
        self.m_answered_number = n
        self._check_answer()
    def answered_correctly(self):
        """
        Return true if the question is answered correctly.
        """
        if self.m_answered_quality:
            q = self.m_answered_quality
        else:
            assert len(self.m_P.header.interval_quality) == 1
            q = self.m_P.header.interval_quality[0]
        if self.m_answered_number:
            n = self.m_answered_number
        else:
            assert len(self.m_P.header.interval_number) == 1
            n = self.m_P.header.interval_number[0]
        try:
            i = mpd.Interval("%s%s" % (q, n))
        except mpd.interval.InvalidIntervalnameException:
            return False
        return i == self.m_interval
    def forget_answers(self):
        self.m_answered_quality = None
        self.m_answered_number = None

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window, True)
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.g_music_displayer.show()
        self.practise_box.pack_start(self.g_music_displayer, False, False)
        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False, False)
        self.g_quality_box = gu.bHBox(self.practise_box)
        self.g_quality_box.show()
        self.g_number_box = gu.bHBox(self.practise_box)
        self.g_number_box.show()
        self.g_new = gu.bButton(self.action_area, _("_New"), self.new_question)
        self.g_new.show()
    def new_question(self, widget):
        try:
            if self.m_t.new_question():
                self.g_music_displayer.display(self.m_t.get_music_string(),
                    self.get_int('config/feta_font_size=20'))
            else:
                self.g_win.display_error_message2(_("Could not satisfy the constraints in the lesson header."), 'You must make more tones available by adjusting the "tones" variable in the lesson file header of the lesson file "%s".' % self.m_t.m_P.m_filename)
        except lessonfile.LessonfileException, e:
            if not self.standard_exception_handler(e, __file__):
                raise
    def on_interval_quality_clicked(self, button, n):
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New' to begin."))
            return
        if self.m_t.q_status == const.QSTATUS_SOLVED:
            self.g_flashbar.flash(_("Already solved. The interval is a '%s'") % self.m_t.m_interval.get_name())
            return
        self.m_t.answer_quality(n)
        if self.m_t.answer_complete():
            self.handle_do_answer()
        else:
            self.show_progress()
    def on_interval_number_clicked(self, button, n):
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New' to begin."))
            return
        if self.m_t.q_status == const.QSTATUS_SOLVED:
            self.g_flashbar.flash(_("Already solved. The interval is a '%s'") % self.m_t.m_interval.get_name())
            return
        self.m_t.answer_number(n)
        if self.m_t.answer_complete():
            self.handle_do_answer()
        else:
            self.show_progress()
    def show_progress(self):
        if self.m_t.m_answered_quality is not None:
            s = str(mpd.Interval.nn_to_translated_quality(self.m_t.m_answered_quality)) + "..."
        else:
            assert self.m_t.m_answered_number is not None
            s = "... %s" % mpd.Interval.number_name(self.m_t.m_answered_number)
        self.g_flashbar.set(s)
    def handle_do_answer(self):
        self.g_flashbar.clear()
        if self.m_t.answered_correctly():
            self.g_flashbar.flash(_("Correct, the interval is a %s") % self.m_t.m_interval.get_name())
        else:
            self.g_flashbar.flash(_("Wrong"))
            self.m_t.forget_answers()
    def on_start_practise(self):
        self.g_music_displayer.clear()
        if self.m_t.m_P.header.lesson_heading:
            self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        else:
            self.set_lesson_heading(_("Name the interval"))
        [btn.destroy() for btn in self.g_number_box.get_children()]
        for n in self.m_t.m_P.header.interval_number:
            b = gtk.Button(_(mpd.Interval.number_name(n)))
            b.connect('clicked', self.on_interval_number_clicked, n)
            self.g_number_box.pack_start(b)
            b.show()
        [btn.destroy() for btn in self.g_quality_box.get_children()]
        for n in self.m_t.m_P.header.interval_quality:
            b = gtk.Button(mpd.Interval.nn_to_translated_quality(n))
            b.connect('clicked', self.on_interval_quality_clicked, n)
            self.g_quality_box.pack_start(b)
            b.show()
            self.g_flashbar.require_size([
                _("Click 'New' to begin."),
            ])
            gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
                self.g_flashbar.flash(_("Click 'New' to begin.")))
    def on_end_practise(self):
        [btn.destroy() for btn in self.g_number_box.get_children()]
        [btn.destroy() for btn in self.g_quality_box.get_children()]
        self.g_music_displayer.clear()
