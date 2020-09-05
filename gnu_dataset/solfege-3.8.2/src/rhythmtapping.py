# GNU Solfege - free ear training software
# Copyright (C) 2006, 2007  Tom Cato Amundsen
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

import time
import gtk
import gu
import abstract
import lessonfile
import mpd
import const

class Teacher(abstract.Teacher):
    OK = 0
    ERR_PICKY = 1
    ERR_NO_ELEMS = 2
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.ElembuilderLessonfile
    def new_question(self):
        self.m_P.select_random_question()
        return self.OK
    def get_timedelta_list(self):
        """
        Return a list of the number of seconds between it should be between
        each tap. Ignore "count-in" tones.
        """
        if 'rhythm' in self.m_P.get_question():
            qvar = 'rhythm'
        else:
            qvar = 'music'
        musictype = self.m_P.get_question()['music'].m_musictype
        lexer = mpd.parser.Lexer(self.m_P.get_question()[qvar].m_musicdata)
        x = len(lexer.m_string)
        retval = []
        while lexer.m_idx < x:
            toc, toc_data = lexer.get()
            if musictype == 'rhythm':
                if toc_data.m_pitch.get_octave_notename() == 'd':
                    continue
                if toc_data.m_pitch.get_octave_notename() != 'c':
                    print "warning: Use only c and d for rhythm music objects"
            retval.append(float(toc_data.m_duration.get_rat_value()) * self.m_P.get_tempo()[1] / self.m_P.get_tempo()[0] * 60)
        return retval
    def start_tapping(self):
        self.m_taps = []
    def tap(self):
        self.m_taps.append(time.time())
    def is_tap_complete(self):
        """
        Return True if the user has tapped as many times as the
        question requires.
        """
        # A little abuse of get_timedelta_list, but this makes it simple.
        return len(self.m_taps) == len(self.get_timedelta_list())
    def get_score(self):
        """
        Return a list of floats telling us how close the users answer was.
        Each float is the timedelta of the question divided by the
        timedelta of the answer.

        If at_question_start == show, then the time between the first
        and the second tap will set the tempo, and all the timedeltas will
        be compared
        """
        retval = []
        question = self.get_timedelta_list()
        if self.m_P.header.at_question_start == 'show':
            # The user can tap in any tempo since he will only se the music.
            # First we change the lists so that the time between the
            # first two taps are 1.0, and the other proportionally to this.
            question = [q/question[0] for q in question]
            answer = []
            for x, i in enumerate(self.m_taps[1:]):
                answer.append(self.m_taps[x+1] - self.m_taps[x])
            answer = [a/answer[0] for a in answer]
            for idx in range(len(answer)):
                retval.append(question[idx] / answer[idx])
        else:
            # Has to tap in the same tempo as the music played.
            for x, i in enumerate(self.m_taps[1:]):
                a = self.m_taps[x+1] - self.m_taps[x]
                retval.append(a / question[x])
        return retval
    def get_answer_status(self):
        """
        Will return a tuple (bool, string) where the bool is True if the
        exercises is answered correctly enough. The string is a message
        to the user describing how acourately the rhythm was tapped.
        """
        score = self.get_score()
        print "rhythmtapping score:", " ".join(["%.2f" % x for x in score])
        s = ""
        max_diff = 0.0
        for f in score:
            s = s + " %.2f" % f
            max_diff = max(max_diff, abs(1.0-f))
        if self.m_P.header.at_question_start == 'show':
            # It is much easier to tap in your own tempo, so we have to
            # require more aquracy.
            limit = 0.05
        else:
            limit = 0.09
        if max_diff < limit:
            s = "OK: %.2f < %.2f" % (max_diff, limit)
        else:
            s = "Not good enough: %.2f > %.2f" % (max_diff, limit)
        return (max_diff < limit, s)

class Gui(abstract.LessonbasedGui):
    please_tap_str = _("Please tap the rhythm.")
    def __init__(self, teacher, window):
        abstract.Gui.__init__(self, teacher, window)
        #
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(None)
        self.practise_box.pack_start(self.g_music_displayer, False)
        #
        self.g_tap = gu.bButton(self.practise_box, "Tap here", self.on_tap)
        self.g_new = gu.bButton(self.action_area, _("_New"),
            self.on_new_question)
        self.g_play_music = gu.bButton(self.action_area, _("P_lay music"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question))
        self.g_display_music = gu.bButton(self.action_area, _("_Display music"),
            self.show_answer)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
            self.on_repeat)
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"), self.on_give_up)
        # Flashbar
        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False)
    def on_new_question(self, widget=None):
        def exception_cleanup():
            self.g_repeat.set_sensitive(False)
            self.g_give_up.set_sensitive(False)
        g = self.m_t.new_question()
        if g == self.m_t.OK:
            if self.m_t.m_P.header.have_music_displayer:
                self.g_music_displayer.clear()
            try:
                self.do_at_question_start_show_play()
            except Exception, e:
                e.m_mpd_varname = 'music'
                e.m_mpd_badcode = self.m_t.m_P.get_question()['music'].m_musicdata
                if not self.standard_exception_handler(e, __file__,
                        exception_cleanup):
                    raise
            else:
                self.m_start_time = time.time()
                self.g_flashbar.push(self.please_tap_str)
                self.m_t.start_tapping()
                self.g_new.set_sensitive(
                   not self.get_bool('config/picky_on_new_question'))
                self.g_repeat.set_sensitive(True)
                self.g_give_up.set_sensitive(True)
                self.g_tap.set_sensitive(True)
                self.g_tap.grab_focus()
                self.g_play_music.set_sensitive(True)
                self.g_display_music.set_sensitive(True)
        elif g == self.m_t.ERR_PICKY:
            self.g_flashbar.flash(_("You have to solve this question first."))
        else:
            assert g == self.m_t.ERR_NO_ELEMS
            self.g_repeat.set_sensitive(False)
            self.g_flashbar.flash(_("You have to configure this exercise properly"))
    def on_give_up(self, widget):
        self.g_new.set_sensitive(True)
        self.g_new.grab_focus()
        self.g_repeat.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.g_flashbar.clear()
        self.g_tap.set_sensitive(False)
        self.m_t.q_status = const.QSTATUS_GIVE_UP
    def on_repeat(self, widget):
        self.m_t.m_P.play_question()
        self.g_tap.grab_focus()
    def on_tap(self, widget=None):
        self.g_flashbar.set(_("Tapping in progress..."))
        self.m_t.tap()
        try:
            if self.m_t.is_tap_complete():
                solved, msg = self.m_t.get_answer_status()
                self.g_flashbar.pop()
                if solved:
                    self.g_new.set_sensitive(True)
                    self.g_new.grab_focus()
                    self.g_tap.set_sensitive(False)
                    self.m_t.q_status = const.QSTATUS_SOLVED
                else:
                    self.g_tap.grab_focus()
                    self.m_t.start_tapping()
                    self.g_flashbar.set(self.please_tap_str)
                self.g_flashbar.flash(msg)
        except mpd.MpdException, e:
            # If the 'rhythm' variable is defined in a question, Solfege will
            # use this variable when comparing the rhythm. But it will still
            # use the 'music' variable when playing and evt. displaying the
            # rhythm.
            if 'rhythm' in self.m_t.m_P.get_question():
                qvar = 'rhythm'
            else:
                qvar = 'music'
            e.m_mpd_varname = qvar
            e.m_mpd_badcode = self.m_t.m_P.get_question()[qvar].m_musicdata
            if not self.standard_exception_handler(e, __file__):
                raise
    def on_start_practise(self):
        self.g_new.set_sensitive(True)
        self.g_tap.set_sensitive(False)
        self.g_repeat.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.show_hide_at_question_start_buttons()
        self.g_play_music.set_sensitive(False)
        self.g_display_music.set_sensitive(False)
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.show()
            self.g_music_displayer.clear()
        else:
            self.g_music_displayer.hide()
        self.g_flashbar.require_size([
         self.please_tap_str,
         ])
