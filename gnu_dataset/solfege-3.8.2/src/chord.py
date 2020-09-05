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
import gobject
import gu
from specialwidgets import QuestionNameCheckButtonTable
import soundcard, mpd, mpd.musicdisplayer
import abstract, const, lessonfile
import utils
import soundcard

class Teacher(abstract.Teacher):
    OK = 0
    ERR_PICKY = 1
    # valid values for self.q_status:
    # QSTATUS_NO       at program startup
    # QSTATUS_NEW      after the new button has been pressed
    # QSTATUS_SOLVED   when all three questions have been answered
    # QSTATUS_GIVE_UP  after 'Give Up' has been pressed.
    CORRECT = 1
    ALL_CORRECT = 2
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.ChordLessonfile
    def new_question(self):
        """
        return OK or ERR_PICKY
        UI will never call this function unless we have a usable lessonfile.
        """
        assert self.m_P
        if self.get_bool('config/picky_on_new_question') \
           and (not self.q_status in (const.QSTATUS_NO, const.QSTATUS_SOLVED,
                                      const.QSTATUS_GIVE_UP)):
            return self.ERR_PICKY
        self.m_P.select_random_question()
        self.m_correct = [self.m_P.get_cname() == -1,
                          self.m_P.get_inversion() == -1,
                          self.m_P.get_toptone() == -1]
        self.q_status = const.QSTATUS_NEW
        return self.OK
    def give_up(self):
        self.q_status = const.QSTATUS_GIVE_UP
    def guess_type(self, t):
        """
        GUI guarantees that this method will not be called after it has
        been guessed correct once.

        return 0 if this was wrong guess.
        return CORRECT if this question is correct.
        return ALL_CORRECT if all parts of the question is correct.
        """
        assert self.q_status == const.QSTATUS_NEW
        if t == self.m_P.get_cname():
            self.m_correct[0] = 1
            if self.m_correct == [1, 1, 1]:
                self.q_status = const.QSTATUS_SOLVED
                return self.ALL_CORRECT
            return self.CORRECT
        else:
            return 0
    def guess_inversion(self, t):
        """
        GUI guarantees that this method will not be called after it has
        been guessed correct once.
        
        return 0 if this was wrong guess.
        return CORRECT if this question is correct.
        return ALL_CORRECT if all parts of the question is correct.
        """
        assert self.q_status == const.QSTATUS_NEW
        if t == self.m_P.get_inversion():
            self.m_correct[1] = 1
            if self.m_correct == [1, 1, 1]:
                self.q_status = const.QSTATUS_SOLVED
                return self.ALL_CORRECT
            return self.CORRECT
        else:
            return 0
        self.m_correct[1] = 1
        return t == self.m_P.get_inversion()
    def guess_toptone(self, t):
        """
        GUI guarantees that this method will not be called after it has
        been guessed correct once.

        return 0 if this was wrong guess.
        return CORRECT if this question is correct.
        return ALL_CORRECT if all parts of the question is correct.
        """
        assert self.q_status == const.QSTATUS_NEW
        if t == self.m_P.get_toptone():
            self.m_correct[2] = 1
            if self.m_correct == [1, 1, 1]:
                self.q_status = const.QSTATUS_SOLVED
                return self.ALL_CORRECT
            return self.CORRECT
        else:
            return 0
        self.m_correct[2] = 1
        return t == self.m_P.get_toptone()

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window)
        ################
        # practise_box #
        ################
        hbox = gu.bHBox(self.practise_box)
        hbox.set_spacing(gu.PAD)
        spacebox = gtk.HBox()
        hbox.pack_start(spacebox)

        table = gtk.Table(4, 5, 0)
        hbox.pack_start(table, False)
        hsep = gtk.HSeparator()
        table.attach(hsep, 0, 5, 1, 2, xoptions=gtk.FILL,
                     yoptions=gtk.FILL, xpadding=0, ypadding=gu.PAD_SMALL)
        self.g_vsep1 = gtk.VSeparator()
        table.attach(self.g_vsep1, 1, 2, 0, 4,
                     xoptions=0, xpadding=gu.PAD)
        self.g_vsep2 = gtk.VSeparator()
        table.attach(self.g_vsep2, 3, 4, 0, 4,
                     xoptions=0, xpadding=gu.PAD)

        table.attach(gtk.Label(_("Chord type")), 0, 1, 0, 1,
                     xoptions=gtk.FILL, yoptions=0)
        self.g_chordtype_box = gtk.VBox()
        table.attach(self.g_chordtype_box, 0, 1, 2, 3, xoptions=gtk.FILL)

        self.g_inversion_label = gtk.Label(_("Inversion"))
        table.attach(self.g_inversion_label, 2, 3, 0, 1,
                     xoptions=gtk.FILL, yoptions=0)
        self.g_inversion_box = gtk.VBox()
        table.attach(self.g_inversion_box, 2, 3, 2, 3, xoptions=gtk.FILL)

        self.g_toptone_label = gtk.Label(_("Toptone"))
        table.attach(self.g_toptone_label, 4, 5, 0, 1,
                     xoptions=gtk.FILL, yoptions=0)
        self.g_toptone_box = gtk.VBox()
        table.attach(self.g_toptone_box, 4, 5, 2, 3, xoptions=gtk.FILL)

        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.g_music_displayer.set_size_request(100, -1)
        hbox.pack_start(self.g_music_displayer, False)
        spacebox = gtk.HBox()
        hbox.pack_start(spacebox)
        self.g_flashbar = gu.FlashBar()
        self.practise_box.pack_start(self.g_flashbar, False)

        self.g_new = gu.bButton(self.action_area, _("_New chord"),
                                self.new_question)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
              lambda w: self.run_exception_handled(self.m_t.m_P.play_question))
        self.g_repeat_arpeggio = gu.bButton(self.action_area,
              _("Repeat _arpeggio"),
              lambda w: self.run_exception_handled(self.m_t.m_P.play_question_arpeggio))
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"),
                                    self.give_up)
        self.practise_box.show_all()
        ##############
        # config_box #
        ##############
        self.config_box.set_spacing(gu.PAD_SMALL)
        self.add_random_transpose_gui()
        # -----------------------------------------
        self.g_select_questions_category_box, category_box= gu.hig_category_vbox(
            _("Chord types to ask"))
        self.config_box.pack_start(self.g_select_questions_category_box, True)
        self.g_select_questions = QuestionNameCheckButtonTable(self.m_t)
        self.g_select_questions.initialize(4, 0)
        category_box.pack_start(self.g_select_questions, False)
        self.g_select_questions.show()

    def update_select_question_buttons(self):
        if self.m_t.m_custom_mode:
            self.g_select_questions_category_box.show()
            self.g_select_questions.initialize(self.m_t.m_P.header.fillnum,
                                 self.m_t.m_P.header.filldir)
            self.m_t.check_askfor()
            for question in self.m_t.m_P.iterate_questions_with_unique_names():
                self.g_select_questions.add(question, 'normal')
        else:
            self.g_select_questions_category_box.hide()
            self.g_select_questions.initialize(0, 0)
    def update_answer_buttons(self, obj=None):
        for x in self.g_chordtype_box.get_children() \
              + self.g_inversion_box.get_children() \
              + self.g_toptone_box.get_children():
            x.destroy()
        if self.m_t.m_P:
            for ctype in self.m_t.m_P.get_unique_cnames():
                translated_type = self.m_t.m_P.m_chord_types[ctype]
                b = gtk.Button(translated_type)
                sr = b.size_request()
                b.set_size_request(int(sr[0]*1.3), sr[1])
                b.set_data('type', ctype)
                b.connect('clicked', self.on_type)
                b.connect('button_release_event', self.on_type)
                self.g_chordtype_box.pack_start(b, False)
                b.show()
            v = self.m_t.m_P.m_inversions
            v.sort()
            for x in v:
                if x == 0:
                    s = _("root position")
                else:
                    s = _("%i. inversion") % x
                b = gtk.Button(s)
                sr = b.size_request()
                b.set_size_request(int(sr[0]*1.3), sr[1])
                b.set_data('inversion', x)
                b.connect('clicked', self.on_inversion)
                b.connect('button_release_event', self.on_inversion)
                self.g_inversion_box.pack_start(b, False)
                b.show()
            v = self.m_t.m_P.m_toptones
            v.sort()
            for x in v:
                b = gtk.Button(str(x))
                b.set_data('toptone', x)
                b.connect('clicked', self.on_toptone)
                b.connect('button_release_event', self.on_toptone)
                self.g_toptone_box.pack_start(b, False)
                b.show()
        self.g_inversion_box.set_sensitive(True)
        self.g_toptone_box.set_sensitive(True)
    def update_gui_after_lessonfile_change(self):
        self.g_music_displayer.clear()
        self.update_select_question_buttons()
        if self.m_t.m_P.header.lesson_heading:
            self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        else:
            self.set_lesson_heading(_("Identify the chord"))
        self.g_new.set_sensitive(True)
        self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        if not [q for q in self.m_t.m_P.m_questions if 'toptone' in q]:
            self.g_toptone_label.hide()
            self.g_vsep2.hide()
        else:
            self.g_toptone_label.show()
            self.g_vsep2.show()
        if not [q for q in self.m_t.m_P.m_questions if 'inversion' in q]:
            self.g_inversion_label.hide()
            self.g_vsep1.hide()
        else:
            self.g_inversion_label.show()
            self.g_vsep1.show()
        self.g_repeat.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.update_answer_buttons()
    def on_type(self, button, event=None):
        if event is None:
            self.on_type_left_clicked(button)
        elif event.button == 3:
            if self.m_t.m_P and self.m_t.m_P.header.enable_right_click:
                self.on_type_right_clicked(button)
    def on_type_left_clicked(self, button):
        if self.m_t.q_status in (const.QSTATUS_NO, const.QSTATUS_GIVE_UP)\
          or self.m_t.m_correct[0]:
            return
        g = self.m_t.guess_type(button.get_data('type'))
        if g:
            self.g_flashbar.flash(_("Correct"))
            for b in self.g_chordtype_box.get_children():
                if b == button:
                    b.get_children()[0].set_name("BoldText")
            if g == self.m_t.ALL_CORRECT:
                self.all_guessed_correct()
        else:
            self.g_flashbar.flash(_("Wrong"))
        if self.m_t.m_correct == [1, 1, 1]:
            self.run_exception_handled(self.show_answer)
            self.g_new.set_sensitive(True)
            self.g_new.grab_focus()
            self.g_give_up.set_sensitive(False)
    def on_type_right_clicked(self, button):
        if self.m_t.q_status == const.QSTATUS_NO:
            return
        if not self.m_t.m_P.has_question():
            return
        if self.m_t.m_P.get_toptone() == -1 \
                and self.m_t.m_P.get_inversion() == -1:
            if 'set' in self.m_t.m_P.get_question():
                for question in self.m_t.m_P.m_questions:
                    if question['set'] == self.m_t.m_P.get_question()['set'] \
                        and question.get_cname() == button.get_data('type'):
                        self.run_exception_handled(self.m_t.m_P.play_question, question)
                        return
            else:
                for question in self.m_t.m_P.m_questions:
                    if question.get_cname() == button.get_data('type'):
                        self.run_exception_handled(self.m_t.m_P.play_question, question)
                        return
        else:
            #Try for exact match
            for question in self.m_t.m_P.m_questions:
                if  question.get_toptone() != self.m_t.m_P.get_toptone():
                    continue
                if question.get_inversion() != self.m_t.m_P.get_inversion():
                    continue
                if question.get_cname() == button.get_data('type'):
                    self.run_exception_handled(self.m_t.m_P.play_question, question)
                    return
        # try to match, ignoring toptone
        for question in self.m_t.m_P.m_questions:
            if question.get_inversion() != self.m_t.m_P.get_inversion():
                continue
            if question.get_cname() == button.get_data('type'):
                self.run_exception_handled(self.m_t.m_P.play_question, question)
                return
        # match if only type matches.
        for question in self.m_t.m_P.m_questions:
            if question.get_cname() == button.get_data('type'):
                self.run_exception_handled(self.m_t.m_P.play_question, question)
                return
    def on_inversion(self, button, event=None):
        if event is None:
            self.on_inversion_left_clicked(button)
        elif event.button == 3:
            if self.m_t.m_P and self.m_t.m_P.header.enable_right_click:
                self.on_inversion_right_clicked(button)
    def on_inversion_left_clicked(self, button):
        if self.m_t.q_status in (const.QSTATUS_NO, const.QSTATUS_GIVE_UP) \
          or self.m_t.m_correct[1]:
            return
        if self.m_t.guess_inversion(button.get_data('inversion')):
            self.g_flashbar.flash(_("Correct"))
            for b in self.g_inversion_box.get_children():
                if b == button:
                    b.get_children()[0].set_name("BoldText")
        else:
            self.g_flashbar.flash(_("Wrong"))
        if self.m_t.m_correct == [1, 1, 1]:
            self.run_exception_handled(self.show_answer)
            self.g_new.set_sensitive(True)
            self.g_new.grab_focus()
            self.g_give_up.set_sensitive(False)
    def on_inversion_right_clicked(self, button):
        """
        First we try to find a chord with the same type and toptone as
        the question, and with toptone as in button.get_data('toptone').

        Second best is to ignore inversion and find a chord where chord type
        is the same as the questions, and with toptone as
        button.get_data('toptone')

        """
        if not self.m_t.m_P.has_question():
            return
        # first we try to get an exact match
        for question in self.m_t.m_P.m_questions:
            if question.get_toptone() != -1 \
                    and question.get_toptone() != self.m_t.m_P.get_toptone():
                continue
            if question.get_cname() == self.m_t.m_P.get_cname() \
                  and question.get_inversion() == button.get_data('inversion'):
                self.run_exception_handled(self.m_t.m_P.play_question, question)
                return
        # then we tries to match chord type and inversion, ignoring toptone
        for question in self.m_t.m_P.m_questions:
            if question.get_cname() == self.m_t.m_P.get_question().get_cname()\
                   and question.get_inversion() == button.get_data('inversion'):
                self.run_exception_handled(self.m_t.m_P.play_question, question)
                return
    def on_toptone(self, button, event=None):
        if not event:
            self.on_toptone_left_clicked(button)
        elif event.button == 3:
            if self.m_t.m_P and self.m_t.m_P.header.enable_right_click:
                self.on_toptone_right_clicked(button)
    def on_toptone_right_clicked(self, button):
        if not self.m_t.m_P.has_question():
            return
        # first we try to get an exact match
        for question in self.m_t.m_P.m_questions:
            if question.get_inversion() != -1 \
                  and question.get_inversion() != self.m_t.m_P.get_inversion():
                continue
            if question.get_cname() == self.m_t.m_P.get_cname() \
                  and question.get_toptone() == button.get_data('toptone'):
                self.run_exception_handled(self.m_t.m_P.play_question, question)
                return
        # then we tries to match chord type and toptone, ignoring inversion
        for question in self.m_t.m_P.m_questions:
            if question.get_cname() == self.m_t.m_P.get_question().get_cname()\
                    and question.get_toptone() == button.get_data('toptone'):
                sel.run_exception_handled(self.m_t.m_P.play_question, question)
                return
    def on_toptone_left_clicked(self, button):
        if self.m_t.q_status in (const.QSTATUS_NO, const.QSTATUS_GIVE_UP) \
           or self.m_t.m_correct[2]:
            return
        if self.m_t.guess_toptone(button.get_data('toptone')):
            self.g_flashbar.flash(_("Correct"))
            for b in self.g_toptone_box.get_children():
                if b == button:
                    b.get_children()[0].set_name("BoldText")
        else:
            self.g_flashbar.flash(_("Wrong"))
        if self.m_t.m_correct == [1, 1, 1]:
            self.run_exception_handled(self.show_answer)
            self.g_new.set_sensitive(True)
            self.g_new.grab_focus()
            self.g_give_up.set_sensitive(False)
    def all_guessed_correct(self):
        self.g_new.set_sensitive(True)
        self.g_give_up.set_sensitive(False)
    def new_question(self, widget=None):
        def exception_cleanup():
            soundcard.synth.stop()
            self.g_give_up.set_sensitive(False)
            self.g_repeat.set_sensitive(False)
            self.g_repeat_arpeggio.set_sensitive(False)
            self.m_t.q_status = const.QSTATUS_NO
        # if we have no lessonfile, then we have to questions.
        if not self.m_t.m_P:
            return
        # make sure all buttons are sensitive.
        for button in self.g_chordtype_box.get_children() \
                    + self.g_inversion_box.get_children() \
                    + self.g_toptone_box.get_children():
            button.get_children()[0].set_name("")
        ##
        try:
            n = self.m_t.new_question()
            if n == Teacher.ERR_PICKY:
                print "picky!"
            else:
                self.g_music_displayer.clear()
                self.m_t.m_P.play_question()
                self.g_give_up.set_sensitive(True)
                self.g_repeat.set_sensitive(True)
                if self.get_bool('config/picky_on_new_question'):
                    self.g_new.set_sensitive(False)
                self.g_repeat_arpeggio.set_sensitive(True)
                self.g_inversion_box.set_sensitive(self.m_t.m_P.get_inversion() != -1)
                self.g_toptone_box.set_sensitive(self.m_t.m_P.get_toptone() != -1)
                self.g_chordtype_box.get_children()[0].grab_focus()
        except Exception, e:
            if not self.standard_exception_handler(e, __file__,
                    exception_cleanup):
                raise
    def on_start_practise(self):
        self.m_t.m_custom_mode = self.get_bool('gui/expert_mode')
        for question in self.m_t.m_P.m_questions:
            question['active'] = 1
        self.update_gui_after_lessonfile_change()
        self.g_flashbar.require_size([
            _("Click 'New chord' to begin."),
            "XXXX, root position, toptone: 5",
        ])
        self.g_new.grab_focus()
        gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
            self.g_flashbar.flash(_("Click 'New chord' to begin.")))
    def on_end_practise(self):
        self.m_t.end_practise()
        self.g_music_displayer.clear()
        self.g_new.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
    def give_up(self, widget=None):
        if self.m_t.q_status == const.QSTATUS_NEW:
            self.m_t.give_up()
            self.run_exception_handled(self.show_answer)
            type_txt = self.m_t.m_P.m_chord_types[self.m_t.m_P.get_cname()]
            i = self.m_t.m_P.get_inversion()
            if i == 0:
                inversion_txt = _("root position")
            elif i > 0:
                inversion_txt = _("%i. inversion") % i
            else:
                inversion_txt = ""
            i = self.m_t.m_P.get_toptone()
            if i != -1:
                toptone_txt = "toptone: %i" % i
            else:
                toptone_txt = ""
            v = [type_txt]
            if inversion_txt:
                v.append(inversion_txt)
            if toptone_txt:
                v.append(toptone_txt)
            if len(v) == 3:
                self.g_flashbar.flash(_("%(one)s, %(two)s and %(three)s") % {'one': v[0], 'two': v[1], 'three': v[2]})
            elif len(v) == 2:
                self.g_flashbar.flash(_("%(one)s and %(two)s") % {'one': v[0], 'two': v[1]})
            else:
                self.g_flashbar.flash(type_txt)
            self.g_new.set_sensitive(True)
            self.g_give_up.set_sensitive(False)
            for button in self.g_chordtype_box.get_children():
                if button.get_data('type') == self.m_t.m_P.get_cname():
                    button.get_children()[0].set_name('BoldText')
                else:
                    button.get_children()[0].set_name('')
            for button in self.g_inversion_box.get_children():
                if button.get_data('inversion') == self.m_t.m_P.get_inversion():
                    button.get_children()[0].set_name('BoldText')
                else:
                    button.get_children()[0].set_name('')
            for button in self.g_toptone_box.get_children():
                if button.get_data('toptone') == self.m_t.m_P.get_toptone():
                    button.get_children()[0].set_name('BoldText')
                else:
                    button.get_children()[0].set_name('')
    def show_answer(self):
        """
        Show the answer in the music displayer. All callers must check
        for exceptions.
        """
        fontsize = self.get_int('config/feta_font_size=20')
        if self.m_t.m_P.get_question()['music'].m_musictype == 'chord':
            clef = mpd.select_clef(self.m_t.m_P.get_music_as_notename_string('music'))
            self.g_music_displayer.display(r"\staff{\clef %s <%s>}" % (clef, self.m_t.m_P.get_music_as_notename_string('music')), fontsize)
        else:
            self.g_music_displayer.display(self.m_t.m_P.get_music(), fontsize)

