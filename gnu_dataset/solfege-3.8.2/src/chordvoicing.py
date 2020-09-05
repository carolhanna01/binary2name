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

import mpd
import mpd.musicdisplayer
import const
import abstract
import lessonfile
import soundcard

class Teacher(abstract.Teacher):
    OK = 0
    ERR_PICKY = 1
    #UGH should we do this here
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.ChordLessonfile
    def new_question(self):
        """
        return 1 if the teacher has a question to ask
        UI will never call this function unless we have a usable lessonfile.
        """
        assert self.m_P
        if self.get_bool('config/picky_on_new_question') \
                 and not(self.q_status in (const.QSTATUS_VOICING_SOLVED, const.QSTATUS_NO)):
            return Teacher.ERR_PICKY

        if self.m_P:
            self.m_P.select_random_question()
            self.q_status = const.QSTATUS_NEW
            return Teacher.OK
    def guess_chordtype(self, t):
        """
        return 1 if correct, None if not.
        This function will set self.q_status, and will raise an
        exception if the function is called with invalid value of
        q_status.

        Before we start: const.QSTATUS_NO
        After 'New' is clicked: const.QSTATUS_NEW

        if chordtype correct: const.QSTATUS_TYPE_SOLVED
        if chordtype wrong: const.QSTATUS_TYPE_WRONG

        if voicing correct const.QSTATUS_VOICING_SOLVED
        if voicing wrong: const.QSTATUS_VOICING_WRONG
        """
        if self.q_status in (const.QSTATUS_NEW, const.QSTATUS_TYPE_WRONG):
            if t == self.m_P.get_cname():
                self.q_status = const.QSTATUS_TYPE_SOLVED
                return 1
            else:
                self.q_status = const.QSTATUS_TYPE_WRONG
                return
    def guess_voicing(self, tones):
        """
        return 1 if correct, None if not.
        Gui should not call this function if the question is already solved.
        """
        v = self.m_P.get_music_as_notename_list('music')
        v.sort(mpd.compare_notenames)
        question_i = []
        for n in v:
            while n[-1] in ",'":
                n = n[:-1]
            question_i.append(mpd.notename_to_int(n))
        answer_i = []
        for n in tones:
            while n[-1] in ",'":
                n = n[:-1]
            answer_i.append(mpd.notename_to_int(n))
        if answer_i == question_i:
            self.q_status = const.QSTATUS_VOICING_SOLVED
            return 1
        self.q_status = const.QSTATUS_VOICING_WRONG
    def give_up(self):
        self.q_status = const.QSTATUS_VOICING_SOLVED

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window)
        self.m_stacking_frame_min_height = 0

        ###############
        # practise_box
        ###############
        self.practise_box.set_spacing(gu.PAD)

        hbox = gu.bHBox(self.practise_box, True, True)
        hbox.set_spacing(gu.PAD)
        ##################
        # chordtype frame 
        ##################
        frame = gtk.Frame(_("Identify chord type"))
        hbox.pack_start(frame)
        self.g_chordtype_box = gtk.VBox()
        self.g_chordtype_box.set_border_width(gu.PAD_SMALL)
        frame.add(self.g_chordtype_box)

        #################
        # stacking frame
        #################
        self.g_stacking_frame = gtk.Frame(_("Chord voicing"))
        self.g_stacking_frame.set_sensitive(False)
        hbox.pack_start(self.g_stacking_frame)
        vbox = gtk.VBox()
        vbox.set_border_width(gu.PAD_SMALL)
        self.g_stacking_frame.add(vbox)
        t = gtk.Table(1, 1, 1)
        vbox.pack_start(t)
        self.g_source = gtk.VBox()
        t.attach(self.g_source, 0, 1, 0, 1, gtk.EXPAND|gtk.FILL)
        self.g_answer = gtk.VBox()
        t.attach(self.g_answer, 1, 2, 0, 1, gtk.EXPAND|gtk.FILL)
        self.g_redo = gtk.Button("<<<")
        self.g_redo.connect('clicked', lambda o, self=self: self.fill_stacking_frame())
        vbox.pack_end(self.g_redo, False, False)

        self.g_flashbar = gu.FlashBar()
        self.practise_box.pack_start(self.g_flashbar, False)

        self.g_new = gu.bButton(self.action_area, _("_New chord"),
                                      self.new_question)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
                 lambda _o: self.m_t.m_P.play_question())
        self.g_repeat_arpeggio = gu.bButton(self.action_area, _("Repeat _arpeggio"), lambda _o: self.m_t.m_P.play_question_arpeggio())
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"), lambda _o, self=self: self.give_up())
        self.practise_box.show_all()
        ##############
        # config_box #
        ##############
        self.config_box.set_spacing(gu.PAD_SMALL)
        self.add_random_transpose_gui()
    def new_question(self, widget=None):
        def exception_cleanup():
            soundcard.synth.stop()
            self.g_new.set_sensitive(True)
            self.g_give_up.set_sensitive(False)
            self.g_repeat.set_sensitive(False)
            self.g_repeat_arpeggio.set_sensitive(False)
            self.m_t.q_status = const.QSTATUS_NO
        # if we have no lessonfile, then we have no questions.
        if not self.m_t.m_P:
            return
        try:
            g = self.m_t.new_question()
            if g == Teacher.OK:
                self.g_give_up.set_sensitive(True)
                self.g_repeat.set_sensitive(True)
                self.g_repeat_arpeggio.set_sensitive(True)
                self.g_new.set_sensitive(
                    not self.get_bool('config/picky_on_new_question'))
                self.clear_stacking_frame()
                self.g_stacking_frame.set_sensitive(False)
                for c in self.g_chordtype_box.get_children():
                    c.set_sensitive(True)
                self.m_t.m_P.play_question()
                self.g_flashbar.flash(_("Chord type"))
                self.g_chordtype_box.get_children()[0].grab_focus()
            elif g == Teacher.ERR_PICKY:
                self.g_flashbar.flash(_("You have to solve this question first."))
        except lessonfile.LessonfileException, e:
            if not self.standard_exception_handler(e, __file__, exception_cleanup):
                raise
        except mpd.MpdException, e:
            if not self.standard_exception_handler(e, __file__, exception_cleanup):
                raise
    def update_gui_after_lessonfile_change(self, *v):
        self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        self.g_new.set_sensitive(True)
        self.fill_chordtype_box()
        self.g_repeat.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.clear_stacking_frame()
    def clear_stacking_frame(self):
        for c in self.g_source.get_children() + self.g_answer.get_children():
            c.destroy()
    def fill_stacking_frame(self):
        """
        Create the buttons in stacking frame.
        """
        self.g_redo.set_sensitive(True)
        self.g_stacking_frame.set_sensitive(True)
        self.m_answer = []
        self.clear_stacking_frame()
        v = self.m_t.m_P.get_music_as_notename_list('music')
        v.sort()
        for n in v:
            nn = mpd.MusicalPitch.new_from_notename(n)
            b = gtk.Button(nn.get_user_notename())
            b.connect('clicked', self.on_notebutton_clicked, nn.get_notename(), nn.get_user_notename())
            self.g_source.pack_end(b, False, False)
            b.show()
        self.g_source.get_children()[0].grab_focus()
    def give_up(self, v=None):
        self.m_t.give_up()
        self.g_give_up.set_sensitive(False)
        self.g_new.set_sensitive(True)
        self.g_new.grab_focus()
        self.set_chordtype_frame_status_solved(self.m_t.m_P.get_cname())
        self.clear_stacking_frame()
        self.g_stacking_frame.set_sensitive(True)
        self.g_redo.set_sensitive(False)
        self.show_music()
    def on_chordtype_right_clicked(self, button, event, t):
        if self.m_t.m_P and not self.m_t.m_P.header.enable_right_click:
            self.g_flashbar.flash(_("Right click is not allowed for this lesson file."))
            return
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New chord' to begin."))
            return
        if self.m_t.q_status == const.QSTATUS_NEW:
            self.g_flashbar.flash(_("You should try to guess before right-clicking."))
            return
        if not self.m_t.m_P.has_question():
            return
        try:
            # We try first with the 'set' variable, as this is what is closest
            if 'set' in self.m_t.m_P.get_question():
                for question in self.m_t.m_P.m_questions:
                    if question['set'] == self.m_t.m_P.get_question()['set'] \
                            and question.get_cname() == t:
                        self.m_t.m_P.play_question(question)
                        return
            #
            for question in self.m_t.m_P.m_questions:
                if question.get_cname() == self.m_t.m_P.get_cname():
                    self.m_t.m_P.play_question(question)
                    return
        except lessonfile.LessonfileException, e:
            # This exception will be raised if for example the music
            # type for a question is invalid.
            if not self.standard_exception_handler(e, __file__):
                raise
        except mpd.MpdException, e:
            # Invalid key variable or music code goes here.
            if not self.standard_exception_handler(e, __file__):
                raise
    def on_chordtype_clicked(self, btn, t):
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New chord' to begin."))
        elif self.m_t.q_status in (const.QSTATUS_NEW, const.QSTATUS_TYPE_WRONG):
            g = self.m_t.guess_chordtype(t)
            if g:
                self.g_flashbar.flash(_("Correct, now stack the tones"))
                self.set_chordtype_frame_status_solved(t)
                self.fill_stacking_frame()
            else:
                self.g_flashbar.flash(_("Wrong"))
        elif self.m_t.q_status in (const.QSTATUS_TYPE_SOLVED, const.QSTATUS_VOICING_WRONG):
            self.g_flashbar.flash(_("Type is already solved, now specify voicing."))
    def on_notebutton_clicked(self, btn, n, user_notename):
        newb = gtk.Button(user_notename)
        newb.show()
        btn.destroy()
        self.m_answer.append(n)
        self.g_answer.pack_end(newb, False, False)
        if not self.g_source.get_children():
            # no children mean that the user has finished answering
            if self.m_t.guess_voicing(self.m_answer):
                self.g_flashbar.flash(_("Correct"))
                self.show_music()
                self.g_new.set_sensitive(True)
                self.g_new.grab_focus()
                self.g_give_up.set_sensitive(False)
                self.g_redo.set_sensitive(False)
            else:
                self.g_flashbar.flash(_("Wrong"))
                self.g_redo.grab_focus()
        else:
            self.g_source.get_children()[0].grab_focus()
    def show_music(self):
        self.clear_stacking_frame()
        md = mpd.musicdisplayer.MusicDisplayer(None)
        self.g_source.pack_start(md)
        md.show()
        md.display(r"\staff{ \clef %s < %s >}" % (
                mpd.select_clef(self.m_t.m_P.get_music_as_notename_string('music')),
                self.m_t.m_P.get_music_as_notename_string('music')), 20)
        # display the notenames on the buttons with octave info
        v = self.m_t.m_P.get_music_as_notename_list('music')
        v.sort(mpd.compare_notenames)
        for n in v:
            b = gtk.Button(mpd.MusicalPitch.new_from_notename(n).get_user_octave_notename())
            b.get_children()[0].set_use_markup(1)
            b.show()
            self.g_answer.pack_end(b, False, False)
    def fill_chordtype_box(self):
        for x in self.g_chordtype_box.get_children():
            x.destroy()
        if self.m_t.m_P:
            for ctype, translated_type in self.m_t.m_P.m_chord_types.iteritems():
                b = gtk.Button(translated_type)
                b.set_data('chordtype', ctype)
                self.g_chordtype_box.pack_start(b, False)
                b.connect('clicked', self.on_chordtype_clicked, ctype)
                b.connect('button_release_event', self.on_chordtype_right_clicked, ctype)
                b.show()
    def set_chordtype_frame_status_solved(self, t):
        for c in self.g_chordtype_box.get_children():
            c.set_sensitive(c.get_data('chordtype')==t)
    def on_start_practise(self):
        self.update_gui_after_lessonfile_change()
        self.g_flashbar.require_size([
            _("You have to solve this question first."),
            _("Type is already solved, now specify voicing."),
        ])
        self.g_new.grab_focus()
        gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
            self.g_flashbar.flash(_("Click 'New chord' to begin.")))
        self.fill_chordtype_box()
        self.clear_stacking_frame()
        self.g_stacking_frame.set_sensitive(False)
    def on_end_practise(self):
        self.m_t.end_practise()
        self.g_new.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_repeat_arpeggio.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
