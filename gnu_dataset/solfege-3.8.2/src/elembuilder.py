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

import gtk

import gu
import const
import lessonfile
import abstract
import statistics
import statisticsviewer
import mpd
import utils

class Teacher(abstract.Teacher):
    OK = 0
    ERR_NO_QUESTION = 2
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.ElembuilderLessonfile
        self.m_statistics = statistics.LessonStatistics(self)
    def new_question(self):
        self.m_P.select_random_question()
        self.q_status = const.QSTATUS_NEW
        return self.OK
    def guess_answer(self, answer):
        if [e['name'] for e in self.m_P.get_question()['elements']] == answer:
            if self.q_status == const.QSTATUS_NEW \
                    and not self.m_custom_mode:
                self.m_statistics.add_correct(self.m_P.get_cname())
            self.q_status = const.QSTATUS_SOLVED
            return True
        else:
            if self.q_status == const.QSTATUS_NEW:
                if not self.m_custom_mode:
                    self.m_statistics.add_wrong(self.m_P.get_cname(), "None")
                self.q_status = const.QSTATUS_WRONG
            return False
    def give_up(self):
        self.q_status = const.QSTATUS_GIVE_UP

class MultiButton(gtk.Button):
    def __init__(self, label):
        gtk.Button.__init__(self)
        if isinstance(label, lessonfile.plstring):
            hp = gu.HarmonicProgressionLabel(label)
            hp.show_all()
            self.add(hp)
        else:
            l = gtk.Label(label)
            l.show()
            self.add(l)
        self.m_marked_wrong = False
    def mark_wrong(self):
        if self.m_marked_wrong:
            return
        self.m_marked_wrong = True
        vbox = gtk.VBox()
        assert len(self.get_children()) == 1
        self.get_children()[0].reparent(vbox)
        self.add(vbox)
        label = gtk.Label()
        label.set_markup("<span size='small'>%s</span>" % _("Wrong"))
        label.show()
        vbox.pack_start(label)
        vbox.show()

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window)
        self.g_lesson_heading.hide()
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.practise_box.pack_start(self.g_music_displayer, False)

        self.g_answer_button_box = gu.NewLineBox()
        self.practise_box.pack_start(self.g_answer_button_box)
        # The user fill the answer in this box
        self.g_answer_frame = gtk.Frame()
        self.g_answer_frame.set_shadow_type(gtk.SHADOW_IN)
        self.practise_box.pack_start(self.g_answer_frame, False, False)
        self.g_answer = gtk.HBox()
        self.g_answer_frame.add(self.g_answer)
        self.g_answer_frame.show_all()
        # Flashbar
        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False)
        # action area
        self.g_new = gu.bButton(self.action_area, _("_New"), self.new_question)
        self.g_play_music = gu.bButton(self.action_area, _("P_lay music"),
            lambda w: self.run_exception_handled(self.m_t.m_P.play_question))
        self.g_display_music = gu.bButton(self.action_area, _("_Display music"),
            self.show_answer)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
                self.repeat_question)
        self.g_guess_answer = gu.bButton(self.action_area, _("Guess _answer"),
                self.guess_answer)
        self.g_play_tonic = gu.bButton(self.action_area, _("Play _tonic"),
                lambda w: self.run_exception_handled(self.m_t.play_tonic))
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"),
                self.give_up)
        self.g_backspace = gu.bButton(self.action_area, _("_Backspace"),
                     self.on_backspace)
        self.g_backspace.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        ##############
        # statistics #
        ##############
        self.setup_statisticsviewer(statisticsviewer.PercentagesStatisticsViewer,
                                   _("elembuilder"))
    def new_question(self, widget):
        self.g_answer.foreach(lambda w: w.destroy())
        self.m_users_answer = []
        self.m_t.new_question()
        # These two will be insensitive if we have an exception or not.
        self.g_backspace.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        #if 'show' in self.m_t.m_P.header.at_question_start \
        #    and 'play' in self.m_t.m_P.header.at_question_start:
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.clear(self.m_t.m_P.header.music_displayer_stafflines)
        try:
            self.do_at_question_start_show_play()
        except Exception, e:
            # Setup buttons when we have an exception. We have to do this
            # before we call standard_exception_handler because we wan't
            # the buttons in this state, even if standard_exception_handler
            # can not handle it.
            self.g_new.set_sensitive(True)
            self.g_repeat.set_sensitive(False)
            self.g_guess_answer.set_sensitive(False)
            self.g_play_tonic.set_sensitive(False)
            if not self.standard_exception_handler(e, __file__):
                raise
        else:
            self.g_new.set_sensitive(
                not self.get_bool('config/picky_on_new_question'))
            self.g_repeat.set_sensitive(True)
            self.g_guess_answer.set_sensitive(True)
            self.g_play_tonic.set_sensitive('tonic' in self.m_t.m_P.get_question())
    def repeat_question(self, widget):
        self.m_t.m_P.play_question()
    def guess_answer(self, widget):
        if self.m_t.q_status == const.QSTATUS_NO:
            if self.m_t.m_app.m_test_mode:
                self.g_flashbar.flash(_("Click 'Start test' to begin."))
            else:
                self.g_flashbar.flash(_("Click 'New' to begin."))
            return
        elif self.m_t.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG):
            if self.m_t.guess_answer(self.m_users_answer):
                self.g_flashbar.flash(_("Correct"))
                self.g_new.set_sensitive(True)
                self.g_new.grab_focus()
                self.g_guess_answer.set_sensitive(False)
                self.g_give_up.set_sensitive(False)
                self.g_backspace.set_sensitive(False)
                if self.m_t.m_P.header.have_music_displayer \
                    and self.m_t.m_P.get_question()['music'].is_displayable():
                        self.display_music('music')
            else:
                self.g_flashbar.flash(_("Wrong"))
                max_button_height = 0
                for idx, a in enumerate(self.m_users_answer):
                    if idx + 1 > len(self.m_t.m_P.get_question()['elements']):
                        break
                    if idx + 1 > len(self.m_users_answer):
                        break
                    if self.m_users_answer[idx] != self.m_t.m_P.get_question()['elements'][idx]['name']:
                        self.g_answer.get_children()[idx].mark_wrong()
                if len(self.m_users_answer) > len(self.m_t.m_P.get_question()['elements']):
                    for btn in self.g_answer.get_children()[len(self.m_t.m_P.get_question()['elements']):]:
                        btn.mark_wrong()

                # We only recalculate the height of the answer frame
                # if there are any buttons in it. This to avoid the frame
                # disappearing.
                if self.g_answer.get_children():
                    for btn in self.g_answer.get_children():
                        max_button_height = max(max_button_height, btn.size_request()[1])
                    self.g_answer_frame.set_size_request(-1, max_button_height)
                self.g_give_up.set_sensitive(True)
    def on_backspace(self, widget):
        if self.m_users_answer:
            del self.m_users_answer[-1]
            self.g_answer.get_children()[-1].destroy()
    def add_element(self, widget, element):
        self.m_users_answer.append(element['name'])
        b = MultiButton(element['label'])
        b.show()
        self.g_answer.pack_start(b, False, False)
        self.g_backspace.set_sensitive(True)
    def give_up(self, widget):
        self.g_answer.foreach(lambda w: w.destroy())
        self.m_t.give_up()
        for elem in self.m_t.m_P.get_question()['elements']:
            b = MultiButton(elem['label'])
            b.show()
            self.g_answer.pack_start(b, False, False)
        if self.m_t.m_P.header.have_music_displayer \
            and self.m_t.m_P.get_question()['music'].is_displayable():
                self.display_music('music')
        self.g_new.set_sensitive(True)
        self.g_give_up.set_sensitive(False)
        self.g_guess_answer.set_sensitive(False)
        self.g_backspace.set_sensitive(False)
    def on_start_practise(self):
        self.m_t.m_custom_mode = False # FIXME
        if self.m_t.m_P.header.elements == 'auto':
            v = []
            for question in self.m_t.m_P.m_questions:
                for elem in question['elements']:
                    if elem not in v:
                        v.append(elem)
            def xcmp(a, b):
                return cmp(self.m_t.m_P.blocklists['element'].index(a),
                           self.m_t.m_P.blocklists['element'].index(b))
            v.sort(xcmp)
            self.m_t.m_P.header.elements = v
        self.g_answer_button_box.empty()
        self.g_answer.foreach(lambda w: w.destroy())
        #
        if not self.m_t.m_P.header.music_displayer_stafflines:
            self.m_t.m_P.header.music_displayer_stafflines = 1
        #
        if self.m_t.m_P.header.have_music_displayer:
            self.g_music_displayer.show()
            self.g_music_displayer.clear(self.m_t.m_P.header.music_displayer_stafflines)
        else:
            self.g_music_displayer.hide()
        #
        self.g_statview.g_heading.set_text(self.m_t.m_P.header.title)
        self.m_elem_button_max_height = 0
        for elem in self.m_t.m_P.header.elements:
            b = MultiButton(elem['label'])
            b.connect('clicked', self.add_element, elem)
            b.show()
            self.g_answer_button_box.add_widget(b)
        self.g_answer_button_box.show_widgets()
        self.g_answer.set_size_request(-1, self.g_answer_button_box.get_max_child_height())
        self.set_lesson_heading(self.m_t.m_P.header.lesson_heading)
        self.m_users_answer = []
        self.g_new.set_sensitive(True)
        self.g_repeat.set_sensitive(False)
        self.g_guess_answer.set_sensitive(False)
        self.g_play_tonic.set_sensitive(False)
        if [q for q in self.m_t.m_P.m_questions if 'tonic' in q]:
            self.g_play_tonic.show()
        else:
            self.g_play_tonic.hide()
        self.show_hide_at_question_start_buttons()
        self.g_flashbar.flash(_("Click 'New' to begin."))
    def on_end_practise(self):
        self.m_t.end_practise()
        self.g_backspace.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
