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
import mpd, mpd.musicdisplayer
import abstract, const, lessonfile
from mpd.rat import Rat
import soundcard
import utils

class Teacher(abstract.Teacher):
    """
    The Teacher and Gui for dictation abuses q_status a little.
    QSTATUS_NEW mean that the lessonfile is ok.
    QSTATUS_NO mean that the question is corrupt and unusable. Other
               questions in the same file might be useable.
    """
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.DictationLessonfile

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window, no_notebook=1)
        ################
        # practise_box #
        ################
        self.g_question_title = gu.bLabel(self.practise_box, "", False, False)

        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.set_size_request(500, -1)
        self.g_music_displayer.show()
        self.practise_box.pack_start(self.g_music_displayer)
        ###############
        # action_area #
        ###############
        self.g_partbox = gu.bHBox(self.practise_box, False)
        self.g_go_back = gtk.Button(stock='gtk-go-back')
        self.g_go_back.connect('clicked', self.select_previous)
        self.g_go_back.show()
        self.g_go_back.get_children()[0].get_children()[0].get_children()[1].hide()
        self.action_area.pack_start(self.g_go_back, False)
        self.g_go_forward = b = gtk.Button(stock='gtk-go-forward')
        self.g_go_forward.show()
        self.g_go_forward.connect('clicked', self.select_next)
        self.g_go_forward.get_children()[0].get_children()[0].get_children()[1].hide()
        self.action_area.pack_start(self.g_go_forward, False)
        self.g_play = gu.bButton(self.action_area, _("_Play the whole music"),
                                 self.play)
        self.g_show = gu.bButton(self.action_area, _("_Show"), self.show_answer)
    def exception_cleanup(self):
        """ cleanup function after exception caught in select_previous
        and select_next
        """
        soundcard.synth.stop()
        self.m_t.q_status = const.QSTATUS_NO
        self.g_play.set_sensitive(False)
        self.g_show.set_sensitive(False)
        self.g_music_displayer.clear()
    def select_previous(self, widget):
        self.m_t.m_P.select_previous()
        try:
            self.display_start_of_music()
        except Exception, e:
            if not self.standard_exception_handler(e, __file__,
                    self.exception_cleanup):
                raise
        else:
            self.m_t.q_status = const.QSTATUS_NEW
            self.g_play.set_sensitive(True)
            self.g_show.set_sensitive(True)
        self._update()
    def select_next(self, widget):
        self.m_t.m_P.select_next()
        try:
            self.display_start_of_music()
        except Exception, e:
            if not self.standard_exception_handler(e, __file__,
                    self.exception_cleanup):
                raise
        else:
            self.m_t.q_status = const.QSTATUS_NEW
            self.g_play.set_sensitive(True)
            self.g_show.set_sensitive(True)
        self._update()
    def play(self, widget=None):
        # see Teacher docstring.
        if self.m_t.q_status != const.QSTATUS_NEW:
            return
        if not self.m_t.m_P:
            return
        try:
            self.m_t.m_P.play_question()
        except Exception, e:
            if not self.standard_exception_handler(e, __file__,
                    soundcard.synth.stop):
                raise
    def on_end_practise(self):
        self.m_t.end_practise()
    def show_answer(self, widget=None):
        # see Teacher docstring.
        if self.m_t.q_status != const.QSTATUS_NEW:
            return
        if not self.m_t.m_P:
            return
        try:
            self.g_music_displayer.display(self.m_t.m_P.get_music(),
                            self.get_int('config/feta_font_size=20'))
        except Exception, e:
            if not self.standard_exception_handler(e, __file__):
                raise
    def _update(self):
        """
        Updates the buttons above the action_area where you have
        one or more buttons with a small note pixmap on. Each of the
        buttons will play one part of the music in the question.
        """
        # tmp func used as callback function
        def f(w, start, end, self=self):
            try:
                track = mpd.music_to_track(self.m_t.m_P.get_music(),
                       self.get_int('config/preferred_instrument_velocity'),
                       start, end)
                track.prepend_bpm(*self.m_t.m_P.get_tempo())
                track.prepend_patch(self.get_int('config/preferred_instrument'))
                soundcard.synth.play_track(track)
            except Exception, e:
                if not self.standard_exception_handler(e, __file__,
                        soundcard.synth.stop):
                    raise
        for i in self.g_partbox.get_children():
            i.destroy()
        # if the lessonfile was invalid, m_P could be None
        if self.m_t.m_P and self.m_t.m_P.m_questions:
            self.g_question_title.set_text(self.m_t.m_P.get_name())
            v = self.m_t.m_P.get_breakpoints()
            if v == []:
                # we display one button that will play the whole music if
                # there are not breakpoints in the music
                btn = self.create_pixmap_button()
                btn.connect('clicked', f, None, None)
                btn.show()
                self.g_partbox.pack_start(btn)
                return
            tmp = [Rat(0, 1)] + v + [Rat(2**30, 1)]
            for i in range(len(tmp) - 1):
                btn = self.create_pixmap_button()
                btn.show()
                btn.connect('clicked', f, tmp[i], tmp[i+1])
                self.g_partbox.pack_start(btn)
        # q_status is QSTATUS_NO if the question is invalid (from the lessonfile)
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_partbox.set_sensitive(False)
        else:
            self.g_partbox.set_sensitive(True)
    def display_start_of_music(self):
        """
        Callers must catch exceptions.
        """
        fontsize = self.get_int('config/feta_font_size=20')
        if self.m_t.m_P.get_clue_music():
            self.g_music_displayer.display(self.m_t.m_P.get_clue_music(), fontsize)
        elif self.m_t.m_P.get_clue_end():
            self.g_music_displayer.display_range(self.m_t.m_P.get_music(),
                        fontsize, Rat(0, 1), self.m_t.m_P.get_clue_end())
        else:
            self.g_music_displayer.display(self.m_t.m_P.get_music(), 
                        fontsize, mpd.FIRST)
    def update_gui_after_lessonfile_change(self):
        self.m_t.q_status = const.QSTATUS_NEW
        if not self.m_t.m_P.m_questions:
            self.g_win.display_error_message(_("The lesson file '%s' contains no questions.") % self.m_t.m_lessonfile)
            self.g_play.set_sensitive(False)
            self.g_show.set_sensitive(False)
            self.g_go_back.set_sensitive(False)
            self.g_go_forward.set_sensitive(False)
            self._update()
            return
        else:
            self.g_go_forward.set_sensitive(True)
            self.g_go_back.set_sensitive(True)
        self.m_t.m_P.select_first()
        self.action_area.set_sensitive(True)
        try:
            self.display_start_of_music()
        except Exception, e:
            def cleanup_function():
                self.m_t.q_status = const.QSTATUS_NO
                self.g_play.set_sensitive(False)
                self.g_show.set_sensitive(False)
                self.g_music_displayer.clear()
            if not self.standard_exception_handler(e, __file__,
                    self.cleanup_function):
                raise
        else:
            self.g_play.set_sensitive(True)
            self.g_show.set_sensitive(True)
        self._update()
    def create_pixmap_button(self):
        im = gtk.Image()
        im.set_from_stock("solfege-rhythm-c4", gtk.ICON_SIZE_LARGE_TOOLBAR)
        im.show()
        btn = gtk.Button()
        btn.add(im)
        return btn
    def on_start_practise(self):
        self.update_gui_after_lessonfile_change()
        self.g_play.grab_focus()

