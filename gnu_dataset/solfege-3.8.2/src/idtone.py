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

import inputwidgets, abstract, const, random, soundcard, gu, mpd
import gobject
import runtime
import gtk
import statistics, statisticsviewer
import lessonfile

class Teacher(abstract.Teacher):
    #FIXME the following lines
    OK, ERR_PICKY, ERR_TONES = range(3)
    ERR_PICKY = 1
    ERR_CONFIG = 2
    OCTAVES = [-2, -1, 0, 1, 2, 3]
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.HeaderLessonfile
        self.m_statistics = statistics.LessonStatistics(self)
        self.m_ask_tones =   {}
        self.m_question = None
        self.m_custom_mode = False
    def new_question(self):
        """
        Return values:
        OK: sucess, new random tone selected
        ERR_PICKY: fail, you are not allowed to select a new tone before you
                   can identify the one you have now.
        ERR_CONFIG: fail, all notes have zero weight or no octaves selected
        """
        if self.m_timeout_handle:
            gobject.source_remove(self.m_timeout_handle)
            self.m_timeout_handle = None

        if self.get_bool('config/picky_on_new_question') \
                and self.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG):
            return Teacher.ERR_PICKY

        self.m_is_first_question = self.q_status == const.QSTATUS_NO

        v = []
        for n in ['c', 'cis', 'd', 'dis', 'e', 'f', 'fis',
                  'g', 'gis', 'a', 'ais', 'b']:
            v.extend([n] * self.get_int(n+"_weight"))
        if not v:
            return self.ERR_CONFIG
        self.m_question = random.choice(v)
        v = []
        for n in self.OCTAVES:
            if self.get_bool("octave"+str(n)):
                v.append(n)
        if not v:
            return self.ERR_CONFIG
        self.m_octave = random.choice(v)
        self.q_status = const.QSTATUS_NEW
        return self.OK
    def guess_answer(self, notename):
        if notename == self.m_question:
            if self.q_status == const.QSTATUS_NEW \
                    and not self.m_custom_mode:
                self.m_statistics.add_correct(notename)
            self.maybe_auto_new_question()
            self.q_status = const.QSTATUS_SOLVED
            return 1
        else:
            if self.q_status == const.QSTATUS_NEW:
                if not self.m_custom_mode:
                    self.m_statistics.add_wrong(self.m_question, notename)
                self.q_status = const.QSTATUS_WRONG
    def play_question(self):
        if self.q_status == const.QSTATUS_NO:
            return
        soundcard.play_note(self.get_int('config/preferred_instrument'), 4,
             mpd.notename_to_int(self.m_question)+self.m_octave*12,
             self.get_int('config/preferred_instrument_velocity'))
    def give_up(self):
        if not self.m_custom_mode:
            self.m_statistics.reset_session()
        self.q_status = const.QSTATUS_GIVE_UP
    def spank_me_play_question(self):
        t1 = soundcard.PercussionTrack()
        t1.note(8, 71, self.get_int('config/preferred_instrument_velocity'))
        t2 = soundcard.Track()
        t2.notelen_time(4)
        t2.set_patch(self.get_int('config/preferred_instrument'))
        t2.note(4, mpd.notename_to_int(self.m_question)+self.m_octave*12,
                self.get_int('config/preferred_instrument_velocity'))
        soundcard.synth.play_track(t1, t2)
    def spank_me(self):
        soundcard.play_perc(4, 71, self.get_int('config/preferred_instrument_velocity'))

class Gui(abstract.Gui):
    def __init__(self, teacher, window):
        abstract.Gui.__init__(self, teacher, window)
        if runtime.use_cairo_widgets:
            self.m_usable = True
        else:
            self.m_usable = False
        if not self.m_usable:
            label = gtk.Label(_("This exercise is disabled because some required features are missing from your computer. You should install PyGTK 2.8.0 or newer."))
            label.set_line_wrap(True)
            self.practise_box.add(label)
            self.practise_box.show_all()
            return
        v = []
        for k in ('c', 'cis', 'd', 'dis', 'e', 'f', 'fis',
                  'g', 'gis', 'a', 'ais', 'b'):
            self.m_key_bindings['tone_%s_ak' % k] \
                = lambda self=self, k=k: self.on_answer_from_user(k)
            v.append(self.get_string('tone_%s_ak' % k))

        self.g_percentage = gu.bLabel(self.practise_box, "")
        self.g_percentage.set_name("Heading1")
        self.g_piano = inputwidgets.PianoOctaveWithAccelName(
                       self.on_answer_from_user, v)
        self.practise_box.pack_start(self.g_piano)

        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False)
        self.practise_box.set_spacing(gu.PAD)

        self.g_new_tone = gu.bButton(self.action_area, _("_New tone"),
                                     self.new_question)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"),
                  lambda _o, self=self: self.m_t.play_question())
        self.g_repeat.set_sensitive(False)
        self.g_give_up = gu.bButton(self.action_area, _("_Give up"), self.give_up)
        self.g_give_up.set_sensitive(False)
        self.practise_box.show_all()
        ##############
        # config_box #
        ##############
        self.config_box.set_spacing(gu.PAD_SMALL)
        # bad variable name, but this is the config options that should
        # not change without chaning lesson_id
        self.g_config_elems = gu.bVBox(self.config_box)
        table = gtk.Table()
        table.set_border_width(gu.PAD_SMALL)
        frame = gtk.Frame(_("Weight"))
        self.g_config_elems.pack_start(frame, False)
        frame.add(table)
        for x, n in [(1, 'cis'), (3, 'dis'), (7, 'fis'),
                     (9, 'gis'), (11, 'ais')]:
            label = gtk.Label(mpd.MusicalPitch.new_from_notename(n).get_user_notename())
            label.set_name("Heading2")
            label.set_alignment(0.2, 1.0)
            table.attach(label, x, x+2, 0, 1, xoptions=gtk.FILL)
            b = gu.nSpinButton(self.m_exname, n+"_weight",
                      gtk.Adjustment(1, 0, 1000, 1, 10), digits=0)
            table.attach(b, x, x+2, 1, 2, xoptions=gtk.FILL)
        for x, n in [(0, 'c'), (2, 'd'), (4, 'e'), (6, 'f'),
                      (8, 'g'), (10, 'a'), (12, 'b')]:
            label = gtk.Label(mpd.MusicalPitch.new_from_notename(n).get_user_notename())
            label.set_name("Heading2")
            label.set_alignment(0.35, 1.0)
            table.attach(label, x, x+2, 2, 3, xoptions=gtk.FILL)
            b = gu.nSpinButton(self.m_exname, n+"_weight",
                   gtk.Adjustment(1, 0, 1000, 1, 10), digits=0)
            table.attach(b, x, x+2, 3, 4, xoptions=gtk.FILL)

        hbox = gu.bHBox(self.g_config_elems, False)
        hbox.pack_start(gtk.Label(_("Octave:")), False, padding=4)
        for oct in self.m_t.OCTAVES:
            b = gu.nCheckButton(self.m_exname, "octave"+str(oct), str(oct),
                                default_value=1)
            hbox.pack_start(b, False)
        #############
        self._add_auto_new_question_gui(self.config_box)
        #############
        frame = gtk.Frame(_("When you guess wrong"))
        vbox = gtk.VBox()
        vbox.set_border_width(gu.PAD_SMALL)
        frame.add(vbox)
        vbox.pack_start(gu.nCheckButton(self.m_exname,
                    "warning_sound", _("Play warning sound")))
        self.config_box.pack_start(frame, False)
        self.config_box.show_all()
        ##############
        # statistics #
        ##############
        self.setup_statisticsviewer(statisticsviewer.StatisticsViewer,
                                   _("Identify tone"))
    def new_question(self, widget=None):
        s = self.m_t.q_status
        g = self.m_t.new_question()
        if g == Teacher.ERR_CONFIG:
            self.g_win.display_error_message(
_("""You have to select some tones practise. Do this on the config page by setting the weight of tones to a value greater than zero."""))
            return
        elif g == Teacher.OK:
            self.g_new_tone.set_sensitive(
                  not self.get_bool('config/picky_on_new_question'))
            self.g_repeat.set_sensitive(True)
            if self.m_t.m_is_first_question:
                self.flash_and_play_first_tone()
            self.g_flashbar.clear()
            self.m_t.play_question()
        self.set_percentage_label()
    def flash_and_play_first_tone(self):
        self.g_flashbar.flash(_("First tone is %s") % self.m_t.m_question)
        self.m_t.play_question()
    def on_answer_from_user(self, notename):
        if self.m_t.q_status == const.QSTATUS_NO:
            self.g_flashbar.flash(_("Click 'New tone' to begin."))
            return
        elif self.m_t.q_status == const.QSTATUS_SOLVED:
            if self.m_t.guess_answer(notename):
                self.g_flashbar.flash(_("Correct, but you have already solved this question"))
            else:
                self.g_flashbar.flash(_("Wrong, but you have already solved this question"))
        elif self.m_t.q_status in (const.QSTATUS_NEW, const.QSTATUS_WRONG):
            if self.m_t.guess_answer(notename):
                self.g_flashbar.flash(_("Correct"))
                self.g_new_tone.set_sensitive(True)
                self.g_give_up.set_sensitive(False)
            else:
                if self.m_t.m_is_first_question:
                    self.flash_and_play_first_tone()
                    return
                self.g_flashbar.flash(_("Wrong"))
                self.g_give_up.set_sensitive(True)
                if self.get_bool("warning_sound"):
                    if self.get_bool("config/auto_repeat_question_if_wrong_answer"):
                        self.m_t.spank_me_play_question()
                    else:
                        self.m_t.spank_me()
                else:
                    if self.get_bool("config/auto_repeat_question_if_wrong_answer"):
                        self.m_t.play_question()
        self.set_percentage_label()
    def give_up(self, _o=None):
        if self.m_t.q_status == const.QSTATUS_WRONG:
            self.g_flashbar.push(_("The answer is: %s")
                % self.m_t.m_question)
            self.m_t.give_up()
            self.g_new_tone.set_sensitive(True)
            self.g_new_tone.grab_focus()
            self.g_give_up.set_sensitive(False)
    def set_percentage_label(self):
        self.g_percentage.set_text("%.1f %%" % (self.m_t.m_statistics.get()*100))
    def on_start_practise(self):
        if not self.m_usable:
            return
        self.m_t.m_custom_mode = not (
                ('white_keys_weight' in self.m_t.m_P.header)
                 or ('black_keys_weight' in self.m_t.m_P.header))
        self.g_flashbar.require_size([
            _("Click 'New tone' to begin."),
            _("Correct, but you have already solved this question"),
            _("Wrong, but you have already solved this question"),
        ])
        if 'white_keys_weight' in self.m_t.m_P.header:
            if type(self.m_t.m_P.header.white_keys_weight) == list \
                    and len(self.m_t.m_P.header.white_keys_weight) == 7:
                for idx, n in enumerate(('c', 'd', 'e', 'f', 'g', 'a', 'b')):
                    self.set_float('%s_weight' % n, self.m_t.m_P.header.white_keys_weight[idx])
            else:
                print "white_keys_weight had wrong type"
        if 'black_keys_weight' in self.m_t.m_P.header:
            if type(self.m_t.m_P.header.black_keys_weight) == list \
                    and len(self.m_t.m_P.header.black_keys_weight) == 5:
                for idx, n in enumerate(('cis', 'dis', 'fis', 'gis', 'ais')):
                    self.set_float('%s_weight' % n, self.m_t.m_P.header.black_keys_weight[idx])
            else:
                print "black_keys_weight had wrong type"
        if self.m_t.m_custom_mode:
            self.g_config_elems.show()
        else:
            self.g_config_elems.hide()
            self.m_t.m_statistics.reset_session()
        self.g_statview.g_heading.set_text("%s - %s" % (_("Identify tone"), self.m_t.m_P.header.title))
        self.set_percentage_label()
        gobject.timeout_add(const.SHORT_WAIT, lambda self=self:
            self.g_flashbar.flash(_("Click 'New tone' to begin.")))
        self.g_new_tone.set_sensitive(True)
        self.g_new_tone.grab_focus()
        self.g_repeat.set_sensitive(False)
        self.g_give_up.set_sensitive(False)
        self.m_t.q_status = const.QSTATUS_NO
    def on_end_practise(self):
        self.m_t.end_practise()
        if self.m_usable:
            self.g_repeat.set_sensitive(False)
