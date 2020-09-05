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
import gu
import inputwidgets
from specialwidgets import RandomTransposeDialog
from exceptiondialog import ExceptionDialog
import soundcard, mpd

import const, utils
import random
import locale
import sys
import traceback
import cfg
import lessonfile
import osutils


class Teacher(cfg.ConfigUtils):
    def __init__(self, exname, app):
        cfg.ConfigUtils.__init__(self, exname)
        self.m_app = app
        self.q_status = const.QSTATUS_NO
        self.m_statistics = None
        self.m_timeout_handle = None
        self.m_P = None
        # The file name, not a file object
        self.m_lessonfile = None
        self.m_question = None
    def get_instrument_config(self, num):
        if self.get_bool('config/override_default_instrument'):
            instr_low = self.get_int('config/lowest_instrument')
            instr_low_vel = self.get_int('config/lowest_instrument_velocity')
            instr_middle = self.get_int('config/middle_instrument')
            instr_middle_vel = self.get_int('config/middle_instrument_velocity')
            instr_high = self.get_int('config/highest_instrument')
            instr_high_vel = self.get_int('config/highest_instrument_velocity')
        else:
            instr_low = instr_middle = instr_high \
                        = self.get_int('config/preferred_instrument')
            instr_low_vel = instr_middle_vel = instr_high_vel \
                         = self.get_int('config/preferred_instrument_velocity')
        if num == 2:
            return instr_low, instr_low_vel, instr_high, instr_high_vel
        else:
            return instr_low, instr_low_vel, \
                instr_middle, instr_middle_vel, instr_high, instr_high_vel
    def maybe_auto_new_question(self):
        if self.get_bool('new_question_automatically'):
            if self.m_timeout_handle is None:
                def remove_timeout(self=self):
                    self.m_timeout_handle = None
                    self.g_view.new_question()
                self.m_timeout_handle = gobject.timeout_add(int(self.get_float('seconds_before_new_question')*1000),  remove_timeout)
    def end_practise(self):
        if self.m_timeout_handle:
            gobject.source_remove(self.m_timeout_handle)
            self.m_timeout_handle = None
        self.q_status = const.QSTATUS_NO
        soundcard.synth.stop()
        if self.m_statistics:
            self.m_statistics.save_data()
    def exit_test_mode(self):
        """
        Shared between harmonic and melodic interval.
        """
        self.m_statistics.exit_test_mode()
    def set_lessonfile(self, lessonfile):
        """
        Set the variable 'm_lessonfile' and
        parse the lesson file and save the statistics.
        """
        self.m_lessonfile = lessonfile
        self.parse_lessonfile()
        if self.m_P and self.m_statistics:
            self.m_statistics.lessonfile_changed(self.m_lessonfile)
    def parse_lessonfile(self):
        self.m_question = None
        self.q_status = const.QSTATUS_NO
        if not self.m_lessonfile:
            self.m_P = None
            return
        self.m_P = None
        try:
            self.m_P = self.lessonfileclass(self.m_lessonfile)
        # We don't have to check for LessonfileExceptions here because
        # the global exception hook will catch them.
        except IOError:
            self.m_app.m_ui.display_error_message(
"""There was an IOError while trying to parse a lesson file. The cause was probably you deleting or renaming a lesson file. Please don't do this while Solfege is running. The program is still a little fragile. 

You really should restart the program now.""")
            return
        if filter(lambda q: 'music' in q and q['music'].m_musictype == 'cmdline',
                  self.m_P.m_questions):
            run = gu.dialog_yesno(_("The lessonfile contain potentially dangerous code because it run external programs. Run anyway?"))
            if not run:
                self.m_P = None
    def check_askfor(self):
        if self.m_custom_mode:
            self.set_list('ask_for_names', range(len(self.m_P.get_unique_cnames())))
    def play_tonic(self):
        """
        Play the tonic of the question, if defined.
        """
        if 'tonic' in self.m_P.get_question():
            self.m_P.play_question(None, 'tonic')

class MelodicIntervalTeacher(Teacher):
    """
    Base class for interval exercises where
    you can have more than one interval.
    When this class was created it was used by melodic-intevall
    and sing-interval.
    """
    OK = 0
    ERR_PICKY = 1
    ERR_CONFIGURE = 2
    def __init__(self, exname, app):
        Teacher.__init__(self, exname, app)
        self.m_tonika = None
        self.m_question = []
    def new_question(self, L, H):
        assert isinstance(L, basestring)
        assert isinstance(H, basestring)
        if self.get_list('ask_for_intervals_0') == []:
            return self.ERR_CONFIGURE
        L, H = utils.adjust_low_high_to_irange(L, H,
                     self.get_list('ask_for_intervals_0'))

        if self.m_timeout_handle:
            gobject.source_remove(self.m_timeout_handle)
            self.m_timeout_handle = None

        if self.m_app.m_test_mode:
            self.m_P.next_test_question()
            self.m_question = [self.m_P.m_test_questions[self.m_P.m_test_idx]]
            #FIXME use tone pitch range from preferences window.
            self.m_tonika = mpd.MusicalPitch()
            self.m_tonika.randomize("f", "f'")
            self.q_status = const.QSTATUS_NEW
            return self.OK

        if self.get_bool('config/picky_on_new_question') \
              and self.q_status in [const.QSTATUS_NEW, const.QSTATUS_WRONG]:
            return self.ERR_PICKY

        self.q_status = const.QSTATUS_NO
        last_tonika = self.m_tonika
        last_question = self.m_question
        for x in range(10):# we try max 10 times to get a question that
                           # is different from the last one.
            self.m_tonika, i = utils.random_tonika_and_interval(L, H,
                            self.get_list('ask_for_intervals_0'))
            self.m_question = [i]
            t = self.m_tonika + i
            for x in range(1, self.get_int('number_of_intervals=1')):
                if not self.get_list('ask_for_intervals_%i' % x):
                    return self.ERR_CONFIGURE
                i = utils.random_interval(t, L, H,
                               self.get_list('ask_for_intervals_%i' % x))
                if not i:
                    # if we can't find an interval that is with the range
                    # we, find the interval that is closest to the range
                    # of notes the user want. This mean that the questions
                    # are not necessarily that random.
                    low = mpd.MusicalPitch.new_from_int(L)
                    high = mpd.MusicalPitch.new_from_int(H)
                    off = 1000
                    best = None
                    for interval in self.get_list('ask_for_intervals_%i'%x):
                        if t + interval > high:
                            if t + interval - high < off:
                                off = t + interval - high
                                best = interval
                        if t + interval < low:
                            if low - (t + interval) < off:
                                off = low - (t + interval)
                                best = interval
                    i = best
                self.m_question.append(i)
                t = t + i
            if last_tonika is not None \
                    and last_tonika == self.m_tonika \
                    and last_question == self.m_question:
                continue
            break
        self.q_status = const.QSTATUS_NEW
        return self.OK
    def play_question(self):
        if self.q_status == const.QSTATUS_NO:
            return
        t = self.m_tonika
        m = soundcard.Track()
        m.set_bpm(self.get_int('config/default_bpm'))
        m.set_patch(self.get_int('config/preferred_instrument'))
        m.note(4, self.m_tonika.semitone_pitch(),
               self.get_int('config/preferred_instrument_velocity'))
        for i in self.m_question:
            t = t + i
            m.note(4, t.semitone_pitch(),
                   self.get_int('config/preferred_instrument_velocity'))
        soundcard.synth.play_track(m)


class RhythmAddOnClass:
    RHYTHMS = ("c4", "c8 c8", "c16 c16 c16 c16", "c8 c16 c16",
               "c16 c16 c8", "c16 c8 c16", "c8. c16", "c16 c8.",
               "r4", "r8c8", "r8 c16 c16", "r16 c16 c8", "r16c8c16",
               "r16 c16 c16 c16", "r8 r16 c16", "r16 c8.",
               "c12 c12 c12", "r12 c12 c12",
               "c12 r12 c12", "c12 c12 r12", "r12 r12 c12", "r12 c12 r12",
               "c4.", "c4 c8", # 22, 23
               "c8 c4", "c8 c8 c8", # 24, 25
               "c4 c16 c16", # 26
               "c16 c16 c4", # 27
               "c8 c8 c16 c16", #28
               "c8 c16 c16 c8", #29
               "c16 c16 c8 c8", #30
               "c8 c16 c16 c16 c16", #31
               "c16 c16 c8 c16 c16", #32
               "c16 c16 c16 c16 c8", #33
               "c16 c16 c16 c16 c16 c16", #34
               )
    def new_question(self):
        """returns:
               self.ERR_PICKY : if the question is not yet solved and the
                                   teacher is picky (== you have to solve the
                                   question before a new is asked).
               self.OK : if a new question was created.
               self.ERR_NO_ELEMS : if no elements are set to be practised.
        """
        if self.m_timeout_handle:
            gobject.source_remove(self.m_timeout_handle)
            self.m_timeout_handle = None

        if self.get_bool('config/picky_on_new_question') \
                 and self.q_status in [const.QSTATUS_NEW, const.QSTATUS_WRONG]:
            return self.ERR_PICKY

        self.q_status = const.QSTATUS_NO

        norest_v = []
        v = []
        for x in self.m_P.header.rhythm_elements:
            if not (self.RHYTHMS[x][0] == "r"
                    and self.get_bool("not_start_with_rest")):
                norest_v.append(x)
            v.append(x)
        if not v:
            return self.ERR_NO_ELEMS
        if not norest_v:
            return self.ERR_NO_ELEMS
        self.m_question = [random.choice(norest_v)]
        for x in range(1, self.get_int("num_beats")):
            self.m_question.append(random.choice(v))
        self.q_status = const.QSTATUS_NEW
        return self.OK
    def get_music_notenames(self, count_in):
        """
        Return a string with the notenames of the current question.
        Include count in if count_in == True
        """
        s = ""
        if count_in:
            if self.m_P.header.count_in_notelen:
                count_in_notelen = self.m_P.header.count_in_notelen
            else:
                count_in_notelen = "4"
            s = "d%s " % count_in_notelen * self.get_int("count_in")
        s += " ".join([self.RHYTHMS[k] for k in self.m_question])
        return s
    def get_music_string(self):
        """
        Return a complete mpd string of the current question that can
        be feed to mpd.play_music.
        """
        return r"\staff{%s}" % self.get_music_notenames(True)
    def play_rhythm(self, rhythm):
        """
        rhythm is a string. Example: 'c4 c8 c8 c4'
        """
        score = mpd.parser.parse_to_score_object(rhythm)
        track = score.get_midi_events_as_percussion(cfg.get_int('config/preferred_instrument_velocity'))[0]
        track.prepend_bpm(self.m_P.header.bpm)
        track.replace_note(mpd.notename_to_int("c"),
                           self.get_int("config/rhythm_perc"))
        track.replace_note(mpd.notename_to_int("d"),
                           self.get_int("config/countin_perc"))
        soundcard.synth.play_track(track)
    def set_elements_variables(self):
        """
        This is called from the on_start_practise() method of exercise
        modules that generate rhythms and use these variables to select
        rhythm elements.
        """
        if self.m_custom_mode:
            if not self.m_P.header.rhythm_elements:
                self.m_P.header.rhythm_elements = self.m_P.header.configurable_rhythm_elements[:3]
            self.m_P.header.visible_rhythm_elements = self.m_P.header.rhythm_elements[:]
        else:
            if not self.m_P.header.visible_rhythm_elements:
                self.m_P.header.visible_rhythm_elements = \
                    self.m_P.header.rhythm_elements[:]
                self.m_P.header.rhythm_elements = \
                  [n for n in self.m_P.header.rhythm_elements if n != 'newline']
    def set_default_header_values(self):
        for n, default in (('bpm', 60),
                  ('count_in', 2),
                  ('num_beats', 4)):
            if n in self.m_P.header:
                self.set_int(n, self.m_P.header[n])
            else:
                self.set_int(n, default)

class Gui(gtk.VBox, cfg.ConfigUtils):
    """Important members:
         - practise_box
         - action_area
         - config_box
    
    """
    def __init__(self, teacher, window, no_notebook=0):
        gtk.VBox.__init__(self)
        cfg.ConfigUtils.__init__(self, teacher.m_exname)
        self.m_key_bindings = {}
        self.m_t = teacher
        self.g_win = window

        vbox = gtk.VBox()
        vbox.set_spacing(gu.PAD)
        vbox.set_border_width(gu.PAD)
        vbox.show()

        self.practise_box = gtk.VBox()
        self.practise_box.show()
        vbox.pack_start(self.practise_box, False)
        self.g_lesson_heading = gtk.Label()
        self.practise_box.pack_start(self.g_lesson_heading, padding=18)

        self.action_area = gtk.HBox()
        self.action_area.show()
        vbox.pack_start(self.action_area, False)

        self.config_box = gtk.VBox()
        self.config_box.set_border_width(gu.PAD)
        self.config_box.show()
        if no_notebook:
            self.pack_start(vbox)
            self.pack_start(self.config_box, False)
            self.g_notebook = None
        else:
            self.g_notebook = gtk.Notebook()
            self.pack_start(self.g_notebook)

            self.g_notebook.append_page(vbox, gtk.Label(_("Practise")))
            self.g_notebook.append_page(self.config_box, gtk.Label(_("Config")))
            self.g_notebook.show()
        self.g_cancel_test = gtk.Button(_("_Cancel test"))
        self.g_cancel_test.connect('clicked', self.on_cancel_test)
        self.action_area.pack_end(self.g_cancel_test, False)
    def on_cancel_test(self, *w):
        self.g_cancel_test.hide()
        self.on_end_practise()
        self.g_win.exit_test_mode()
    def do_test_complete(self):
        self.on_end_practise()
        req = self.m_t.m_P.get_test_requirement()
        res = self.m_t.m_statistics.get()
        if res >= req:
            gu.dialog_ok(_("Test completed!\nYour score was %(score).1f%%.\nThe test requirement was %(requirement).1f%%.") % {'score': res * 100, 'requirement': req * 100})
            self.m_t.m_statistics.store_test_passed()
        else:
            gu.dialog_ok(_("Test failed.\nYour score was %(score).1f%%.\nThe test requirement was %(requirement).1f%%.") % {'score': res * 100, 'requirement': req * 100})
        self.g_cancel_test.hide()
        self.g_win.exit_test_mode()
    def set_lesson_heading(self, txt):
        if txt:
            self.g_lesson_heading.set_text('<span size="large"><b>%s</b></span>' % txt)
            self.g_lesson_heading.set_use_markup(True)
            self.g_lesson_heading.show()
        else:
            self.g_lesson_heading.hide()
    def on_start_practise(self):
        """Dummy function that child classes can override.
        """
        print "dummy abstract.Gui.on_start_practise", self.m_exname
    def on_end_practise(self):
        print "dummy abstract.Gui.on_end_practise", self.m_exname
    def handle_config_box_visibility(self):
        """
        Show self.config_box if it has any visible children, otherwise
        hide it.
        """
        if filter(lambda c: c.get_property('visible'),
                  self.config_box.get_children()):
            self.config_box.show()
        else:
            self.config_box.hide()
    def on_key_press_event(self, widget, event):
        if (self.g_notebook is None or self.g_notebook.get_current_page() == 0) \
           and event.type == gtk.gdk.KEY_PRESS:
            for s in self.m_key_bindings:
                if self.keymatch(event, s):
                    self.m_key_bindings[s]()
                    return 1
    def keymatch(self, event, cfname):
        a, b = utils.parse_key_string(self.get_string(cfname))
        return ((event.state & (gtk.gdk.CONTROL_MASK|gtk.gdk.SHIFT_MASK|gtk.gdk.MOD1_MASK)) == a) and (event.keyval == b)
    def setup_statisticsviewer(self, viewclass, heading):
        self.g_statview = viewclass(self.m_t.m_statistics, heading)
        self.g_statview.show()
        self.g_notebook.append_page(self.g_statview, gtk.Label(_("Statistics")))
        self.g_notebook.connect('switch_page', self.on_switch_page)
    def on_switch_page(self, notebook, obj, pagenum):
        if pagenum == 2:
            if isinstance(self.m_t, Teacher):
                if self.m_t.m_P:
                    self.g_statview.update()
                else:
                    self.g_statview.clear()
            else:
                self.g_statview.update()
    def _add_auto_new_question_gui(self, box):
        hbox = gu.bHBox(box, False)
        hbox.set_spacing(gu.PAD_SMALL)
        adj = gtk.Adjustment(0, 0, 10, 0.1, 1, 1)
        spin = gu.nSpinButton(self.m_exname, 'seconds_before_new_question',
                       adj)
        spin.set_digits(1)
        label = gtk.Label(_("Delay (seconds):"))
        label.show()
        def f(button, spin=spin, label=label):
            spin.set_sensitive(button.get_active())
            label.set_sensitive(button.get_active())
        b = gu.nCheckButton(self.m_exname, 'new_question_automatically',
                            _("_New question automatically."), callback=f)
        hbox.pack_start(b, False)
        label.set_sensitive(b.get_active())
        hbox.pack_start(label, False)
        spin.set_sensitive(b.get_active())
        hbox.pack_start(spin, False)
    def _lessonfile_exception(self, exception, sourcefile, lineno):
        if self.m_t.m_P._idx is None:
            msg = ""
        else:
            msg = _('Please check question number %(idx)i in the lesson file "%(lf)s".\n\n') % {'idx': self.m_t.m_P._idx+1, 'lf': self.m_t.m_P.m_filename}
        msg += _('The exception was caught in\n"%(filename)s", line %(lineno)i.') % {'filename': sourcefile, 'lineno': lineno}
        m = ExceptionDialog(exception)
        m.add_text(msg)
        if 'm_nonwrapped_text' in dir(exception):
            m.add_nonwrapped_text(exception.m_nonwrapped_text)
        m.run()
        m.destroy()
    def _mpd_exception(self, exception, sourcefile, lineno):
        m = ExceptionDialog(exception)
        if 'm_mpd_varname' in dir(exception):
            m.add_text(_('Failed to parse the music in the variable "%(varname)s" in question number %(idx)i in the lesson file "%(lf)s".') % {
                'idx': self.m_t.m_P._idx+1, 
                'lf': self.m_t.m_P.m_filename, 
                'varname': exception.m_mpd_varname})
        else:
            m.add_text(_('Failed to parse the music for question number %(idx)i in the lesson file "%(lf)s".') % {'idx': self.m_t.m_P._idx+1, 'lf': self.m_t.m_P.m_filename})
        if 'm_mpd_badcode' in dir(exception):
            m.add_nonwrapped_text(exception.m_mpd_badcode)
        m.add_text(_('The exception was caught in\n"%(filename)s", line %(lineno)i.') % {'filename': sourcefile, 'lineno': lineno})
        m.run()
        m.destroy()
    def run_exception_handled(self, method, *args, **kwargs):
        """
        Call method() and catch exceptions with standard_exception_handler.
        """
        try:
            return method(*args, **kwargs)
        except lessonfile.LessonfileException, e:
            t = traceback.extract_stack()
            self._lessonfile_exception(e, t[-2][0], t[-2][1])
        except mpd.MpdException, e:
            t = traceback.extract_stack()
            self._mpd_exception(e, t[-2][0], t[-2][1])
        except Exception, e:
            t = traceback.extract_stack()
            if not self.standard_exception_handler(e, __file__):
                raise
    def standard_exception_handler(self, e, sourcefile, cleanup_function=lambda: False):
        """
        Use this method to try to catch a few common solfege exceptions.
        It should only be used to catch exceptions after the file has
        successfully parsed by the lessonfile parser, and only used in
        exercises where the mpd code that might be wrong is comming from
        lesson files, not generated code.

        Usage:
        try:
            do something
        except Exception, e:
            if not self.standard_exception_handler(e, __file__):
                raise
        """
        sourcefile = sourcefile.decode(locale.getpreferredencoding(), 'replace')
        if self.m_t.m_app.m_options.disable_exception_handler:
            return False
        if isinstance(e, lessonfile.LessonfileException):
            cleanup_function()
            self._lessonfile_exception(e, sourcefile, sys.exc_info()[2].tb_lineno)
            return True
        elif isinstance(e, mpd.MpdException):
            cleanup_function()
            self._mpd_exception(e, sourcefile, sys.exc_info()[2].tb_lineno)
            return True
        elif isinstance(e, osutils.ExecutableDoesNotExist):
            cleanup_function()
            self.g_win.display_exception_message(e, sourcefile)
            return True
        return False

class RhythmAddOnGuiClass(object):
    def add_select_elements_gui(self):
        self.g_element_frame = frame = gtk.Frame(_("Rhythms to use in question"))
        self.config_box.pack_start(frame, False)
        self.g_select_rhythms_box = gu.NewLineBox()
        self.g_select_rhythms_box.set_border_width(gu.PAD_SMALL)
        frame.add(self.g_select_rhythms_box)
    def add_select_num_beats_gui(self):
        table = gtk.Table(2, 4, False)
        self.config_box.pack_start(table, False)
        ###
        label = gtk.Label(_("Number of beats in question:"))
        label.set_alignment(1.0, 0.5)
        table.attach(label, 0, 1, 0, 1, xpadding=gu.PAD_SMALL,
                      xoptions=gtk.FILL)
        table.attach(gu.nSpinButton(self.m_exname, "num_beats",
                     gtk.Adjustment(4, 1, 100, 1, 10)),
                     1, 2, 0, 1, xoptions=gtk.FILL)
        label = gtk.Label(_("Count in before question:"))
        label.set_alignment(1.0, 0.5)
        table.attach(label, 0, 1, 1, 2, xpadding=gu.PAD_SMALL,
                     xoptions=gtk.FILL)
        table.attach(gu.nSpinButton(self.m_exname, "count_in",
                     gtk.Adjustment(2, 0, 10, 1, 10)),
                     1, 2, 1, 2, xoptions=gtk.FILL)
        table.show_all()
    def pngcheckbutton(self, i):
        btn = gtk.CheckButton()
        btn.add(gu.create_rhythm_image(RhythmAddOnClass.RHYTHMS[i]))
        btn.show()
        btn.connect('clicked', self.select_element_cb, i)
        return btn
    def update_select_elements_buttons(self):
        """
        (Re)create the checkbuttons used to select which rhythm elements
        to be used when creating questions. We only need to do this if
        we are in m_custom_mode.
        """
        self.g_select_rhythms_box.empty()
        for n in self.m_t.m_P.header.configurable_rhythm_elements:
            if n == 'newline':
                self.g_select_rhythms_box.newline()
            else:
                b = self.pngcheckbutton(n)
                self.g_select_rhythms_box.add_widget(b)
                b.set_active(n in self.m_t.m_P.header.rhythm_elements)
        self.g_select_rhythms_box.show_widgets()
    def select_element_cb(self, button, element_num):
        def sortlike(orig, b):
            ret = []
            for n in orig:
                if n == 'newline':
                    ret.append('newline')
                elif n in b:
                    ret.append(n)
            return ret
        if button.get_active():
            if element_num not in self.m_t.m_P.header.rhythm_elements:
                self.m_t.m_P.header.rhythm_elements.append(element_num)
                self.m_t.m_P.header.rhythm_elements = sortlike(
                    self.m_t.m_P.header.configurable_rhythm_elements,
                    self.m_t.m_P.header.rhythm_elements)
        else:
            if element_num in self.m_t.m_P.header.rhythm_elements:
                self.m_t.m_P.header.rhythm_elements.remove(element_num)
        self.m_t.m_P.header.visible_rhythm_elements = \
            self.m_t.m_P.header.rhythm_elements[:]
        self.m_t.m_P.header.rhythm_elements = \
            [n for n in self.m_t.m_P.header.rhythm_elements if n != 'newline']

class IntervalGui(Gui):
    """
    Creates 'New interval' and 'Repeat' buttons in the action_area.
    """
    def __init__(self, teacher, window):
        Gui.__init__(self, teacher, window)

        self.g_input = None

        self.g_flashbar = gu.FlashBar()
        self.g_flashbar.show()
        self.practise_box.pack_start(self.g_flashbar, False)
        self.practise_box.set_spacing(gu.PAD)

        self.g_new_interval = gu.bButton(self.action_area, _("_New interval"),
                                          self.new_question)
        self.g_repeat = gu.bButton(self.action_area, _("_Repeat"), self.repeat_question)
        self.g_repeat.set_sensitive(False)
        self.setup_key_bindings()
    def _create_select_inputwidget_gui(self):
        """
        This will be called by HarmonicInterval and MelodicInterval
        constructor
        """
        hbox = gu.bHBox(self.config_box, False)
        hbox.set_spacing(gu.PAD_SMALL)
        gu.bLabel(hbox, _("Input interface:"), False)

        combo = gtk.combo_box_new_text()
        for i in range(len(inputwidgets.inputwidget_names)):
            combo.append_text(inputwidgets.inputwidget_names[i])
        if self.get_int('inputwidget') < len(inputwidgets.inputwidget_names):
            combo.set_active(self.get_int('inputwidget'))
        else:
            combo.set_active(0)
        combo.connect('changed', lambda w: self.use_inputwidget(w.get_active()))
        hbox.pack_start(combo, False)

        self.g_disable_unused_buttons = gu.nCheckButton(self.m_exname,
                    'disable_unused_intervals', _("_Disable unused buttons"))
        hbox.pack_start(self.g_disable_unused_buttons)
    def select_inputwidget(self):
        """
        This will be called by HarmonicInterval and MelodicInterval
        constructor
        """
        i = self.get_int('inputwidget')
        if i >= len(inputwidgets.inputwidget_names):
            i = 0
        self.use_inputwidget(i)
    def use_inputwidget(self, i):
        self.set_int('inputwidget', i)
        if self.g_input:
            self.g_input.destroy()
        # FIXME UGH ugly ugly ugly, I'm lazy lazy lazy
        import harmonicinterval
        if isinstance(self, harmonicinterval.Gui):
            v = ['intervals']
        else:
            v = []
            for x in range(self.get_int('maximum_number_of_intervals')):
                v.append('ask_for_intervals_%i' % x)
        if i == 0:
            assert inputwidgets.inputwidget_names[i] == _("Buttons")
            self.g_input = inputwidgets.IntervalButtonsWidget(self.m_exname,
                  'intervals', self.click_on_interval, self.get_interval_input_list, v)
        else:
            self.g_input = inputwidgets.name_to_inputwidget(
                                 inputwidgets.inputwidget_names[i],
                                 self.click_on_interval)
        self.practise_box.pack_start(self.g_input)
        self.practise_box.reorder_child(self.g_input, 1)
        self.g_input.show()
        if self.m_t.m_tonika:
            # Don't call on_end_practise if we are starting up the exercise.
            # This whole thing is a mess.
            self.on_end_practise()
        self.g_disable_unused_buttons.set_sensitive(self.get_int('inputwidget')==0)
    def setup_key_bindings(self):
        keys = ['minor2', 'major2', 'minor3', 'major3',
                'perfect4', 'diminished5', 'perfect5', 'minor6',
                'major6', 'minor7', 'major7', 'perfect8',
                'minor9', 'major9', 'minor10', 'major10']
        self.m_key_bindings = {}
        for idx in range(len(keys)):
            self.m_key_bindings['interval_input/'+keys[idx]] = lambda idx=idx, self=self: self.click_on_interval(1, idx+1, None)
    def repeat_question(self, *w):
        self.m_t.play_question()
        self.g_input.grab_focus_first_sensitive_button()


class LessonbasedGui(Gui):
    def __init__(self, teacher, window, no_notebook=0):
        Gui.__init__(self, teacher, window, no_notebook)
    def add_random_transpose_gui(self):
        self.g_random_transpose_box = hbox = gu.bHBox(self.config_box, False, False)
        label = gtk.Label(_("Random transpose:"))
        label.show()
        hbox.pack_start(label, False)
        hbox.set_spacing(6)
        self.g_random_transpose = gtk.Label()
        self.g_random_transpose.show()
        hbox.pack_start(self.g_random_transpose)

        button = gtk.Button(_("Change ..."))
        button.show()
        button.connect('clicked', self.run_random_transpose_dialog)
        hbox.pack_start(button)
    def run_random_transpose_dialog(self, widget):
        dlg = RandomTransposeDialog(self.m_t.m_P.header.random_transpose, self.g_win)
        response = dlg.run()
        if response == gtk.RESPONSE_OK:
            self.m_t.m_P.header.random_transpose = dlg.get_value()
            if self.m_t.m_P.header.random_transpose == True:
                self.g_random_transpose.set_text(_("Yes"))
            elif self.m_t.m_P.header.random_transpose == False:
                self.g_random_transpose.set_text(_("No"))
            else:
                self.g_random_transpose.set_text(str(self.m_t.m_P.header.random_transpose))
        dlg.destroy()
    def show_answer(self, widget=None):
        """
        Show the answer in the g_music_displayer if we have one, if not
        use a new window.
        """
        if not self.m_t.m_P.get_question()['music'].is_displayable():
            return
        if 'vmusic' in self.m_t.m_P.get_question():
            varname = 'vmusic'
        else:
            varname = 'music'
        self.display_music(varname)
    def display_music(self, varname):
        """
        Display the music in the variable named by varname from
        the currently selected question. This method will handle
        the normal mpd and lessonfile exceptions.
        """
        try:
            if self.m_t.m_P.header.have_music_displayer:
                fontsize = self.get_int('config/feta_font_size=20')
                self.g_music_displayer.display(self.m_t.m_P.get_music(varname), fontsize)
            else:
                self.g_win.display_in_musicviewer(self.m_t.m_P.get_music(varname))
        except mpd.MpdException, e:
            e.m_mpd_varname = varname
            e.m_mpd_badcode = self.m_t.m_P.get_question()[varname].m_musicdata
            if not self.standard_exception_handler(e, __file__):
                raise e
    def do_at_question_start_show_play(self):
        """
        This method is shared by idbyname and elembuilder, and possibly
        other exercises later. It will show and/or play music based on
        the header.at_question_start  variable.

        It might raise mpd.MpdExceptions.
        """
        if self.m_t.m_P.header.at_question_start == 'show':
            self.show_answer()
        elif self.m_t.m_P.header.at_question_start == 'play':
            self.m_t.m_P.play_question()
            if 'cuemusic' in self.m_t.m_P.get_question():
                self.display_music('cuemusic')
        else:
            self.m_t.m_P.play_question()
            if 'show' in self.m_t.m_P.header.at_question_start \
                and 'play' in self.m_t.m_P.header.at_question_start:
                self.show_answer()
            elif 'cuemusic' in self.m_t.m_P.get_question():
                self.display_music('cuemusic')
    def show_hide_at_question_start_buttons(self):
        """
        Show and hide g_play_music, g_repeat and g_display_music
        depending on the content of header.at_question_start.
        This method is used by at least idbyname and elembuilder.
        """
        if self.m_t.m_P.header.at_question_start == 'show':
            self.g_play_music.show()
            self.g_repeat.hide()
        else:
            self.g_play_music.hide()
            self.g_repeat.show()
        if self.m_t.m_P.header.at_question_start == 'play':
            self.g_display_music.show()
        else:
            self.g_display_music.hide()

