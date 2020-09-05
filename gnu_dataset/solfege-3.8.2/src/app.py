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

import configureoutput
import errno, sys
import locale
import soundcard, mpd
import lessonfile
import dataparser
import os
import cfg, utils
import webbrowser
import gu
from osutils import *
import reportlib
import const

def linux_play_wav_file(filename):
    if not cfg.get_string('sound/wav_player'):
        raise ExecutableDoesNotExist('')
    run_external_program(cfg.get_string('sound/wav_player'), "", filename)

def linux_play_midi_file(filename):
    run_external_program(cfg.get_string('sound/midi_player'), "", filename)

soundcard.play_wav_file = linux_play_wav_file
soundcard.play_midi_file = linux_play_midi_file

def check_rcfile():
    """See default.config for rcfileversion values, meanings and
    a description of how to add config variables.
    """
    rcfileversion = 12
    if cfg.get_int("app/rcfileversion") > rcfileversion:
        cfg.drop_user_config()
        return
    if cfg.get_int("app/rcfileversion") <= 1:
        if not "example-files" in cfg.get_string('config/lessoncollections'):
            cfg.set_string('config/lessoncollections', 
                "%s example-files" % cfg.get_string('config/lessoncollections'))
    if cfg.get_int("app/rcfileversion") <= 5:
        # This is more complicated that necessary to fix an old
        # error.
        if cfg.get_string("sound/commandline"):
            cfg.del_key("sound/commandline")
    if cfg.get_int("app/rcfileversion") <= 3:
        cfg.set_list("config/lessoncollections",
            cfg.get_string("config/lessoncollections").split())
    if cfg.get_int("app/rcfileversion") <= 4:
        cfg.del_key("config/web_browser")
    if sys.platform == 'win32':
        if cfg.get_string('sound/wav_player'):
            cfg.del_key('sound/wav_player')
    if cfg.get_int("app/rcfileversion") <= 5:
        cfg.set_string("mainwin/history_back_ak", "<alt>Left")
        cfg.set_string("mainwin/history_forward_ak", "<alt>Right")
        cfg.set_string("mainwin/history_reload_ak", "<ctrl>r")
    if cfg.get_int("app/rcfileversion") <= 6:
        cfg.set_list("config/lessoncollections", ['solfege', 'user'])
    if cfg.get_int("app/rcfileversion") <= 7:
        cfg.set_int("rhythm/countin_perc", 80)
    if cfg.get_int("app/rcfileversion") <= 8:
        cfg.del_key("singinterval/highest_tone")
        cfg.del_key("singinterval/lowest_tone")
        cfg.del_key("melodicinterval/highest_tone")
        cfg.del_key("melodicinterval/lowest_tone")
        cfg.del_key("harmonicinterval/highest_tone")
        cfg.del_key("harmonicinterval/lowest_tone")
    if cfg.get_int("app/rcfileversion") <= 9:
        cfg.del_section("mainwin")
    if cfg.get_int("app/rcfileversion") <= 10:
        cfg.del_section("lessoncollections")
        cfg.del_key("config/lessoncollections")
        for n in cfg.iterate_sections():
            cfg.del_key("%s/lessoncollection" % n)
            cfg.del_key("%s/lessonfile" % n)
    if cfg.get_int("app/rcfileversion") <= 11:
        for s in ('rhythm', 'rhythmtapping2'):
            cfg.del_key("%s/countin_perc" % s)
            cfg.del_key("%s/rhythm_perc" % s)
    cfg.set_int("app/rcfileversion", rcfileversion)

class SolfegeApp(cfg.ConfigUtils):
    def __init__(self, options, ui, lessonfile_manager):
        """
        options -- command line options parsed by optparse
        """
        cfg.ConfigUtils.__init__(self, 'solfege-app')
        # test_mode is when we are running a test from the Tests menu
        self.m_test_mode = False
        self.m_options = options
        self.m_ui = ui
        self.m_teachers = {}
        self.m_running_exercise = None
        self.lessonfile_manager = lessonfile_manager
        self.m_sound_init_exception = None
        self.setup_sound()
    def setup_sound(self):
        if sys.platform == 'win32' and \
                    cfg.get_string("sound/type") == "sequencer-device":
            # just in case c:\home\.solfegerc is wrong
            cfg.set_string("sound/type", "winsynth")
        if self.m_options.no_sound \
           or cfg.get_string("sound/type") == "fake-synth":
            soundcard.initialise_using_fake_synth(self.m_options.verbose_sound_init)
        elif cfg.get_string("sound/type") == "winsynth":
            try:
                soundcard.initialise_winsynth(cfg.get_int("sound/synth_number"),
                      verbose_init=self.m_options.verbose_sound_init)
            except:#FIXME should use except: ExceptionName
                cfg.set_int("sound/synth_number", 0)
                soundcard.initialise_winsynth(cfg.get_int("sound/synth_number"),
                      self.m_options.verbose_sound_init)
        elif cfg.get_string("sound/type") == "external-midiplayer":
            soundcard.initialise_external_midiplayer(
                    cfg.get_string("sound/midi_player"),
                    verbose_init=self.m_options.verbose_sound_init)
            soundcard.synth.error_report_cb = self.m_ui.display_error_message
        elif cfg.get_string("sound/type") == '':
            self.m_ui.display_error_message(
_("You should configure sound from the 'Sound' page of the preferences window."))
        elif cfg.get_string("sound/type") == "sequencer-device":
            if not configureoutput.HAVE_LINUX_AWE_VOICE_H and \
                    cfg.get_string("sound/card_info") == "awe":
                cfg.set_string("sound/card_info", "");
            try:
                soundcard.initialise_devicefile(
                             cfg.get_string("sound/device_file"),
                             cfg.get_int("sound/synth_number"),
                             cfg.get_string("sound/card_info"),
                             verbose_init=self.m_options.verbose_sound_init)
            except (soundcard.SoundInitException, OSError, ImportError), e:
                self.m_sound_init_exception = e
                soundcard.initialise_using_fake_synth(True)
    def display_sound_init_error_message(self, e):
        if isinstance(e, soundcard.SoundInitException):
            self.m_ui.display_error_message(
            """%s""" % str(e).decode(locale.getpreferredencoding(), 'replace'))
        elif isinstance(e, ImportError):
            estr = str(e).decode(locale.getpreferredencoding(), 'replace')
            self.m_ui.display_error_message("%(message)s\n%(exception)s\n\n%(todo)s" % {
              'exception': estr,
              'message': "<b>%s</b>" % _("Error loading python module:"),
              'todo': _("You should configure sound from the preferences window, and try to use an external midi player. Or try to recompile the program and check for error messages to see why the module is not built."),
              })
        elif e.errno == errno.EACCES:
            self.m_ui.display_error_message(
"""The sound init failed: %s
The errno EACCES indicates that you don't have write
permission to the device.""" % str(e).decode(locale.getpreferredencoding(), 'replace'))
        elif e.errno == errno.EBUSY:
            self.m_ui.display_error_message(
"""The sound init failed: %s
It seems like some other program is using the device. You
should try to quit that other program and restart Solfege."""
 % str(e).decode(locale.getpreferredencoding(), 'replace'))
        else:
            self.m_ui.display_error_message(
"""The sound init failed: 
    %s
You should configure sound from the 'Sound' page of the preferences window.

It is also possible that the OS sound setup is incorrect.
"""
 % str(e).decode(locale.getpreferredencoding(), 'replace'))
    def play_happy_sound(self):
        mpd.play_music(r"\staff\relative c'{c16 e g a}", 180, 8,
               cfg.get_int('config/preferred_instrument_velocity'))
    def play_sad_sound(self):
        mpd.play_music(r"\staff\relative c'{<c,,8 cis>", 80, 58,
               cfg.get_int('config/preferred_instrument_velocity'))
    def please_help_me(self):
        # If m_viewer == 'docviewer', then we see the welcome message, and
        # there is no help for that.
        if self.m_ui.m_viewer != 'docviewer':
            if self.m_teachers[self.m_running_exercise].m_P.header.help:
                self.handle_href('%s.html' % self.m_teachers[self.m_running_exercise].m_P.header.help)
            else:
                self.handle_href('%s.html' % self.m_ui.m_viewer)
    def show_exercise_theory(self):
        if self.m_teachers[self.m_running_exercise].m_P.header.theory:
            self.m_ui.display_docfile("%s.html" % self.m_teachers[self.m_running_exercise].m_P.header.theory, "")
    def _practise_lesson_id(self, lesson_id, urlobj=None):
        """
        return the module name.
        """
        module = self.lessonfile_manager.get(lesson_id, 'module')
        if self.m_running_exercise:
            self.m_ui.box_dict[self.m_running_exercise].on_end_practise()
        if module not in self.m_teachers:
            self.create_teacher(module)
        if module not in self.m_ui.box_dict:
            self.m_ui.initialise_exercise(self.m_teachers[module])
        self.m_teachers[module].set_lessonfile(
            self.lessonfile_manager.get(lesson_id, 'filename'))
        self.m_ui.activate_exercise(module, urlobj)
        self.m_running_exercise = module
        self.m_teachers[module].g_view = self.m_ui.box_dict[module]
        self.m_ui.show_help_on_current()
        return module
    def practise_lesson_id(self, lesson_id):
        try:
            module = self._practise_lesson_id(lesson_id)
        except (lessonfile.LessonfileParseException,
                dataparser.DataparserException), e:
            self.m_ui.display_exception_message(e, __file__)
            module = self.lessonfile_manager.get(lesson_id, 'module')
        if not self.m_teachers[module].m_P:
            for n in self.m_ui.box_dict[module].action_area.get_children():
                n.set_sensitive(False)
            return
        self.m_ui.box_dict[module].on_start_practise()
        w = self.m_ui.g_ui_manager.get_widget("/Menubar/HelpMenu/PerExerciseHelp/HelpTheory")
        if w:
            w.set_sensitive(bool(self.m_teachers[module].m_P.header.theory))
        return module
    def test_lesson_id(self, lesson_id):
        self.m_test_mode = True
        module = self.practise_lesson_id(lesson_id)
        self.m_ui.box_dict[module].on_start_practise()
        self.m_ui.enter_test_mode()
    def handle_href(self, href, display_docfile_set_adj=1):
        # All generatet docbook documents are in utf-8
        urlobj = utils.Url(href)
        if not urlobj.protocol:
            if urlobj.filename.endswith(".midi"):
                if os.path.exists(os.path.join(self.m_ui.box_dict['docviewer'].m_htmlwidget.m_document_wd, urlobj.filename)):
                    midifilename = urlobj.filename
                elif len(urlobj.filename.split('-')) > 1:
                    # Filename-nl.midi => Filename.midi
                    v = urlobj.filename.split('-')
                    w = v[-1].split('.')
                    midifilename = "-".join(v[:-1])+"."+w[1]
                else:
                    midifilename = urlobj.filename
                run_external_program(self.get_string("sound/midi_player"),
                        self.m_ui.box_dict['docviewer'].m_htmlwidget.m_document_wd, midifilename)
            else:
                self.m_ui.display_docfile(urlobj.filename, urlobj.anchor)
        elif urlobj.protocol == 'solfege':
            self.handle_solfege_href(urlobj)
        elif urlobj.protocol == 'http':
            webbrowser.open_new(href)
        elif urlobj.protocol == 'mailto':
            if self.get_string("config/mua"):
                try:
                    cmdline = self.get_string("config/mua") % urlobj.href
                except TypeError:
                    cmdline = "%s %s" % (self.get_string("config/mua"), urlobj.href)
                os.system(cmdline)
            else:
                gu.dialog_ok(_("You have not selected email program. You can do so in the preferences window."))
        else:
            print "unknown link type", urlobj.protocol
    def handle_solfege_href(self, urlobj):
        """
        This method handles three kins of urls:
        solfege:all-lessonfiles
            will display a page with links to all installed lesson files
        solfege:practise/lesson_id
            practise the exercise with the lesson id 'lesson_id'
        solfege:practise/path/to/lessonfile
            Practise a specific lessonfile. Relative path:
                solfege:practise/lesson-files/chord-min-major
            Absolute path:
                solfege:practise//home/user/src/solfege/lesson-files/filename
        """
        self.m_ui.hide_help_on_current()
        if urlobj.action == 'all-lessonfiles':
            self.m_ui.display_html(self.lessonfile_manager.m_htmldoc)
            return
        # the Url class does not know anything about lesson ids, but we
        # assume that lesson_ids don't include the '/' character.
        if '/' not in urlobj.lessonfile:
            self.practise_lesson_id(urlobj.lessonfile)
            return
        if urlobj.lessonfile:
            if not os.path.isfile(urlobj.lessonfile):
                self.m_ui.display_error_message(_("Lessonfile not found:\n%s") % urlobj.lessonfile)
                return
        p = lessonfile.LessonfileCommon(urlobj.lessonfile)#FIXME it would have been
        # better if we didn't have to parse the file twice.
        p.parse_file(urlobj.lessonfile)
        self._practise_lesson_id(p.header.lesson_id, urlobj)
        # This loop changes variables in the lesson file header if they
        # have set values in the url.
        for key, val in urlobj.config.iteritems():
            try:
                self.m_teachers[p.header.module].m_P.header[key] = eval(val)
            except SyntaxError:
                gu.dialog_ok(_("Bad url parameter in link: '%s'.\nTrying to continue anyway.") % ("%s=%s" % (key, val)))
        self.m_ui.box_dict[p.header.module].on_start_practise()
    def create_teacher(self, modulename):
        """
        Create the teacher in 'modulename' and add it to self.m_teachers.
        """
        exec("import %s" % modulename)
        self.m_teachers[modulename] = locals()[modulename].Teacher(modulename, self)
    def reset_exercise(self, w=None):
        """
        Call on_end_practise, and then on_start_practise in
        the currently active exercise, if we have a exercise.
        """
        if self.m_ui.m_viewer != 'docviewer':
            self.m_ui.box_dict[self.m_ui.m_viewer].on_end_practise()
            self.m_ui.box_dict[self.m_ui.m_viewer].on_start_practise()
    def quit_program(self):
        cfg.sync()
        if self.m_ui.m_viewer != 'docviewer':
            self.m_ui.box_dict[self.m_ui.m_viewer].on_end_practise()
        if soundcard.synth:
            soundcard.synth.close()
    def export_training_set(self, export_data, export_dir, output_format):
        """
        This function requires a program that can create WAV files
        from MIDI files and MP3 files from WAV.
        """
        def delay(n, tempo):
            """
            tempo is a dict of two integers
            """
            track = mpd.Track()
            track.set_bpm(*tempo)#self.get_int('config/default_bpm'))
            track.note(mpd.Rat(n, 4), 80, 0)
            soundcard.synth.play_track(track)
        track_idx = 0
        num = sum([x['count'] for x in export_data])
        # MainWin will set this to True if the user want to cancel
        # the export.
        self.m_abort_export = False
        report = reportlib.Report()
        report.append(reportlib.Heading(1, "Exported exercises"))
        table = reportlib.Table()
        report.append(table)
        for lesson_info in export_data:
            lesson_id = lesson_info['lesson_id']
            module = self.lessonfile_manager.get(lesson_id, 'module')
            if module not in self.m_teachers:
                self.create_teacher(module)
            p = self.m_teachers[module].lessonfileclass(
                self.lessonfile_manager.get(lesson_id, 'filename'))
            for c in range(lesson_info['count']):
                if module == 'idbyname':
                    p.select_random_question()
                    if p.header.lesson_heading:
                        s = p.header.lesson_heading
                    else:
                        s = p.header.title
                    table.append_row("%i" % track_idx,
                                     p.get_question()['name'],
                                     s)
                    soundcard.start_export(os.path.join(
                            export_dir, "track-%i.mid" % track_idx))
                    for n in range(lesson_info.get('repeat', 1)):
                        p.play_question()
                        if n != lesson_info.get('repeat', 1) - 1:
                            if 'delay' in lesson_info:
                                delay(lesson_info['delay'], p.get_tempo())
                    soundcard.end_export()
                elif module in ('melodicinterval', 'harmonicinterval'):
                    t = self.m_teachers[module]
                    t.set_lessonfile(self.lessonfile_manager.get(lesson_id, 'filename'))
                    t.start_practise()
                    t.new_question("c", "c''")
                    t.q_status = const.QSTATUS_SOLVED
                    try:
                        table.append_row("%i" % track_idx, "%s" % utils.int_to_intervalname(t.m_interval))
                    except AttributeError:
                        table.append_row("%i" % track_idx, "%s" % (" + ".join([utils.int_to_intervalname(q, False, True) for q in t.m_question])))
                    soundcard.start_export(os.path.join(
                            export_dir, "track-%i.mid" % track_idx))
                    for n in range(lesson_info.get('repeat', 1)):
                        t.play_question()
                        if n != lesson_info.get('repeat', 1) - 1:
                            if 'delay' in lesson_info:
                                delay(lesson_info['delay'],
                                    (self.get_int('config/default_bpm'), 4))
                    soundcard.end_export()
                else:
                    print "ignoring exercise with module='%s'" % module
#####
                def do_convert(from_format, to_format):
                    """
                    Return False if we think the convert failed.
                    """
                    filter_str = "app/%s_to_%s_cmd" % (from_format, to_format)
                    if from_format == 'midi':
                        from_ext = 'mid'
                    else:
                        from_ext = from_format
                    to_ext = to_format
                    if not cfg.get_string(filter_str):
                        self.m_ui.display_error_message2("Config variable not defined", "The missing or empty variable was '%s'" % filter_str)
                        return False
                    try:
                        s = cfg.get_string(filter_str) % {
                            'in': os.path.join(export_dir, "track-%i.%s" % (track_idx, from_ext)),
                            'out': os.path.join(export_dir, "track-%i.%s" % (track_idx, to_ext))}
                        os.system(s)
                        if os.path.exists(os.path.join(export_dir, "track-%i.%s" % (track_idx, to_ext))):
                            os.remove(os.path.join(export_dir, "track-%i.%s" % (track_idx, from_ext)))
                        else:
                            # This means that the program failed to generate
                            # the WAV file. We set output_format to 'midi'
                            # because we don't want to display this error for
                            # every single file.
                            output_format = 'midi'
                            self.m_ui.display_error_message2("External program must have failed", "The file in %(from)s format was not generated from the %(to)s file as expected. Please check your setup." % {'to':to_format.upper(), 'from': from_format.upper()})
                    except (TypeError, KeyError):
                        self.m_ui.display_error_message2("%(from)s to %(to)s config error", "There was a format string error. Will not generate WAV files. Please check the app/midi_to_wav_cmd config variable." % {'from': from_format, 'to': to_format})
                        output_format = 'midi'
                    return True
#####
                if output_format in ('mp3', 'wav', 'ogg'):
                    do_convert('midi', 'wav')
                if output_format in ('mp3', 'ogg'):
                    if not do_convert('wav', output_format):
                        output_format = 'wav'
                track_idx += 1
                yield 1.0 * track_idx / num
                if self.m_abort_export:
                    del self.m_abort_export
                    return
        reportlib.HtmlReport(report, os.path.join(export_dir, "toc.html"))
