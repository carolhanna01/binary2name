# -*- coding: iso-8859-1 -*-

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

copyright = "Copyright © 1999-2006 Tom Cato Amundsen <tca@gnu.org>, and others."

import time
import sys
import os
import time
import urllib

# The sets module was added in python 2.3. So we will use that module
# if we are running python 2.3, and the builtin set if we are using
# python 2.4
# FIXME: remove this code when we require python 2.4
try:
    set()
except NameError:
    import __builtin__
    import sets
    __builtin__.set = sets.Set

import runtime

import configureoutput
import src
import osutils

import optparse

class MyOptionParser(optparse.OptionParser):
    def print_help(self, file=None):
        if file is None:
            file = sys.stdout
        encoding = file.encoding
        if not encoding:
            encoding = "iso-8859-1"
        file.write(self.format_help().encode(encoding, 'replace'))

opt_parser = MyOptionParser()
opt_parser.add_option('-v', '--version', action='store_true', dest='version')
opt_parser.add_option('-w', '--warranty', action='store_true', dest='warranty',
    help=_('Show warranty and copyright.'))
opt_parser.add_option('--no-splash', action='store_false', dest='no_splash',
    help=_('Do not show the startup window.'),
    default=False)
opt_parser.add_option('--verbose-sound-init', action='store_true',
    default=False,
    dest='verbose_sound_init',
    help=_('Display more info about the sound setup.'))
opt_parser.add_option('--no-sound', action='store_true', dest='no_sound',
    default=False,
    help=_('Do not play any sounds. Instead some data is printed to standard output. Use this for debugging and porting.'))
opt_parser.add_option('--debug', action='store_true', dest='debug',
    help=_('Include features used by the Solfege developers to debug the program.'))
opt_parser.add_option('--disable-exception-handler', action='store_true',
    dest='disable_exception_handler',
    help=_("Disable the exception handling in Gui.standard_exception_handler."))
opt_parser.add_option('--no-random', action='store_true', dest='no_random',
    help=_('For debugging only: Select questions from lesson files in sequential order.'))
opt_parser.add_option('--without-gtkhtml', action='store_true', 
    dest='without_gtkhtml',
    help=_('Run without using gtkhtml2 even if it is installed.'))
opt_parser.add_option('--no-cairo-widgets', action='store_true',
    dest='no_cairo_widgets',
    help=_("Do not use the cairo version of input widgets, even if we run gtk+ 2.8.0 or newer."))
opt_parser.add_option('--show-gtk-warnings', action='store_true',
    dest='show_gtk_warnings',
    help=_('Show GtkWarnings and PangoWarnings in the traceback window.'))


options, args = opt_parser.parse_args()

if options.version:
    print """GNU Solfege %s
This is free software. It is covered by the GNU General Public License,
and you are welcome to change it and/or distribute copies of it under
certain conditions. Invoke as `solfege --warranty` for more information.

%s
        """ % (configureoutput.VERSION_STRING, copyright)
    sys.exit()

if options.warranty:
    print """GNU Solfege %s
%s
    This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License version 2
as published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

    You should have received a copy (refer to the file COPYING) of the
GNU General Public License along with this program; if not, write to
the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
USA.
    """ % (configureoutput.VERSION_STRING, copyright)
    sys.exit()

# silent, GNOME, be silent!
#sys.argv.append('--disable-sound')
runtime.init(options)

import gtk

class SplashWin(gtk.Window):
    def __init__(self):
        gtk.Window.__init__(self, gtk.WINDOW_POPUP)
        self.set_position(gtk.WIN_POS_CENTER)
        self.set_resizable(True)
        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)
        self.add(frame)
        vbox = gtk.VBox()
        vbox.set_border_width(20)
        frame.add(vbox)
        l = gtk.Label(_("Starting GNU Solfege %s") % configureoutput.VERSION_STRING)
        l.set_name("Heading1")
        vbox.pack_start(l)
        l = gtk.Label("http://www.solfege.org")
        vbox.pack_start(l)
        self.g_infolabel = gtk.Label('')
        vbox.pack_start(self.g_infolabel)
        self.show_all()
    def show_progress(self, txt):
        self.g_infolabel.set_text(txt)
        while gtk.events_pending():
            gtk.main_iteration(0)

if not options.no_splash:
    splash_win = SplashWin()
    time.sleep(0.1)
    gtk.gdk.flush()
    while gtk.events_pending():
        gtk.main_iteration(0)
else:
    splash_win = None

if splash_win:
    splash_win.show_progress("importing application modules")

import tracebackwindow
# redirect error messages to a window that will popup if
# something bad happens.

sys.stderr = tracebackwindow.TracebackWindow(options.show_gtk_warnings)

import const
import app
from htmlwidget import HtmlWidget
import utils
from configwindow import ConfigWindow
import soundcard
import mpd
import mpd.musicdisplayer
import gu
import cfg
import stock
import lessonfile
import lessonfilegui
import abstract

import chord
import chordvoicing
import harmonicinterval
import melodicinterval
import singinterval
import nameinterval
import example

import idbyname
import dictation
import twelvetone
import idtone
import compareintervals
import singchord
import rhythm
import identifybpm
import harmonicprogressiondictation
import singanswer
import elembuilder
import rhythmtapping
import rhythmtapping2

import learning_tree_editor
from trainingsetdlg import TrainingSetDialog
from docviewer import DocViewer
from helpbrowser import HelpBrowser
import gethomedir

class MusicViewerWindow(gtk.Dialog):
    def __init__(self, app):
        gtk.Dialog.__init__(self)
        self.m_app = app
        self.set_default_size(500, 300)
        self.g_music_displayer = mpd.musicdisplayer.MusicDisplayer(utils.play_tone)
        self.vbox.pack_start(self.g_music_displayer)
        b = gu.bButton(self.action_area, _("Close"), self.m_app.close_musicviewer)
        b.grab_focus()
        self.connect('destroy', self.m_app.close_musicviewer)
        self.show_all()
    def display_music(self, music):
        fontsize = cfg.get_int('config/feta_font_size=20')
        self.g_music_displayer.display(music, fontsize)

class App(gtk.Window):
    def __init__(self, a, b):
        gtk.Window.__init__(self, gtk.WINDOW_TOPLEVEL)
        self._cid = 4
        self.__vbox = gtk.VBox()
        self.add(self.__vbox)
        self.__menubox = gu.bHBox(self.__vbox, False)
        self.__toolbarbox = gu.bHBox(self.__vbox, False)
        self.__contentsbox = gu.bVBox(self.__vbox)
        self.__appbarbox = gu.bHBox(self.__vbox, False)
        self.__dockitems = {}
    def show(self):
        self.__vbox.show()
        self.__menubox.show()
        self.__toolbarbox.show()
        self.__contentsbox.show()
        self.__appbarbox.show()
        gtk.Window.show(self)
    def set_statusbar(self, bar):
        self.__appbarbox.pack_start(bar)
        bar.show()
        self.__appbar = bar
    def set_contents(self, c):
        self.__contentsbox.pack_start(c)
    def set_menus(self, menu):
        hdlbox = gtk.HandleBox()
        hdlbox.show()
        self.__dockitems['Menubar'] = hdlbox

        self.__menubox.pack_start(hdlbox, True, True)
        hdlbox.add(menu)
        menu.show()
    def add_toolbar(self, toolbar, name, behavior, placement, band_num,
                    band_position, offset):
        hdlbox = gtk.HandleBox()
        hdlbox.show()
        self.__dockitems[name] = hdlbox
        self.__toolbarbox.pack_start(hdlbox)
        self.__toolbarbox.reorder_child(hdlbox, offset)
        hdlbox.add(toolbar)
        toolbar.show()
    def get_dock_item_by_name(self, name):
        return self.__dockitems[name]

class MainWin(App, cfg.ConfigUtils):
    default_learning_tree = ('solfege', 'learningtree.txt')
    debug_learning_tree = ('solfege', 'debugtree.txt')
    def __init__(self, options, datadir, lessonfile_manager):
        App.__init__(self, 'solfege', 'GNU Solfege')
        self.icons = stock.SolfegeIconFactory(self, datadir)
        self.set_default_size(400, 400)
        pixbuf = self.render_icon('solfege-icon', gtk.ICON_SIZE_DIALOG)
        self.set_icon(pixbuf)
        cfg.ConfigUtils.__dict__['__init__'](self, 'mainwin')
        self.set_resizable(self.get_bool('gui/mainwin_user_resizeable'))
        self.add_watch('gui/mainwin_user_resizeable', lambda s: self.set_resizable(self.get_bool('gui/mainwin_user_resizeable')))
        self.connect('destroy', self.quit_program)
        self.connect('key_press_event', self.on_key_press_event)
        self.g_about_window = None
        self.g_help_browser = None
        self.g_learning_tree_editor = None
        self.main_box = gtk.VBox()
        self.main_box.show()
        self.set_contents(self.main_box)
        self.m_exercise = None
        self.m_viewer = None
        self.box_dict = {}
        self.g_config_window = None
        self.g_musicviewer_window = None
        self.m_app = app.SolfegeApp(options, self, lessonfile_manager)
        self.g_ui_manager = gtk.UIManager()
        self.m_action_groups = {
            'Practise': gtk.ActionGroup('Practise'),
            'Exit': gtk.ActionGroup('Exit'),
            'NotExit': gtk.ActionGroup('NotExit'),
        }
        for a in self.m_action_groups.values():
            self.g_ui_manager.insert_action_group(a, 1)
        self.setup_menu()
        self.display_docfile_in_mainwin('welcome.html')
    def change_learning_tree(self, tree_fn):
        """
        Change to a different learning tree.
        """
        self.set_list('app/learningtree', tree_fn)
        self.on_learning_tree_changed()
    def on_learning_tree_changed(self, *v):
        """
        We call this when we have changed the current tree or
        switched tree.
        """
        self.m_tree = learning_tree_editor.LearningTree(self.m_app.lessonfile_manager)
        try:
            loc, filename = self.get_list("app/learningtree")
        except ValueError:
            loc, filename = self.default_learning_tree
        if (loc, filename) == ('solfege', 'debugtree.txt') and not self.m_app.m_options.debug:
            self.set_list("app/learningtree", ('solfege', 'learningtree.txt'))
            loc, filename = self.default_learning_tree
        try:
            if loc == 'solfege':
                self.m_tree.load(filename)
            else:
                assert loc == 'user'
                self.m_tree.load(os.path.join(gethomedir.get_home_dir(), '.solfege', 'learningtrees', filename))
        except IOError:
            if splash_win:
                splash_win.hide()
            self.m_tree.load(self.default_learning_tree[1])
            self.set_list('app/learningtree', self.default_learning_tree)
            gu.dialog_ok(_("Learning tree '%s' not found. Using default tree." % filename))
            if splash_win:
                splash_win.show()
        self.create_practise_and_test_menu()
    def setup_menu(self):
        self.m_action_groups['Exit'].add_actions([
          ('FileMenu', None, _('_File')),
          ('AppQuit', 'gtk-quit', None, None, None, self.quit_program),
        ])
        self.m_action_groups['NotExit'].add_actions([
          ('TheoryMenu', None, _('The_ory')),
          ('TheoryIntervals', None, _('_Intervals'), None, None,
            lambda o: self.m_app.handle_href('theory-intervals.html')),
          ('EditMenu', None, _("_Edit")),
          ('TreeEditor', None, _('Edit learning tree'), None, None,
            self.do_tree_editor),
          ('TestsMenu', None, _("_Tests")),
          ('ExportTrainingSet', None, _('New training set editor window'), None, None,
            self.new_training_set_editor),
          ('OpenPreferencesWindow', 'gtk-preferences', None, '<ctrl>F12', None,
            self.open_preferences_window),
          ('HelpMenu', None, _('_Help')),
          ('HelpHelp', 'gtk-help', _('_Help on the current exercise'), 'F1', None,
            lambda o: self.m_app.please_help_me()),
          ('HelpTheory', None, _('_Music theory on the current exercise'), 'F3', None, lambda o: self.m_app.show_exercise_theory()),
          ('HelpIndex', None, _('_User manual'), None, None,
            lambda o: self.m_app.handle_href('index.html')),
          ('HelpAllLessonFiles', None, _('All installed _lesson files'), None,
            None, lambda o: self.m_app.handle_href('solfege:all-lessonfiles')),
          ('HelpOnline', None, _('_Mailinglists, web page etc.'), None, None,
            lambda o: self.m_app.handle_href('online-resources.html')),
          ('HelpReportingBugs', None, _('Reporting _bugs'), None, None,
            lambda o: self.m_app.handle_href('bug-reporting.html')),
          ('HelpAbout', 'gtk-about', None, None, None, self.show_about_window),
          ('ShowBugReports', None, _('See your bug reports'), None, None,
            self.show_bug_reports),
        ])

        self.g_ui_manager.add_ui_from_file("ui.xml")

        self.add_accel_group(self.g_ui_manager.get_accel_group())
        self.g_ui_manager.get_accel_group().connect_group(gtk.keysyms.KP_Equal,
           gtk.gdk.CONTROL_MASK, 0,
           lambda w, a, b, c: self.box_dict['docviewer'].m_htmlwidget.g_view.zoom_reset())
        self.g_ui_manager.get_accel_group().connect_group(gtk.keysyms.KP_Subtract,
           gtk.gdk.CONTROL_MASK, 0,
           lambda w, a, b, c: self.box_dict['docviewer'].m_htmlwidget.g_view.zoom_out())
        self.g_ui_manager.get_accel_group().connect_group(gtk.keysyms.KP_Add,
           gtk.gdk.CONTROL_MASK, 0,
           lambda w, a, b, c: self.box_dict['docviewer'].m_htmlwidget.g_view.zoom_in())
        self.set_menus(self.g_ui_manager.get_widget('/Menubar'))
        self.m_help_on_current_merge_id = None
    def create_learning_trees_menu(self):
        if self.m_learning_trees_merge_id:
            self.g_ui_manager.remove_ui(self.m_learning_trees_merge_id)
        try:
            v = os.listdir(os.path.join(gethomedir.get_home_dir(), ".solfege", "learningtrees"))
        except OSError:
            v = []
        if not self.m_app.m_options.debug and not v:
            return
        actions = [('LearningTreesMenu', None, _('Learning tree')),
            ('DefaultTree', None, _("Default learning tree"),
            None, None,
           lambda o: self.change_learning_tree(self.default_learning_tree)
            )]
        if self.m_app.m_options.debug:
            actions = [('LearningTreesMenu', None, _('Learning tree')),
                ('DefaultTree', None, _("Default learning tree"),
                 None, None,
                 lambda o: self.change_learning_tree(self.default_learning_tree)
                ),
                ('DebuggingTree', None, _("Debugging"),
                 None, None,
                 lambda o: self.change_learning_tree(self.debug_learning_tree)
                )]
        s = """
<menubar name='Menubar'>
 <menu action='FileMenu'>
  <menu action='LearningTreesMenu' position='top'>
    <menuitem action='DefaultTree'/>"""
        if self.m_app.m_options.debug:
            s += "<menuitem action='DebuggingTree'/>"
        for tree_fn in v:
            s += "<menuitem action='LTree-%s'/>" % tree_fn
            actions.append(('LTree-%s' % tree_fn, None,
                    tree_fn, None, None,
                    lambda o, t=tree_fn: self.change_learning_tree(('user', t))))
        s += "</menu></menu></menubar>"
        self.m_action_groups['NotExit'].add_actions(actions)
        self.m_learning_trees_merge_id = self.g_ui_manager.add_ui_from_string(s)
    def create_practise_and_test_menu(self):
        """
        Create the Practise menu.
        This function has to be called after we have cleared out any
        lesson_id crashes.
        """
        actions = []
        if self.m_practise_and_test_merge_id:
            self.g_ui_manager.remove_ui(self.m_practise_and_test_merge_id)
            self.g_ui_manager.ensure_update()
        s = "<menubar name='Menubar'>"
        tests = "<menu action='TestsMenu'>"
        for menu in self.m_tree.m_menus:
            self.m_action_groups['NotExit'].add_action(
                gtk.Action(menu.name, _(menu.name), None, None))
            s += "<menu action='%s'>" % menu.name
            # The following line might look strange here, but it makes sure
            # that the menus on the menu bar stay at the correct order after
            # we save the learning tree in the learning tree editor.
            self.g_ui_manager.add_ui_from_string("<menubar name='Menubar'><menu action='%s'/></menubar>" % menu.name)
            for topic in menu.children:
                s += "<menu action='%s'>" % topic['name']
                tests += "<menu action='TEST_%s'>" % topic['name']
                actions.append((topic['name'], None, _(topic['name'])))
                actions.append(('TEST_%s' % topic['name'], None, _(topic['name'])))
                for lesson_id in topic.children:
                    if self.m_tree.m_visibilities[lesson_id] > self.m_tree.m_visibility:
                        continue
                    menutitle = self.m_app.lessonfile_manager.m_uiddb[lesson_id]['header']['title']
                    actions.append((
                        lesson_id, None, menutitle, None, None,
                        lambda o, e=lesson_id: self.m_app.practise_lesson_id(e)))
                    if self.m_app.lessonfile_manager.is_test_passed(lesson_id):
                        menutitle = _("%s (passed)") % menutitle
                    if self.m_app.lessonfile_manager.get(lesson_id, 'test'):
                        actions.append((
                            'TEST_%s' % lesson_id, None,
                            menutitle,
                            None, None,
                            lambda o, e=lesson_id: self.m_app.test_lesson_id(e)))
                        tests += "<menuitem action='TEST_%s' />" % lesson_id
                    s += "<menuitem action='%s' />" % lesson_id
                s += "</menu>"
                tests += "</menu>"
            s += "</menu>"
        self.m_action_groups['NotExit'].add_actions(actions)
        tests += "</menu>"
        s += tests
        s += "</menubar>"
        # The following line might look strange here, but it makes sure
        # that the menus on the menu bar stay at the correct order after
        # we save the learning tree in the learning tree editor.
        self.g_ui_manager.add_ui_from_string("<menubar name='Menubar'><menu action='TestsMenu'/></menubar>")
        self.m_practise_and_test_merge_id = self.g_ui_manager.add_ui_from_string(s)
    def show_help_on_current(self):
        """
        Show the menu entries for the exercise help and music theory
        pages on the Help menu.
        """
        if self.m_help_on_current_merge_id:
            return
        self.m_help_on_current_merge_id = self.g_ui_manager.add_ui_from_string("""
<menubar name='Menubar'>
  <menu action='HelpMenu'>
    <placeholder name='PerExerciseHelp'>
      <menuitem position='top' action='HelpHelp' />
      <menuitem action='HelpTheory' />
    </placeholder>
  </menu>
</menubar>""")
    def hide_help_on_current(self):
        """
        Hide the menu entries for the help and music theory pages on the
        Help menu.
        """
        if not self.m_help_on_current_merge_id:
            return
        self.g_ui_manager.remove_ui(self.m_help_on_current_merge_id)
        self.m_help_on_current_merge_id = None
    def show_bug_reports(self, *v):
        m = gtk.Dialog(_("Question"), self, 0)
        m.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
        m.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
        vbox = gtk.VBox()
        m.vbox.pack_start(vbox, False)
        vbox.set_spacing(18)
        vbox.set_border_width(12)
        l = gtk.Label(_("Please enter the email used when you submitted the bugs:"))
        vbox.pack_start(l, False)
        self.g_email = gtk.Entry()
        m.action_area.get_children()[0].grab_default()
        self.g_email.set_activates_default(True)
        vbox.pack_start(self.g_email, False)
        m.show_all()
        ret = m.run()
        m.destroy()
        if ret == gtk.RESPONSE_OK:
            params = urllib.urlencode({
                    'pagename': 'SITS-Incoming/SearchBugs',
                    'q': 'SITS-Incoming/"Submitter: %s"' % utils.mangle_email(self.g_email.get_text()),
                })
            import webbrowser
            webbrowser.open_new("http://www.solfege.org?%s" % params)
    def display_exception_message(self, exception, err_file=None):
        """Call this function only inside an except clause."""
        from exceptiondialog import ExceptionDialog
        m = ExceptionDialog(exception)
        if err_file:
            m.add_text(_('The exception was caught in\n"%(filename)s", line %(lineno)i.') % {'filename': err_file.decode(locale.getpreferredencoding(), 'replace'), 'lineno': sys.exc_info()[2].tb_lineno})
        if 'm_nonwrapped_text' in dir(exception):
            m.add_nonwrapped_text(exception.m_nonwrapped_text)
        m.run()
        m.destroy()
    def display_error_message2(self, text, secondary_text):
        """
        This is the new version of display_error_message, and it will
        eventually replace the old.
        """
        if splash_win and splash_win.props.visible:
            splash_win.hide()
            reshow_splash = True
        else:
            reshow_splash = False
        if not isinstance(text, unicode):
            text = text.decode(locale.getpreferredencoding(), 'replace')
        if not isinstance(secondary_text, unicode):
            secondary_text = secondary_text.decode(locale.getpreferredencoding(), 'replace')
        m = gtk.MessageDialog(None, gtk.DIALOG_MODAL, gtk.MESSAGE_ERROR,
                              gtk.BUTTONS_CLOSE, text)
        if secondary_text:
            m.format_secondary_text(secondary_text)
        m.run()
        m.destroy()
        if reshow_splash:
            splash_win.show()
            while gtk.events_pending():
                gtk.main_iteration(0)
    def display_error_message(self, msg, title=None, secondary_text=None):
        if splash_win and splash_win.props.visible:
            splash_win.hide()
            reshow_splash = True
        else:
            reshow_splash = False
        if not isinstance(msg, unicode):
            msg = msg.decode(locale.getpreferredencoding(), 'replace')
        m = gtk.MessageDialog(None, gtk.DIALOG_MODAL, gtk.MESSAGE_ERROR,
                              gtk.BUTTONS_CLOSE, None)
        m.set_markup(msg)
        if title:
            m.set_title(title)
        if secondary_text:
            m.format_secondary_text(secondary_text)
        m.run()
        m.destroy()
        if reshow_splash:
            splash_win.show()
            while gtk.events_pending():
                gtk.main_iteration(0)
    def display_question_music_error_message(self, idx, lessonfile, exception, err_file=None):
        """Call this function only inside an except clause."""
        msg = _("Failed to parse the music for question number %(idx)i in '%(lf)s':\n%(ex)s") % {'idx': idx, 'lf': lessonfile, 'ex': exception}
        self.display_exception_message(msg, err_file)
    def show_about_window(self, widget):
        trans = _("SOLFEGETRANSLATORS")
        if trans == 'SOLFEGETRANSLATORS':
            trans = ""
        pixbuf = self.render_icon('solfege-icon', gtk.ICON_SIZE_DIALOG)
        a = self.g_about_window = gtk.AboutDialog()
        a.set_logo(pixbuf)
        a.set_name("GNU Solfege")
        a.set_website("http://www.solfege.org")
        a.set_version(configureoutput.VERSION_STRING)
        a.set_copyright(copyright.decode('iso-8859-1') + """

GNU Solfege is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.""")
        a.set_authors(["Tom Cato Amundsen",
              'Oliver Vollmer %s' % _("(toolbar icons)"),
              'Giovanni Chierico %s' % _("(some lessonfiles)"),
              'Michael Becker %s' % _("(some lessonfiles)"),
              'Joe Lee %s' % _("(sound code for the MS Windows port)"),
              'Steve Lee %s' % _("(ported winmidi.c to gcc)"),
              'Thibaus Cousin %s' % _("(spec file for SuSE 8.2)"),
              'David Coe %s' %_("(spec file cleanup)"),
              'David Petrou %s' % _("(testing and portability fixes for FreeBSD)"),
              'Han-Wen Nienhuys %s' % _("(the music font from Lilypond)"),
              'Jan Nieuwenhuizen %s' % _("(the music font from Lilypond)"),
              'Davide Bonetti %s' % _("(scale exercises)"),
              ])
        a.set_documenters(["Tom Cato Amundsen",
                "Tom Eykens",
                ])
        a.set_translator_credits(_("SOLFEGETRANSLATORS"))
        self.g_about_window.run()
        self.g_about_window.destroy()
    def do_tree_editor(self, *v):
        # Raise it if it already exist.
        if self.g_learning_tree_editor:
            self.g_learning_tree_editor.present()
            return
        # And if it don't exist, then we have to create it.
        self.g_learning_tree_editor = win = learning_tree_editor.Window(self.m_app)
        try:
            loc, filename = self.get_list("app/learningtree")
        except ValueError:
            loc, filename = self.default_learning_tree
        win.load_file2(loc, filename)
        win.show()
    def post_constructor(self):
        global splash_win
        self.m_practise_and_test_merge_id = None
        self.m_learning_trees_merge_id = None
        self.on_learning_tree_changed()
        self.create_learning_trees_menu()
        self.g_ui_manager.add_ui_from_file("help-menu.xml")
        self.box_dict['docviewer'].grab_focus()
        if self.m_app.m_sound_init_exception is not None:
            if splash_win:
                splash_win.destroy()
                splash_win = None
            self.m_app.display_sound_init_error_message(self.m_app.m_sound_init_exception)
    def activate_exercise(self, module, urlobj=None):
        if self.m_viewer:
            self.box_dict[self.m_viewer].hide()
        self.m_viewer = module
        self.box_dict[module].show()
        # We need this test because not all exercises use a notebook.
        if self.box_dict[self.m_viewer].g_notebook:
            if urlobj and urlobj.action in ['practise', 'config', 'statistics']:
                self.box_dict[self.m_viewer].g_notebook.set_current_page(
                   ['practise', 'config', 'statistics'].index(urlobj.action))
            else:
                self.box_dict[self.m_viewer].g_notebook.set_current_page(0)
        if self.box_dict[module].m_t.m_P:
            if isinstance(self.box_dict[module].m_t, abstract.Teacher):
                self.set_title("Solfege - " + self.box_dict[module].m_t.m_P.header.title)
            else:
                self.set_title("Solfege - " + self.box_dict[module].m_t.exercise_data['name'].replace("_", ""))
        else:
            self.set_title("Solfege")
    def _show_docviewer(self):
        if 'docviewer' not in self.box_dict:
            self.box_dict['docviewer'] = DocViewer(self.m_app.handle_href)
            self.main_box.pack_start(self.box_dict['docviewer'])
        if self.m_viewer and (self.m_viewer != 'docviewer'):
            self.box_dict[self.m_viewer].hide()
        self.m_viewer = 'docviewer'
        self.box_dict['docviewer'].show()
    def display_docfile(self, fn, anchor):
        """
        Display the HTML file named by fn in the help browser window.
        """
        if not self.g_help_browser:
            self.g_help_browser = HelpBrowser(self.m_app)
        self.g_help_browser.show()
        self.g_help_browser.present()
        self.g_help_browser.show_docfile(fn, anchor)
    def display_docfile_in_mainwin(self, fn, anchor=None):
        self._show_docviewer()
        self.box_dict['docviewer'].read_docfile(fn, anchor)
        self.set_title("Solfege - %s" % fn)
    def display_html(self, html):#FIXME do in helpbrowser
        self._show_docviewer()
        self.box_dict['docviewer'].source(html)
    def initialise_exercise(self, teacher):
        """
        Create a Gui object for the exercise and add it to
        the box_dict dict.
        """
        assert teacher.m_exname not in self.box_dict
        n = utils.exercise_name_to_module_name(teacher.m_exname)
        self.box_dict[teacher.m_exname] = globals()[n].Gui(teacher, self)
        self.main_box.pack_start(self.box_dict[teacher.m_exname])
        if self.m_viewer:
            self.box_dict[self.m_viewer].hide()
        self.box_dict[n].show()
        self.m_viewer = n
    def on_key_press_event(self, widget, event):
        self.box_dict[self.m_viewer].on_key_press_event(widget, event)
    def open_preferences_window(self, widget=None):
        if not self.g_config_window:
            self.g_config_window = ConfigWindow(self.m_app)
            self.g_config_window.show()
        else:
            self.g_config_window.show()
    def on_zoom_in(self, *v):
        self.box_dict['docviewer'].m_htmlwidget.g_view.zoom_in()
    def quit_program(self, w=None):
        if self.g_learning_tree_editor:
            self.g_learning_tree_editor.close_window()
        self.m_app.quit_program()
        gtk.main_quit()
    def display_in_musicviewer(self, music):
        if not self.g_musicviewer_window:
            self.g_musicviewer_window = MusicViewerWindow(self)
            self.g_musicviewer_window.show()
        self.g_musicviewer_window.display_music(music)
    def close_musicviewer(self, widget=None):
        self.g_musicviewer_window.destroy()
        self.g_musicviewer_window = None
    def enter_test_mode(self, *v):
        if 'enter_test_mode' not in dir(self.box_dict[self.m_viewer]):
            gu.dialog_ok(_("The '%s' exercise module does not support test yet." % self.m_viewer))
            return
        self.m_action_groups['NotExit'].set_sensitive(False)
        self.g = self.box_dict[self.m_viewer].g_notebook.get_nth_page(0)
        self.box_dict[self.m_viewer].g_notebook.get_nth_page(0).reparent(self.main_box)
        self.box_dict[self.m_viewer].g_notebook.hide()
        self.box_dict[self.m_viewer].enter_test_mode()
    def exit_test_mode(self, *v):
        self.m_app.m_test_mode = False
        self.m_action_groups['NotExit'].set_sensitive(True)
        box = gtk.VBox()
        self.box_dict[self.m_viewer].g_notebook.insert_page(box, gtk.Label(_("Practise")), 0)
        self.g.reparent(box)
        self.box_dict[self.m_viewer].g_notebook.show()
        self.box_dict[self.m_viewer].g_notebook.get_nth_page(0).show()
        self.box_dict[self.m_viewer].g_notebook.set_current_page(0)
        self.box_dict[self.m_viewer].exit_test_mode()
        # rebuild the menus in case we hav passed a test.
        self.create_practise_and_test_menu()
    def new_training_set_editor(self, widget):
        dlg = TrainingSetDialog(self.m_app)
        dlg.show_all()


import locale
locale.setlocale(locale.LC_NUMERIC, "C")

# check_rcfile has to be called before and
# functions that use the cfg module.
app.check_rcfile()

cfg.set_bool('config/no_random', bool(options.no_random))

if splash_win:
    splash_win.show_progress("creating LessonfileManager")
lessonfile_manager = lessonfile.LessonFileManager(options.debug)

if splash_win:
    splash_win.hide()
lessonfilegui.handle_lesson_id_crash(lessonfile_manager)
if splash_win:
    splash_win.show()

lessonfile_manager.create_lessonfile_index()

if splash_win:
    splash_win.show_progress("creating MainWin")

def start_app(prefix, datadir):
    global splash_win
    w = MainWin(options, datadir, lessonfile_manager)
    w.show()
    w.post_constructor()
    if splash_win:
        splash_win.destroy()
        splash_win = None

    def ef(t, value, traceback):
        if options.debug:
            msg = "ehooked:" + str(value)
        else:
            msg = str(value)
        if issubclass(t, lessonfile.LessonfileException):
            w.display_error_message(msg, str(t))
        elif issubclass(t, osutils.ExecutableDoesNotExist):
            w.display_error_message(msg, str(t))
        else:
            sys.__excepthook__(t, msg, traceback)
    sys.excepthook = ef
    gtk.main()

