# GNU Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
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

import gobject
import gtk
import os
import time
import pickle
from gethomedir import get_home_dir
import gu
import configureoutput
from dataparser import Dataparser, DataparserException
import lessonfile


# The definition of a training set will be stored in a file in the
# ~/.solfege/trainingsets

class TrainingSetDialog(gtk.Window):
    instance_counter = 0
    fileformat_version = 1
    STORE_LESSON_ID = 0
    STORE_TITLE = 1
    STORE_COUNT = 2
    STORE_REPEAT = 3
    STORE_DELAY = 4
    def __init__(self, app):
        gtk.Window.__init__(self)
        TrainingSetDialog.instance_counter += 1
        self.savedir = os.path.join(get_home_dir(), ".solfege", "trainingsets")
        self.connect('delete_event', self.on_delete_event)
        self.set_default_size(800, 300)
        self.m_app = app
        # This VBox will have 2 parts.
        # 1: the tool bar
        # 2: another container widget that has the content of a file
        self.g_vbox = gtk.VBox()
        self.add(self.g_vbox)
        self.setup_toolbar()
        #
        self.g_settings_box = gtk.VBox()
        self.g_settings_box.set_border_width(6)
        self.g_vbox.pack_start(self.g_settings_box, False)
        self.g_output = {}
        self.g_output['midi'] = gtk.RadioButton(None, _("MIDI"))
        self.g_output['wav'] = gtk.RadioButton(self.g_output['midi'], _("WAV"))
        self.g_output['mp3'] = gtk.RadioButton(self.g_output['midi'], _("MP3"))
        self.g_output['ogg'] = gtk.RadioButton(self.g_output['midi'], _("OGG"))
        hbox = gtk.HBox()
        hbox.set_spacing(6)
        self.g_settings_box.pack_start(hbox)
        hbox.pack_start(gtk.Label(_("Preferred output format:")), False)
        for s in ('midi', 'wav', 'mp3', 'ogg'):
            hbox.pack_start(self.g_output[s], False)
        ####
        self.g_liststore = gtk.ListStore(
            gobject.TYPE_STRING, # lesson_id
            gobject.TYPE_STRING, # visible exercise name
            gobject.TYPE_INT, # count
            gobject.TYPE_INT, # repeat
            gobject.TYPE_INT) # delay
        self.g_treeview = gtk.TreeView(self.g_liststore)
        self.g_treeview.set_size_request(400, 100)
        self.g_treeview.connect('cursor-changed',
            self.on_treeview_cursor_changed)

        renderer = gtk.CellRendererText()
        column = gtk.TreeViewColumn(_("Title"), renderer, text=self.STORE_TITLE)
        self.g_treeview.append_column(column)
        renderer = gtk.CellRendererText()
        renderer.set_property('editable', True)
        renderer.connect('edited', self.on_count_edited)
        column = gtk.TreeViewColumn(_("Count"), renderer,
                                    text=self.STORE_COUNT)
        self.g_treeview.append_column(column)
        renderer = gtk.CellRendererText()
        renderer.set_property('editable', True)
        renderer.connect('edited', self.on_repeat_edited)
        column = gtk.TreeViewColumn(_("Repeat"), renderer,
                                    text=self.STORE_REPEAT)
        self.g_treeview.append_column(column)
        renderer = gtk.CellRendererText()
        renderer.set_property('editable', True)
        renderer.connect('edited', self.on_delay_edited)
        column = gtk.TreeViewColumn(_("Delay"), renderer, text=self.STORE_DELAY)
        self.g_treeview.append_column(column)
        self.g_vbox.pack_start(self.g_treeview)
        self.init_empty_file()
        self.show_all()
    def on_treeview_cursor_changed(self, treeview):
        self.g_ui_manager.get_widget("/ExportToolbar/SetRemove").set_sensitive(True)
    def on_count_edited(self, renderer, path, text):
        self._edit_col(2, path, text)
    def on_repeat_edited(self, renderer, path, text):
        self._edit_col(3, path, text)
    def on_delay_edited(self, renderer, path, text):
        self._edit_col(4, path, text)
    def _edit_col(self, col_num, path, text):
        """
        This method does the real work when on_XXXX_edited is called.
        """
        try:
            i = int(text)
        except ValueError:
            i = None
        if i is not None:
            iter = self.g_liststore.get_iter_from_string(path)
            self.g_liststore.set_value(iter, col_num, i)
    def on_remove_lesson_clicked(self, *w):
        path, column = self.g_treeview.get_cursor()
        assert path
        iter = self.g_liststore.get_iter(path)
        if iter:
            if self.g_liststore.remove(iter):
                self.g_treeview.set_cursor(self.g_liststore.get_path(iter))
            elif path[0] > 0:
                self.g_treeview.set_cursor((path[0]-1,))
            else:
                self.g_ui_manager.get_widget("/ExportToolbar/SetRemove").set_sensitive(False)
        else:
            print "NOt delete because not iter"
        print "ITER:", iter
    def on_add_lesson_clicked(self, button):
        menu = self.create_learning_tree_menu()
        menu.popup(None, None, None, 1, 0)
    def on_select_exercise(self, item):
        """
        This method is called when the user has selected an exercise to
        add.
        """
        lesson_id = item.get_data('lesson_id')
        module = self.m_app.lessonfile_manager.get(lesson_id, 'module')
        if module not in ('harmonicinterval', 'melodicinterval', 'idbyname'):
            print "Only harmonicinterval, melodicinterval and idbyname module exercises are working now. Ignoring..."
            return
        if module == 'idbyname':
            fn = self.m_app.lessonfile_manager.get(lesson_id, 'filename')
            p = lessonfile.LessonfileCommon(fn)
            p.parse_file(fn)
            if not [q for q in p.m_questions if q['music'].is_mpd_parsable()]:
                gu.dialog_ok(_("This lesson file cannot be exported because some of the music in the file are not parsable by the mpd module."))
                return
        self.m_changed = True
        self.g_liststore.append((
            lesson_id, self.get_lessonfile_title(lesson_id),
            3, 3, 4))
    def get_lessonfile_title(self, lesson_id):
        """
        Return a string we use the name the lesson file in the GUI.
        """
        return self.m_app.lessonfile_manager.get(lesson_id, 'title') \
            + " (%s)" % self.m_app.lessonfile_manager.get(lesson_id, 'filename')
    def create_learning_tree_menu(self):
        """
        Create and return a gtk.Menu object that has submenus that
        let us select all lessons on the learning tree.
        """
        def create_submenu(menudata, parent_name):
            """
            Menudata is a dict. Key we will use:
            'name'
            'children': list of dict or string (or maybe both?).
            """
            if isinstance(menudata, list):
                menu = gtk.Menu()
                for lesson_id in menudata:
                    if self.m_app.lessonfile_manager.get(lesson_id, 'module') not in ('melodicinterval', 'harmonicinterval', 'idbyname'):
                        continue
                    # We don't want to add these lesson files because we know
                    # that they cannot be exported. It would be better
                    # to catch these with a more generit algorithm, but
                    # then we would have to parse all the files, and that
                    # would be too slow.
                    if lesson_id in (
                            # melodic-interval-self-config
                            "f62929dc-7122-4173-aad1-4d4eef8779af",
                            # harmonic-interval-self-config
                            "466409e7-9086-4623-aff0-7c27f7dfd13b",
                            # the csound-fifth-* files:
                            "b465c807-d7bf-4e3a-a6da-54c78d5b59a1",
                            "aa5c3b18-664b-4e3d-b42d-2f06582f4135",
                            "5098fb96-c362-45b9-bbb3-703db149a079",
                            "3b1f57e8-2983-4a74-96da-468aa5414e5e",
                            "a06b5531-7422-4ea3-8711-ec57e2a4ce22",
                            "e67c5bd2-a275-4d9a-96a8-52e43a1e8987",
                            "1cadef8c-859e-4482-a6c4-31bd715b4787",
                            ):
                        continue
                    i = gtk.MenuItem(
                        self.m_app.lessonfile_manager.get(lesson_id, 'title'))
                    i.set_data('lesson_id', lesson_id)
                    i.set_data('menus', parent_name)
                    i.connect('activate', self.on_select_exercise)
                    menu.append(i)
                return menu
            item = gtk.MenuItem(_(menudata['name']))
            menu = gtk.Menu()
            for m in menudata['children']:
                i = gtk.MenuItem(_(m['name']))
                menu.append(i)
                i.set_submenu(create_submenu(m['children'], parent_name + [_(m['name'])]))
            item.set_submenu(menu)
            return item
        menu = gtk.Menu()
        for m in self.m_app.m_ui.m_tree.m_menus:
            menu.append(create_submenu(m, [_(m['name'])]))

        menu.show_all()
        self._menu_hide_stuff(menu)
        return menu
    def _menu_hide_stuff(self, menu):
        """
        Hide the menu if it has no menu items, or all menu items are hidden.
        """
        for sub in menu.get_children():
            assert isinstance(sub, gtk.MenuItem)
            if sub.get_submenu():
                self._menu_hide_stuff(sub.get_submenu())
                if not [c for c in sub.get_submenu().get_children() if c.get_property('visible')]:
                    sub.hide()
    def init_empty_file(self):
        self.m_changed = False
        self.m_filename = None
        self.m_savetime = time.time()
        self.set_title(self._get_a_filename())
        self.g_ui_manager.get_widget("/ExportToolbar/SetRemove").set_sensitive(False)
    def _get_a_filename(self):
        """
        Return a file name. UntitledN (where N is an integer) if we
        have to real name.
        """
        if not self.m_filename:
            return _("Untitled%s") % TrainingSetDialog.instance_counter
        return self.m_filename
    def setup_toolbar(self):
        self.g_toolbar = gtk.Toolbar()
        self.g_ui_manager = gtk.UIManager()
        actiongroup = gtk.ActionGroup('TrainingSetActions')
        actiongroup.add_actions([
         ('SetClose', gtk.STOCK_CLOSE, None, None, None, self.close_window),
         ('SetSave', gtk.STOCK_SAVE, None, None, None, self.on_save),
         ('SetSaveAs', gtk.STOCK_SAVE_AS, None, None, None, self.on_save_as),
         ('SetNew', gtk.STOCK_NEW, None, None, None, self.new_file),
         ('SetOpen', gtk.STOCK_OPEN, None, None, None, self.on_open),
         ('SetExport', gtk.STOCK_EXECUTE, _("Export"), None, None, self.on_export),
         ('SetAdd', gtk.STOCK_ADD, None, None, None, self.on_add_lesson_clicked),
         ('SetRemove', gtk.STOCK_REMOVE, None, None, None, self.on_remove_lesson_clicked),
         ('SetHelp', gtk.STOCK_HELP, None, None, None, self.on_show_help),
        ])
        self.g_ui_manager.insert_action_group(actiongroup, 0)
        uixml = """
        <ui>
         <toolbar name='ExportToolbar'>
          <toolitem action='SetAdd'/>
          <toolitem action='SetRemove'/>
          <toolitem action='SetNew'/>
          <toolitem action='SetOpen'/>
          <toolitem action='SetSave'/>
          <toolitem action='SetSaveAs'/>
          <toolitem action='SetExport'/>
          <toolitem action='SetClose'/>
          <toolitem action='SetHelp'/>
         </toolbar>
        </ui>
        """
        self.g_ui_manager.add_ui_from_string(uixml)
        self.g_vbox.pack_start(self.g_ui_manager.get_widget("/ExportToolbar"), False)
        self.g_ui_manager.get_widget("/ExportToolbar").set_style(gtk.TOOLBAR_BOTH)
        hbox = gtk.HBox()
        hbox.set_spacing(8)
        self.g_vbox.pack_start(hbox, False)
    def on_show_help(self, widget):
        self.m_app.handle_href("trainingset-editor.html")
    def on_delete_event(self, *ignore):
        self.close_window()
        return True
    def close_window(self, *w):
        if not self.m_changed:
            self.destroy()
            return
        m = gtk.MessageDialog(self, gtk.DIALOG_MODAL,
            gtk.MESSAGE_WARNING, gtk.BUTTONS_NONE,
            _("Save changes to \"%s\" before closing?") % self._get_a_filename())
        t = time.time() - self.m_savetime
        if t < 60:
            msg = _("If you don't save, changes from the past %i seconds will be permanently lost.") % int(time.time() - self.m_savetime)
        else:
            msg = _("If you don't save, changes from the past %i minutes will be permanently lost.") % int((time.time() - self.m_savetime) / 60.0)
        m.format_secondary_text(msg)
        m.add_button(_("_Close without Saving"), gtk.RESPONSE_CLOSE)
        m.add_button("gtk-cancel", gtk.RESPONSE_CANCEL)
        m.add_button("gtk-save", gtk.RESPONSE_OK)
        m.set_default_response(gtk.RESPONSE_OK)
        r = m.run()
        m.destroy()
        if r == gtk.RESPONSE_CLOSE:
            self.destroy()
        elif r in (gtk.RESPONSE_CANCEL, gtk.RESPONSE_DELETE_EVENT):
            return
        else:
            assert r == gtk.RESPONSE_OK
            self.on_save()
            self.destroy()
    def get_trainingset_filenames(self):
        #FIXME we need to know 100% sure that we will get
        #unicode strings from this method
        try:
            return os.listdir(os.path.join(get_home_dir(),
                                       ".solfege", "trainingsets"))
        except OSError, e:
            if e.errno != 2:
                print "warning: errno != 2"
            return []
    def on_open(self, widget):
        dialog = gtk.FileChooserDialog(None, self,
            gtk.FILE_CHOOSER_ACTION_OPEN,
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
             gtk.STOCK_OK, gtk.RESPONSE_OK))
        dialog.set_current_folder(self.savedir)
        ret = dialog.run()
        if ret == gtk.RESPONSE_OK:
            try:
                if (not self.m_filename) and (not self.m_changed):
                    self.load_file(dialog.get_filename())
                else:
                    win = self.new_file()
                    win.load_file(dialog.get_filename())
            except Exception, e:
                self.m_app.m_ui.display_exception_message(e, __file__)
        dialog.destroy()
    def load_file(self, filename):
        p = Dataparser({'yes': True, 'no': False}, {})
        p.parse_file(filename)
        s = p.globals.setdefault('output_format', 'midi')
        s = s.lower()
        if s in self.g_output:
            self.g_output[s].set_active(True)
        else:
            # MIDI is the default format
            self.g_output['midi'].set_active(True)
        self.m_filename = filename
        for lesson in p.blocklists.setdefault('lesson', []):
            self.g_liststore.append((lesson['lesson_id'],
                self.get_lessonfile_title(lesson['lesson_id']),
                lesson['count'],
                lesson['repeat'],
                lesson['delay']))
        self.set_title(self.m_filename)
    def on_save_as(self, widget):
        """
        Return True if the file was saved, False if not.
        """
        dialog = gtk.FileChooserDialog(_("Save As..."), self,
            gtk.FILE_CHOOSER_ACTION_SAVE,
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
             gtk.STOCK_OK, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)
        if not os.path.exists(self.savedir):
            try:
                os.mkdir(self.savedir)
            except OSError, e:
                pass
        # if self.m_filename is set to a real filename, we will
        # display the folder where that file exists. If not, we will
        # use .solfege/trainingsets
        if os.path.dirname(self._get_a_filename()):
            dialog.set_current_folder(os.path.dirname(self._get_a_filename()))
        else:
            dialog.set_current_folder(self.savedir)
        if dialog.get_current_folder() == os.path.dirname(self._get_a_filename()):
            dialog.set_current_name(os.path.basename(self._get_a_filename()))
        else:
            dialog.set_current_name(self._get_a_filename())
        ret = dialog.run()
        if ret == gtk.RESPONSE_OK:
            try:
                self.m_filename = dialog.get_filename()
                self.save()
                self.set_title(self.m_filename)
            except IOError, e:
                gu.dialog_ok(_("Error saving file"), self, str(e))
                dialog.destroy()
                return False
        dialog.destroy()
        return ret == gtk.RESPONSE_OK
    def on_save(self, widget=None):
        """
        Return True if the file was saved, False if not.
        """
        if not self.m_filename:
            return self.on_save_as(widget)
        else:
            self.save()
            return True
    def save(self):
        """
        Save the file to a file named by self.m_filename
        """
        assert self.m_filename
        f = open(self.m_filename, 'w')
        print >> f, "# Training set definition file for GNU Solfege %s" % configureoutput.VERSION_STRING
        print >> f, "\nfileformat_version = %i" % self.fileformat_version
        print >> f, "output_format = \"%s\"" % [k for k in self.g_output if self.g_output[k].get_active()][0]
        iter = self.g_liststore.get_iter_first()
        while iter:
            print >> f, "lesson {"
            lesson_id = self.g_liststore.get_value(iter, self.STORE_LESSON_ID)
            if self.m_app.lessonfile_manager.get(lesson_id, 'title'):
                print >> f, "  # %s (%s)" % (
                    self.m_app.lessonfile_manager.get(lesson_id, 'title'),
                    self.m_app.lessonfile_manager.get(lesson_id, 'filename')
                    )
            print >> f, '  lesson_id = "%s"' % lesson_id
            print >> f, '  count = %i' \
                % self.g_liststore.get_value(iter, self.STORE_COUNT)
            print >> f, '  repeat = %i' \
                % self.g_liststore.get_value(iter, self.STORE_REPEAT)
            print >> f, '  delay = %i' \
                % self.g_liststore.get_value(iter, self.STORE_DELAY)
            print >> f, "}\n"
            iter = self.g_liststore.iter_next(iter)
        f.close()
        self.m_changed = False
        self.m_savetime = time.time()
    def new_file(self, action=None):
        """
        Return the new dialog window.
        """
        m = TrainingSetDialog(self.m_app)
        m.show_all()
        return m
    def on_export(self, widget):
        msg = _("Select an empty directory, since we want to fill it with files.")
        dialog = gtk.FileChooserDialog(_("Select where to export the files"),
            self, gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
             gtk.STOCK_OK, gtk.RESPONSE_OK))
        label = gtk.Label(msg)
        label.show()
        dialog.vbox.pack_start(label, False, False)
        while 1:
            res = dialog.run()
            if res in (gtk.RESPONSE_CANCEL, gtk.RESPONSE_DELETE_EVENT):
                dialog.destroy()
                return
            if os.listdir(dialog.get_filename()):
                msg_dlg = gtk.MessageDialog(self, gtk.DIALOG_MODAL,
                    gtk.MESSAGE_INFO, gtk.BUTTONS_OK, msg)
                msg_dlg.run()
                msg_dlg.destroy()
            else:
                break
        export_to = dialog.get_filename()
        dialog.destroy()
        progress_dialog = gtk.Dialog(_("Exporting training set"), self,
            0, (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL))
        progress_dialog.show()
        label = gtk.Label()
        label.set_markup('<span weight="bold">%s</span>' % _("Export training set"))
        label.show()
        progress_dialog.vbox.pack_start(label, False)
        def _cancel(widget, response):
            self.m_app.m_abort_export = True
        progress_dialog.connect('response', _cancel)
        progress_bar = gtk.ProgressBar()
        progress_bar.show()
        progress_dialog.vbox.pack_start(progress_bar)
        # We have to make a version of the data without gtk widgets
        v = []
        iter = self.g_liststore.get_iter_first()
        while iter:
            v.append({
                'lesson_id': \
                        self.g_liststore.get_value(iter, self.STORE_LESSON_ID),
                'count': self.g_liststore.get_value(iter, self.STORE_COUNT),
                'repeat': self.g_liststore.get_value(iter, self.STORE_REPEAT),
                'delay': self.g_liststore.get_value(iter, self.STORE_DELAY),
                        })
            iter = self.g_liststore.iter_next(iter)
        output_format = [k for k in self.g_output if self.g_output[k].get_active()][0]
        progress_dialog.queue_draw()
        while gtk.events_pending():
            gtk.main_iteration(0)
        time.sleep(0.1)
        while gtk.events_pending():
            gtk.main_iteration(0)
        for prog in self.m_app.export_training_set(v, export_to, output_format):
            progress_bar.set_fraction(prog)
            while gtk.events_pending():
                gtk.main_iteration(0)
        progress_dialog.destroy()


