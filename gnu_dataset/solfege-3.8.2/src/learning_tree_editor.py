# GNU Solfege - free ear training software
# Copyright (C) 2005, 2006, 2007  Tom Cato Amundsen
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
import pprint
import copy
import sys
import os
import cfg
import gu
import lessonfilegui
from gpath import Path
import gethomedir

class Menu(dict):
    def new_from_name(name):
        m = Menu({'name': name, 'children': []})
        return m
    new_from_name = staticmethod(new_from_name)
    def get_name(self):
        return self['name']
    def set_name(self, name):
        self['name'] = name
    name = property(get_name, set_name)
    def get_children(self):
        return self['children']
    def set_children(self, children):
        self['children'] = children
    children = property(get_children, set_children)
    def pformat(self, indent):
        return "%(indent)s{'name': '%(name)s',\n" \
        "%(indent)s 'nametotranslate': _('%(name)s'),\n" \
        "%(indent)s 'children': [\n" \
        "%(children)s ]\n" \
        "%(indent)s}," %  {
            'indent': " " * indent,
            'name': self.name,
            'children': self.pformat_children(indent + 4)
        }
    def pformat_children(self, indent):
        v = []
        for child in self.children:
            if isinstance(child, basestring):
                v.append("%s'%s'," % (" " * indent, child))
            else:
                v.append(child.pformat(indent))
        return "\n".join(v)


class LearningTree:
    def __init__(self, lessonfile_manager):
        self.lessonfile_manager = lessonfile_manager
        self.m_menus = []
        self.m_deps = {}
        self.m_modified = False
    def load(self, filename):
        self.m_visibility = 0
        self.m_learning_tree_version = 0
        g = {'_': lambda s: s}
        v = eval(open(filename, 'r').read(), g)
        if len(v) == 2:
            self.m_menus, self.m_deps = v
        elif len(v) == 3:
            self.m_menus, self.m_deps, self.m_visibility = v
        elif len(v) == 4:
            self.m_learning_tree_version, self.m_menus, self.m_deps, self.m_visibility = v
        if self.m_learning_tree_version == 0:
            # Change from old style with one menu to multiple exercise
            # menus on the menu bar. If version == 0, then the tree only
            # have one menu "Exercises"
            self.m_menus = [
              {'name': "_Practise",
               'submenus': self.m_menus},
            ]
            self.m_menus[0]['children'] = [{'name': n['name'], 'children': n['lessons']} for n in self.m_menus[0]['submenus']]
            del self.m_menus[0]['submenus']
        self.m_menus = [Menu(d) for d in self.m_menus]
        for idx, menu in enumerate(self.m_menus):
            self.m_menus[idx].children = [Menu(s) for s in menu.children]
        self.m_learning_tree_version = 2
        junk_id = []
        # FIXME here we are assuming that there are only two levels of menus
        if self.lessonfile_manager:
            # lessonfile_manager is None in some test cases
            for menu in self.m_menus:
                for topic in menu.children:
                    for j in topic.children:
                        if j not in self.lessonfile_manager.m_uiddb:
                            print >> sys.stderr, "Junking the registration of lesson_id '%s'\nfrom learning tree. File not found." % j
                            junk_id.append(j)
        for j in junk_id:
            for menu in self.m_menus:
                for topic in menu.children:
                    if j in topic.children:
                        del topic.children[topic.children.index(j)]
                if j in self.m_deps:
                    del self.m_deps[j]
                for d in self.m_deps:
                    if j in self.m_deps[d]:
                        self.m_deps[d].remove(j)
        self.sort_topics()
        self.calculate_visibilities()
        self.m_modified = False
    def save(self, filename):
        ofile = open(filename, 'w')
        print >> ofile, "[ # toplevel list"
        print >> ofile, "  %s, # learning tree file format version" % self.m_learning_tree_version
        print >> ofile, "  [ # start of list of menus"
        for menu in self.m_menus:
            print >> ofile, menu.pformat(4)
        print >> ofile, "  ], # end of list of menus"
        ofile.write("%s,\n%s]" % (pprint.pformat(self.m_deps), self.m_visibility))
        ofile.close()
        self.m_modified = False
    def new_menu(self, menuname):
        self.m_menus.append(Menu({'name': menuname, 'children': []}))
        self.m_modified = True
    def new_topic(self, menu_idx, topicname):
        assert topicname not in [s.name for s in self.m_menus[menu_idx].children]
        self.m_menus[menu_idx].children.append(Menu.new_from_name(topicname))
        self.m_modified = True
    def sort_topics(self):
        """
        Sort the lessons in each topic.
        """
        for menu in self.m_menus:
            for topic in menu.children:
                topic.children.sort(lambda a, b: self.cmp(a, b))
    def move_elem_up(self, path):
        """
        Move elem up. Return True if success.
        Return False if we are the first elem.
        This function only moves the element within the menu.
        """
        if path[-1] == 0:
            return False
        p = path[:-1]
        new_path = path[:-1] + (path[-1],)
        try:
            self.get(p).children[path[-1]], self.get(p).children[path[-1]-1] = \
                self.get(p).children[path[-1]-1], self.get(p).children[path[-1]]
        except IndexError:
            return False
        self.m_modified = True
        return True
    def move_elem_to_prev_menu(self, path):
        """
        Move the element pointed to by path to the prev menu.
        Return None if we are on the first menu.
        """
        assert path[-2] > 0
        to_path = list(path[:-1])
        to_path[-1] -= 1
        to_path = tuple(to_path)
        self.get(to_path).children.append(self.get(path))
        del self.get(path[:-1]).children[path[-1]]
        self.m_modified = True
        return to_path + (len(self.get(to_path).children)-1,)
    def move_elem_down(self, path):
        """
        Move move the element (submenu or lesson) one step down on the menu
        containing it, and return True if successfull.  Return False and do
        nothing if the element path points to are the last element.
        """
        p = path[:-1]
        try:
            self.get(p).children[path[-1]], \
                    self.get(p).children[path[-1]+1] = \
                    self.get(p).children[path[-1]+1], \
                    self.get(p).children[path[-1]]
        except IndexError:
            return False
        self.m_modified = True
        return True
    def move_elem_to_next_menu(self, path):
        """
        Move the element pointed to by path to the next menu.
        Faild miserably if we are on the last menu, because the
        gui checks this right now. Return the path to the new
        position.
        """
        pn = list(path[:-1])
        pn[0] += 1
        pn = tuple(pn)
        self.get(pn).children.insert(0, self.get(path))
        del self.get(path[:-1]).children[path[-1]]
        self.m_modified = True
        return pn + (0,)
    def move_lesson_up(self, path):
        """
        Return True if successful, else False.
        The tree is unchanged if we return False.
        """
        if path[-1] == 0:
            # We are the first lesson
            return False
        move_id = self.get(path)
        prev_id = self.get(Path(path).prev())
        if prev_id not in list(self.iter_subdeps(move_id)):
            self.move_elem_up(path)
            self.m_modified = True
            return True
        return False
    def move_lesson_down(self, path):
        """
        Return True if successful, else None.
        The tree is unchanged if we return None.
        """
        p = path[:-1]
        i = path[-1]
        move_id = self.get(path)
        try:
            next_id = self.get(Path(path).next())
        except IndexError:
            return
        if move_id not in list(self.iter_subdeps(next_id)):
            try:
                self.get(p).children[i], self.get(p).children[i + 1] = \
                    self.get(p).children[i + 1], self.get(p).children[i]
            except IndexError:
                return False
            self.m_modified = True
            return True
        return False
    def add_lesson(self, path, lesson_id):
        """
        Each lesson can only be once in a topic.
        Return True if sucessful, False if not
        """
        menu = self.get(path)
        if lesson_id not in menu.children:
            if not lesson_id in self.m_deps:
                self.m_deps[lesson_id] = []
            menu.children.append(lesson_id)
            menu.children.sort(lambda a, b: self.cmp(a, b))
        else:
            return False
        self.m_modified = True
        return True
    def delete_lesson(self, path):
        menu = self.get(path[:-1])
        del menu.children[path[-1]]
        self.m_modified = True
    def add_dependency(self, lesson_id, dep_id):
        assert dep_id not in self.m_deps[lesson_id]
        self.m_deps[lesson_id].append(dep_id)
        self.sort_topics()
        self.m_modified = True
    def delete_dependency(self, lesson_id, id_to_delete):
        i = self.m_deps[lesson_id].index(id_to_delete)
        del self.m_deps[lesson_id][i]
        self.sort_topics()
        self.m_modified = True
    def iterate_all_lessons2(self):
        """
        Iterate all lessons that are added to the learning tree.
        Yields the tuple (lesson_ids, path,)
        """
        def do_children(item, path):
            # path == (0,) is the first menu on the menubar
            path = path.child()
            for c in item.children:
                if isinstance(c, Menu):
                    for x in do_children(c, path):
                        yield x
                else:
                    assert isinstance(c, basestring)
                    yield c, path
                path = path.next()
        path = Path((0,))
        for menu in self.m_menus:
            for x in do_children(menu, path):
                yield x
            path = path.next()
    def iterate_all_lessons(self):
        """
        Iterate all lessons that are added to the learning tree.
        Yields lesson_ids
        """
        def do_children(item):
            for c in item.children:
                if isinstance(c, Menu):
                    for x in do_children(c):
                        yield x
                else:
                    assert isinstance(c, basestring)
                    yield c
        for menu in self.m_menus:
            for x in do_children(menu):
                yield x
    def iterate_topics_for_id(self, lesson_id):
        """
        Yield a string with the name of the submenu containing
        the lesson_id.
        """
        def do_menu(menu):
            if lesson_id in menu.children:
                yield menu.name
            for child in menu.children:
                if isinstance(child, Menu):
                    for n in do_menu(child):
                        yield n
        for menu in self.m_menus:
            for n in do_menu(menu):
                yield n
    def iterate_deps_for_id(self, lesson_id):
        """
        Iterate all the direct dependencies for lesson_id.
        It does not iterate the sub-dependencies.
        """
        for dep in self.m_deps[lesson_id]:
            yield dep
    def iterate_possible_deps_for(self, path):
        """
        All lessons, except those on the x-list.
        You get on the x-list if:
        1. is OBJECT
        2. already in the depends list of OBJECT
        3. depend on anything in the x-list
        4. is a dep (of dep)* of OBJ


        Filter out lessons that
        1. is OBJECT
        2. is in depends tree below OBJECT
        3. has OBJECT in its depends tree
        """
        # The lesson_id we are finding possible deps for
        this_id = self.get(path)
        # First, lets make a list of all lessons that currently are in a topic
        used = {}
        for lesson_id in self.iterate_all_lessons():
            used[lesson_id] = True
        # Filter out this_id (point 1 in the list in the docstring)
        del used[this_id]
        def check(lesson_id):
            # Filter out according to #2 and #3 in the docstring
            if lesson_id in list(self.iter_subdeps(this_id)) \
                    or this_id in list(self.iter_subdeps(lesson_id)):
                return False
            return True
        for i in filter(check, used.keys()):
            yield i
    def iter_subdeps(self, lesson_id):
        for n in self.m_deps[lesson_id]:
            yield n
            for nn in self.iter_subdeps(n):
                yield nn
    def is_practisable(self, lesson_id):
        for i in self.iterate_deps_for_id(lesson_id):
            if not self.lessonfile_manager.is_test_passed(i):
                return False
        return True
    def calculate_visibilities(self):
        self.m_visibilities = {}
        v = self.m_deps.keys()
        v.sort(lambda a, b: self.cmp(a, b))
        for i in v:
            if i not in self.lessonfile_manager.m_uiddb:
                continue
            if not self.lessonfile_manager.get(i, 'test'):
                self.m_visibilities[i] = 0
            elif not list(self.iterate_deps_for_id(i)):
                self.m_visibilities[i] = 0
            elif self.is_practisable(i):
                self.m_visibilities[i] = 0
            else:
                self.m_visibilities[i] = max([self.m_visibilities[x] for x in self.m_deps[i]]) + 1
    def cmp(self, id_a, id_b):
        """
        Return -1, 0, 1, like a cmp function.
        """
        deps_a = list(self.iter_subdeps(id_b))
        if id_a in deps_a:
            return -1
        deps_b = list(self.iter_subdeps(id_a))
        if id_b in deps_b:
            return 1
        return cmp(len(deps_b), len(deps_a))
    def get_use_count(self, lesson_id):
        """
        Return an integer telling how many times the lesson lesson_id
        is used as an exercise.
        """
        count = 0
        for i in self.iterate_all_lessons():
            if i == lesson_id:
                count += 1
        return count
    def get_dep_use_count(self, lesson_id):
        """
        Return an integer telling how many lessons that depends on lesson_id.
        """
        count = 0
        for v in self.m_deps.values():
            for i in v:
                if i == lesson_id:
                    count += 1
        return count
    def remove_all_deps_of(self, del_id):
        for v in self.m_deps.values():
            if del_id in v:
                del v[v.index(del_id)]
        self.m_modified = True
    def get(self, path):
        """
        Return the element pointed to by path.
        """
        elem = self.m_menus[path[0]]
        for idx in path[1:]:
            elem = elem.children[idx]
        return elem

class LessonFileDialogCommon(gtk.Dialog):
    def __init__(self, app, tree, path):
        gtk.Dialog.__init__(self, _("Select lesson file"), self, buttons=
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,))
        self.m_app = app
        self.m_tree = tree
        self.m_app.lessonfile_manager.parse(self.m_app.m_options.debug)
        lessonfilegui.handle_lesson_id_crash(self.m_app.lessonfile_manager)
        self.m_cur_menu_path = path
        self.g_ok_button = self.add_button("gtk-ok", gtk.RESPONSE_OK)
        self.g_ok_button.set_sensitive(False)
        self.set_default_size(800, 500)
        vbox = gtk.VBox()
        self.vbox.pack_start(vbox, False)
        self.g_liststore = gtk.ListStore(gobject.TYPE_STRING,
            gobject.TYPE_STRING, gobject.TYPE_STRING, gobject.TYPE_INT,
            gobject.TYPE_STRING)
        self.update_filelist()
        self.g_treeview = gtk.TreeView(self.g_liststore)
        self.g_treeview.connect('cursor-changed', self.on_cursor_changed)
        renderer = gtk.CellRendererText()
        column = gtk.TreeViewColumn(_("Used"), renderer, text=3)
        column.set_sort_column_id(3)
        column.set_resizable(True)
        self.g_treeview.append_column(column)
        column = gtk.TreeViewColumn(_("Title"), renderer, text=0)
        column.set_resizable(True)
        column.set_sort_column_id(0)
        self.g_treeview.append_column(column)
        column = gtk.TreeViewColumn(_("Filename"), renderer, text=1)
        column.set_resizable(True)
        self.g_treeview.append_column(column)
        def sort_func(store, itera, iterb):
            return cmp(store.get(itera, 1)[0], store.get(iterb, 1)[0])
        self.g_treeview.get_model().set_sort_func(1, sort_func)
        column.set_sort_column_id(1)
        scrolled_window = gtk.ScrolledWindow()
        scrolled_window.add(self.g_treeview)
        self.vbox.pack_start(scrolled_window)
        hbox = gtk.HBox()
        hbox.set_spacing(8)
        hbox.set_border_width(12)
        self.vbox.pack_start(hbox, False)
        hbox.pack_start(gtk.Label(_("Module:")), False)
        self.g_module = gtk.Label()
        hbox.pack_start(self.g_module, False)
        #
        hbox = gtk.HBox()
        hbox.set_spacing(8)
        hbox.set_border_width(12)
        self.vbox.pack_start(hbox, False)
        label = gtk.Label(_("Used by topic(s):"))
        label.set_alignment(0.0, 0.0)
        hbox.pack_start(gtk.Label(_("Used by topic(s):")), False)
        self.g_topics_vbox = gtk.VBox()
        hbox.pack_start(self.g_topics_vbox, False)
        #
        self.show_all()
        self.g_treeview.connect('row-activated', self.on_row_activated)
    def get_selected_lesson_id(self):
        """
        Returns None if no lesson file is selected.
        """
        i = self.g_treeview.get_cursor()[0]
        if i:
            i = i[0]
            iter = self.g_liststore.iter_nth_child(None, i)
            return self.g_liststore.get(iter, 2)[0]
    def on_row_activated(self, treeview, row, treeviewcolumn):
        self.response(gtk.RESPONSE_OK)
    def on_cursor_changed(self, treeview):
        i = self.g_treeview.get_cursor()[0][0]
        iter = self.g_liststore.iter_nth_child(None, i)
        self.g_module.set_text(self.g_liststore.get(iter, 4)[0])
        self.g_topics_vbox.foreach(lambda w: w.destroy())
        for s in self.m_tree.iterate_topics_for_id(
                self.g_liststore.get(iter, 2)):
            label = gtk.Label(s)
            label.set_alignment(0.0, 0.5)
            label.show()
            self.g_topics_vbox.pack_start(label)
        self.g_ok_button.set_sensitive(True)


class SelectLessonFileDialog(LessonFileDialogCommon):
    def __init__(self, app, tree, path):
        LessonFileDialogCommon.__init__(self, app, tree, path)
        g_show_used = gu.nCheckButton('SelectLessonFileDialog',
            'show_used_files', _("_Show files used in other topics"),
            False, self.update_filelist)
        self.vbox.pack_start(g_show_used, False)
        self.vbox.reorder_child(g_show_used, 0)
    def update_filelist(self, w=None):
        self.g_liststore.clear()
        if cfg.get_bool('SelectLessonFileDialog/show_used_files'):
            lesson_id_list = [i for i in list(self.m_app.lessonfile_manager.iterate_lesson_ids()) if i not in self.m_tree.get(self.m_cur_menu_path).children]
        else:
            lesson_id_list = [i for i in list(self.m_app.lessonfile_manager.iterate_lesson_ids()) if self.m_tree.get_use_count(i) == 0]
        for lesson_id in lesson_id_list:
            d = self.m_app.lessonfile_manager.m_uiddb[lesson_id]
            self.g_liststore.append((d['header']['title'], d['filename'], lesson_id, self.m_tree.get_use_count(lesson_id), self.m_app.lessonfile_manager.get(lesson_id, 'module')))


class DepsLessonFileDialog(LessonFileDialogCommon):
    def __init__(self, app, tree, path):
        """
        i is the index from the lesson in the topic in the tree.
        """
        LessonFileDialogCommon.__init__(self, app, tree, path)
        self.update_filelist()
    def update_filelist(self, w=None):
        self.g_liststore.clear()
        for lesson_id in list(self.m_tree.iterate_possible_deps_for(self.m_cur_menu_path)):
            d = self.m_app.lessonfile_manager.m_uiddb[lesson_id]
            self.g_liststore.append((d['header']['title'], d['filename'], lesson_id, self.m_tree.get_use_count(lesson_id), self.m_app.lessonfile_manager.get(lesson_id, 'module')))


class Window(gtk.Window):
    def __init__(self, app):
        gtk.Window.__init__(self)
        self.m_app = app
        self.set_default_size(500, 500)
        self.vbox = gtk.VBox()
        self.add(self.vbox)
        hbox = gtk.HBox()
        self.vbox.pack_start(hbox, False)
        g = gtk.Button(stock='gtk-new')
        g.connect('clicked', self.on_new)
        hbox.pack_start(g, False)
        g = gtk.Button(stock='gtk-save')
        g.connect('clicked', self.on_save)
        hbox.pack_start(g, False)
        g = gtk.Button(stock='gtk-save-as')
        g.connect('clicked', self.on_save_as)
        hbox.pack_start(g, False)
        g = gtk.Button(stock='gtk-close')
        g.connect('clicked', self.close_window)
        hbox.pack_start(g, False)
        hbox = gtk.HBox()
        self.vbox.pack_start(hbox, False)
        gu.bLabel(hbox, _("Learning tree:"), False, False)
        self.g_trees_liststore = gtk.ListStore(gobject.TYPE_STRING)
        self.g_trees = gtk.ComboBox(self.g_trees_liststore)
        cell = gtk.CellRendererText()
        self.g_trees.pack_start(cell, True)
        self.g_trees.add_attribute(cell, 'text', 0)
        self.g_trees.connect('changed', self.on_learning_tree_combo_changed)
        self.fill_trees_combo()
        hbox.pack_start(self.g_trees)
        self.g_trees.show()
        #######################
        self.g_content_vbox = gtk.VBox()
        self.vbox.pack_start(self.g_content_vbox)
        self.g_content_vbox.set_border_width(12)
        self.g_content_vbox.set_spacing(8)
        buttons_sizegroup = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)
        # Visibility
        hbox = gtk.HBox()
        hbox.set_spacing(8)
        l = gtk.Label(_("Default visibility:"))
        hbox.pack_start(l, False)
        self.g_visibility = gtk.Entry()
        self.g_visibility.connect('changed', self.on_visibility_entry_changed)
        hbox.pack_start(self.g_visibility, False)
        self.g_content_vbox.pack_start(hbox, False)
        ##############
        # TreeView with menus and submenus
        hbox = gu.bHBox(self.g_content_vbox)
        hbox.set_spacing(8)
        hbox.show()
        vbox = gu.bVBox(hbox, False, False)
        buttons_sizegroup.add_widget(vbox)
        vbox.show()
        gu.bButton(vbox, _("New toplevel menu"), self.on_new_menu, expand=False)
        self.g_new_submenu = gu.bButton(vbox, _("New submenu"),
                                        self.on_new_topic, expand=False)
        self.g_add_lesson = gtk.Button(_("Add lesson"))
        self.g_add_lesson.connect('clicked', self.on_new_lesson)
        vbox.pack_start(self.g_add_lesson, False)
        self.g_delete_lesson = gtk.Button(_("Delete"))
        self.g_delete_lesson.connect('clicked', self.on_delete)
        vbox.pack_start(self.g_delete_lesson, False)
        self.g_move_topic_up = gtk.Button(_("Move topic up"))
        self.g_move_topic_up.connect('clicked', self.on_move_topic_up)
        vbox.pack_start(self.g_move_topic_up, False)
        self.g_move_topic_down = gtk.Button(_("Move topic down"))
        self.g_move_topic_down.connect('clicked', self.on_move_topic_down)
        vbox.pack_start(self.g_move_topic_down, False)
        self.g_move_lesson_up = gtk.Button(_("Move lesson up"))
        self.g_move_lesson_up.connect('clicked', self.on_move_lesson_up)
        vbox.pack_start(self.g_move_lesson_up, False)
        #
        self.g_move_lesson_down = gtk.Button(_("Move lesson down"))
        self.g_move_lesson_down.connect('clicked', self.on_move_lesson_down)
        vbox.pack_start(self.g_move_lesson_down, False)
        #
        scrolledwindow = gtk.ScrolledWindow()
        scrolledwindow.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scrolledwindow.set_size_request(300, 300)
        scrolledwindow.show()
        self.g_treeview = gtk.TreeView()
        scrolledwindow.add(self.g_treeview)
        hbox.pack_start(scrolledwindow, True, True)
        self.g_treeview.show()
        self.g_treeview.set_rules_hint(True)
        self.g_treeview.connect("cursor-changed", self.treeview_cursor_changed)
        self.__create_columns()
        ###
        self.g_fileinfo_box = gtk.VBox()
        self.g_content_vbox.pack_start(self.g_fileinfo_box, False)
        box = gtk.HBox()
        box.set_spacing(8)
        self.g_fileinfo_box.pack_start(box, False)
        box.pack_start(gtk.Label(_("Lesson filename:")), False)
        self.g_filename = gtk.Label()
        box.pack_start(self.g_filename, False)
        box = gtk.HBox()
        box.set_spacing(8)
        self.g_fileinfo_box.pack_start(box, False)
        box.pack_start(gtk.Label(_("Exercise module:")), False)
        self.g_exercisemodule = gtk.Label()
        box.pack_start(self.g_exercisemodule, False)
        ### Deps
        self.g_deps_box = gtk.HBox()
        self.g_deps_box.set_spacing(8)
        self.g_content_vbox.pack_start(self.g_deps_box, True, True)
        vbox = gtk.VBox() # The box with deps
        buttons_sizegroup.add_widget(vbox)
        #hbox.pack_start(vbox, False)
        self.g_deps_box.pack_start(vbox, False, False)
        self.g_new_dependency = gtk.Button(_("New dependency"))
        self.g_new_dependency.connect('clicked', self.on_add_dep)
        self.g_new_dependency.set_sensitive(False)
        vbox.pack_start(self.g_new_dependency, False)
        self.g_delete_dependency = gtk.Button(_("Delete dependency"))
        self.g_delete_dependency.connect('clicked', self.on_delete_dep)
        self.g_delete_dependency.set_sensitive(False)
        vbox.pack_start(self.g_delete_dependency, False)

        # The box with deps tree and heading
        vbox = gtk.VBox()
        #hbox.pack_start(vbox)
        self.g_deps_box.pack_start(vbox, True, True)
        self.g_deps_heading = gtk.Label()
        self.g_deps_heading.set_alignment(0.0, 0.5)
        self.g_deps_heading.set_line_wrap(True)
        self.g_deps_heading.show()
        vbox.pack_start(self.g_deps_heading, False)
        self.g_deps_scrollwin = gtk.ScrolledWindow()
        self.g_deps_scrollwin.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        vbox.pack_start(self.g_deps_scrollwin, False)

        self.g_deps = gtk.TreeView()
        self.g_deps_scrollwin.add(self.g_deps)
        self.g_deps.set_headers_visible(False)
        self.g_deps.connect('cursor-changed', self.on_deps_change)
        #
        renderer = gtk.CellRendererText()
        column = gtk.TreeViewColumn(_("Title"), renderer, text=0)
        self.g_deps.append_column(column)
        self.g_deps_liststore = gtk.ListStore(gobject.TYPE_STRING,
            gobject.TYPE_STRING)
        self.g_deps.set_model(self.g_deps_liststore)
        ###
        self.connect('delete_event', self.on_delete_event)
        self.m_modified = False
        self.show_all()
        self.treeview_cursor_changed()
        self.__load_file2_lock = 0
    def fill_trees_combo(self):
        self.m_trees = [('solfege', 'learningtree.txt', os.path.join(os.getcwd(), 'learningtree.txt'))]
        if self.m_app.m_options.debug:
            self.m_trees.append(('solfege', 'debugtree.txt', os.path.join(os.getcwd(), 'debugtree.txt')))
        try:
            v = os.listdir(os.path.join(gethomedir.get_home_dir(), ".solfege", "learningtrees"))
        except OSError:
            v = []
        for fn in v:
            self.m_trees.append(('user', fn, os.path.join(gethomedir.get_home_dir(), ".solfege", "learningtrees", fn)))
        self.g_trees_liststore.clear()
        for t, fn, txt in self.m_trees:
            self.g_trees_liststore.append((txt,))
    def set_trees_combo(self, place, filename):
        # Set the combo to the correct learning tree.
        self.__load_file2_lock += 1
        idx = 0
        while idx < len(self.m_trees):
            if self.m_trees[idx][0] == place \
                    and self.m_trees[idx][1] == filename:
                self.g_trees.set_active(idx)
            idx += 1
        self.__load_file2_lock -= 1
    def on_learning_tree_combo_changed(self, combobox):
        t, fn, txt = self.m_trees[combobox.get_active()]
        if self.m_tree.m_modified:
            if gu.dialog_yesno(_("The file is not saved. Save before changing?"), self):
                self._do_save(self.m_filename)
        if not self.__load_file2_lock:
            self.load_file2(t, fn)
        self.treeview_cursor_changed()
        cfg.set_list('app/learningtree', [t, fn])
        self.m_app.m_ui.on_learning_tree_changed()
    def treeview_cursor_changed(self, w=None):
        path = self.g_treeview.get_cursor()[0]
        self.g_new_submenu.set_sensitive(path is not None and len(path) < 2)
        self.g_add_lesson.set_sensitive(path is not None and len(path) == 2)
        self.g_delete_lesson.set_sensitive(path is not None)
        self.g_move_lesson_up.set_sensitive(path is not None and len(path) == 3)
        self.g_move_lesson_down.set_sensitive(path is not None and len(path) == 3)
        self.g_move_topic_up.set_sensitive(path is not None and len(path) == 2)
        self.g_move_topic_down.set_sensitive(path is not None and len(path) == 2)
        if path and isinstance(self.m_tree.get(path), basestring):
            self.g_filename.set_text(self.m_app.lessonfile_manager.get(self.m_tree.get(path), 'filename'))
            self.g_exercisemodule.set_text(self.m_app.lessonfile_manager.get(self.m_tree.get(path), 'module'))
        else:
            self.g_filename.set_text("")
            self.g_exercisemodule.set_text("")
            self.g_deps_liststore.clear()
        self.__selected_lesson_changed()
    def __create_columns(self):
        #
        renderer = gtk.CellRendererText()
        renderer.set_property("xalign", 0.0)
        renderer.set_property('editable', True)
        renderer.connect('edited', self.on_title_edited)
        model = self.g_treeview.get_model()
        column = gtk.TreeViewColumn("Name", renderer, text=0)
        self.g_treeview.append_column(column)
        column = gtk.TreeViewColumn("Type", renderer, text=1)
        self.g_treeview.append_column(column)
    def __create_model(self):
        self.m_model = gtk.TreeStore(gobject.TYPE_STRING, gobject.TYPE_STRING)
        for menu in self.m_tree.m_menus:
            def create_for_children(menu, iter):
                if iter is None:
                    toplevel = True
                else:
                    toplevel = False
                #iter = self.m_model.append(iter)
                if toplevel:
                    type_string = _("Menu")
                else:
                    type_string = _("Submenu")
                iter = self.m_model.append(iter, (menu.name, type_string))
                for child in menu.children:
                    if isinstance(child, Menu):
                        create_for_children(child, iter)
                    else:
                        child_iter = self.m_model.append(iter)
                        self.m_model.set(child_iter, 0, self.m_app.lessonfile_manager.get(child, 'title'), 1, _("Lesson"))
            create_for_children(menu, None)
        self.g_treeview.set_model(self.m_model)
    def on_new(self, widget):
        if self.m_tree.m_modified:
            if gu.dialog_yesno(_("The file is not saved. Save before changing?"), self):
                self._do_save(self.m_filename)
        dlg = gtk.Dialog(_("Create new learning tree"), self, buttons=
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
            gtk.STOCK_OK, gtk.RESPONSE_OK))
        dlg.set_default_response(gtk.RESPONSE_OK)
        vbox = gtk.VBox()
        dlg.vbox.pack_start(vbox)
        vbox.set_border_width(18)
        self.g_heading = gtk.Label("<b>%s</b>" % _("Create new learning tree"))
        self.g_heading.set_alignment(0.0, 0.5)
        vbox.pack_start(self.g_heading, False)
        hbox = gtk.HBox()
        hbox.set_spacing(12)
        vbox.pack_start(hbox, False)
        self.g_heading.set_use_markup(True)
        donot_str = _("Do not enter any directory names, only the filename.")
        self.g_filename_description = gtk.Label(donot_str)
        self.g_message = gtk.Label(_("File name:"))
        self.g_entry = gtk.Entry()
        self.g_entry.set_activates_default(True)
        hbox.pack_start(self.g_message, False)
        hbox.pack_start(self.g_entry, False)
        dlg.show_all()
        vbox.pack_start(self.g_filename_description, False)
        while 1:
            ret = dlg.run()
            if ret == gtk.RESPONSE_OK:
                path, filename = os.path.split(self.g_entry.get_text())
                if path or (not filename):
                    self.g_filename_description.set_text(donot_str)
                    self.g_filename_description.show()
                elif os.path.exists(os.path.join(gethomedir.get_home_dir(), '.solfege', 'learningtrees', filename)):
                    self.g_filename_description.set_text(_("The file already exists."))
                    self.g_filename_description.show()
                else:
                    try:
                        if not os.path.exists(os.path.join(gethomedir.get_home_dir(), '.solfege', 'learningtrees')):
                            os.makedirs(os.path.join(gethomedir.get_home_dir(), '.solfege', 'learningtrees'))
                        f = open(os.path.join(gethomedir.get_home_dir(), '.solfege', 'learningtrees', filename), 'w')
                        f.write("""
[ # toplevel list
  2, # learning tree file format version
  [ # start of list of menus
  ], # end of list of menus
{},
100]
""")
                        f.close()
                        self.fill_trees_combo()
                        self.load_file2('user', filename)
                        cfg.set_list('app/learningtree', ['user', filename])
                        self.m_app.m_ui.on_learning_tree_changed()
                    except IOError, e:
                        gu.dialog_ok(_("An error occurred while saving the file:\n%s") % e, self)
                    break
            if ret == gtk.RESPONSE_CANCEL:
                break
        dlg.destroy()
    def on_delete_event(self, *v):
        self.close_window()
    def close_window(self, *v):
        if self.tree_has_changes():
            if gu.dialog_yesno(_("Save changes?"), self):
                self.on_save()
        self.m_app.m_ui.g_learning_tree_editor = None
        self.destroy()
    def on_title_edited(self, renderer, path, txt):
        iter = self.m_model.get_iter_from_string(path)
        p = tuple([int(i) for i in path.split(":")])
        if isinstance(self.m_tree.get(p), basestring):
            return
        self.m_model.set_value(iter, 0, txt)
        self.m_tree.get(p).name = txt
    def on_deps_change(self, treeview_ignore=None):
        self.g_delete_dependency.set_sensitive(True)
    def __selected_lesson_changed(self):
        self.g_deps_liststore.clear()
        path = self.g_treeview.get_cursor()[0]
        self.g_delete_dependency.set_sensitive(False)
        if not path:
            return
        selected = self.m_tree.get(path)
        if isinstance(selected, basestring):
            for x in self.m_tree.iterate_deps_for_id(selected):
                self.g_deps_liststore.append((
                    self.m_app.lessonfile_manager.get(x, 'title'), x))
            self.g_new_dependency.set_sensitive(True)
        else:
            self.g_deps_heading.set_text("")
            self.g_new_dependency.set_sensitive(False)
            return
        lesson_title = self.m_app.lessonfile_manager.get(selected, 'title')
        if list(self.m_tree.iterate_deps_for_id(selected)):
            self.g_deps.show()
            self.g_deps_heading.set_text("<b>%s</b>" % _("Dependencies of the lesson '%s'") % lesson_title)
        else:
            self.g_deps.hide()
            self.g_deps_heading.set_text("<b>%s</b>" % _("No dependencies for the lesson '%s'.") % lesson_title)
        self.g_deps_heading.set_use_markup(True)
        self.g_fileinfo_box.show()
        self.g_deps_box.show()
        self.g_delete_lesson.set_sensitive(True)
    def update_gui(self):
        self.__create_model()
        self.set_title(self.m_filename)
    def on_visibility_entry_changed(self, *v):
        try:
            if self.g_visibility.get_text():
                self.m_tree.m_visibility = int(self.g_visibility.get_text())
        except ValueError, e:
            print >> sys.stderr, e
            print >> sys.stderr, "Setting learning tree visibility to 0."
            self.m_tree.m_visibility = 0
    def _do_save(self, filename):
        """
        Save the tree to 'filename', and return True if sucessful.
        Display message describing the problem and return False is failed.
        """
        try:
            self.m_tree.save(filename)
        except IOError, e:
            gu.dialog_ok(_("An error occurred while saving the file:\n%s") % e,
                    self)
            return False
        return True
    def on_save(self, *v):
        if self._do_save(self.m_filename):
            self.m_app.m_ui.on_learning_tree_changed()
            self._m_orig_deps = copy.deepcopy(self.m_tree.m_deps)
            self._m_orig_menus = copy.deepcopy(self.m_tree.m_menus)
            self._m_orig_visibility = self.m_tree.m_visibility
    def tree_has_changes(self):
        """
        Return True if the tree has unsaved changes.
        """
        return not (self.m_tree.m_menus == self._m_orig_menus
                and self.m_tree.m_deps == self._m_orig_deps
                and self.m_tree.m_visibility == self._m_orig_visibility)
    def on_save_as(self, *v):
        dialog = gtk.FileChooserDialog(_("Save as..."), self,
                                   gtk.FILE_CHOOSER_ACTION_SAVE,
                                   (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                    gtk.STOCK_SAVE_AS, gtk.RESPONSE_ACCEPT))
        dialog.set_default_response(gtk.RESPONSE_ACCEPT)
        run = dialog.run()
        filename = dialog.get_filename()
        dialog.destroy()
        if run == gtk.RESPONSE_ACCEPT:
            if self._do_save(filename):
                self.m_filename = filename
                self.set_title(os.path.basename(self.m_filename))
    def on_new_menu(self, *v):
        dlg = gtk.Dialog(_("Create new menu"), self, buttons=
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
            gtk.STOCK_OK, gtk.RESPONSE_OK))
        dlg.set_default_response(gtk.RESPONSE_OK)
        vbox = gtk.VBox()
        dlg.vbox.pack_start(vbox)
        vbox.set_border_width(18)
        self.g_heading = gtk.Label("<b>%s</b>" % _("Create new menu"))
        self.g_heading.set_alignment(0.0, 0.5)
        vbox.pack_start(self.g_heading, False)
        hbox = gtk.HBox()
        hbox.set_spacing(12)
        vbox.pack_start(hbox, False)
        self.g_heading.set_use_markup(True)
        self.g_message = gtk.Label(_("Menu name:"))
        self.g_entry = gtk.Entry()
        self.g_entry.set_activates_default(True)
        hbox.pack_start(self.g_message, False)
        hbox.pack_start(self.g_entry, False)
        dlg.show_all()
        self.g_explain = gtk.Label()
        vbox.pack_start(self.g_explain, False)
        while 1:
            ret = dlg.run()
            if ret == gtk.RESPONSE_OK:
                n = self.g_entry.get_text()
                if n in [s['name'] for s in self.m_tree.m_menus]:
                    self.g_explain.set_text(_("The menu name is already used."))

                    self.g_explain.show()
                elif not n:
                    self.g_explain.set_text(_("An empty string is not allowed."))
                    self.g_explain.show()
                else:
                    self.m_tree.new_menu(n)
                    self.m_model.append(None, (n, _("Menu")))
                    break
            elif ret == gtk.RESPONSE_CANCEL:
                break
        dlg.destroy()
    def on_new_topic(self, *v):
        menu_idx = self.g_treeview.get_cursor()[0][0]
        dlg = gtk.Dialog(_("New topic"), self, buttons=
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
            gtk.STOCK_OK, gtk.RESPONSE_OK))
        dlg.set_default_response(gtk.RESPONSE_OK)
        vbox = gtk.VBox()
        dlg.vbox.pack_start(vbox)
        vbox.set_border_width(18)
        self.g_heading = gtk.Label("<b>%s</b>" % _("Create new topic"))
        self.g_heading.set_alignment(0.0, 0.5)
        vbox.pack_start(self.g_heading, False)
        hbox = gtk.HBox()
        hbox.set_spacing(12)
        vbox.pack_start(hbox, False)
        self.g_heading.set_use_markup(True)
        self.g_message = gtk.Label(_("Topic name:"))
        self.g_entry = gtk.Entry()
        self.g_entry.set_activates_default(True)
        hbox.pack_start(self.g_message, False)
        hbox.pack_start(self.g_entry, False)
        dlg.show_all()
        self.g_explain = gtk.Label()
        vbox.pack_start(self.g_explain, False)
        while 1:
            ret = dlg.run()
            if ret == gtk.RESPONSE_OK:
                n = self.g_entry.get_text()
                if not n:
                    self.g_explain.set_text(_("An empty string is not allowed."))
                    self.g_explain.show()
                elif n in [c['name'] for c in self.m_tree.m_menus[menu_idx]['children']]:
                    self.g_explain.set_text(_("The menu name is already used."))

                    self.g_explain.show()
                else:
                    self.m_tree.new_topic(menu_idx, n)
                    iter = self.m_model.iter_nth_child(None, menu_idx)
                    new_iter = self.m_model.append(iter, (n, _("Submenu")))
                    self.g_treeview.expand_to_path(self.m_model.get_path(new_iter))
                    self.g_treeview.set_cursor(self.m_model.get_path(new_iter))
                    break
            elif ret == gtk.RESPONSE_CANCEL:
                break
        dlg.destroy()
    def on_move_topic_up(self, *v):
        path = self.g_treeview.get_cursor()[0]
        if path[-1] > 0:
            iter = self.m_model.get_iter(path)
            iter_b = self.m_model.get_iter(path[:-1] + (path[-1]-1,))
            self.m_model.swap(iter, iter_b)
            self.m_tree.move_elem_up(path)
            self.g_treeview.scroll_to_cell(Path(path).prev())
        elif path[-2] > 0:
            path_new = self.m_tree.move_elem_to_prev_menu(path)
            self.m_model.clear()
            self.__create_model()
            self.g_treeview.expand_to_path(path[:-1])
            self.g_treeview.expand_to_path(path_new[:-1])
            self.g_treeview.set_cursor(path_new)
            self.g_treeview.scroll_to_cell(path_new)
    def on_move_topic_down(self, *v):
        path = self.g_treeview.get_cursor()[0]
        iter = self.m_model.get_iter(path)
        iter_b = self.m_model.iter_next(iter)
        if iter_b:
            # Here we move a menu within the parent menu.
            self.m_model.swap(iter, iter_b)
            if not self.m_tree.move_elem_down(path):
                raise Exception("Completely broken in on_move_topic_down. Should never get here.")
            self.g_treeview.scroll_to_cell(Path(path).next())
        else:
            # Get an iter for the next menu. If iter_b becomes None, then
            # we are the last menu.
            iter_b = self.m_model.iter_parent(iter)
            iter_b = self.m_model.iter_next(iter_b)
            if not iter_b:
                return
                if isinstance(self.m_tree.get(p), basestring):
                    return
            path_new = self.m_tree.move_elem_to_next_menu(path)
            self.m_model.clear()
            self.__create_model()
            self.g_treeview.expand_to_path(path)
            self.g_treeview.expand_to_path(path_new[:-1])
            self.g_treeview.set_cursor(path_new)
            gobject.idle_add(self.g_treeview.scroll_to_cell, path_new)
    def on_delete_dep(self, *v):
        path = self.g_treeview.get_cursor()[0]
        dep_path = self.g_deps.get_cursor()[0]
        if dep_path:
            dep_iter = self.g_deps_liststore.get_iter(dep_path)
            dep_id = self.g_deps_liststore.get(dep_iter, 1)[0]
            lesson_id = self.m_tree.get(path)
            self.m_tree.delete_dependency(lesson_id, dep_id)
            # When we remove deps, the topic is resorted, so we have to
            # recreate the model
            self.m_model.clear()
            self.__create_model()
            self.g_treeview.expand_to_path(path)
            path = Path(path)
            path = path.first()
            while self.m_tree.get(path) != lesson_id:
                path = path.next()
            self.g_treeview.set_cursor(path)
            gobject.idle_add(self.g_treeview.scroll_to_cell, path, None, True, 0.5)
            self.__selected_lesson_changed()
    def on_add_dep(self, *v):
        path = self.g_treeview.get_cursor()[0]
        if path:
            dlg = DepsLessonFileDialog(self.m_app, self.m_tree, path)
            ret = dlg.run()
            if ret == gtk.RESPONSE_OK:
                new_dep_id = dlg.get_selected_lesson_id()
                if new_dep_id:
                    added_to_id = self.m_tree.get(path)
                    self.m_tree.add_dependency(added_to_id, new_dep_id)
                    self.m_model.clear()
                    self.__create_model()
                    self.g_treeview.expand_to_path(path)
                    path = Path(path)
                    path = path.first()
                    while self.m_tree.get(path) != added_to_id:
                        path = path.next()
                    self.g_treeview.set_cursor(path)
                    self.g_treeview.scroll_to_cell(path)
            dlg.destroy()
    def on_new_lesson(self, *v):
        path = self.g_treeview.get_cursor()[0]
        dlg = SelectLessonFileDialog(self.m_app, self.m_tree, path)
        dlg.run()
        lesson_id = dlg.get_selected_lesson_id()
        if lesson_id:
            self.m_tree.add_lesson(path, lesson_id)
            iter = self.m_model.get_iter(path)
            self.m_model.append(iter, (self.m_app.lessonfile_manager.get(lesson_id, 'title'), _("Lesson")))
            self._recreate_tree(lesson_id)
        dlg.destroy()
    def _recreate_tree(self, focus_id):
        self.m_model.clear()
        self.__create_model()
        for lesson_id, path in self.m_tree.iterate_all_lessons2():
            if lesson_id == focus_id:
                break
        self.g_treeview.expand_to_path(path)
        self.g_treeview.set_cursor(path)
    def delete_menu(self, path):
        """
        FIXME Deleting a menu will not delete the dependencies between
        the exerises. This mean that the dependencies will exist when you
        add the lessons later.
        """
        if len(path) == 1:
            del self.m_tree.m_menus[path[0]]
        else:
            v = self.m_tree.get(path[:-1])
            del v['children'][path[-1]]
        self.m_model.clear()
        self.__create_model()
        if len(path) > 1:
            self.g_treeview.expand_to_path(path[:-1])
    def on_delete(self, *v):
        path = self.g_treeview.get_cursor()[0]
        if path:
            del_elem = self.m_tree.get(path)
            if isinstance(del_elem, Menu):
                self.delete_menu(path)
                self.treeview_cursor_changed()
                return
            del_id = del_elem
            do_del = True
            if self.m_tree.get_use_count(del_id) == 1:
                if self.m_tree.get_dep_use_count(del_id) > 0:
                    do_del = gu.dialog_yesno(_("Lessons depend on this lesson. Delete anyway, and update the dependency list for lessons that depend on this lesson?"), self)
                    if do_del:
                        self.m_tree.remove_all_deps_of(del_id)
            if do_del:
                self.m_tree.delete_lesson(path)
                self.m_model.remove(self.m_model.get_iter(path))
                self.g_treeview.set_cursor(path)
    def on_move_lesson_up(self, *v):
        path = self.g_treeview.get_cursor()[0]
        if not path:
            return
        if self.m_tree.move_lesson_up(path):
            itera = self.m_model.get_iter(path)
            iterb = self.m_model.get_iter(Path(path).prev())
            self.m_model.swap(itera, iterb)
            self.g_treeview.scroll_to_cell(Path(path).prev())
    def on_move_lesson_down(self, *v):
        path = self.g_treeview.get_cursor()[0]
        if not path:
            return
        if self.m_tree.move_lesson_down(path):
            next_path = Path(path).next()
            iter = self.m_model.get_iter(path)
            self.m_model.swap(iter, self.m_model.get_iter(next_path))
            self.g_treeview.scroll_to_cell(next_path)
    def load_file2(self, place, filename):
        """
        place is 'solfege' or 'user'
        filename is a filename, with no directory part.
        This function will also make sure the g_trees combo is set.
        """
        if place == 'solfege':
            self._load_file(filename)
        else:
            assert place == 'user'
            self._load_file(os.path.join(gethomedir.get_home_dir(), '.solfege', 'learningtrees', filename))
        self.set_trees_combo(place, filename)
        # 
        if place == 'solfege':
            if not os.access(filename, os.W_OK):
                m = gtk.MessageDialog(self, gtk.DIALOG_MODAL, gtk.MESSAGE_INFO,
                    gtk.BUTTONS_CLOSE, _("The default learning tree is write protected in your install. This is normal. If you want to edit a learning tree, you have to select one of the trees stored in .solfege/learningtrees in your home directory."))
                m.run()
                m.destroy()
    def _load_file(self, filename):
        self.m_tree = LearningTree(self.m_app.lessonfile_manager)
        # We use _m_orig_tree just to see if things has changed.
        self.m_filename = filename
        self.m_tree.load(filename)
        self.g_visibility.set_text(str(self.m_tree.m_visibility))
        self._m_orig_deps = copy.deepcopy(self.m_tree.m_deps)
        self._m_orig_menus = copy.deepcopy(self.m_tree.m_menus)
        self._m_orig_visibility = self.m_tree.m_visibility
        self.update_gui()
        #FIXME uncomment when complete
        #if self.g_topics_liststore.get_iter_first():
        #    self.g_topics.set_cursor("0", None)

