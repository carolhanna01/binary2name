# GNU Solfege - free ear training software
# Copyright (C) 2006, 2007 Tom Cato Amundsen
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

# Lesson file related GUI code.


import gtk
import locale
import lessonfile

class ResolveLessonIdCrashDlg(gtk.Dialog):
    def __init__(self, lessonfile_manager, lesson_id):
        gtk.Dialog.__init__(self, _("Resolve lesson_id crash"), None,
            buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                     gtk.STOCK_OK, gtk.RESPONSE_OK))
        vbox = gtk.VBox()
        vbox.set_border_width(12)
        self.vbox.pack_start(vbox)
        msg1 = gtk.Label("<b>%s</b>" % _("Lesson_id crash"))
        msg1.set_alignment(0.0, 0.5)
        msg1.set_use_markup(True)
        vbox.pack_start(msg1, False)
        msg2 = gtk.Label(_("The lesson_id has to be unique for each lesson file. Please select which lesson file shall keep the current lesson_id. The preselected file is Solfege's educated guess. Press 'Cancel' to postpone the decision."))
        msg2.set_alignment(0.0, 0.5)
        msg2.set_line_wrap(True)
        vbox.pack_start(msg2)
        self.radio_dict = {}
        for info in lessonfile_manager.get_lesson_file_info(lesson_id):
            if self.radio_dict.keys():
                g = self.radio_dict[self.radio_dict.keys()[0]]
            else:
                g = None
            ss = "%s %s" % (info['filename'], info['timestr'])
            if not isinstance(ss, unicode):
                ss = ss.decode(locale.getpreferredencoding(), 'replace')
            radiobutton = gtk.RadioButton(g, ss)
            radiobutton.show()
            radiobutton.set_data('filename', info['filename'])
            self.radio_dict[info['mtime']] = radiobutton
            vbox.pack_start(radiobutton)
        self.radio_dict[min(self.radio_dict.keys())].set_active(True)
        self.show_all()


def handle_lesson_id_crash(lessonfile_manager):
    for lesson_id in lessonfile_manager.iterate_duplicated_lesson_id():
        dlg = ResolveLessonIdCrashDlg(lessonfile_manager, lesson_id)
        ret = dlg.run()
        if ret == gtk.RESPONSE_OK:
            for b in dlg.radio_dict.values():
                # Create a new lesson_id and parse them into m_uiddb
                if not b.get_active():
                    f = lessonfile.LessonIdParser(b.get_data('filename'))
                    f.new_lesson_id()
                    lessonfile_manager.parse_into_uiddb(b.get_data('filename'))
                # and remove the duplicate entries from the entry we
                # want to keep.
                else:
                    lessonfile_manager.delete_not_fn(lesson_id, b.get_data('filename'))
        elif ret == gtk.RESPONSE_CANCEL:
            lessonfile_manager.ignore_duplicates_with_lesson_id(lesson_id)
        dlg.destroy()


