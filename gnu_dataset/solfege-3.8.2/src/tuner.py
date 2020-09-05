# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007  Tom Cato Amundsen
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

import gtk, gu
import abstract
import soundcard, soundcard.solfege_c_midi
import utils

class Teacher(abstract.Teacher):
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)

class Gui(abstract.Gui):
    def __init__(self, teacher, window):
        abstract.Gui.__init__(self, teacher, window)
        self.g_hz = gu.bLabel(self.practise_box, "")
        self.g_notename = gu.bLabel(self.practise_box, "")
        self.g_cent = gu.bLabel(self.practise_box, "")
    def on_start_practise(self):
        soundcard.solfege_c_midi.dsp_open_record()
        self.__idle_tag = gobject.idle_add(self.update_view)
        #self.__idle_tag = gobject.timeout_add(30, self.update_view)
    def update_view(self):
        freq = soundcard.solfege_c_midi.idle_loop()
        print freq
        notename, cent = utils.freq_to_notename_cent(freq)
        self.g_hz.set_text(str(freq))
        self.g_notename.set_text(notename)
        self.g_cent.set_text(str(cent))
        return True
    def on_end_practise(self):
        gtk.idle_remove(self.__idle_tag)
        #gobject.source_remove(self.__idle_tag)
        soundcard.solfege_c_midi.dsp_close()
