# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2006, 2007  Tom Cato Amundsen
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


class MatrixTable(gtk.VBox):
    def __init__(self, label_style, heading, keys, key_to_pretty_name, dict):
        gtk.VBox.__init__(self)
        label = gtk.Label(heading)
        label.set_name("StatisticsH2")
        label.set_alignment(0.0, 0.0)
        self.pack_start(label, False)
        hbox = gu.bHBox(self, False)
        frame = gtk.Frame()
        hbox.pack_start(frame, False)
        t = gtk.Table()
        frame.add(t)
        for x in range(len(keys)):
            t.attach(gtk.VSeparator(), x*2+1, x*2+2, 0, len(keys)*2)
        for x in range(len(keys)-1):
            t.attach(gtk.HSeparator(), 0, len(keys)*2+1, x*2+1, x*2+2)
        for y in range(len(keys)):
            if label_style == 'progression':
                l = gu.HarmonicProgressionLabel(
                                     key_to_pretty_name(keys[y]), 'left')
            else:
                l = gtk.Label(key_to_pretty_name(keys[y]))
                l.set_alignment(0.0, 0.5)
            t.attach(l, 0, 1, y*2, y*2+1, xpadding=gu.PAD)
            for x in range(len(keys)):
                if keys[x] in dict and keys[y] in dict[keys[x]]:
                    s = str(dict[keys[x]][keys[y]])
                else:
                    s = "0"
                l = gtk.Label(s)
                if x == y:
                    l.set_name('BoldText')
                t.attach(l, x*2+2, x*2+3, y*2, y*2+1, xpadding=gu.PAD)
        self.show_all()


class PercentagesTable(gtk.Frame):
    def __init__(self, statistics):
        gtk.Frame.__init__(self)
        table = gtk.Table()
        self.add(table)

        self.boxdict = {}
        for k, l, x in (('session', _("Session"), 2), ('today', _("Today"), 5),
                     ('last7', _("Last 7 days"), 8), ('total', _("Total"), 11)):
            table.attach(gtk.Label(l), x, x+2, 0, 1)
            b = gtk.VBox()
            table.attach(b, x, x+1, 4, 5)
            self.boxdict[k+'percent'] = b
            b = gtk.VBox()
            table.attach(b, x+1, x+2, 4, 5)
            self.boxdict[k+'count'] = b
        for x in (2, 5, 8, 11):
            table.attach(gtk.Label(_("Percent")), x, x+1, 2, 3)
            table.attach(gtk.Label(_("Count")), x+1, x+2, 2, 3)
        table.attach(gtk.HSeparator(), 0, 13, 1, 2)
        table.attach(gtk.HSeparator(), 0, 13, 3, 4)
        table.attach(gtk.VSeparator(), 1, 2, 0, 6)
        table.attach(gtk.VSeparator(), 4, 5, 0, 6)
        table.attach(gtk.VSeparator(), 7, 8, 0, 6)
        table.attach(gtk.VSeparator(), 10, 11, 0, 6)
        self.boxdict['keys'] = key_box = gtk.VBox()
        table.attach(key_box, 0, 1, 4, 5)
        for box in self.boxdict.values():
            box.set_border_width(gu.PAD_SMALL)
        self.update(statistics)
        self.show_all()
    def update(self, statistics):
        for box in self.boxdict.values():
            for o in box.get_children():
                o.destroy()
        for k in statistics.get_keys():
            # FIXME  Has this bug been fixed? This should NEVER happen.
            if k is None:
                print "WARNING: statistics: key==None. Please, please report to"
                print "bug-solfege@gnu.org if you get this message. Preferably,"
                print "make a copy ~/.solfege so I can debug this."
            if statistics.get_label_style() == 'progression':
                l = gu.HarmonicProgressionLabel(statistics.key_to_pretty_name(k), 'left')
            else:
                l = gtk.Label(statistics.key_to_pretty_name(k))
                l.set_alignment(0.0, 0.5)
            self.boxdict['keys'].pack_start(l)
            for sk, st in (('session', statistics.m_session_stat),
                       ('today', statistics.m_today_stat),
                       ('last7', statistics.m_last7_stat),
                       ('total', statistics.m_total_stat)):

                if statistics.get_percentage_correct(st, k) == 0.0:
                    self.boxdict[sk+'percent'].pack_start(gtk.Label("-"))
                else:
                    self.boxdict[sk+'percent'].pack_start(
                        gtk.Label("%.0f" % statistics.get_percentage_correct(st, k)))
                self.boxdict[sk+'count'].pack_start(
                    gtk.Label(str(statistics.get_num_guess(st, k))))
        self.show_all()

class NewAbstractStatisticsViewer(gtk.ScrolledWindow):
    def __init__(self, statistics, heading):
        gtk.ScrolledWindow.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.vbox = gtk.VBox()
        self.vbox.set_spacing(gu.PAD)
        self.vbox.set_border_width(gu.PAD)
        self.add_with_viewport(self.vbox)
        self.g_heading = gtk.Label(heading)
        self.g_heading.set_alignment(0.0, 0.5)
        self.g_heading.set_name("StatisticsH1")
        self.vbox.pack_start(self.g_heading, False)
        self.m_statistics = statistics
        self.g_tables = gtk.VBox()
        self.g_tables.show()
        self.vbox.pack_start(self.g_tables)
        self.show_all()

class PercentagesStatisticsViewer(NewAbstractStatisticsViewer):
    """
    A statistics viewer that will only display the Percentages table.
    """
    def __init__(self, statistics, heading):
        NewAbstractStatisticsViewer.__init__(self, statistics, heading)
        #self.clear = self.g_p.clear
    def clear(self):
        #UGH why cant we just destroy the children of g_tables??!!
        #for c in self.g_tables.children():
        #    c.destroy()
        self.g_tables.destroy()
        self.g_tables = gtk.VBox()
        self.g_tables.show()
        self.vbox.pack_start(self.g_tables)
    def update(self):
        self.clear()
        self.g_p = PercentagesTable(self.m_statistics)
        self.g_p.show_all()
        self.g_tables.pack_start(self.g_p, False, False)



class StatisticsViewer(NewAbstractStatisticsViewer):
    def __init__(self, statistics, heading):
        NewAbstractStatisticsViewer.__init__(self, statistics, heading)
        self.matrix_dict = {}
    def clear(self):
        #UGH why cant we just destroy the children of g_tables??!!
        #for c in self.g_tables.children():
        #    c.destroy()
        self.g_tables.destroy()
        self.g_tables = gtk.VBox()
        self.g_tables.show()
        self.vbox.pack_start(self.g_tables)
    def update(self):
        self.clear()
        self.g_p = PercentagesTable(self.m_statistics)
        self.g_p.show_all()
        self.g_tables.pack_start(self.g_p, False)
        self.matrix_dict['session'] = MatrixTable(
                                           self.m_statistics.get_label_style(), 
                                           _("Session"),
                                           self.m_statistics.get_keys(1),
                                           self.m_statistics.key_to_pretty_name,
                                           self.m_statistics.m_session_stat)
        self.matrix_dict['today'] = MatrixTable(
                                           self.m_statistics.get_label_style(),
                                           _("Today"),
                                           self.m_statistics.get_keys(1),
                                           self.m_statistics.key_to_pretty_name,
                                           self.m_statistics.m_today_stat)
        self.matrix_dict['last7'] = MatrixTable(
                                           self.m_statistics.get_label_style(),
                                           _("Last 7 days"),
                                           self.m_statistics.get_keys(1),
                                           self.m_statistics.key_to_pretty_name,
                                           self.m_statistics.m_last7_stat)
        self.matrix_dict['total'] = MatrixTable(
                                           self.m_statistics.get_label_style(),
                                           _("Total"),
                                           self.m_statistics.get_keys(1),
                                           self.m_statistics.key_to_pretty_name,
                                           self.m_statistics.m_total_stat)

        for k in ('session', 'today', 'last7', 'total'):
            self.g_tables.pack_start(self.matrix_dict[k], False)

