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


class History:
    def __init__(self):
        self.m_list = []
        self.m_idx = -1
        self.m_lock = 0
    def add(self, data):
        """
        Add history at the current position. Data after the current
        position is lost.
        """
        if self.m_lock:
            return
        # First we remove if necessary
        self.m_list = self.m_list[:self.m_idx + 1]
        # we don't want duplicate entries in the history
        if not self.m_list or self.m_list[-1][0] != data:
            self.m_list.append([data, None])
            self.m_idx = self.m_idx + 1
    def set_adj_of_current(self, adj):
        if self.m_lock:
            raise Exception("Called set_adj_of_current when locked.")
        self.m_list[self.m_idx][1] = adj
    def back(self):
        if self.m_idx > 0:
            self.m_idx = self.m_idx - 1
    def forward(self):
        if self.m_idx + 1 < len(self.m_list):
            self.m_idx = self.m_idx + 1
    def get_current(self):
        if self.m_idx >= 0:
            return self.m_list[self.m_idx]
    def lock(self):
        "Do not record any history"
        self.m_lock = 1
    def unlock(self):
        self.m_lock = 0
if __name__ == '__main__':
    def test(s):
        try:
            exec("assert " + s)
        except:
            print "test failed: %s" % s
    h = History()
    for i in 'one', 'two', 'three', 'four':
        h.add(i)
    test("h.get_current() == 'four'")
    h.back()
    test("h.get_current() == 'three'")
    h.back()
    test("h.get_current() == 'two'")
    h.add("five")
    test("h.get_current() == 'five'")
    h.back()
    test("h.get_current() == 'two'")
    h.forward()
    test("h.get_current() == 'five'")
