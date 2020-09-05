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

"""
>>> d1=Duration(4, 0, Rat(1, 1))
>>> d2=Duration(4, 1, Rat(1, 1))
>>> d1==d2
False
>>> d1.get_rat_value()
(Rat 1/4)
>>> d2.get_rat_value()
(Rat 3/8)
>>> d3=Duration(4, 2, Rat(2, 3))
>>> d3.get_rat_value()
(Rat 7/24)
"""

from rat import Rat

class Duration:
    def __init__(self, nh, dots, tuple=Rat(1, 1)):
        self.m_nh = nh
        self.m_dots = dots
        self.m_tuple = tuple
    def __cmp__(self, B):
        """
        >>> A=Duration(4, 1, Rat(1, 1))
        >>> B=Duration(4, 1, Rat(1, 1))
        >>> C=Duration(2, 2, Rat(4, 7))
        >>> A==None, A==B, A==C
        (False, True, False)
        >>> (cmp(A, C), cmp(A, B))
        (-1, 0)
        """
        if not B:
            return -1
        return cmp(self.get_rat_value(), B.get_rat_value())
    def get_rat_value(self):
        """
        >>> A=Duration(4, 1, Rat(1, 1))
        >>> B=Duration(4, 2, Rat(3, 5))
        >>> A.get_rat_value(), B.get_rat_value()
        ((Rat 3/8), (Rat 21/80))
        """
        d = Rat(1, self.m_nh)
        if self.m_dots > 0:
            d = d + Rat(1, self.m_nh * 2)
        if self.m_dots > 1:
            d = d + Rat(1, self.m_nh * 4)
        return d * self.m_tuple
    def clone(self):
        return Duration(self.m_nh, self.m_dots, self.m_tuple)
    def __str__(self):
        return "(Duration:%s:%idot:%s)" % (self.m_nh, self.m_dots, self.m_tuple)

