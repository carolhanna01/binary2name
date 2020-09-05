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

"""
The Interval class is a class that is used to do math with
intervals and musical pitches. You use this class if you
need to know the difference between a augmented second and
a minor third. If you don't require this you can probably
ise the simpler function mpdutils.int_to_intervalname
"""

import re
import _exceptions

# We have to list all possible interval names here so that xgettext
# can find them and add them to the .pot file for translation. We
# cannot translate the interval quality and interval number separately
# because we don't know how to concat them on other languages.
if 0:
    _("perfect unison")
    _("diminished unison")
    _("augmented unison")
    #
    _("diminished second")
    _("minor second")
    _("major second")
    _("augmented second")
    #
    _("diminished third")
    _("minor third")
    _("major third")
    _("augmented third")
    #
    _("diminished fourth")
    _("perfect fourth")
    _("augmented fourth")
    #
    _("diminished fifth")
    _("perfect fifth")
    _("augmented fifth")
    #
    _("diminished sixth")
    _("minor sixth")
    _("major sixth")
    _("augmented sixth")
    #
    _("diminished seventh")
    _("minor seventh")
    _("major seventh")
    _("augmented seventh")
    #
    _("diminished octave")
    _("perfect octave")
    _("augmented octave")
    # translators: Only translate the word after interval| , and don't include
    # interval| in the translated string. So for Norwegians, translate
    # "interval|diminished" to "forminsket". Do similar for all strings
    # that are preceded with "interval|"
    _("interval|diminished")
    _("interval|pure")
    _("interval|augmented")
    _("interval|minor")
    _("interval|major")
    _("interval|doubly-diminished")
    _("interval|doubly-augmented")


class InvalidIntervalnameException(_exceptions.MpdException):
    def __init__(self, notename):
        _exceptions.MpdException.__init__(self)
        self.m_intervalname = notename
    def __str__(self):
        return _("Invalid interval name: %s") % self.m_intervalname


class Interval:
    """
    The interval is internally a interval less than octave
    pluss n octaves. The data variables:
      m_dir
      m_octave
      m_interval
      m_mod
    should NOT be touched by anyone, except MusicalPitch.__add__
    """
    _nn_to_interval_quality = {
        'p': 'perfect',
        'dd': 'doubly-diminished',
        'd': 'diminished',
        'm': 'minor',
        'M': 'major',
        'a': 'augmented',
        'aa': 'doubly-augmented',
        }
    def __init__(self, iname=None):
        self.m_dir = 1 # value as to be 1 or -1 for initialised obj
        self.m_octave = 0 # 0, 1, 2, 3 etc
        self.m_interval = 0 # 0:unison, 1:seond, ... 6: septim
        # unison:              dim   perfect   aug
        # second:  -2:dim -1:minor 0:major   1:aug
        # third:      dim    minor   major     aug
        # fourth:              dim   perfect   aug
        # fifth:               dim   perfect   aug
        # sixth:      dim    minor   major     aug
        # seventh:    dim    minor   major     aug
        self.m_mod = 0
        if iname:
            self.set_from_string(iname)
    def nn_to_translated_quality(interval_quality):
        """
        Return translated interval quality from internal short string.
        interval_quality can be: dd, d, m, M, a, a, p
        The C locale will return english names, as 'perfect' and 'diminished'
        """
        # Hack, just to xgettext should not grab the string for translation
        xgettext_wont_find_us = _i
        return xgettext_wont_find_us("interval|%s" % Interval._nn_to_interval_quality[interval_quality])
    nn_to_translated_quality = staticmethod(nn_to_translated_quality)
    def number_name(steps):
        try:
            return {
                1: "unison",
                2: "second",
                3: "third",
                4: "fourth",
                5: "fifth",
                6: "sixth",
                7: "seventh",
                8: "octave",
                9: "ninth",
                10: "decim"}[steps]
        except KeyError:
            return "%ith" % steps
    number_name = staticmethod(number_name)
    def errorcheck(self):
        assert 0 <= self.m_interval <= 6
        assert -2 <= self.m_mod <= 1 # should be increased to -3 <= x <= 2
        assert self.m_octave >= 0
        assert self.m_dir in (-1, 1)
    def _set(self, direction, interval, mod, octave):
        self.m_dir = direction
        self.m_interval = interval
        self.m_mod = mod
        self.m_octave = octave
        if __debug__:
            self.errorcheck()
        return self
    def new_from_int(i):
        assert isinstance(i, int)
        new_int = Interval()
        new_int.set_from_int(i)
        return new_int
    new_from_int = staticmethod(new_from_int)
    def set_from_int(self, i):
        """It returns self to allow chaining: set_from_int(4).pretty_name()
        """
        if i < 0:
            self.m_dir = -1
        else:
            self.m_dir = 1
        self.m_octave = abs(i) / 12
        self.m_mod, self.m_interval = (
               (0, 0),          # unison
               (-1, 1), (0, 1), # second
               (-1, 2), (0, 2), # third
               (0, 3),          # fourth
               (-1, 4),         # dim 5, tritonus
               (0, 4),          # fifth
               (-1, 5), (0, 5), # sixth
               (-1, 6), (0, 6))[abs(i) % 12] # seventh
        return self
    def set_from_string(self, s):
        """
        unison  p1
        second  m2 M2
        third   m3 M3
        fourth  p4
        fifth   d5 5
        sixth   m6 M6
        seventh m7 M7
        octave  p8
        none    m9 M9
        decim   m10 M10
        """
        # up or down
        s_orig = s[:]
        s = s.strip()
        if s[0] == "-":
            self.m_dir = -1
            s = s[1:]
        else:
            self.m_dir = 1
        m = re.match("(m|M|d|a|p)(\d+)", s)
        if not m:
            raise InvalidIntervalnameException(s_orig)
        modifier, i = m.groups()
        i = int(i)
        if i <= 7:
            self.m_octave = 0
        else:
            self.m_octave = (i - 1) // 7
        self.m_interval = i - 1 - self.m_octave * 7
        if self.m_interval in (1, 2, 5, 6):
            try:
                self.m_mod = {'d': -2, 'm': -1, 'M': 0, 'a': 1}[modifier]
            except:
                raise InvalidIntervalnameException(s_orig)
        elif self.m_interval in (0, 3, 4):
            try:
                self.m_mod = {'d': -1, 'p': 0, '': 0, 'a': 1}[modifier]
            except:
                raise InvalidIntervalnameException(s_orig)
    def get_intvalue(self):
        if __debug__:
            self.errorcheck()
        return ([0, 2, 4, 5, 7, 9, 11][self.m_interval] + self.m_octave * 12 + self.m_mod) * self.m_dir
    def __str__(self):
        if __debug__:
            self.errorcheck()
        ret = "(Interval %i %imod %io" % (self.m_interval, self.m_mod, self.m_octave)
        if self.m_dir == -1:
            ret = ret + " down)"
        else:
            ret = ret + " up)"
        return ret
    def __repr__(self):
        if self.m_interval in (0, 3, 4):
            return "%s%s" % ({-2: 'dd', -1: 'd', 0: 'p', 1: 'a', 2: 'aa'}[self.m_mod], self.m_interval + 1 + self.m_octave * 7)
        return "%s%s" % ({-2: 'd', -1: 'm', 0: 'M', 1: 'a'}[self.m_mod],  (self.m_interval + 1 + self.m_octave * 7))
    def __eq__(self, interval):
        return self.m_dir == interval.m_dir \
            and self.m_octave == interval.m_octave \
            and self.m_mod == interval.m_mod \
            and self.m_interval == interval.m_interval
    def get_number_name(self):
        """
        Return the translated general name of the interval, like second, third
        etc. (major, minor etc.)
        """
        return _(self.number_name(self.steps()))
    def get_quality_short(self):
        """
        Return a short string telling the quality.
        This is a non-translated short string mostly used
        internally in the program.
        """
        if self.m_interval in (0, 3, 4):
            return {-2: "dd",
                    -1: "d",
                     0: "p",
                     1: "a",
                     2: "aa"}[self.m_mod]
        else:
            assert self.m_interval in (1, 2, 5, 6)
            return {-2: "d",
                    -1: "m",
                     0: "M",
                     1: "a"}[self.m_mod]
    def get_quality(self):
        """
        Return the non-translated interval quality.
        """
        return self._nn_to_interval_quality[self.get_quality_short()]
    def get_cname(self):
        """
        Return the full untranslated interval name, both the number and quality.
        """
        return "%s %s" % (self.get_quality(), self.number_name(self.steps()))
    def get_name(self):
        """
        Return the full translated intervalname, both the number and quality.
        """
        return _(self.get_cname())
    def steps(self):
        return self.m_octave * 7 + self.m_interval + 1



