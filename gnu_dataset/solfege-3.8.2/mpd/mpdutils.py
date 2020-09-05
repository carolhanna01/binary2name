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
Only utility functions and classes that are private to the mpd module
should go into this file.
"""

from musicalpitch import MusicalPitch
import _exceptions

class UnknownClefException(_exceptions.MpdException):
    def __init__(self, clef):
        _exceptions.MpdException.__init__(self)
        self.m_clef = clef
    def __str__(self):
        return "'%s' is not a valid clef. Maybe a bug in your lessonfile?" % self.m_clef

def int_to_notename(i):
    p = MusicalPitch()
    p.set_from_int(i)
    return p.get_octave_notename()

def int_to_user_notename(i):
    p = MusicalPitch()
    p.set_from_int(i)
    return p.get_user_octave_notename()

def notename_to_int(n):
    p = MusicalPitch.new_from_notename(n)
    return p.semitone_pitch()

def semitonepitch_to_ylinepos(i, clef):
    n = MusicalPitch.new_from_int(i)
    return steps_to_ylinepos(n.steps(), clef)

def steps_to_ylinepos(steps, clef):
    try:
        return {'treble': 13, #treble and violin are two names for the same clef
            'violin': 13,
            'subbass': -1,
            'bass': 1,
            'baritone': 3,
            'varbaritone': 3,
            'tenor': 5,
            'alto': 7,
            'mezzosoprano': 9,
            'soprano': 11,
            'french': 15}[clef] - steps
    except KeyError:
        raise UnknownClefException(clef)

def notename_to_ylinepos(n, clef):
    n = MusicalPitch.new_from_notename(n)
    i = n.steps()
    return steps_to_ylinepos(i, clef)

def an_to_ylinepos(an, clef):
    if an[-2:] == 'es':
        l = 3
        h = -3
    else:
        l = 1
        h = -5
    i = notename_to_ylinepos(an, clef)
    while i > l:
        an = an + "'"
        i =  notename_to_ylinepos(an, clef)
    while i < h:
        an = an + ","
        i = notename_to_ylinepos(an, clef)
    return i

def key_to_accidentals(key):
    i = ['aeses', 'eeses', 'beses', 'fes', 'ces', 'ges', 'des', 'aes',
         'ees', 'bes', 'f', 'c', 'g', 'd', 'a', 'e', 'b', 'fis', 'cis',
         'gis', 'dis', 'ais', 'eis', 'bis'].index(key[0])-11
    if key[1] == 'minor':
        i = i - 3
    if i > 0:
        r = ['fis', 'cis', 'gis', 'dis', 'ais', 'eis',
             'bis', 'fis', 'cis', 'gis', 'dis'][:i]
        m = 'is'
    elif i < 0:
        r = ['bes', 'ees', 'aes', 'des', 'ges', 'ces',
             'fes', 'bes', 'ees', 'aes', 'des'][:-i]
        m = 'es'
    else:
        r = []
    retval = []
    for a in r:
        if a not in retval:
            retval.append(a)
        else:
            del retval[retval.index(a)]
            retval.append(a+m)
    return retval

