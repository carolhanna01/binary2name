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

import urlparse
import gtk
import re, math
import mpd
import const
import random
import soundcard, cfg

def mangle_email(email):
    email = email.replace("@", " ,, ")
    email = email.replace(".", " ,, ")
    return email

def play_tone(midi_int):
    soundcard.play_note(cfg.get_int('config/preferred_instrument'),
                        4, midi_int,
                        cfg.get_int('config/preferred_instrument_velocity'))

def int_to_intervalname(i, shortname=None, updown=None):
    if shortname:
        n = const.short_interval_name[abs(i)]
    else:
        n = const.int_interval[abs(i)]
    if updown:
        if i > 0:
            n = "%s %s" % (n, _("up"))
        elif i < 0:
            n = "%s %s" % (n, _("down"))
    return n


def filter_intervals_within_range(lowest, highest, irange):
    assert isinstance(lowest, basestring)
    assert isinstance(highest, basestring)
    lowest = mpd.notename_to_int(lowest)
    highest = mpd.notename_to_int(highest)
    i = highest - lowest
    return filter(lambda x, i=i: abs(x) <= i, irange)

def random_interval(tonika, lowest, highest, irange):
    """
    Return an int representing the interval.
    Return None if it is not possible to create an interval.
    """
    if isinstance(tonika, mpd.MusicalPitch):
        tonika = tonika.semitone_pitch()
    assert isinstance(tonika, int)
    assert type(lowest) == type(highest)
    if isinstance(lowest, basestring):
        lowest = mpd.notename_to_int(lowest)
    if isinstance(highest, basestring):
        highest = mpd.notename_to_int(highest)
    assert isinstance(lowest, int)
    assert isinstance(highest, int)
    assert lowest <= highest
    assert isinstance(irange, list)
    v = []
    for i in irange:
        if lowest <= tonika + i <= highest:
            v.append(i)
    if not v:
        return None
    return random.choice(v)


def random_tonika_and_interval(lowest, highest, irange):
    """ Return a tuple (tonika, interval) of types (MusicalPitch, int).
    Return (None, None) if it is not possible to make an interval
    because of a conflict between the lowest/highest and the available
    intervals

    lowest
    highest  an integer or a string representing a notename
    irange   list of integers representing intervals. 1 is minor
             second up, -2 is major second down
    """
    assert type(lowest) == type(highest)
    if isinstance(lowest, basestring):
        lowest = mpd.notename_to_int(lowest)
    if isinstance(highest, basestring):
        highest = mpd.notename_to_int(highest)
    assert isinstance(lowest, int)
    assert isinstance(highest, int)
    assert lowest <= highest
    assert isinstance(irange, list)
    # first we find out what is the largest interval we can have
    i = highest - lowest
    # then filter irange to only use intervals <= i
    v = filter(lambda x, i=i: abs(x) <= i, irange)
    if not v:
        return None, None
    interval = random.choice(v)
    # then, using that interval, make a list of possible tonikas
    tl = []
    for t in range(lowest, highest + 1):
        if lowest <= t + interval <= highest:
            tl.append(t)
    tonika = mpd.MusicalPitch.new_from_int(random.choice(tl))
    return tonika, interval


def adjust_low_high_to_irange(lowest, highest, irange):
    """
    lowest
    highest  string representing a notename
    irange   list of integers representing intervals. 1 is minor
             second up, -2 is major second down
    return tuple with two integers
    """
    assert isinstance(lowest, basestring)
    assert isinstance(highest, basestring)
    L = mpd.notename_to_int(lowest)
    H = mpd.notename_to_int(highest)
    # find the largest interval that can be asked for
    r = max(abs(max(irange)), abs(min(irange)))
    # adjust the lowest and highest note, if the intervals are larger than
    # the range we should try to stay within
    if H - L < r:
        H = H + (r - (H - L)) / 2
        L = L - (r - (H - L))
    return L, H

def un_escape_url_string(s):
    r = re.compile("(%([0-9A-F][0-9A-F]))")
    m = r.search(s)
    def f(m):
        return chr(eval("0x%s" % m.groups()[1]))
    return r.sub(f, s)

def _str_to_dict(s):
    D = {}
    if s:
        V = s.split(";")
        for e in V:
            n, v = e.split("=")
            D[n.strip()] = v.strip()
    for k in D:
        D[k] = un_escape_url_string(D[k])
    return D


def get_modifier(s):
    m = (('<ctrl>', gtk.gdk.CONTROL_MASK),
         ('<shift>', gtk.gdk.SHIFT_MASK),
         ('<alt>', gtk.gdk.MOD1_MASK))
    for mod, mask in m:
        if s.startswith(mod):
            return mask, s[len(mod):]
    return None, s

def parse_key_string(string):
    if not string:
        return None, None
    mod = 0
    m, s = get_modifier(string)
    while m:
        mod = mod + m
        m, s = get_modifier(s)
    if len(s) == 1:
        return mod, ord(s)
    else:
        return mod, gtk.keysyms.__dict__[s]

def freq_to_notename_cent(freq):
    e = 440.0
    if e > freq:
        while e > freq:
            e = e / 2
    else:
        while e < freq/2:
            e = e * 2
    d = freq / e
    v = 12 * math.log(d) / math.log(2)
    i = int(v)
    cent = (v-i) * 100
    n = ('a', 'ais', 'b', 'c', 'cis', 'd', 'dis', 'e', 'f', 'fis', 'g', 'gis')
    if cent > 50:
        return n[(i+1) % 12], cent-100
    return n[int(v)], (v-int(v)) * 100

def exercise_name_to_module_name(name):
    # it is just plain luck that this works...
    return name.replace('-', '')

def compare_version_strings(A, B):
    """
    Works with version strings like 1, 1.0, 1.1.3, 1.4.3.2
    Returns:
        -1 if A < B
         0 if A == B
         1 if A > B
    """
    if A == B == "":
        return 0
    elif A == "":
        return -1
    elif B == "":
        return 1
    av = map(lambda s: int(s), A.split("."))
    bv = map(lambda s: int(s), B.split("."))
    x = 0
    while len(av) > x < len(bv):
        if av[x] > bv[x]:
            return 1
        elif av[x] < bv[x]:
            return -1
        x = x + 1
    if len(av) > len(bv):
        return 1
    elif len(av) < len(bv):
        return -1
    return 0

class Url:
    def __init__(self, href):
        self.href = href
        self.protocol, loc, path, params, query, self.anchor = urlparse.urlparse(self.href)
        if not self.protocol:
            self.filename = path
        elif self.protocol == 'solfege':
            v = path.split("/")
            self.action = v[0]
            self.lessonfile = "/".join(v[1:])
            if len(v) > 2 and self.lessonfile and '?' in self.lessonfile:
                self.lessonfile, self.config = self.lessonfile.split('?', 1)
            else:
                self.config = {}
            if self.config:
                self.config = _str_to_dict(self.config)

