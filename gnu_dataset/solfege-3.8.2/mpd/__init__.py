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

"""
FIXME update.
In addition to the names in this file, only this is public in
the mpd module:

  Classes:
	MusicalPitch
	MusicDisplayer
"""


LOWEST_NOTENAME="c,,,,"
HIGHEST_NOTENAME="g''''''"
from const import FIRST, LAST, ALL

import parser
import soundcard
from musicalpitch import MusicalPitch, InvalidNotenameException
from interval import Interval
from mpdutils import notename_to_int, int_to_notename, int_to_user_notename
from rat import Rat
from track import Track
from _exceptions import MpdException

def music_to_tracklist(music, velocity):
    """
    return a list of tracks, where track[0] use only channel 0,
    track[1] only use channel 1 etc.
    """
    score = parser.parse_to_score_object(music)
    tracks = score.get_midi_events(velocity)
    return tracks

def music_to_track(music, velocity, start=None, end=None):
    score = parser.parse_to_score_object(music)
    tracklist = score.get_midi_events(velocity, start, end)
    track = tracklist[0]
    for x in range(1, len(tracklist)):
        track.merge_with(tracklist[x])
    return track

def play_music(music, tempo, patch, velocity):
    if type(tempo) == type(0):
        bpm = tempo
        nl = 4
    else:
        bpm, nl = tempo
    score = parser.parse_to_score_object(music)
    tracklist = score.get_midi_events(velocity, None, None)
    tracklist[0].prepend_bpm(bpm, nl)
    [track.prepend_patch(patch) for track in tracklist]
    soundcard.synth.play_track(*tracklist)

##################
# midi functions #
##################

def transpose_notename(n, t):
    assert isinstance(n, basestring)
    assert isinstance(t, int)
    # 1 2 sekund
    # 3 4 ters
    # 5 6 kvart
    # 7   kvint
    # 8 9 sekst
    # 10 11 septim
    return int_to_notename(notename_to_int(n) + t)

def compare_notenames(n1, n2):
    return notename_to_int(n1) - notename_to_int(n2)

def select_clef(s):
    """
    argument s is a string with notenames like this: " c e g c' f' g''"
    """
    lowest = HIGHEST_NOTENAME
    highest = LOWEST_NOTENAME
    for n in s.split():
        if compare_notenames(n, lowest) < 0:
            lowest = n
        if compare_notenames(n, highest) > 1:
            highest = n
    if compare_notenames(highest, "c'") < 0:
        return "bass"
    if compare_notenames(lowest, "c'") >= 0:
        return "violin"
    if compare_notenames(highest, "c'") > compare_notenames("c'", lowest):
        return "violin"
    else:
        return "bass"

