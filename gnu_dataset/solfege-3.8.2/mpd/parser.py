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
REMEMBER: down is positive, up is negative.

All voices begin at the beginning of the staff. It is
not possible to split a voice in two like in Lilypond.

The parser will not handle fis and f in the same octave on one stem.

Rules:
 * The clef has to be set in the beginning of the staff, and it cannot
   be changed.
 * It can be different timesignatures in different staffs.
 * \key has to come before \time

The parser does not care if you have correct number of notes in a bar.
To get bar lines you have to insert a '|'
"""

import weakref

import re
from duration import Duration
from musicalpitch import MusicalPitch
from engravers import *
from requests import *
from rat import Rat
import const
import soundcard
import mpdutils
import _exceptions

class ParseError(_exceptions.MpdException):
    def __init__(self, toc):
        self.m_toc = toc
    def __str__(self):
        return "Parse error: %s" % self.m_toc


def musicalpitch_relative(first, second):
    """
    think:  \relative c'{ first second }
    Tritonus handling is the same as GNU Lilypond

    I placed here instead of in MusicalPitch since it is only used
    once in parse_to_score_object and I don't think anyone need this
    in MusicalPitch.
    """
    assert isinstance(first, MusicalPitch)
    assert isinstance(second, MusicalPitch)
    n1 = second.clone()
    n1.m_octave_i = first.m_octave_i
    n2 = n1.clone()
    if n1 < first:
        n2.m_octave_i += 1
    else:
        n1.m_octave_i -= 1
    if n2.steps() - first.steps() < first.steps() - n1.steps():
        # we go  up
        n2.m_octave_i += second.m_octave_i
        return n2
    else:
        # we go down
        n1.m_octave_i += second.m_octave_i
        return n1


class TimeSignature:
    def __init__(self, num, den):
        self.m_num = num
        self.m_den = den

class VoiceColObj:
    def __init__(self):
        self.m_rest = None
        # the key for this dictionary is semitonepitch for the notehead
        self.m_music = {}
        self.m_beaminfo = None
        self.m_tupletinfo = None
        self.m_duration = None
        self.m_stempos = 0

class StaffColObj:
    def __init__(self):
        self.m_timesignature = None
        self.m_keysignature = None
        self.m_clef = None
        self.m_barline = None
        self.m_ledger_up = 0
        self.m_ledger_down = 0


class ScoreColObj:
    """
    """
    def __init__(self):
        self.m_timesignature_obj = None
        # These variables is used by Score to remember how wide the
        # different elements of a score column is.
        self.m_clef = 0
        self.m_keysignature = 0
        self.m_barline = 0
        self.m_timesignature = 0
        self.m_accidentals = 0
        self.m_music = 0
        self.m_leftshift = 0
        self.m_rightshift = 0

class Score:
    def __init__(self):
        # m_timeposdict is a dictionary of ScoreColObj objects, one
        # object for each column that has music or rests. The keys
        # for the dict is timepos.
        self.m_timeposdict = {}
        self.m_staffs = []
    def announce_timepos(self, timepos):
        if timepos not in self.m_timeposdict:
            self.m_timeposdict[timepos] = ScoreColObj()
    def get_engravers(self, fontsize):
        tv = self.m_timeposdict.keys()
        tv.sort()
        return self._generate_engravers(tv, fontsize)
    def get_first_engravers(self, fontsize):
        tv = self.m_timeposdict.keys()
        tv.sort()
        return self._generate_engravers([tv[0]], fontsize)
    def _generate_engravers(self, tv, fontsize):
        V = []
        clef = None
        self.m_spanner_list = []
        self.m_stem_list = []
        for staff in self.m_staffs:
            staff.refill_accidentals_info(("c", "major"))
            key = ('c', 'major')
            se = []
            V.append(se)
            ###################
            beam = None
            tuplet = None
            for voice in staff.m_voice_list:
                for timepos in tv:
                    if timepos not in staff.m_coldict:
                        continue
                    if timepos not in voice.m_coldict:
                        continue
                    if staff.m_coldict[timepos].m_clef:
                        clef = staff.m_coldict[timepos].m_clef
                    ########
                    # stems
                    v = []
                    for n in voice.m_coldict[timepos].m_music.values():
                        v.append(mpdutils.steps_to_ylinepos(n.m_pitch.steps(), clef))
                    if v and (voice.m_coldict[timepos].m_music.values()[0].m_duration.m_nh > 1):
                        v.sort()
                        if voice.m_coldict[timepos].m_beaminfo == 'start':
                            beam = BeamEngraver(fontsize)
                            self.m_spanner_list.append(beam)
                            se.append(beam)
                        if beam and not voice.m_coldict[timepos].m_beaminfo:
                            beam = None
                        se.append(StemEngraver(timepos, fontsize, v,
                                  voice.m_coldict[timepos],
                                  beam is not None))
                        self.m_stem_list.append(se[-1])
                        if beam:
                            beam.add_stem(se[-1])
                    ################################
                    # tuplets are created per-voice
                    ################################
                    if voice.m_coldict[timepos].m_tupletinfo:
                        if voice.m_coldict[timepos].m_tupletinfo == 'continue':
                            tuplet.add_stem(se[-1])
                        else:
                            tuplet = TupletEngraver(fontsize, voice.m_coldict[timepos].m_tupletinfo)
                            tuplet.add_stem(se[-1])
                            self.m_spanner_list.append(tuplet)
                            se.append(tuplet)
                    else:
                        #if tuplet is not None: tuplet = None
                        tuplet = None
            ###################
            # this loop takes care of stuff that is decided on a per-staff-basis
            for timepos in tv:
                if timepos not in staff.m_coldict:
                    continue
                # clef
                if staff.m_coldict[timepos].m_clef:
                    clef = staff.m_coldict[timepos].m_clef
                    se.append(ClefEngraver(timepos, fontsize,
                                           staff.m_coldict[timepos].m_clef))
                    self.m_timeposdict[timepos].m_clef \
                       = max(self.m_timeposdict[timepos].m_clef, se[-1].get_width())
                #key signature
                if staff.m_coldict[timepos].m_keysignature:
                    se.append(KeySignatureEngraver(timepos, fontsize, key,
                                     staff.m_coldict[timepos].m_keysignature, clef))
                    self.m_timeposdict[timepos].m_keysignature \
                       = max(self.m_timeposdict[timepos].m_keysignature,
                             se[-1].get_width())
                    key = staff.m_coldict[timepos].m_keysignature
                    staff.refill_accidentals_info(key)
                # barline
                if staff.m_coldict[timepos].m_barline:
                    se.append(BarlineEngraver(timepos, fontsize, "|"))
                    self.m_timeposdict[timepos].m_barline \
                       = max(self.m_timeposdict[timepos].m_barline,
                             se[-1].get_width())
                    staff.refill_accidentals_info(key)
                # time signature
                if self.m_timeposdict[timepos].m_timesignature_obj:
                    se.append(TimeSignatureEngraver(timepos, fontsize,
                       self.m_timeposdict[timepos].m_timesignature_obj))
                    self.m_timeposdict[timepos].m_timesignature \
                       = max(self.m_timeposdict[timepos].m_timesignature,
                             se[-1].get_width())
                ##############
                # accidentals
                v = {}
                for voice in staff.m_voice_list:
                    if timepos not in voice.m_coldict:
                        continue
                    for music in voice.m_coldict[timepos].m_music.itervalues():
                        e = staff.needed_accidental(music.m_pitch)
                        if e is not None:
                            v[mpdutils.steps_to_ylinepos(music.m_pitch.steps(), clef)] = e
                if v:
                    se.append(AccidentalsEngraver(timepos, fontsize, v))
                    self.m_timeposdict[timepos].m_accidentals \
                       = max(self.m_timeposdict[timepos].m_accidentals,
                            se[-1].get_width())
                ################################
                # xshift noteheads that need it

                # first we have to find out what voice has the highest tones,
                # because we have to lay out the noteheads in the highest
                # voice first.
                voicelist = []
                if len(staff.m_voice_list) == 1:
                    voicelist = [staff.m_voice_list[0]]
                else:
                    def f(A, B, timepos=timepos):
                        if timepos not in B.m_coldict:
                            return 1
                        if timepos not in A.m_coldict:
                            return 1
                        return cmp(B.m_coldict[timepos].m_music.values()[0].m_pitch.semitone_pitch(), A.m_coldict[timepos].m_music.values()[0].m_pitch.semitone_pitch())
                        return 1
                    staff.m_voice_list.sort(f)
                    voicelist = staff.m_voice_list
                voice1_lowest_ylinepos = None
                for voice in voicelist:
                    if timepos not in voice.m_coldict:
                        continue
                    # nd:
                    # * the keys in the dictionary is the position on the
                    #   staff the notehead will have. 0 is the middle line,
                    #   1 is below the middle line, -2 is on the line above
                    #   the middle line.
                    # * the values is the requests.MusicRequest that represents
                    #   the notehead.
                    nd = {}
                    for mm in voice.m_coldict[timepos].m_music.itervalues():
                        nd[mpdutils.steps_to_ylinepos(mm.m_pitch.steps(), clef)] = mm
                    if nd == {}:
                        # nd == {} when there are not noteheads, for example
                        # when there is a rest here.
                        break
                    v = nd.keys()
                    v.sort()
                    if voice.m_coldict[timepos].m_stemdir == const.UP:
                        # if we are stemUp, we assume this is the first voice
                        # to be layed out.
                        voice1_lowest_ylinepos = v[-1]
                        v.reverse()
                        for n in range(1, len(v)):
                            if nd[v[n]].m_pitch.steps() == nd[v[n-1]].m_pitch.steps()+1 and (not nd[v[n-1]].m_shift):
                                nd[v[n]].m_shift = 1
                                self.m_timeposdict[timepos].m_rightshift = 1
                    else:
                        # the first notehead will decide where to place
                        # the stem
                        stempos = 0 # default
                        if voice1_lowest_ylinepos is not None and v[0] == voice1_lowest_ylinepos + 1:
                            stempos = 1 # stem moved right
                            voice.m_coldict[timepos].m_stempos = 1
                            nd[v[0]].m_shift = 1
                        for n in range(1, len(v)):
                            if nd[v[n]].m_pitch.steps()+1 == nd[v[n-1]].m_pitch.steps() and nd[v[n-1]].m_shift == stempos:
                                nd[v[n]].m_shift = stempos - 1
                                if stempos == 0:
                                    self.m_timeposdict[timepos].m_leftshift = 1
                            else:
                                nd[v[n]].m_shift = stempos
                #####################################
                # create notehead and rest engravers
                for voice in staff.m_voice_list:
                    if timepos not in voice.m_coldict:
                        continue
                    for music in voice.m_coldict[timepos].m_music.itervalues():
                        if music.m_duration.m_nh < 2:
                            head = const.NOTEHEAD_0
                        elif music.m_duration.m_nh > 2:
                            head = const.NOTEHEAD_2
                        else:
                            head = const.NOTEHEAD_1
                        ylinepos = mpdutils.steps_to_ylinepos(music.m_pitch.steps(), clef)
                        se.append(NoteheadEngraver(timepos, fontsize, music.m_shift,
                             ylinepos, head, music.m_duration.m_dots,
                             music.m_pitch.semitone_pitch(),
                             voice.m_coldict[timepos]))
                        self.m_timeposdict[timepos].m_music \
                           = max(self.m_timeposdict[timepos].m_music, se[-1].get_width())
                    l = self.m_timeposdict[timepos].m_leftshift
                    r = self.m_timeposdict[timepos].m_rightshift
                    self.m_timeposdict[timepos].m_music \
                       = max(self.m_timeposdict[timepos].m_music, (1+l+r)*dimentions[fontsize].xshift) + 2
                    #
                    if voice.m_coldict[timepos].m_rest:
                        se.append(RestEngraver(timepos, fontsize, 0,
                              voice.m_coldict[timepos].m_rest.m_duration))
                        self.m_timeposdict[timepos].m_music \
                           = max(self.m_timeposdict[timepos].m_music, se[-1].get_width())
                ####################################
                # Find out if wee need ledger lines
                up = 0
                down = 0
                for voice in staff.m_voice_list:
                    if timepos not in voice.m_coldict:
                        continue
                    for music in voice.m_coldict[timepos].m_music.itervalues():
                        ypos = music.m_pitch.steps()
                        ypos = mpdutils.steps_to_ylinepos(ypos, clef)
                        if up > ypos < -5:
                            up = ypos
                        if down < ypos > 5:
                            down = ypos
                ###############################
                # Create ledger line engravers
                if timepos in staff.m_coldict:
                    if up:
                        up = - up / 2 - 2
                    else:
                        up = 0
                    if down:
                        down = down / 2 - 2
                    else:
                        down = 0
                    e = LedgerLineEngraver(timepos, fontsize, up, down)
                    se.append(e)
            for voice in staff.m_voice_list:
                for tie in voice.m_ties:
                    sh1 = tie[3].m_shift
                    sh2 = tie[4].m_shift
                    se.append(TieEngraver(fontsize, tie[0], tie[1],
                                          sh1, sh2,
                                          mpdutils.steps_to_ylinepos(tie[2][0], clef)))
        #########################################
        xv = {}
        p = 0
        pv = self.m_timeposdict.keys()
        pv.sort()
        class Dummy:
            pass
        for timepos in pv:
            xv[timepos] = Dummy()
            xv[timepos].m_clef = p
            p += self.m_timeposdict[timepos].m_clef
            xv[timepos].m_keysignature = p
            p += self.m_timeposdict[timepos].m_keysignature
            xv[timepos].m_barline = p
            p += self.m_timeposdict[timepos].m_barline
            xv[timepos].m_timesignature = p
            p += self.m_timeposdict[timepos].m_timesignature
            xv[timepos].m_accidentals = p
            p += self.m_timeposdict[timepos].m_accidentals
            if self.m_timeposdict[timepos].m_leftshift:
                p += 10
            xv[timepos].m_music = p
            p += self.m_timeposdict[timepos].m_music
        for ev in V:
            for e in ev:
                #FIXME all engravers should take two arguments, i think...
                if isinstance(e, AccidentalsEngraver):
                    e.set_xpos(xv, self.m_timeposdict)
                else:
                    e.set_xpos(xv)
        for e in self.m_spanner_list:
            e.do_layout()
        # we delete it because it is not used any more, and to help avoid
        # circular references
        del self.m_spanner_list
        for e in self.m_stem_list:
            e.calc_xpos()
        del self.m_stem_list
        return V
    def add_staff(self):
        staff = Staff(self)
        self.m_staffs.append(staff)
        return staff
    def get_midi_events(self, velocity, start=None, end=None):
        kv = self.m_timeposdict.keys()
        kv.sort()
        if start is None and end is not None:
            return self._generate_midi_events(
                     filter(lambda i, end=end: i < end, kv), velocity,
                     soundcard.Track)
        elif start is not None and end is None:
            return self._generate_midi_events(
                     filter(lambda i, start=start: i >= start, kv), velocity,
                     soundcard.Track)
        elif start is None and end is None:
            return self._generate_midi_events(kv, velocity, soundcard.Track)
        else:
            assert start is not None and end is not None
            return self._generate_midi_events(
               filter(lambda i, start=start, end=end: start <= i < end, kv), velocity,
               soundcard.Track)
    def get_first_beat_midi_events(self):
        kv = self.m_timeposdict.keys()
        kv.sort()
        return self._generate_midi_events([kv[0]], velocity, soundcard.Track)
    def get_last_beat_midi_events(self):
        kv = self.m_timeposdict.keys()
        kv.sort()
        return self._generate_midi_events([kv[-1]], velocity, soundcard.Track)
    def get_midi_events_as_percussion(self, velocity):
        kv = self.m_timeposdict.keys()
        kv.sort()
        return self._generate_midi_events(kv, velocity, soundcard.PercussionTrack)
    def _generate_midi_events(self, kv, velocity, tracktype):
        """
        kv is a list of rat.Rat that tell the timepos for all the tones
        we should generate midi events for.
        Return a list of tracks, one track for each voice.
        """
        track_list = []
        for staff in self.m_staffs:
            for voice in staff.m_voice_list:
                track_list.append(voice.generate_track_for_voice(staff, kv, velocity, tracktype))
        return track_list


class Staff:
    def __init__(self, score):
        self.w_score = weakref.ref(score)
        self.m_coldict = {}
        self.m_voice_list = []
    def add_voice(self):
        voice = Voice(self)
        self.m_voice_list.append(voice)
        return voice
    def refill_accidentals_info(self, key):
        """Fill the .m_accidentals_info dict with the accidentals
        that exist in the key signature `key`.
        """
        self.m_accidentals_info = {}
        for step in range(MusicalPitch.LOWEST_STEPS, MusicalPitch.HIGHEST_STEPS+1):
            self.m_accidentals_info[step] = 0
        for a in mpdutils.key_to_accidentals(key):
            n = MusicalPitch.new_from_notename(a)
            for oct in range(-4, 7):
                n.m_octave_i = oct
                if n.semitone_pitch() < 128:
                    if a[-4:] == 'eses':
                        self.m_accidentals_info[n.steps()] = -2
                    elif a[-2:] == 'es':
                        self.m_accidentals_info[n.steps()] = -1
                    elif a[-4:] == 'isis':
                        self.m_accidentals_info[n.steps()] = 2
                    else:
                        self.m_accidentals_info[n.steps()] = 1
    def needed_accidental(self, m):
        steps = m.steps()
        if m.m_accidental_i != self.m_accidentals_info[steps]:
            if (self.m_accidentals_info[steps] == 2 and m.m_accidental_i == 1) \
                    or (self.m_accidentals_info[steps] == -2 and m.m_accidental_i == -1):
                self.m_accidentals_info[steps] = m.m_accidental_i
                return [0, m.m_accidental_i]
            self.m_accidentals_info[steps] = m.m_accidental_i
            return [m.m_accidental_i]
    def barline(self, timepos):
        #assert timepos not in self.m_coldict
        self.m_coldict[timepos] = StaffColObj()
        self.m_coldict[timepos].m_barline = 1
    def add_timesignature(self, t, timepos):
        #self.m_coldict[timepos].m_timesignature = t
        self.w_score().announce_timepos(timepos)
        self.w_score().m_timeposdict[timepos].m_timesignature_obj = t
    def add_keysignature(self, timepos, key):
        self.m_coldict[timepos].m_keysignature = key
    def add_clef(self, timepos, clef):
        self.m_coldict[timepos].m_clef = clef
    def announce_timepos(self, timepos):
        if timepos not in self.m_coldict:
            self.m_coldict[timepos] = StaffColObj()
            self.w_score().announce_timepos(timepos)
    def maybe_ledger_lines(self, timepos, ypos):
        if timepos not in self.m_coldict:
            self.m_coldict[timepos] = StaffColObj()
        if self.m_coldict[timepos].m_ledger_up > ypos < -5:
            self.m_coldict[timepos].m_ledger_up = ypos
        if self.m_coldict[timepos].m_ledger_down < ypos > 5:
            self.m_coldict[timepos].m_ledger_down = ypos


class Voice:
    def __init__(self, parent_staff):
        self.m_coldict = {}
        self.w_parent_staff = weakref.ref(parent_staff)
        self._tmp_tie = {}
        self.m_ties = []
        self.m_beams = []
        self.m_is_beaming = None
        self.m_doing_tuplet = None
    def add_notehead(self, timepos, music, stemdir):
        assert timepos in self.m_coldict
        key = (music.m_pitch.steps(), music.m_pitch.m_accidental_i)
        if __debug__:
            if key in self.m_coldict[timepos].m_music:
                print "warning, adding the same notehead twice", key
            if (self.m_coldict[timepos].m_duration is not None) \
                    and (self.m_coldict[timepos].m_duration != music.m_duration):
                print "::", self.m_coldict[timepos].m_duration, music.m_duration
                print "mpd: warning: All noteheads on the same stem has to have the same length"
        self.m_coldict[timepos].m_duration = music.m_duration
        if self.m_is_beaming and self.m_coldict[timepos].m_duration.get_rat_value() >= Rat(1, 4):
            print "mpd: warning: beamed stems has to be 1/8-note or shorter. Ignoring invalid beam request."
            self.m_is_beaming = None
        music.m_shift = 0
        self.m_coldict[timepos].m_music[key] = music
        self.m_coldict[timepos].m_stemdir = stemdir
    def add_rest(self, rest, timepos):
        assert timepos in self.m_coldict
        self.m_coldict[timepos].m_rest = rest
    def announce_timepos2(self, timepos):
        if timepos not in self.m_coldict:
            self.m_coldict[timepos] = VoiceColObj()
            self.w_parent_staff().announce_timepos(timepos)
    def announce_timepos(self, timepos):
        if timepos not in self.m_coldict:
            self.m_coldict[timepos] = VoiceColObj()
            if self.m_is_beaming:
                self.m_coldict[timepos].m_beaminfo = 'continue'
            if self.m_doing_tuplet:
                self.m_coldict[timepos].m_tupletinfo = 'continue'
            self.w_parent_staff().announce_timepos(timepos)
    def start_tuplet(self, timepos, times, dir):
        self.m_coldict[timepos].m_tupletinfo = (times.m_den, dir)
        self.m_doing_tuplet = 1
    def end_tuplet(self):
        self.m_doing_tuplet = None
    def start_beam(self, timepos):
        if self.m_is_beaming:
            print "mpd-warning: we are already beaming, ignoring start_beam request"
            return
        self.m_is_beaming = 1
        self.m_coldict[timepos].m_beaminfo = 'start'
    def end_beam(self):
        if not self.m_is_beaming:
            print "mpd-warning: we are not beaming, ignoring stop_beam request"
        self.m_is_beaming = 0
    def do_tie_end(self, pos1, pos2):
        for p in self.m_coldict[pos1].m_music:
            if p in self.m_coldict[pos2].m_music:
                self.m_ties.append((pos1, pos2, p,
                       # need these to generate midi track
                       self.m_coldict[pos1].m_music[p],
                       self.m_coldict[pos2].m_music[p]))
    def generate_track_for_voice(self, staff, kv, velocity, tracktype):
        # first we find the id()'s of music to tie
        tie_from_v = []
        tie_to_v = []
        for t in self.m_ties:
            tie_from_v.append(id(t[3]))
            tie_to_v.append(id(t[4]))
        ################
        D = {}
        i = 2
        musictimepos = Rat(0, 1)
        last_timepos = kv[0]
        id_D = {}
        for idx in range(len(kv)):
            coltimepos = kv[idx]
            musictimepos = musictimepos + (coltimepos - last_timepos)
            if coltimepos in self.m_coldict:
                for n in self.m_coldict[coltimepos].m_music.values():
                    #FIXME this is ugly
                    if id(n) in tie_from_v and (id(n) not in tie_to_v):
                        id_D[n.m_pitch.semitone_pitch()] = i
                        if musictimepos not in D:
                            D[musictimepos] = []
                        D[musictimepos].append((i, const.START_NOTE, n.m_pitch.semitone_pitch()))
                    elif id(n) in tie_to_v:
                        stop_pos = musictimepos + n.m_duration.get_rat_value()
                        if stop_pos not in D:
                            D[stop_pos] = []
                        D[stop_pos].append((id_D[n.m_pitch.semitone_pitch()],
                               const.STOP_NOTE, n.m_pitch.semitone_pitch()))
                    else:
                        if musictimepos not in D:
                            D[musictimepos] = []
                        D[musictimepos].append((i, const.START_NOTE, n.m_pitch.semitone_pitch()))

                        stop_pos = musictimepos + n.m_duration.get_rat_value()
                        if stop_pos not in D:
                            D[stop_pos] = []
                        D[stop_pos].append((i, const.STOP_NOTE, n.m_pitch.semitone_pitch()))
                        i = i + 1
            last_timepos = coltimepos
        return self.__gen_midi_last_step(D, velocity, tracktype)
    def __gen_midi_last_step(self, D, velocity, tracktype):
        keys = D.keys()
        keys.sort()
        prev_time = Rat(0)
        ms = tracktype()
        for k in keys:
            delta = None
            if k != Rat(0, 1):
                delta = k-prev_time
            prev_time = k
            for e in D[k]:
                if e[1] == const.START_NOTE:
                    if delta:
                        ms.notelen_time(delta)
                    ms.start_note(e[2], velocity)
                elif e[1] == const.STOP_NOTE:
                    if delta:
                        ms.notelen_time(delta)
                    ms.stop_note(e[2], velocity)
                delta = None
        return ms

def parse_to_score_object(music):
    lexer = Lexer(music)
    relative_mode = None
    relto=None
    transpose_pitch = None
    TOPLEVEL = 1#'toplevel'
    NOTES = 2#'notes'
    START_OF_CHORD = 3#'start-of-chord'
    CHORD = 4#'chord'
    context = TOPLEVEL
    score = Score()
    chord_duration = None
    tie_is_in_the_air = 0
    x = len(lexer.m_string)
    times = None
    cur_staff = None
    while lexer.m_idx < x:
        toc, toc_data = lexer.get()
        if toc == Lexer.STAFF:
            assert context == TOPLEVEL
            cur_staff = score.add_staff()
            cur_voice = cur_staff.add_voice()
            stem_dir = const.BOTH
            tuplet_dir = const.BOTH
            relative_mode = None
            timepos = Rat(0)
            last_pos = timepos
            cur_staff.announce_timepos(timepos)
            cur_staff.add_clef(timepos, "violin")
        elif toc == Lexer.VOICE:
            if not cur_staff:
                raise ParseError("Don't use \\addvoice before \\staff")
            relative_mode = None
            timepos = Rat(0)
            cur_voice = cur_staff.add_voice()
        elif toc == Lexer.RELATIVE:
            assert not relative_mode
            relative_mode = 1
            relto = toc_data
        elif toc == Lexer.TRANSPOSE:
            transpose_pitch = toc_data
        elif toc == Lexer.TIME:
            cur_staff.announce_timepos(timepos)
            cur_staff.add_timesignature(toc_data, timepos)
        elif toc == Lexer.KEY:
            p = MusicalPitch.new_from_notename(toc_data[0])
            if transpose_pitch:
                p.transpose_by_musicalpitch(transpose_pitch)
            k = (p.get_notename(), toc_data[1])
            cur_staff.announce_timepos(timepos)
            cur_staff.add_keysignature(timepos, k)
        elif toc == Lexer.TIMES:
            if not times:
                times = toc_data
                cur_voice.announce_timepos(timepos)
                cur_voice.start_tuplet(timepos, times, tuplet_dir)
            else:
                raise ParseError(r"\times nn/nn does not nest")
        elif toc == Lexer.CLEF:
            cur_staff.announce_timepos(timepos)
            cur_staff.add_clef(timepos, toc_data)
        elif toc == '|':
            cur_staff.announce_timepos(timepos)
            cur_staff.barline(timepos)
        elif toc == '{':
            if (context == TOPLEVEL):
                context = NOTES
                if not cur_staff.m_coldict[Rat(0, 1)].m_keysignature:
                    if transpose_pitch:
                        k = (transpose_pitch.get_notename(), 'major')
                    else:
                        k = ('c', 'major')
                    cur_staff.add_keysignature(Rat(0, 1), k)
            else:
                raise ParseError("Token '{' not allowed here.")
        elif toc == '<':
            if context == NOTES:
                context = START_OF_CHORD
            else:
                raise ParseError("Token '<' not allowed here.")
        elif toc == '>':
            if context == CHORD:
                if tie_is_in_the_air:
                    tie_is_in_the_air = 0
                    cur_voice.do_tie_end(last_pos, timepos)
                last_pos = timepos
                timepos = timepos + chord_duration.get_rat_value()
                chord_duration = None
                relto = relto_backup; relto_backup = None
                context = NOTES
            else:
                raise ParseError("Token '>' not allowed here.")
        elif toc == '}':
            if context == NOTES:
                if times:
                    times = None
                    cur_voice.end_tuplet()
                else:
                    context = TOPLEVEL
            else:
                raise ParseError("Token '}' not allowed here.")
        elif toc == '[':
            cur_voice.announce_timepos(timepos)
            cur_voice.start_beam(timepos)
        elif toc == ']':
            cur_voice.end_beam()
            # we call announce_timepos2, because the regular
            # version breaks tuplets. FIXME
            cur_voice.announce_timepos2(timepos)
        elif toc == '~':
            tie_is_in_the_air = 1
        elif toc == Lexer.NOTE and (context in [NOTES, CHORD, START_OF_CHORD]):
            if times:
                toc_data.m_duration.m_tuple = times
            if context in [NOTES, START_OF_CHORD]:
                cur_voice.announce_timepos(timepos)
            if relative_mode:
                toc_data.m_pitch = musicalpitch_relative(
                                          relto, toc_data.m_pitch)
                relto = toc_data.m_pitch.clone()
            if transpose_pitch:
                toc_data.transpose(transpose_pitch)
            cur_voice.add_notehead(timepos, toc_data, stem_dir)
            if context == NOTES:
                if tie_is_in_the_air:
                    cur_voice.do_tie_end(last_pos, timepos)
                    tie_is_in_the_air = 0
                last_pos = timepos
                timepos = timepos + toc_data.m_duration.get_rat_value()
            if context == START_OF_CHORD:
                relto_backup = relto
                chord_duration = toc_data.m_duration
                context = CHORD
        elif toc == Lexer.SKIP and context == NOTES:
            last_pos = timepos
            timepos = timepos + toc_data.get_rat_value()
        elif toc == Lexer.REST and context == NOTES:
            cur_voice.announce_timepos(timepos)
            cur_voice.add_rest(toc_data, timepos)
            last_pos = timepos
            timepos = timepos + toc_data.m_duration.get_rat_value()
        elif toc == Lexer.STEMDIR:
            stem_dir = toc_data
        elif toc == Lexer.TUPLETDIR:
            tuplet_dir = toc_data
        else:
            print "*%s*" % toc, type(toc), lexer.m_idx
            print "music: '%s'" % music[lexer.m_idx-4:]
            raise ParseError(toc)
    return score


class Lexer:
    STAFF = 1
    VOICE = 2
    CLEF = 3
    STEMDIR = 4
    TRANSPOSE = 5
    TIME = 6
    KEY = 7
    NOTE = 8
    SKIP = 9
    REST = 10
    RELATIVE = 11
    TIMES = 12
    TUPLETDIR = 13
    re_staff = re.compile(r"\s*\\staff", re.UNICODE)
    re_voice = re.compile(r"\s*\\addvoice", re.UNICODE)
    re_clef = re.compile(r"\s*\\clef\s+(\w*)", re.UNICODE)
    re_stem_updown = re.compile(r"\s*(\\stem)(Up|Down|Both)\s+", re.UNICODE)
    re_tuplet_updown = re.compile(r"\s*(\\tuplet)(Up|Down|Both)\s+", re.UNICODE)
    re_relative = re.compile(r"\s*\\relative\s+(([a-zA-Z]+)([',]*))", re.UNICODE)
    re_transpose = re.compile(r"\s*\\transpose\s+(([a-zA-Z]+)([',]*))", re.UNICODE)
    re_rest = re.compile(r"\s*(r)([\d]*)(\.*)", re.UNICODE)
        #FIXME we are a little more strict than Lilypond, since ~ has to
        # be before ]
        #FIXME don't use named regex if we don't need it.
    re_melodic = re.compile(r"""(?x)
                            \s*
                             ((?P<notename>[a-zA-Z]+)
                             (?P<octave>[',]*))
                             (?P<len>[\d]*)
                             (?P<dots>\.*)""", re.UNICODE)
    re_skip = re.compile(r"""(?x)
                            \s*
                             (s)
                             (?P<len>[\d]*)
                             (?P<dots>\.*)""", re.UNICODE)
    re_tempo = re.compile(r"\s*\\tempo\s+(\d+)\s*=+s*(\d+);", re.UNICODE)
    re_time = re.compile(r"\s*\\time\s+(\d+)\s*/\s*(\d+)", re.UNICODE)
    re_key = re.compile(r"\s*\\key\s+([a-z]+)\s*\\(major|minor)", re.UNICODE)
    re_times = re.compile(r"\s*\\times\s+(\d+)\s*/\s*(\d+)\s*{", re.UNICODE)
    def __init__(self, s):
        self.m_string = s.strip()
        self.m_notelen = Duration(4, 0)
        self.m_idx = 0
    def get(self):
        m = self.re_rest.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            notelen = m.group(2)
            if notelen:
                notelen = int(notelen)
            else:
                notelen = 0
            numdots = len(m.group(3))
            r = RestRequest(notelen, numdots)
            if not r.m_duration:
                r.m_duration = self.m_notelen
                if numdots:
                    self.m_notelen.m_dots = numdots
            else:
                self.m_notelen = r.m_duration
            return self.REST, r

        m = self.re_skip.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            IGN1, skiplen, dots = m.groups()
            if skiplen:
                skiplen = int(skiplen)
            else:
                skiplen = 0
                if dots:
                    #FIXME
                    raise ParseError("Error")
            numdots = len(dots)
            if skiplen is 0:
                retval = self.m_notelen
            else:
                self.m_notelen = retval = Duration(skiplen, numdots)
            return self.SKIP, retval
        m = self.re_melodic.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            notename, IGN1, IGN2, notelen, dots = m.groups()
            numdots = len(dots)
            if notelen:
                notelen = int(notelen)
            else:
                notelen = 0
                if dots:
                    #FIXME
                    raise ParseError("LexerError")
            n = MusicRequest(notename, notelen, numdots)
            if not n.m_duration:
                n.m_duration = self.m_notelen.clone()
            else:
                self.m_notelen = n.m_duration.clone()
            return self.NOTE, n
        m = self.re_staff.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.STAFF, None
        m = self.re_voice.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.VOICE, None
        m = self.re_relative.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.RELATIVE, MusicalPitch.new_from_notename(m.group(1))
        m = self.re_clef.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.CLEF, m.group(1)
        m = self.re_stem_updown.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            d = [const.UP, const.DOWN, const.BOTH][['Up', 'Down', 'Both'].index(m.group(2))]
            return self.STEMDIR, d
        m = self.re_tuplet_updown.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            d = [const.UP, const.DOWN, const.BOTH][['Up', 'Down', 'Both'].index(m.group(2))]
            return self.TUPLETDIR, d
        m = self.re_transpose.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.TRANSPOSE, MusicalPitch.new_from_notename(m.group(1))
        m = self.re_time.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.TIME, TimeSignature(int(m.group(1)), int(m.group(2)))
        m = self.re_key.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.KEY, (m.group(1), m.group(2))
        m = self.re_times.match(self.m_string, self.m_idx)
        if m:
            self.m_idx += len(m.group())
            return self.TIMES, Rat(int(m.groups()[0]), int(m.groups()[1]))
        while self.m_string[self.m_idx] in [' ', '\n', '\t']:
            self.m_idx += 1
        self.m_idx += 1
        return self.m_string[self.m_idx-1], None
