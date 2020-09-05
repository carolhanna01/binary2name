# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Tom Cato Amundsen
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


import glob
import xrandom
import uuid
import stat
import time
import locale
import gethomedir

class LessonfileException(Exception):
    pass

class LessonfileParseException(Exception):
    pass

class NoQuestionInFileException(LessonfileException):
    """
    Raised by find_random_question if the lesson file contains
    no questions at all.
    """
    def __init__(self, lessonfilename):
        self.m_filename = lessonfilename
    def __str__(self):
        return "The lesson file '%s' contains no questions." % self.m_filename

class FileNotFound(LessonfileException):
    def __init__(self, filename):
        self.m_filename = filename
    def __str__(self):
        return "The external file '%s' was not found." % self.m_filename

class NoQuestionConfiguredException(LessonfileException):
    """
    This exception is raised by select_random_question if the user has
    unselected all the questions available in the lesson file.
    """
    def __str__(self):
        return "All questions are unselected. Select some questions from the config page."

class UnknownMusicType(LessonfileParseException):
    """
    This exception is raised if we encounter an unknown music type.
    This might happen if the user use the music constructor instead
    of the predefined functions and spells the music type wrongly.
    An example:

      music = music("c e g", "cord")
    """
    def __init__(self, musictype):
        LessonfileParseException.__init__(self)
        self.m_musictype = musictype
    def __unicode__(self):
        return u"Unknown music type \"%(musictype)s\"" % {'musictype': self.m_musictype}

import re, os, sys
import const
import random
import mpd
import cfg
import utils
from mpd.rat import Rat
import soundcard
import dataparser
import osutils

class plstring(unicode):
    pass

# The keys in the dict say how many steps up or down in the
# circle of fifths we go if we transpose.
_keys_to_interval = {
              -10: '-M6', #c -> eses
               -9: '-a2', #c -> beses
               -8: '-a5', #c -> fes
               -7: '-au', #c -> ces
               -6: '-a4', #c -> ges
               -5: 'm2', # c -> des
               -4: '-M3',# c -> as
               -3: 'm3', # c -> es
               -2: '-M2', # c -> bes
               -1: 'p4', # c -> f
                0: 'p1',
                1: '-p4', # c -> g,
                2: 'M2', # c -> d
                3: '-m3',# c -> a
                4: 'M3', # c -> e
                5: '-m2', # c -> b
                6: 'a4', # c -> fis
                7: 'au', #c -> cis
                8: 'a5', # c -> gis
                9: 'a2', # c -> dis
                10: 'M6', # c -> ais
            }

keywords = (
    # exercise modules
    'dictation',
    'rhythm',
    'harmonicprogressiondictation',
    'singchord',
    'singanswer',
    'chordvoicing',
    'chord',
    'compareintervals',
    'idbyname',
    'singinterval',
    'melodicinterval',
    'harmonicinterval',
    'example',
    'idtone',
    'twelvetone',
    'identifybpm',
    'nameinterval',
    'elembuilder',
    'rhythmtapping',
    'rhythmtapping2',
    #
    'harmonic',
    'melodic',
    'progression',
    'normal',
    'voice',
    'rvoice',
    'satb',
    'rhythm',
    'cmdline',
    'wavfile',
    'midifile',
    'horiz',
    'vertic',
    'accidentals',
    'key',
    'semitones',
    'play',
    'show',
    # elembuilder:
    'auto',
    # Used by the rhythm module:
    'newline',
    'd1', 'p1', 'a1',
    'd2', 'm2', 'M2', 'a2',
    'd3', 'm3', 'M3', 'a3',
    'd4', 'p4', 'a4',
    'd5', 'p5', 'a5',
    'd6', 'm6', 'M6', 'a6',
    'd7', 'm7', 'M7', 'a7',
    'd8', 'p8', 'a8',
    'd9', 'm9', 'M9', 'a9',
    'd10', 'm10', 'M10', 'a10',
)
predef = {
    'tempo': (60, 4),
    'yes': True,
    'no': False,
}
for n in keywords:
    predef[n] = n

lessonfile_functions = {
        '_': dataparser.dataparser_i18n_func,
        '_i': _i, #FIXME
        # play_wav should probably be removed. Replaced by wavfile
        'play_wav': lambda f: Music(cfg.get_string("sound/wav_player") + " " + f, "cmdline"),
        # Update Music.is_displayable and is_mpd_parsable
        # when adding music types
        'music': lambda m, t='normal': Music(m, t),
        'chord': lambda m: Music(m, 'chord'),
        'satb': lambda m: Music(m, 'satb'),
        'voice': lambda m: Music(m, 'voice'),
        'rvoice': lambda m: Music(m, 'rvoice'),
        'rhythm': lambda m: Music(m, 'rhythm'),
        'percussion': lambda m: Music(m, 'percussion'),
        'cmdline': lambda m: Music(m, 'cmdline'),
        'wavfile': lambda m: Music(m, 'wavfile'),
        'midifile': lambda m: Music(m, 'midifile'),
        'progressionlabel': lambda s: plstring(s),
}

class _Header(dict):
    def __init__(self, headerdict):
        dict.__init__(self, headerdict)
        for key, value in (
                           ('version', ''),
                           ('title', ''),
                           ('description', ''),
                           ('musicformat', 'normal'),
                           ('random_transpose', True),
                           ('labelformat', 'normal'),
                           ('fillnum', 1),
                           ('filldir', 'horiz'),
                           ('have_repeat_slowly_button', False),
                           ('have_repeat_arpeggio_button', False),
                           ('at_question_start', []),
                           ('have_music_displayer', False),
                           ('enable_right_click', True),
                           ('disable_unused_intervals', True),
                           ):
            if key not in self:
                self[key] = value
    def __getattr__(self, name):
        """
        This function let us write

          header.variable_name

        as a shortcut or:

            header['variable_name']
        """
        if name in self:
            return dataparser.get_translated_string(self, name)
        # FIXME print "warning: lessonfile.Header.%s not set. Returning empty string" % name
        return ""
        #raise NameError(name)

class Music:
    types = ('normal', 'chord', 'satb', 'voice', 'rvoice', 'rhythm',
             'percussion', 'cmdline', 'wavfile', 'midifile')
    def __init__(self, musicdata, musictype='normal'):
        if musictype not in self.types:
            raise UnknownMusicType(musictype)
        self.m_musicdata = musicdata
        self.m_musictype = musictype
    def is_displayable(self):
        """
        Return True if it is possible to display this music object  in a
        MusicDisplayer. If it returns False, it is possible that this
        particular music type in theory can be displayed on a
        MusicDisplayer, but that the code for it has not been written yet.
        """
        return self.m_musictype in ('normal', 'chord',
                'satb', 'voice', 'rvoice')
    def is_mpd_parsable(self):
        """
        Return True if the mpd module can parse and play this music object.
        """
        return self.m_musictype in ('normal', 'chord',
                'satb', 'voice', 'rvoice', 'rhythm', 'percussion')

def parse_test_def(s):
    m = re.match("(\d+)\s*x", s)
    count = int(m.groups()[0])
    return (count, 'x')

class LessonfileCommon(object):
    def __init__(self, filename):
        self.m_prev_question = None
        # This variable stores the directory the lesson file is located in.
        # We need this to we can find other files relative to this file.
        self.m_location = os.path.split(filename)[0]
        self._idx = None
    def parse_file(self, filename):
        """Parse the file named filename. Set these variables:
        self.header     a Header instance
        self.questions  a list of all question
        """
        self.dataparser = dataparser.Dataparser(predef, lessonfile_functions, ('tempo',))
        self.m_filename = filename
        self._parse_worker(self.dataparser.parse_file, filename)
    def parse_string(self, s):
        """
        See parse_file docstring.
        """
        self.dataparser = dataparser.Dataparser(predef, lessonfile_functions, ('tempo',))
        self.m_filename = "<STRING>"
        self._parse_worker(self.dataparser.parse_string, s)
    def _parse_worker(self, func, data):
        try:
            func(data)
        except LessonfileParseException, e:
            e.m_nonwrapped_text = self.dataparser._lexer.get_err_context(self.dataparser._lexer.pos - 2)
            e.m_token = self.dataparser._lexer.m_tokens[self.dataparser._lexer.pos - 2]
            raise
        self.m_transpose = mpd.MusicalPitch.new_from_notename("c'")
        self.header = _Header(self.dataparser.header)
        self.m_globals = self.dataparser.globals
        self.m_questions = self.dataparser.questions
        self.blocklists = self.dataparser.blocklists
        del self.dataparser
        for question in self.m_questions:
            question['active'] = 1
            # FIXMECOMPAT
            if 'music' in question and isinstance(question['music'], basestring):
                # The following line is for backward compatibility
                question['music'] = Music(question['music'], self.header.musicformat)
            question['m_lessonfile'] = self
        self.m_random = xrandom.Random(range(len(self.m_questions)))
        if self.header.random_transpose == True:
            self.header.random_transpose = ['key', -5, 5]
        # Backward compatability to handle old style
        # random_transpose = -4, 5 FIXMECOMPAT
        if self.header.random_transpose and len(self.header.random_transpose) == 2:
            self.header.random_transpose \
                = ['semitones'] + self.header.random_transpose
        # Some variables does only make sense if we have a music displayer
        if self.header.at_question_start:
            self.header.have_music_displayer = True

class QuestionsLessonfile(LessonfileCommon):
    def __init__(self, filename):
        LessonfileCommon.__init__(self, filename)
    def select_random_question(self):
        """
        Select a new question by random. It will use the music in the
        lesson file question variable 'music' when selecting transposition.
        """
        # when we start the program with --no-random, we want to go
        # throug all the questions in the lesson file in sequential order.
        if cfg.get_bool('config/no_random'):
            try:
                self.m_no_random_idx
            except:
                self.m_no_random_idx = 0
            self.header.random_transpose = False

        count = 0
        available_question_idx = []
        if not self.m_questions:
            raise NoQuestionInFileException(self.m_filename)
        for i in range(len(self.m_questions)):
            if self.m_questions[i]['active']:
                available_question_idx.append(i)
        if not available_question_idx:
            raise NoQuestionConfiguredException()
        while 1:
            count += 1
            if cfg.get_bool('config/no_random'):
                if self.m_no_random_idx < len(available_question_idx):
                    self._idx = self.m_no_random_idx
                    self.m_no_random_idx += 1
                else:
                    self._idx = self.m_no_random_idx = 0
            else:
                if cfg.get_string("app/random_function") == 'random_by_random':
                    self._idx = self.m_random.random_by_random(available_question_idx)
                elif cfg.get_string("app/random_function") == 'random_by_random2':
                    self._idx = self.m_random.random_by_random2(available_question_idx)
                elif cfg.get_string("app/random_function") == 'random_by_selection':
                    self._idx = self.m_random.random_by_selection(available_question_idx)
                else:
                    self._idx = random.choice(available_question_idx)
            if self.header.random_transpose:
                self.m_transpose = self.find_random_transpose()
            if count == 10:
                break
            if self.m_prev_question == self.get_music() \
                and (len(self.m_questions) > 1 or self.header.random_transpose):
                continue
            break
        self.m_random.add(self._idx)
        self.m_prev_question = self.get_music()
    def find_random_transpose(self):
        """
        Return a MusicalPitch representing a suggested random
        transposition for the currently selected question,
        m_questions[self._idx]
        """
        if 'key' in self.m_questions[self._idx]:
            key = self.m_questions[self._idx]['key']
        else:
            key = "c \major"
        if self.header.random_transpose == True:
            self.header.random_transpose = ['key', -5, 5]
        if self.header.random_transpose[0] == 'semitones':
            retval = self.semitone_find_random_transpose()
            if random.randint(0, 1):
                retval.enharmonic_flip()
        else:
            retval = self._xxx_find_random_transpose(key)
        return retval
    def semitone_find_random_transpose(self):
        """
        Called to find random transposition in "semitone" mode.
        Create and return a random MusicalPitch representing this transposition.
        """
        assert self.header.random_transpose[0] == 'semitones'
        return mpd.MusicalPitch().randomize(
              mpd.transpose_notename("c'", self.header.random_transpose[1]),
              mpd.transpose_notename("c'", self.header.random_transpose[2]))
    def _xxx_find_random_transpose(self, key):
        """
        Called to create random transposition in "accidentals" or "key" mode.
        Create and return a random MusicalPitch representing this transposition.
        Keyword arguments:
        key -- the key the question is written in, for example "c \major"
        """
        assert self.header.random_transpose[0] in ('key', 'accidentals')
        low, high = self.header.random_transpose[1:3]
        tone, minmaj = key.split()
        k = mpd.MusicalPitch.new_from_notename(tone).get_octave_notename()
        #FIXME this list say what key signatures are allowed in sing-chord
        # lesson files. Get the correct values and document them.
        kv = ['des', 'aes', 'ees', 'bes', 'f', 'c',
              'g', 'd', 'a', 'e', 'b', 'fis', 'cis', 'gis']
        # na tell the number of accidentals (# is positive, b is negative)
        # the question has from the lessonfile before anything is transpose.
        na = kv.index(k) - 5
        if minmaj == '\\minor':
            na -= 3
        if self.header.random_transpose[0] == 'accidentals':
            # the number of steps down the circle of fifths we can go
            n_down = low - na
            # the number of steps up the circle of fifths we can go
            n_up = high - na
        else:
            assert self.header.random_transpose[0] == 'key'
            n_down = low
            n_up = high
        interv = mpd.Interval()
        interv.set_from_string(_keys_to_interval[random.choice(range(n_down, n_up+1))])
        return mpd.MusicalPitch.new_from_notename("c'") + interv
    def iterate_questions_with_unique_names(self):
        """Iterate the questions in the lessonfile, but only yield the
        first question if several questions have the same name. The
        untranslated name is used when deciding if a name is unique.
        """
        names = {}
        for question in self.m_questions:
            if question.get_cname() not in names:
                names[question.get_cname()] = 1
                yield question
    def get_unique_cnames(self):
        """Return a list of all cnames in the file, in the same order
        as they appear in the file. Only list each cname once, even if
        there are more questions with the same cname.
        """
        names = []
        for question in self.m_questions:
            if not question.get_cname() in names:
                names.append(question.get_cname())
        return names
    def get_question(self):
        """
        Return the currently selected question.
        """
        assert self._idx is not None
        return self.m_questions[self._idx]
    def get_tempo(self):
        assert self._idx is not None
        return self.m_questions[self._idx]['tempo']
    def get_name(self):
        """
        Return the translated name of the currently selected question.
        """
        assert self._idx is not None
        return self.m_questions[self._idx].get_name()
    def get_cname(self):
        """
        The 'cname' of a question is the C locale of the question name.
        Said easier: If the lesson file supplies translations, then 'cname'
        is the untranslated name.
        """
        assert self._idx is not None
        return self.m_questions[self._idx].get_cname()
    def get_mpd_music_string(self, question, varname):
        """
        Use the musictype and the musicdata of the music objects in the
        variable named by varname to create a complete music code string
        that the mpd module can parse.
        """
        assert question[varname].is_mpd_parsable()
        if question[varname].m_musictype == 'chord':
            return r"\staff\transpose %s{< %s > }" \
              % (self.m_transpose.get_octave_notename(), question[varname].m_musicdata)
        elif question[varname].m_musictype == 'normal':
            return self._get_mpd_music_string_normal_format(question, varname)
        elif question[varname].m_musictype == 'satb':
            return self._get_mpd_music_string_satb_format(question, varname)
        elif question[varname].m_musictype in ('rhythm', 'percussion'):
            return r"\staff{ %s }" % question[varname].m_musicdata
        elif question[varname].m_musictype == 'voice':
            return r"\staff\transpose %s{ %s }" \
              % (self.m_transpose.get_octave_notename(), question[varname].m_musicdata)
        else:
            assert question[varname].m_musictype == 'rvoice'
            def find_first_note(v):
                """arg v is a list of the musicdata splittet by whitespace.
                Return the index of the first note.
                """
                #FIXME tidy up a little
                i = 0
                while 1:
                    n = re.match("(\<?)[a-zA-Z]+[',]*", v[i])
                    if n:
                        break
                    if v[i] == r"\clef":
                        i += 2
                    elif v[i] == r"\key":
                        i += 3
                    elif v[i] == r"\time":
                        i += 2
                    else:
                        i += 1
                    if i >= len(v):
                        return -1
                return i
            v = question[varname].m_musicdata.split()
            i = find_first_note(v)
            if i == -1:
                return r"\staff\transpose %s{ %s }"\
                   % (self.m_transpose.get_octave_notename(), " ".join(v))
            m = re.match("(?P<pre>\<?)(?P<note>[a-zA-Z]+[',]*)", v[i])
            rel_note = m.group('note')
            nn = mpd.MusicalPitch.new_from_notename(rel_note)
            aa = m.group('pre') + nn.get_octave_notename()
            v[i] = m.group('pre') + nn.get_notename() + v[i][len(aa):]
            return r"\staff\transpose %s\relative %s{ %s }" \
              % (self.m_transpose.get_octave_notename(), 
                 rel_note, " ".join(v))
    def get_music(self, varname='music'):
        """
        Return the music for the currently selected question. This is complete
        music code that can be fed to mpd.play_music(...).

        If the music type not of a type that mpd.play_music can handle,
        for example a midi file or a cmdline type, then we return a string
        that can be used to compare if the music of two questions are equal.
        This string is not parsable by any functions and should only be used
        to compare questions.
        """
        assert self._idx is not None
        if self.m_questions[self._idx][varname].is_mpd_parsable():
            return self.get_mpd_music_string(self.m_questions[self._idx], varname)
        return "%s:%s" % (self.m_questions[self._idx][varname].m_musictype,
                          self.m_questions[self._idx][varname].m_musicdata)
    def _get_mpd_music_string_satb_format(self, question, varname):
        """
        It should not be necessary to call this method directly. Call
        .get_music() or .get_mpd_music_string() and let that
        function decide.
        """
        assert question[varname].m_musictype == 'satb'
        v = question[varname].m_musicdata.split('|')
        if 'key' in question:
            k = question['key']
        else:
            k = "c \major"
        music = r"""
                \staff{ \key %s\stemUp <%s> }
                \addvoice{ \stemDown <%s> }
                \staff{ \key %s\clef bass \stemUp <%s>}
                \addvoice{ \stemDown <%s>}
                """ % (k, v[0], v[1], k, v[2], v[3])
        if self.header.random_transpose:
            music = music.replace(r"\staff",
                      r"\staff\transpose %s" % self.m_transpose.get_octave_notename())
            music = music.replace(r"\addvoice",
                      r"\addvoice\transpose %s" % self.m_transpose.get_octave_notename())
        return music
    def _get_mpd_music_string_normal_format(self, question, varname):
        """
        It should not be necessary to call this method directly. Call
        .get_music() or .get_mpd_music_string() and let that
        function decide.
        """
        assert question[varname].m_musictype == 'normal'
        if self.header.random_transpose:
            s = question[varname].m_musicdata.replace(r'\staff',
               r'\staff\transpose %s' % self.m_transpose.get_octave_notename())
            s = s.replace(r'\addvoice',
               r'\addvoice\transpose %s' % self.m_transpose.get_octave_notename())
            return s
        return question[varname].m_musicdata
    def music_to_notename_list(self, music):
        if music.m_musictype == 'chord':
            if not self.header.random_transpose:
                return music.m_musicdata.split()
            v = []
            for n in music.m_musicdata.split():
                v.append(mpd.MusicalPitch.new_from_notename(n).transpose_by_musicalpitch(
                     self.m_transpose).get_octave_notename())
            return v
        elif music.m_musictype == 'satb':
            v = []
            for n in re.split("\s|\|", music.m_musicdata):
                v.append(mpd.MusicalPitch.new_from_notename(n).transpose_by_musicalpitch(self.m_transpose).get_octave_notename())
            return v
        else:
            raise LessonfileException("get_music_as_notename_list cannot handle m_musictype==%s" % music.m_musictype)
    def get_music_as_notename_list(self, varname):
        """
        Return a list of notenames from the variabale VARNAME in the
        currently selected question. The notes are transposed if
        header.random_transpose is set.

        Assert m_musictype in ('chord', 'satb')
        """
        assert self._idx is not None
        return self.music_to_notename_list(self.get_question()[varname])
    def get_music_as_notename_string(self, varname):
        """
        Return a string with notenames representing the question currently
        selected question. The notes are transposed if
        header.random_transpose is set.

        Assert musicformat in ('chord', 'satb')
        """
        return " ".join(self.get_music_as_notename_list(varname))
    def has_question(self):
        """
        Return True if a question is selected.
        """
        return self._idx is not None
    def play_question(self, question=None, varname='music'):
        """Play the question. Play the current question if question is none.
        varname is the name of the variable that contains the music.
        """
        if not question:
            question = self.get_question()
        try:
            if question[varname].m_musictype == 'cmdline':
                osutils.run_external_program(str(question[varname].m_musicdata), self.m_location, "")
            elif question[varname].m_musictype == 'wavfile':
                if os.path.exists(os.path.join(
                            self.m_location,
                            question[varname].m_musicdata)):
                        soundcard.play_wav_file(os.path.join(
                            self.m_location,
                            question[varname].m_musicdata))
                else:
                    raise FileNotFound(question[varname].m_musicdata)
            elif question[varname].m_musictype == 'midifile':
                soundcard.play_midi_file(os.path.join(
                    self.m_location,
                    question[varname].m_musicdata))
            elif question[varname].m_musictype in 'rhythm':
                self.play_question_musicformat_rhythm(question, varname)
            elif question[varname].m_musictype in 'percussion':
                self.play_question_musicformat_percussion(question, varname)
            elif question[varname].m_musictype in ('normal', 'satb', 'voice', 'rvoice'):
                self.play_question_musicformat_normal_satb(question, varname)
            else:
                assert question[varname].m_musictype in 'chord'
                self.play_question_musicformat_chord(question, varname)
        except mpd.MpdException, e:
            e.m_mpd_varname = varname
            e.m_mpd_badcode = question[varname].m_musicdata
            raise
    def play_question_musicformat_rhythm(self, question, varname):
        """
        This will play music objects of the type 'rhythm'. See the
        docbook documentation for a description.
        """
        #FIXME can this be used by src.rhythm too?
        score = mpd.parser.parse_to_score_object(question.get_music(varname))
        track = score.get_midi_events_as_percussion(cfg.get_int('config/preferred_instrument_velocity'))[0]
        track.prepend_bpm(self.get_tempo()[0], self.get_tempo()[1])
        track.replace_note(mpd.notename_to_int("c"), 37)
        track.replace_note(mpd.notename_to_int("d"), 80)
        soundcard.synth.play_track(track)
    def play_question_musicformat_percussion(self, question, varname):
        score = mpd.parser.parse_to_score_object(question.get_music(varname))
        track = score.get_midi_events_as_percussion(cfg.get_int('config/preferred_instrument_velocity'))[0]
        track.prepend_bpm(self.get_tempo()[0], self.get_tempo()[1])
        soundcard.synth.play_track(track)
    def play_question_musicformat_normal_satb(self, question, varname):
        instrument = self.prepare_instrument_list(question)
        mpd.play_music(question.get_music(varname), self.get_tempo(),
                       instrument[0], instrument[1])
    def play_question_slowly(self, question=None, varname='music'):
        if not question:
            question = self.get_question()
        #assert question['music'].m_musictype == 'normal'
        #FIXME what music types should be handled differently
        self.play_question_musicformat_normal_slowly(question, varname)
    def play_question_musicformat_normal_slowly(self, question, varname='music'):
        instrument = self.prepare_instrument_list(question)
        bpm = self.get_tempo()
        bpm = (bpm[0]/2, bpm[1])
        mpd.play_music(question.get_music(varname), bpm,
                       instrument[0], instrument[1])
    def play_question_musicformat_chord(self, question, varname='music'):
        assert question[varname].m_musictype == 'chord'
        instrument = self.prepare_instrument_list(question)
        assert len(instrument) in (2, 6)
        if len(instrument) == 2:
            mpd.play_music(question.get_music(varname), self.get_tempo(),
                       instrument[0], instrument[1])
        else:
            assert len(instrument) == 6
            t1 = soundcard.Track()
            t2 = soundcard.Track()
            t3 = soundcard.Track()
            t1.set_bpm(cfg.get_int('config/default_bpm'))#FIXME skulle dette vart tatt fra lesson file?
            nlist = self.music_to_notename_list(question[varname])
            t1.set_patch(instrument[0])
            t2.set_patch(instrument[2])
            t3.set_patch(instrument[4])
            # start notes
            t1.note(4, mpd.notename_to_int(nlist[0]), instrument[1])
            for notename in nlist[1:-1]:
                t2.start_note(mpd.notename_to_int(notename), instrument[3])
            t2.notelen_time(4)
            for notename in nlist[1:-1]:
                t2.stop_note(mpd.notename_to_int(notename), instrument[3])
            t3.note(4, mpd.notename_to_int(nlist[-1]), instrument[5])
            soundcard.synth.play_track(t1, t2, t3)
    def play_question_arpeggio(self, varname='music'):
        musictype = self.get_question()[varname].m_musictype
        if musictype == 'chord':
            self.play_question_musicformat_chord_arpeggio(varname)
        elif musictype == 'satb':
            self.play_question_musicformat_satb_arpeggio(varname)
        else:
            raise LessonfileException("\nUknown music type: '%s'" % self.get_question()[varname].m_musictype)
    def play_question_musicformat_chord_arpeggio(self, varname):
        """FIXME -- for completeness, should this function should take a question argument?
        """
        assert self.get_question()[varname].m_musictype == 'chord'
        instrument = self.prepare_instrument_list(self.get_question())
        assert len(instrument) in (2, 6)
        if len(instrument) == 2:
            m = self.get_music_as_notename_string(varname)
            mpd.play_music(r"\staff{%s}" % m, cfg.get_int('config/arpeggio_bpm'),
                       instrument[0], instrument[1])
        else:
            assert len(instrument) == 6
            t1 = soundcard.Track()
            t2 = soundcard.Track()
            t3 = soundcard.Track()
            t1.set_bpm(cfg.get_int('config/arpeggio_bpm'))
            nlist = self.get_music_as_notename_list(varname)
            # set patches
            t1.set_patch(instrument[0])
            t2.set_patch(instrument[2])
            t3.set_patch(instrument[4])
            # start notes
            t1.note(4, mpd.notename_to_int(nlist[0]), instrument[1])
            t2.notelen_time(4)
            t3.notelen_time(4)
            for notename in nlist[1:-1]:
                t2.note(4, mpd.notename_to_int(notename), instrument[3])
                t3.notelen_time(4)
            t3.note(4, mpd.notename_to_int(nlist[-1]), instrument[5])
            soundcard.synth.play_track(t1, t2, t3)
    def play_question_musicformat_satb_arpeggio(self, varname='music'):
        """FIXME -- for completeness, should this function should take a question argument?
        """
        instrument = self.prepare_instrument_list(self.get_question())
        track = soundcard.Track()
        track.set_bpm(cfg.get_int('config/default_bpm'))
        track.set_patch(instrument[0])
        for x in 0, 1:
            v = self.get_music_as_notename_list(varname)[x]
            s = v.strip().split(" ")
            for n in s:
                if cfg.get_string('user/sex') == 'female':
                    track.note(4, mpd.notename_to_int(n), instrument[1])
                else:
                    track.note(4, mpd.notename_to_int(n)-12, instrument[1])
        for x in 2, 3:
            v = self.get_music_as_notename_list(varname)[x]
            s = v.strip().split(" ")
            for n in s:
                if cfg.get_string('user/sex') == 'male':
                    track.note(4, mpd.notename_to_int(n), instrument[1])
                else:
                    track.note(4, mpd.notename_to_int(n)+12, instrument[1])
        soundcard.synth.play_track(track)
    def play_question_as_choral(self, question, varname='music'):
        instrument = self.prepare_instrument_list(self.get_question())
        if len(instrument) == 2:
            instrument = instrument * 3
        # FIXME, only the velocity from instrument[1] is used since
        # music_to_tracklist is not complete.
        tracklist = mpd.music_to_tracklist(question.get_music(varname), instrument[1])
        tracklist[0].prepend_patch(instrument[4])
        a, b = self.get_tempo()
        # FIXME the bpm api is wrong. We set the tempo to one track, and it works for all.
        tracklist[0].prepend_bpm(a, b)
        for track in tracklist[:-1][:1]:
            track.prepend_patch(instrument[2])
        tracklist[-1].prepend_patch(instrument[0])
        soundcard.synth.play_track(*tracklist)
    def prepare_instrument_list(self, question):
        """Return a list created from the instrument variable the question.
        Use app default values if the variable is missing.
        Convert instrument names to integer values.
        Returns: lowest, middle, highest
        """
        if cfg.get_bool('config/override_default_instrument'):
            instrument = [cfg.get_int('config/lowest_instrument'),
                         cfg.get_int('config/lowest_instrument_velocity'),
                         cfg.get_int('config/middle_instrument'),
                         cfg.get_int('config/middle_instrument_velocity'),
                         cfg.get_int('config/highest_instrument'),
                         cfg.get_int('config/highest_instrument_velocity')]
        elif 'instrument' in question:
            instrument = question['instrument']
        elif 'instrument' in self.m_globals:
            instrument = self.m_globals['instrument']
        else:
            instrument = [cfg.get_int('config/preferred_instrument'),
                          cfg.get_int('config/preferred_instrument_velocity')]
        if isinstance(instrument, (unicode, int)):
            instrument = [instrument,
                          cfg.get_int('config/preferred_instrument_velocity')]
        assert len(instrument) in (2, 6)
        if len(instrument) == 2:
            if isinstance(instrument[0], unicode):
                try:
                    instrument[0] = soundcard.find_midi_instrument_number(instrument[0])
                except KeyError, e:
                    print >> sys.stderr, "Warning: Invalid instrument name '%s' in lesson file:" % instrument[0], e
                    instrument[0] = cfg.get_int('config/preferred_instrument')
            if not (0 <= instrument[1] < 128):
                print >> sys.stderr, "Warning: Adjusting instrument velocity since this value is invalid '%s'" % instrument[1]
                instrument[1] = cfg.get_int('config/preferred_instrument_velocity')
        elif len(instrument) == 6:
            for x in (0, 2, 4):
                if isinstance(instrument[x], unicode):
                    try:
                        instrument[x] = soundcard.find_midi_instrument_number(instrument[x])
                    except KeyError, e:
                        print "Warning: Invalid instrument name in lesson file:", e
                        instrument[0] = cfg.get_int('config/preferred_instrument')
                if not (0 <= instrument[x+1] < 128):
                    instrument[x+1] = cfg.get_int('config/preferred_instrument_velocity')
        return instrument

class TestSupport(object):
    """
    Lessonfile classes can add this class to the list of classes it
    inherits from if the exercise want to have tests.
    """
    def _generate_test_questions(self):
        count, t = parse_test_def(self.header.test)
        q = range(len(self.m_questions)) * count
        random.shuffle(q)
        return q
    def get_test_requirement(self):
        """
        Return the amount of exercises that has to be correct to
        pass the test. (values 0.0 to 1.0)
        """
        m = re.match("([\d\.]+)%", self.header.test_requirement)
        return float(m.groups()[0])/100.0
    def enter_test_mode(self):
        self.m_test_questions = self._generate_test_questions()
        self.m_test_idx = -1
    def next_test_question(self):
        assert self.m_test_idx < len(self.m_test_questions)
        self.m_test_idx += 1
        self._idx = self.m_test_questions[self.m_test_idx]
        if self.header.random_transpose:
            old = self.m_transpose
            # try really hard not to get the same tonika:
            for x in range(100):
                self.m_transpose = self.find_random_transpose()
                if old != self.m_transpose:
                    break
    def is_test_complete(self):
        """
        Return True if the test is compleded.
        """
        return self.m_test_idx == len(self.m_test_questions) -1

class HeaderLessonfile(LessonfileCommon):
    """
    This lesson file class should be used by all the exercise modules
    that does not need any question blocks defined.
    """
    def __init__(self, filename):
        LessonfileCommon.__init__(self, filename)
        self.parse_file(filename)

class DictationLessonfile(QuestionsLessonfile):
    def __init__(self, filename):
        QuestionsLessonfile.__init__(self, filename)
        self.parse_file(filename)
    def get_breakpoints(self):
        assert self._idx is not None
        r = []
        if 'breakpoints' in self.m_questions[self._idx]:
            r = self.m_questions[self._idx]['breakpoints']
            if not type(r) == type([]):
                r = [r]
        r = map(lambda e: Rat(e[0], e[1]), r)
        return r
    def get_clue_end(self):
        assert self._idx is not None
        if 'clue_end' in self.m_questions[self._idx]:
            return Rat(*self.m_questions[self._idx]['clue_end'])
    def get_clue_music(self):
        assert self._idx is not None
        if 'clue_music' in self.m_questions[self._idx]:
            return self.m_questions[self._idx]['clue_music']
    def select_previous(self):
        """
        Select the previous question. Do nothing if we are on the first
        question.
        """
        assert self._idx is not None
        if self._idx > 0:
            self._idx = self._idx - 1
    def select_next(self):
        """
        Select the next question. Do nothing if we are on the last question.
        """
        assert self._idx is not None
        if self._idx < len(self.m_questions) -1:
            self._idx = self._idx + 1
    def select_first(self):
        """
        Select the first question.
        """
        self._idx = 0

class SingChordLessonfile(QuestionsLessonfile):
    def __init__(self, filename):
        QuestionsLessonfile.__init__(self, filename)
        self.parse_file(filename)

class NameIntervalLessonfile(HeaderLessonfile):
    def __init__(self, filename):
        HeaderLessonfile.__init__(self, filename)
        iquality = []
        inumbers = []
        self.header.intervals = [mpd.Interval(n) for n in self.header.intervals]
        for i in self.header.intervals:
            if i.get_quality_short() not in iquality:
                iquality.append(i.get_quality_short())
            if i.steps() not in inumbers:
                inumbers.append(i.steps())
        def quality_sort(a, b):
            v = ['dd', 'd', 'm', 'M', 'p', 'a', 'aa']
            return cmp(v.index(a), v.index(b))
        iquality.sort(quality_sort)
        inumbers.sort()
        if not self.header.interval_number:
            self.header.interval_number = inumbers
        if not isinstance(self.header.interval_number, list):
            self.header.interval_number = [self.header.interval_number]
        if not self.header.interval_quality:
            self.header.interval_quality = iquality
        if not isinstance(self.header.interval_quality, list):
            self.header.interval_number = [self.header.interval_quality]
        if self.header.accidentals == "":
            self.header.accidentals = 1
        if self.header.clef == "":
            self.header.clef = u"violin"
        if not self.header.tones:
            self.header.tones = [mpd.MusicalPitch.new_from_notename("b"),
                                mpd.MusicalPitch.new_from_notename("g''")]
        else:
            if len(self.header.tones) != 2:
                raise LessonfileException("Error in the lesson file '%s':\nThe length of self.header.tones is not 2" % self.m_filename)
            self.header.tones = [mpd.MusicalPitch.new_from_notename(n) for n in self.header.tones]

class IdByNameLessonfile(QuestionsLessonfile, TestSupport):
    def __init__(self, filename):
        QuestionsLessonfile.__init__(self, filename)
        TestSupport.__init__(self)
        self.parse_file(filename)
    def parse_file(self, filename):
        super(IdByNameLessonfile, self).parse_file(filename)
        # Also, if some questions has cuemusic, then we need the displayer
        if [q for q in self.m_questions if 'cuemusic' in q]:
            self.header.have_music_displayer = True
        # Delete questions that does not have a name
        q = self.m_questions
        self.m_questions = []
        for question in q:
            if 'name' not in question:
                print >> sys.stderr, "IdByNameLessonfile.parse_file: discarding question in the lessonfile '%s'\nbecause of missing 'name' variable" % filename
                continue
            else:
                self.m_questions.append(question)

class SingAnswerLessonfile(QuestionsLessonfile):
    def __init__(self, filename):
        """
        teacher -- the teacher class contructing this lesson file class
        """
        QuestionsLessonfile.__init__(self, filename)
        self.parse_file(filename)
    def parse_file(self, filename):
        super(SingAnswerLessonfile, self).parse_file(filename)
        v = [q for q in self.m_questions if 'question_text' not in q]
        if [q for q in self.m_questions if 'question_text' not in q]:
            raise LessonfileException(_('Question number %(index)i in the lesson file "%(filename)s" is missing the "question_text" variable.') % {
                'index': self.m_questions.index(v[0]),
                'filename': filename})

class IntervalsLessonfile(HeaderLessonfile, TestSupport):
    """
    Common lesson file class for some interval exercises.
    We inherit from TestSupport, but overwrites some methods from it.
    """
    def enter_test_mode(self):
        count, t = parse_test_def(self.header.test)
        if self.header.intervals:
            self.m_test_questions = self.header.intervals * count
        else:
            self.m_test_questions = self.header.ask_for_intervals_0 * count
        random.shuffle(self.m_test_questions)
        self.m_test_idx = -1
    def next_test_question(self):
        self.m_test_idx += 1

class ChordLessonfile(QuestionsLessonfile):
    def __init__(self, filename):
        QuestionsLessonfile.__init__(self, filename)
        self.parse_file(filename)
    def parse_file(self, filename):
        """
        Call ChordLessonfile.parse_file and after that set these variables:
        self.m_chord_types
        self.m_inversions
        self.m_toptone
        """
        super(ChordLessonfile, self).parse_file(filename)
        self.m_chord_types = {}
        self.m_inversions = []
        self.m_toptones = []
        for question in self.m_questions:
            self.m_chord_types[question.get_cname()] = question.get_name()

            # inversion
            if 'inversion' in question \
                    and question['inversion'] not in self.m_inversions:
                self.m_inversions.append(question['inversion'])
            # toptone
            if 'toptone' in question \
                    and question['toptone'] not in self.m_toptones:
                self.m_toptones.append(question['toptone'])
    def get_inversion(self):
        """
        Return the inversion of the currently selected question.
        Return 1 for root position, 3 for first inversion etc.
        Return -1 if no inversion info is set in the question.
        """
        assert self._idx is not None
        if 'inversion' in self.m_questions[self._idx]:
            return self.m_questions[self._idx]['inversion']
        return -1
    def get_toptone(self):
        """
        Return the toptone of the currently question.
        Return -1 if no toptone info is set in the question.
        """
        assert self._idx is not None
        if 'toptone' in self.m_questions[self._idx]:
            return self.m_questions[self._idx]['toptone']
        return -1

class ElembuilderLessonfile(QuestionsLessonfile):
    def __init__(self, filename):
        QuestionsLessonfile.__init__(self, filename)
        self.parse_file(filename)
    def parse_file(self, filename):
        super(ElembuilderLessonfile, self).parse_file(filename)


class LessonFileManager:
    def __init__(self, debug):
        self.parse(debug)
    def parse(self, debug):
        lessonpath = ['lesson-files',
                      os.path.join(gethomedir.get_home_dir(), 'lessonfiles')]
        if debug:
            lessonpath.append('regression-lesson-files')
        self.m_uiddb = {}
        vim_tmpfile_re = re.compile("\..*\.swp")
        for dir in lessonpath:
            if not os.path.isdir(dir):
                print "warning: invalid directory in path:", dir
                continue
            v = glob.glob(os.path.join(dir, "*"))
            for filename in v:
                filename = filename.decode(locale.getpreferredencoding())
                # since I usually run solfege from the source dir:
                if os.path.split(filename)[-1] in ('.arch-ids', 'Makefile', 'Makefile.in') \
                  or not os.path.isfile(filename):
                    continue
                # We save the returned lesson_id because we need it below
                lesson_id = self.parse_into_uiddb(filename)
                # We have to check if the lesson file has changed,
                # and if it has, then the results has to be deleted.
                # This because Solfege has no way of knowing what kind
                # of changes has been done to the lesson file.
                h = hash(open(filename, 'r').read())
                hash_filename = os.path.join(gethomedir.expanduser("~/.solfege"),
                           "testresults", "%s_hash" % lesson_id)
                if os.path.exists(hash_filename):
                    h2 = int(open(hash_filename, 'r').read())
                    if h != h2:
                        dirname = os.path.join(gethomedir.get_home_dir(),
                                ".solfege", "testresults", lesson_id)
                        for f in os.listdir(dirname):
                            os.remove(os.path.join(dirname, f))
    def parse_into_uiddb(self, filename):
                """
                Returns the lesson_id of the parsed file.
                """
                try:
                    f = LessonIdParser(filename)
                    if not f.has_lesson_id():
                        f.add_lesson_id()
                except IOError, e:
                    print >> sys.stderr, "add_lesson_id('%s'):" % filename, e
                    return
                try:
                    p = parse_lesson_file_header(filename)
                except dataparser.DataparserException, e:
                    print >> sys.stderr, "LessonFileManager.parse: Discarding lessonfile '%s' because the file contain errors:" % filename
                    print >> sys.stderr, str(e)
                    return
                if not 'title' in p.header:
                    p.header['title'] = '@@@NOT TITLE'
                n = {
                   'filename': filename,
                   'mtime': os.stat(filename)[stat.ST_MTIME],
                   'header': p.header,
                }
                if not p.header['lesson_id'] in self.m_uiddb:
                    self.m_uiddb[p.header['lesson_id']] = n
                else:
                    if type(self.m_uiddb[p.header['lesson_id']]) != list:
                        self.m_uiddb[p.header['lesson_id']] = \
                            [self.m_uiddb[p.header['lesson_id']]]
                    self.m_uiddb[p.header['lesson_id']].append(n)
                return p.header['lesson_id']
    def create_lessonfile_index(self):
        self.m_htmldoc = """<html><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></head><body><p>%s</p>""" % _("This page lists all the lesson files that Solfege can find. You can click on the links to start practising.")
        d = {}
        for lesson_id, n in self.m_uiddb.items():
            if not 'module' in n['header']:
                print "warning: %s is missing a module declaration" %  self.get(lesson_id, 'filename')
                continue
            if n['header']['module'] not in d:
                d[n['header']['module']] = []
            d[n['header']['module']].append(lesson_id)
        for module in d.keys():
            self.m_htmldoc = "%s<h2>%s</h2>" % (self.m_htmldoc, module)
            self.m_htmldoc += "<ul>"
            for n in d[module]:
                self.m_htmldoc += "<li><a href='solfege:practise/%s'>%s</a>: %s</li>" % (
                    n,
                    self.get(n, 'filename'),
                    self.get(n, 'title'))
            self.m_htmldoc += "</ul>"
    def get(self, lesson_id, fieldname):
        if fieldname in ('title', 'test', 'module'):
            if fieldname in self.m_uiddb[lesson_id]['header']:
                return self.m_uiddb[lesson_id]['header'][fieldname]
        if fieldname in self.m_uiddb[lesson_id]:
            return self.m_uiddb[lesson_id][fieldname]
    def is_test_passed(self, lesson_id):
        return os.path.exists(os.path.join(gethomedir.get_home_dir(),
            '.solfege', 'testresults', lesson_id, 'passed'))
    def ignore_duplicates_with_lesson_id(self, lesson_id):
        """
        Delete the duplicates with lesson_id, keep the oldest file.
        """
        def sort_func(a, b):
            return cmp(a['mtime'], b['mtime'])
        self.m_uiddb[lesson_id].sort(sort_func)
        self.m_uiddb[lesson_id] = self.m_uiddb[lesson_id][0]
    def delete_not_fn(self, lesson_id, fn):
        """
        Assumes the lesson_id has duplicate entries.
        Delete the entries that is not the file fn.
        """
        self.m_uiddb[lesson_id] = [d for d in self.m_uiddb[lesson_id] if d['filename'] == fn][0]
    def iterate_duplicated_lesson_id(self):
        """
        Return the a lesson_id if there exist lesson_ids that
        are duplicated. If not, return None.
        """
        for v in self.m_uiddb.values():
            if isinstance(v, list):
                yield v[0]['header']['lesson_id']
    def iterate_lesson_ids(self):
        for k in self.m_uiddb:
            yield k
    def get_lesson_file_info(self, lesson_id):
        """
        FIXME: the function name is not very good.
        Return data used to fix things when we have a lesson_id crash.
        """
        return [{'filename': d['filename'], 'timestr': time.strftime('%c', time.localtime(d['mtime'])), 'mtime': d['mtime']} for d in self.m_uiddb[lesson_id]]

def parse_lesson_file_header(filename):
    """
    This function is used at program starup to get the info the
    lessonfile_manager needs. This might not be bullet proof, but
    it provides a 22x speedup, and that was necessary when we got
    many lesson files.
    """
    r = re.compile("\\header\s*{.*?}", re.MULTILINE|re.DOTALL)
    s = open(filename, 'rU').read()
    m = r.search(s)
    p = dataparser.Dataparser(predef, lessonfile_functions)
    p.m_ignore_lookup_error = True
    p.parse_string(m.group())
    return p


class LessonIdParser(object):
    """
    This is a light weight parser for lesson files that is only used when
    checking/adding/updating lesson_ids.
    """
    def __init__(self, filename):
        self.m_filename = filename
        f = open(filename, 'rU')
        self.m_file_content = f.read()
        f.close()
    def has_header_block(self):
        """
        Return True if the file has a header block.
        """
        return re.search("^\s*header", self.m_file_content, re.MULTILINE) is not None
    def has_lesson_id(self):
        """
        Return True if the lesson file has a lesson_id.
        """
        return re.search("lesson_id", self.m_file_content) is not None
    def new_lesson_id(self):
        """
        Generate and add a new lesson_id for the file filename.
        """
        if self.has_lesson_id():
            ofile = open(self.m_filename, 'w')
            m = re.search("lesson_id\s*=\s*\".+?\"", self.m_file_content)
            ofile.write(self.m_file_content[:m.start()])
            self.m_new_id = uuid.generate()
            ofile.write("lesson_id = \"%s\"" % self.m_new_id)
            ofile.write(self.m_file_content[m.end():])
            ofile.close()
        else:
            print "warning: new_lesson_id when we have no id. Ok anyway."
            self.add_lesson_id()
    def add_header_block(self):
        """
        Add a header block with a lesson id.
        """
        assert not self.has_header_block()
        v = self.m_file_content.split("\n")
        for i in range(len(v)):
            if not v[i].startswith('#'):
                break
        self.m_new_id = uuid.generate()
        v.insert(i+1, "header { \n    lesson_id=\"%s\"\n}" % self.m_new_id)
        self.m_file_content = "\n".join(v)
        ofile = open(self.m_filename, 'w')
        ofile.write(self.m_file_content)
        ofile.close()
    def add_lesson_id(self):
        """
        Add a lesson id to m_file_content and write it to disk.
        """
        assert not self.has_lesson_id()
        if not self.has_header_block():
            self.add_header_block()
        else:
            ofile = open(self.m_filename, 'w')
            m = re.search("^\s*header\s*{", self.m_file_content,
                            re.MULTILINE)
            if m:
                ofile.write(self.m_file_content[:m.start()])
                ofile.write("\nheader {")
                self.m_new_id = uuid.generate()
                ofile.write("\n    lesson_id = \"%s\"" % self.m_new_id)
                ofile.write(self.m_file_content[m.end():])
            ofile.close()


