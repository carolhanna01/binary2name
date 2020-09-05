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
When running on a non win32 platform, the soundcard.play_wav_file
functions has to be set after initialisation:

import soundcard

def play_wav(filename):
    os.system("aplay %s" % filename)

soundcard.initialise_external_midiplayer(....)
soundard.play_wav_file = play_wav
----

Api used to export exercises to midi file
=========================================
soundcard.start_export(export_to_filename)
    Change soundcard.synth to point to another object that collects
    the music send to it.
soundcard.end_export()
    Write the music collected since start_export to the file named when
    start_export was called.
    Then set soundcard.synth back to what is was earlier.

"""
import sys
from mpd.track import Track, PercussionTrack
from soundcardexceptions import SoundInitException
from exporter import MidiExporter
synth = None
midiexporter = None

# _saved_synth is used to store the object pointed to by synth when
# we are exporting using midiexporter.
_saved_synth = None

def win32_play_wav_file(filename):
    winsound.PlaySound(filename, winsound.SND_FILENAME | winsound.SND_ASYNC)

def linux_play_wav_file(filename):
    print "play_wav_file not set. Not playing:", filename

def linux_play_midi_file(fileame):
    print "play_midi_file not set. Not playing:", filename

if sys.platform == 'win32':
    import winsound
    play_wav_file = win32_play_wav_file
else:
    play_wav_file = linux_play_wav_file

def initialise_winsynth(synthnum, verbose_init=0):
    import winsynth
    global synth
    synth = winsynth.WinSynth(synthnum, verbose_init)


def initialise_external_midiplayer(cmdline, verbose_init=0):
    global synth
    import midifilesynth
    synth = midifilesynth.MidiFileSynth(cmdline, verbose_init)


def initialise_devicefile(devicefile, devicenum=0, card_info=None,
                          verbose_init=0):
    global synth
    if devicefile == '/dev/sequencer2' or devicefile == '/dev/music':
        import oss_sequencer2
        synth = oss_sequencer2.OSSSequencer2Synth(devicefile, devicenum, 
                                                  verbose_init)
    else:#if devicefile == '/dev/sequencer':
        if devicefile != '/dev/sequencer':
            print "warning: the device file is unknown. Assuming it is /dev/sequencer - compatible"
        import oss_sequencer
        synth = oss_sequencer.OSSSequencerSynth(devicefile, devicenum, 
                                                card_info, verbose_init)

def initialise_using_fake_synth(verbose_init=None):
    global synth
    import fakesynth
    synth = fakesynth.Synth(verbose_init)

def play_note(patch, notelen, pitch, vel=64):
    assert isinstance(patch, int) and 0 <= patch < 128
    assert isinstance(notelen, int)
    assert isinstance(pitch, int) and 0 <= pitch < 128
    assert isinstance(vel, int) and 0 <= vel < 128
    m = Track()
    m.set_patch(patch)
    m.note(notelen, pitch, vel)
    synth.play_track(m)

def play_perc(notelen, pitch, vel):
    assert isinstance(notelen, int)
    assert isinstance(pitch, int) and 0 <= pitch < 128
    assert isinstance(vel, int) and 0 <= vel < 128
    m = PercussionTrack()
    m.note(notelen, pitch, vel)
    synth.play_track(m)

def start_export(filename):
    global midiexporter, _saved_synth, synth
    if not midiexporter:
        midiexporter = MidiExporter()
    assert _saved_synth is None
    _saved_synth = synth
    synth = midiexporter
    midiexporter.start_export(filename)

def end_export():
    global midiexporter, _saved_synth, synth
    midiexporter.end_export()
    assert _saved_synth is not None
    synth = _saved_synth
    _saved_synth = None

instrument_sections = (
    'piano',
    'cromatic percussion',
    'organ', 
    'guitar',
    'bass',
    'strings',
    'ensemble',
    'brass',
    'reed',
    'pipe',
    'synth lead',
    'synth pad',
    'synth effects',
    'ethnic',
    'percussive',
    'sound effects')

instrument_names = (
    "acoustic grand", # 0
    "bright acoustic", # 1
    "electric grand", # 2
    "honky-tonk", # 3
    "electric piano 1", # 4
    "electric piano 2", # 5
    "harpsichord", # 6
    "clav", # 7
    "celesta", # 8
    "glockenspiel", # 9
    "music box", # 10
    "vibraphone", # 11
    "marimba", # 12
    "xylophone", # 13
    "tubular bells", # 14
    "dulcimer", # 15
    "drawbar organ", # 16
    "percussive organ", # 17
    "rock organ", # 18
    "church organ", # 19
    "reed organ", # 20
    "accordion", # 21
    "harmonica", # 22
    "concertina", # 23
    "acoustic guitar (nylon)", # 24
    "acoustic guitar (steel)", # 25
    "electric guitar (jazz)", # 26
    "electric guitar (clean)", # 27
    "electric guitar (muted)", # 28
    "overdriven guitar", # 29
    "distorted guitar", # 30
    "guitar harmonics", # 31
    "acoustic bass", # 32
    "electric bass (finger)", # 33
    "electric bass (pick)", # 34
    "fretless bass", # 35
    "slap bass 1", # 36
    "slap bass 2", # 37
    "synth bass 1", # 38
    "synth bass 2", # 39
    "violin", # 40
    "viola", # 41
    "cello", # 42
    "contrabass", # 43
    "tremolo strings", # 44
    "pizzicato strings", # 45
    "orchestral strings", # 46
    "timpani", # 47
    "string ensemble 1", # 48
    "string ensemble 2", # 49
    "synthstrings 1", # 50
    "synthstrings 2", # 51
    "choir aahs", # 52
    "voice oohs", # 53
    "synth voice", # 54
    "orchestra hit", # 55
    "trumpet", # 56
    "trombone", # 57
    "tuba", # 58
    "muted trumpet", # 59
    "french horn", # 60
    "brass section", # 61
    "synthbrass 1", # 62
    "synthbrass 2", # 63
    "soprano sax", # 64
    "alto sax", # 65
    "tenor sax", # 66
    "baritone sax", # 67
    "oboe", # 68
    "english horn", # 69
    "bassoon", # 70
    "clarinet", # 71
    "piccolo", # 72
    "flute", # 73
    "recorder", # 74
    "pan flute", # 75
    "blown bottle", # 76
    "skakuhachi", # 77
    "whistle", # 78
    "ocarina", # 79
    "lead 1 (square)", # 80
    "lead 2 (sawtooth)", # 81
    "lead 3 (calliope)", # 82
    "lead 4 (chiff)", # 83
    "lead 5 (charang)", # 84
    "lead 6 (voice)", # 85
    "lead 7 (fifths)", # 86
    "lead 8 (bass+lead)", # 87
    "pad 1 (new age)", # 88
    "pad 2 (warm)", # 89
    "pad 3 (polysynth)", # 90
    "pad 4 (choir)", # 91
    "pad 5 (bowed)", # 92
    "pad 6 (metallic)", # 93
    "pad 7 (halo)", # 94
    "pad 8 (sweep)", # 95
    "fx 1 (rain)", # 96
    "fx 2 (soundtrack)", # 97
    "fx 3 (crystal)", # 98
    "fx 4 (atmosphere)", # 99
    "fx 5 (brightness)", # 100
    "fx 6 (goblins)", # 101
    "fx 7 (echoes)", # 102
    "fx 8 (sci-fi)", # 103
    "sitar", # 104
    "banjo", # 105
    "shamisen", # 106
    "koto", # 107
    "kalimba", # 108
    "bagpipe", # 109
    "fiddle", # 110
    "shanai", # 111
    "tinkle bell", # 112
    "agogo", # 113
    "steel drums", # 114
    "woodblock", # 115
    "taiko drum", # 116
    "melodic tom", # 117
    "synth drum", # 118
    "reverse cymbal", # 119
    "guitar fret noise", # 120
    "breath noise", # 121
    "seashore", # 122
    "bird tweet", # 123
    "telephone ring", # 124
    "helicopter", # 125
    "applause", # 126
    "gunshot") # 127

def find_midi_instrument_number(instr_name):
    """
    Try to find the integer representing the instrument instr_name.
    Do a substring search if we don't get an exact match.
    Raise KeyError if we don't find the instrument.
    """
    for i in range(len(instrument_names)):
        if instr_name == instrument_names[i]:
            return i
    for i in range(len(instrument_names)):
        if instr_name in instrument_names[i]:
            return i
    raise KeyError(instr_name)

# the names are taken directly from the OSS documentation (pdf file)
percussion_names = [
        "Acoustic Bass Drum", # 35
        "Bass Drum 1",
        "Side Stick",
        "Acoustic Snare",
        "Hand Clap",
        "Electric Snare",
        "Low Floor Tom",
        "Closed Hi Hat",
        "High Floor Tom",
        "Pedal Hi Hat",
        "Low Tom",
        "Open HiHat",
        "Low-Mid Tom",
        "Hi-Mid Tom",
        "Crash Cymbal 1",
        "High Tom",
        "Ride Cymbal 1",
        "Chinese Cymbal",
        "Ride Bell",
        "Tambourine",
        "Splash Cymbal",
        "Cowbell",
        "Crash Cymbal 2",
        "Vibraslap",
        "Ride Cymbal 2",
        "Hi Bongo",
        "Low Bongo",
        "Mute Hi Conga",
        "Open High Conga",
        "Low Conga",
        "High Timbale",
        "Low Timbale",
        "High Agogo",
        "Agogo Low",
        "Cabasa",
        "Maracas",
        "Short Whistle",
        "Long Whistle",
        "Short Guiro",
        "Long Guiro",
        "Claves",
        "Hi Wood Block",
        "Low Wood Block",
        "Mute Cuica",
        "Open Cuica",
        "Mute Triangle",
        "Open Triangle"]

def percussionname_to_int(name):
    assert isinstance(name, basestring)
    return percussion_names.index(name) + 35

def int_to_percussionname(i):
    assert isinstance(i, int)
    return percussion_names[i-35]


