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

import oss_common
import solfege_c_midi
from mpd.track import MidiEventStream
import os

class OSSSequencerSynth(oss_common.AbstractSynth):
    """
    This class wrap /dev/sequencer, so we can use it the same
    way as /dev/music, that is with 16 channels numbered from 0 to 15,
    with the possibility to have more than one note on each channel
    and different instrument on each channel.

    This class also handle out-of-voice situations.

    REMEMBER:
            we ALWAYS have 16 channels, but the number
            of voices depend on your hardware.
    """
    def __init__(self, device, devnum, card_info, verbose_init):
        oss_common.AbstractSynth.__init__(self, device, devnum, verbose_init)
        self.m_card_info = card_info
        self.__tempo = (60, 4)
        if self.m_card_info == 'awe':
            solfege_c_midi.awe_set_channel_mode(self.m_devnum, 1);
            # I thought the following line was necessary, but it is not...
            #solfege_c_midi.awe_drum_channels(self.m_devnum, 512)
    def reset(self):
        # list of available voices, number depends on the soundcard.
        # AWE32 and GUS has 32, OPL3: 18, OPL2: 9
        self.m_voices = range(self.m_num_voices)
        # dictionary remembering what instrument we have set on the
        # different channels. There are by definition NUM_CHANNELS channels
        self.m_channel_patches = {}
        for c in range(self.NUM_CHANNELS):
            self.m_channel_patches[c] = 0
        # dictionary remembering what instrument has been set for all
        # the voices.
        self.m_voice_patches = {}
        for n in range(self.m_num_voices):
            self.m_voice_patches[n] = 0
            solfege_c_midi.seq_set_patch(self.m_devnum, n, 0)
        #
        self.m_channel_dict = {}
        for x in range(self.NUM_CHANNELS):
            self.m_channel_dict[x] = {}
    def alloc_voice(self, chan, note):
        """
        Return None if there are no available voices or
                    if 
        """
        if self.m_voices:
            voice = self.m_voices.pop()
        else:
            return None
        # first, if NOTE is already playing on channel CHAN, we need
        # to stop it and free the voice used for that.
        if note in self.m_channel_dict[chan]:
            solfege_c_midi.seq_stop_note(self.m_devnum,
                                    self.m_channel_dict[chan][note], note, 100)
            self.free_voice(chan, note)
        # the instrumnet we want the voice to use
        p = self.m_channel_patches[chan]
        if self.m_voice_patches[voice] != p:
            # we need to set the patch for this voice
            solfege_c_midi.seq_set_patch(self.m_devnum, voice, p)
            self.m_voice_patches[voice] = p
        assert note not in self.m_channel_dict[chan]
        self.m_channel_dict[chan][note] = voice
        return voice
    def free_voice(self, chan, note):
        if note not in self.m_channel_dict[chan]:
            return None
        voice = self.m_channel_dict[chan][note]
        self.m_voices.append(voice)
        del self.m_channel_dict[chan][note]
        return voice
    def set_patch(self, chan, patch):
        self.m_channel_patches[chan] = patch
    def play_track(self, *tracks):
        self.play_midieventstream(MidiEventStream(*tracks))
    def play_midieventstream(self, midieventstream):
        solfege_c_midi.sndctl_seq_reset()
        self.reset()
        solfege_c_midi.seq_start_timer()
        for e in midieventstream:
            if e[0] == midieventstream.TEMPO:
                self.__tempo = (e[1], e[2])
            elif e[0] == midieventstream.NOTE_ON:
                channel, note, vel = e[1:4]
                voice = self.alloc_voice(channel, note)
                if voice is not None:
                    #FIXME ugly hack for percussion. This will break if
                    # we play polyphonic music with very many voices
                    if channel == 9:#voice == 31:
                        voice = 9
                    solfege_c_midi.seq_start_note(self.m_devnum, voice, note, vel)
            elif e[0] == midieventstream.NOTE_OFF:
                channel, note, vel = e[1:4]
                voice = self.free_voice(channel, note)
                if voice is not None:
                    #FIXME, see 10 lines up
                    if channel == 9:#voice == 31:
                        voice = 9
                    solfege_c_midi.seq_stop_note(self.m_devnum, voice, note, vel)
            elif e[0] == midieventstream.NOTELEN_TIME:
                # 96 is a const, also used in soundcard.initialize that
                # I don't understand.
                solfege_c_midi.seq_delta_time(int(int(96*e[1]) * 60.0 * self.__tempo[1] / self.__tempo[0]))
            elif e[0] == midieventstream.SET_PATCH:
                self.set_patch(e[1], e[2])
            elif e[0] == midieventstream.BENDER:
                print "FIXME: DevSequencerSynth.BENDER"
                #solfege_d_midi.seq_bender(self.m_devnum, e[1], e[2])
            else:
                raise "play track error"
        solfege_c_midi.seqbuf_dump()
