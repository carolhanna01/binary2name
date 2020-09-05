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

class OSSSequencer2Synth(oss_common.AbstractSynth):
    def __init__(self, device, devnum, verbose_init):
        oss_common.AbstractSynth.__init__(self, device, devnum, verbose_init)
        # FIXME-LEARNTHIS: is the value 96 special in any way,
        # or can I use whatever value i want???
        solfege_c_midi.sndctl_tmr_timebase(96);
        solfege_c_midi.sndctl_tmr_tempo(60);
    def set_patch(self):
        """
        Set
        """
        pass
    def play_track(self, *tracks):
        self.play_midieventstream(MidiEventStream(*tracks))
    def play_midieventstream(self, midieventstream):
        m = solfege_c_midi
        m.sndctl_seq_reset()
        for c in range(self.NUM_CHANNELS):
            m.seq_set_patch(self.m_devnum, c, 0)
        m.sndctl_tmr_timebase(96);
        m.sndctl_tmr_tempo(60);
        m.seq_start_timer();
        self.handle_midi_events(midieventstream)
    def handle_midi_events(self, midieventstream):
        m = solfege_c_midi
        for e in midieventstream:
            if e[0] == midieventstream.TEMPO:
                t = e[1] * 4 / e[2]
                if t < 256:
                    m.sndctl_tmr_timebase(96)
                    m.sndctl_tmr_tempo(t)
                else:
                    if t > 511:
                        print "devmusicsynth.py: warning: bpm > 511"
                    m.sndctl_tmr_timebase(96*2)
                    m.sndctl_tmr_tempo(int(t/2))
            elif e[0] == midieventstream.NOTE_ON:
                m.seq_start_note(self.m_devnum, e[1], e[2], e[3])
            elif e[0] == midieventstream.NOTE_OFF:
                m.seq_stop_note(self.m_devnum, e[1], e[2], e[3])
            elif e[0] == midieventstream.NOTELEN_TIME:
                # 96 is a const, also used in soundcard.initialize that
                # I don't understand.
                m.seq_delta_time(int(96*4*e[1]))
            elif e[0] == midieventstream.SET_PATCH:
                m.seq_set_patch(self.m_devnum, e[1], e[2])
            elif e[0] == midieventstream.BENDER:
                m.seq_bender(self.m_devnum, e[1], e[2])
            else:
                raise "Corrupt midieventstream error"
        m.seqbuf_dump()

def test():
    synth = OSSSequencer2Synth("/dev/sequencer2", 1, None)
    import sys
    from mpd.track import Track
    from mpd.rat import Rat
    sys.path.insert(0, ".")
    import src.i18n
    src.i18n.setup(".")
    src.i18n.setup(".")
    t1 = Track()
    t1.start_note(70, 127)
    t1.notelen_time(Rat(1, 4))
    t1.stop_note(70, 127)
    t1.start_note(70, 127)
    t1.notelen_time(Rat(1, 4))
    t1.stop_note(70, 127)
    t1.set_patch(8)
    t1.start_note(71, 126)
    t1.notelen_time(Rat(1, 16))
    t1.stop_note(71, 126)
    t2 = Track()
    t2.start_note(80, 127)
    t2.notelen_time(Rat(1, 2))
    t2.stop_note(80, 127)
    synth.play_track(track.MidiEventStream(t1, t2))

if __name__ == '__main__':
    test()
