# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2006, 2007   Tom Cato Amundsen
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

from rat import Rat
import mfutils

class EventBase(object):
    def __init__(self):
        self.m_time = None
    def __str__(self):
        return "(%s, time:%s)" % ( self.__class__.__name__, self.m_time)

class NoteEventBase(EventBase):
    def __init__(self, pitch, velocity):
        EventBase.__init__(self)
        assert 0 <= pitch 
        self.m_pitch = pitch
        self.m_velocity = velocity
    def __str__(self):
        return "(%s, pitch:%s, vel:%s, time:%s)" % (self.__class__.__name__, self.m_pitch, self.m_velocity, self.m_time)

class NoteOnEvent(NoteEventBase):
    def __init__(self, pitch, velocity):
        NoteEventBase.__init__(self, pitch, velocity)

class NoteOffEvent(NoteEventBase):
    def __init__(self, pitch, velocity):
        NoteEventBase.__init__(self, pitch, velocity)

class Delay(EventBase):
    def __init__(self, duration):
        """
        duration is a Rat. Rat(1, 4) denotes a quarter-note.
        """
        EventBase.__init__(self)
        self.m_duration = duration
    def __str__(self):
        return "(%s, dur:%s, time:%s)" % (self.__class__.__name__, self.m_duration, self.m_time)

class SetPatchEvent(EventBase):
    def __init__(self, patch):
        EventBase.__init__(self)
        assert 0 <= patch < 128
        self.m_patch = patch
    def __str__(self):
        return "(%s, time:%s, patch:%i)" % ( self.__class__.__name__, self.m_time, self.m_patch)


class TempoEvent(EventBase):
    def __init__(self, bpm, notelen):
        EventBase.__init__(self)
        self.m_bpm = bpm
        self.m_notelen = notelen

class MidiEventStream(object):
    TEMPO = 'tempo'
    NOTE_ON = 'note-on'
    NOTE_OFF = 'note-off'
    NOTELEN_TIME = 'notelen-time'
    BENDER = 'bender'
    SET_PATCH = 'program-change'
    class ChannelDevice(object):
        """
        Bad name, but I don't have a better idea right now. This
        class will handle all 16 midi channels.
        """
        class MidiChannel(object):
            def __init__(self, number):
                self.m_number = number
                self.m_tones = set()
            def start_tone(self, i):
                assert i not in self.m_tones
                self.m_tones.add(i)
            def stop_tone(self, i):
                assert i in self.m_tones
                self.m_tones.remove(i)
            def is_silent(self):
                """
                Return True if no tones are playing on this channel.
                """
                return not bool(self.m_tones)
        def __init__(self):
            # We are zero-indexed, so this is MIDI channel 10
            self.percussion_MIDI_channel = 9
            self.free_MIDI_channels = []
            for i in range(self.percussion_MIDI_channel) \
                     + range(self.percussion_MIDI_channel, 16):
                self.free_MIDI_channels.append(self.MidiChannel(i))
            # The dict key will be the patch number.
            # The value is a MidiChannel object.
            self.allocated_MIDI_channels = {}
            # This dict maps from MIDI channel number to the actual
            # MidiChannel object
            self.int_to_channel_object = {}
            for channel in self.free_MIDI_channels:
                self.int_to_channel_object[channel.m_number] = channel
        def alloc_channel(self, patch):
            """
            Allocate a midi channel for the patch and
            return the MIDI channel number of the allocated channel.
            """
            #FIXME need to handle running out of available midi channels.
            assert patch not in self.allocated_MIDI_channels
            for num, channel in self.allocated_MIDI_channels.items():
                if channel.is_silent():
                    self.allocated_MIDI_channels[patch] = \
                        self.allocated_MIDI_channels[num]
                    del self.allocated_MIDI_channels[num]
                    return self.allocated_MIDI_channels[patch].m_number
            self.allocated_MIDI_channels[patch] = self.free_MIDI_channels.pop(0)
            return self.allocated_MIDI_channels[patch].m_number
        def get_channel_for_patch(self, patch):
            """
            Return the MIDI channel number we should use to play a tone
            with this patch number. Raise KeyError if no channel is allocated
            yet.
            """
            return self.allocated_MIDI_channels[patch].m_number
        def start_note(self, channel, pitch):
            self.int_to_channel_object[channel].start_tone(pitch)
        def stop_note(self, channel, pitch):
            self.int_to_channel_object[channel].stop_tone(pitch)
        def is_playing(self, channel, pitch):
            return pitch in self.int_to_channel_object[channel].m_tones
    def __init__(self, *tracks):
        self.m_tracks = tracks
        for track in self.m_tracks:
            track.calculate_event_times()
    def __iter__(self):
        for e in self.generate_track_events():
            yield e
    def _create_dict_of_track(self, track):
        # create a dict of the track, where the key is a list with
        # all events with the same m_time variable.
        # The dict values are a dict with three keys:
        # NoteOffEvents, OtherEvents, NoteOnEvents
        retval = {}
        for event in track.m_v:
            retval[event.m_time] = retval.get(event.m_time, [])
            retval[event.m_time].append(event)
        for key in retval:
            retval[key] = {
              'NoteOffEvents': [x for x in retval[key] if isinstance(x, NoteOffEvent)],
              'OtherEvents': [x for x in retval[key] if not isinstance(x, (NoteOffEvent, NoteOnEvent))],
              'NoteOnEvents': [x for x in retval[key] if isinstance(x, NoteOnEvent)]}
        return retval
    def generate_track_events(self):
        #FIXME find better method name
        retval = []
        # tpos_set will know all the positions in time where anything happens
        # on any staff
        tpos_set = set()
        for track in self.m_tracks:
            tpos_set.update([x.m_time for x in track.m_v if x.m_time])
        tracks2 = [self._create_dict_of_track(track) for track in self.m_tracks]
        tpos_list = list(tpos_set)
        tpos_list.sort()
        # We use this variable to remember which instrument
        # we want the track to play.
        track_instrument = [0] * len(self.m_tracks)
        # We use this list of dicts to know which tones are playing
        # which patch on the tracks. The key of the dicts are the integer
        # value representing the pitch, and the value is the patch number.
        track_notes = []
        for x in range(len(self.m_tracks)):
            track_notes.append({})
        ch_dev = self.ChannelDevice()
        last_pos = Rat(0, 1)
        for tpos in tpos_list:
            if tpos != last_pos: # Just to not insert before the first events
                yield self.NOTELEN_TIME, tpos - last_pos
            for idx, track in enumerate(tracks2):
                if tpos in track:
                    for e in track[tpos]['NoteOffEvents']:
                        if e.m_pitch not in track_notes[idx]:
                            # This could happen if the user adds extra NoteOffEvents or adds one
                            # with the wrong pitch.
                            print "info: not stopping, not playing now:", e
                            continue
                        patch = track_notes[idx][e.m_pitch]
                        del track_notes[idx][e.m_pitch]
                        if isinstance(self.m_tracks[idx], PercussionTrack):
                            chn = ch_dev.percussion_MIDI_channel
                        else:
                            chn = ch_dev.get_channel_for_patch(patch)
                        if ch_dev.is_playing(chn, e.m_pitch):
                            ch_dev.stop_note(chn, e.m_pitch)
                            yield self.NOTE_OFF, chn, e.m_pitch, e.m_velocity
                    for e in track[tpos]['OtherEvents']:
                        if isinstance(e, SetPatchEvent):
                            # Let us check if it is necessary to alloc a
                            # channel before we do so. It might have been
                            # allocated before.
                            try:
                                chn = ch_dev.get_channel_for_patch(e.m_patch)
                                track_instrument[idx] = e.m_patch
                            except:
                                chn = ch_dev.alloc_channel(e.m_patch)
                                track_instrument[idx] = e.m_patch
                                yield self.SET_PATCH, chn, e.m_patch
                        elif isinstance(e, TempoEvent):
                            yield self.TEMPO, e.m_bpm, e.m_notelen
                        else:
                            print "NOT HANDLING EVENT:", e
                    for e in track[tpos]['NoteOnEvents']:
                        assert e.m_pitch not in track_notes[idx]
                        if isinstance(self.m_tracks[idx], PercussionTrack):
                            chn = ch_dev.percussion_MIDI_channel
                        else:
                            try:
                                chn = ch_dev.get_channel_for_patch(track_instrument[idx])
                            except KeyError:
                                chn = ch_dev.alloc_channel(track_instrument[idx])
                                # We will only yield SET_PATCH if a channel
                                # was allocated.
                                yield self.SET_PATCH, chn, track_instrument[idx]
                        if ch_dev.is_playing(chn, e.m_pitch):
                            print "info: ignoring duplicate tone:", e
                            continue
                        track_notes[idx][e.m_pitch] = track_instrument[idx]
                        # ch_dev must know which tones are sounding on which
                        # MIDI channels, so it can handle the midi resources.
                        ch_dev.start_note(chn, e.m_pitch)
                        yield self.NOTE_ON, chn, e.m_pitch, e.m_velocity
            last_pos = tpos
    def create_midifile(self, filename, appendstreams=[]):
        """
        filename -- a string naming the file to write the generated midi file to.
                    Will overwrite a existing file.
        """
        v = []
        notelen = 0
        v += mfutils.mf_tempo(60 * 4 / 4)
        for stream in [self] + appendstreams:
            for e in stream:
                if e[0] == self.TEMPO:
                    v = v + mfutils.mf_tempo(e[1] * 4 / e[2])
                elif e[0] == self.NOTELEN_TIME:
                    notelen = e[1]
                elif e[0] == self.NOTE_ON:
                    v = v + mfutils.mf_note_on(int(96 * 4 * notelen), e[1], e[2], e[3])
                    notelen = 0
                elif e[0] == self.NOTE_OFF:
                    v = v + mfutils.mf_note_off(int(96 * 4 * notelen), e[1], e[2], e[3])
                    notelen = 0
                elif e[0] == self.SET_PATCH:
                    v = v + mfutils.mf_program_change(e[1], e[2])
                elif e[0] == self.BENDER:
                    print "FIXME todo: seq_bender for play_with_drvmidi"
                    #m.seq_bender(DEV, e[1], e[2])
                else:
                    raise "Corrupt track error"
        f = open(filename, "w")
        mfutils.MThd(f)
        f.write("MTrk")
        mfutils.write_int32(f, len(v)+4)
        v = v + mfutils.mf_end_of_track()
        mfutils.write_vect(f, v)
        f.close()


class Track:
    """
    A pitch is represented by an integer value 0-127.
    * There can only be one instance of a pitch sounding at the same time.
    * There can only be one instrument sounding at the same time.
    Right now there are no code that checks that this is true while
    adding notes.
    """
    def txtdump(self):
        for event in self.m_v:
            print event
    def __init__(self):
        self.m_v = []
    def start_note(self, pitch, vel):
        assert 0 <= int(pitch) < 128
        assert 0 <= vel < 128
        self.m_v.append(NoteOnEvent(int(pitch), int(vel)))
    def stop_note(self, pitch, vel):
        assert 0 <= int(pitch) < 128
        assert 0 <= vel < 128
        self.m_v.append(NoteOffEvent(int(pitch), int(vel)))
    def notelen_time(self, notelen):
        """
        To avoid having to alter all code calling this, we interpret
        notelen in two different ways depending on its type:
        int: replace to Rat(1, notelen)
        Rat: the value tell the note length. For example Rat(1, 4) for a
             quarter note.
        """
        if isinstance(notelen, int):
            self.m_v.append(Delay(Rat(1, notelen)))
        else:
            assert isinstance(notelen, Rat)
            self.m_v.append(Delay(notelen))
    def note(self, notelen, pitch, vel):
        """
        See notelen_time docstring.
        """
        self.start_note(pitch, vel)
        self.notelen_time(notelen)
        self.stop_note(pitch, vel)
    def set_patch(self, patch):
        """
        Add an event that will change the midi instrument for the
        notes following this event.
        """
        self.m_v.append(SetPatchEvent(patch))
    def prepend_patch(self, patch):
        """
        Insert an event that will change the midi instrument at the
        beginning of the track. If you call this method several times,
        only the first call will have any effect.
        """
        self.m_v.insert(0, SetPatchEvent(patch))
    def set_bpm(self, bpm, notelen=4):
        self.m_v.append(TempoEvent(bpm, notelen))
    def prepend_bpm(self, bpm, notelen=4):
        self.m_v.insert(0, TempoEvent(bpm, notelen))
    def bender(self, chn, value):
        "value >= 0"
        self.m_v.append([self.BENDER, chn, value])
    def create_merge(self, track1, track2):
        """
        Return a new track that merges track1 and track2.
        """
    create_merge = staticmethod(create_merge)
    def merge_with(self, B):
        D = {}
        for track in [self, B]:
            pos = Rat(0, 1)
            for event in track.m_v:
                if isinstance(event, Delay):
                    pos = pos + event.m_duration
                else:
                    if pos not in D:
                        D[pos] = []
                    D[pos].append(event)
        kv = D.keys()
        kv.sort()
        self.m_v = []
        for x in range(len(kv)-1):
            for event in D[kv[x]]:
                self.m_v.append(event)
            self.m_v.append(Delay(kv[x+1]-kv[x]))
        for event in D[kv[-1]]:
            self.m_v.append(event)
    def replace_note(self, old, new):
        assert isinstance(old, int)
        assert 0 <= old < 128
        assert isinstance(new, int)
        assert 0 <= new < 128
        for event in self.m_v:
            if isinstance(event, (NoteOnEvent, NoteOffEvent)) \
                    and event.m_pitch == old:
                event.m_pitch = new
    def calculate_event_times(self):
        """
        Set the variable m_time on each Event.
        """
        pos = Rat(0, 1)
        for e in self.m_v:
            if isinstance(e, Delay):
                pos += e.m_duration
            else:
                e.m_time = pos

class PercussionTrack(Track):
    def __init__(self):
        Track.__init__(self)

