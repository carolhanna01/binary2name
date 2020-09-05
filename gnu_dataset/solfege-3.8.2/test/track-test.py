#!/usr/bin/python

import sys
sys.path.insert(0, ".")

import soundcard
from rat import Rat
from track import Track

soundcard.initialise_external_midiplayer("/usr/bin/timidity %s", 0)
#soundcard.initialise_using_fake_synth()

def test_scale():
    track = Track()
    track.set_bpm(150)
    for x in (70, 72, 74, 75, 77, 79, 81, 82):
        track.start_note(x, 127)
        track.notelen_time(Rat(1, 4))
        track.stop_note(x, 127)
    soundcard.synth.play_track(track)

def test_note():
    track = Track()
    track.set_bpm(150)
    for x in (70, 72, 74, 75, 77, 79, 81, 82):
        track.note(Rat(1, 4), x, 127)
    soundcard.synth.play_track(track)

def test_set_patch():
    """Only one instrument sounding at the same time."""
    track = Track()
    track.set_patch(40)
    track.note(Rat(1, 4), 80, 127)
    track.set_patch(42)
    track.note(Rat(1, 4), 82, 127)
    track.set_patch(0)
    track.note(Rat(1, 4), 80, 127)
    soundcard.synth.play_track(track)

def test_set_patch2():
    """Two different instruments will sound at the same time."""
    t1 = Track()
    t1.set_patch(40)
    t1.note(Rat(1, 2), 80, 127)
    #t1.note(Rat(1, 2), 80, 127)
    #t1.note(Rat(1, 2), 80, 127)
    print "t1", "*" * 50
    t1.txtdump()
    t2 = Track()
    t2.set_patch(42)
    t2.note(Rat(1, 1), 88, 127)
    #t1.note(Rat(1, 1), 88, 127)
    print "t2", "*" * 50
    t2.txtdump()
    soundcard.synth.play_track(t1, t2)

def test_set_patch3():
    """Two different instruments will sound at the same time."""
    t1 = Track()
    t1.set_patch(40)
    t1.note(Rat(1, 2), 80, 127)
    t1.notelen_time(Rat(1, 2))
    t1.note(Rat(1, 2), 80, 127)
    print "t1", "*" * 50
    t1.txtdump()
    t2 = Track()
    t2.set_patch(42)
    t2.note(Rat(1, 2), 88, 127)
    t2.set_patch(13)
    t2.note(Rat(1, 2), 89, 127)
    print "t2", "*" * 50
    t2.txtdump()
    soundcard.synth.play_track(t1, t2)

def test_set_patch4():
    """Two different instruments will sound at the same time."""
    t1 = Track()
    for x in range(2):
        t1.set_patch(40+x)
        t1.note(Rat(1, 4), 80+x, 127)
    t1.txtdump()
    t2 = Track()
    t2.set_patch(42)
    t2.note(Rat(1, 2), 88, 127)
    t2.set_patch(13)
    t2.note(Rat(1, 2), 89, 127)
    print "t2", "*" * 50
    t2.txtdump()
    soundcard.synth.play_track(t1, t2)

def test_set_patch5():
    """Two different instruments will sound at the same time."""
    t1 = Track()
    for x in range(18):
        t1.set_patch(40+x)
        t1.start_note(80+x, 127)
        t1.notelen_time(Rat(1, 4))
    for x in range(18):
        t1.stop_note(80+x, 127)
    t1.txtdump()
    t2 = Track()
    t2.set_patch(42)
    t2.note(Rat(1, 2), 88, 127)
    t2.set_patch(13)
    t2.note(Rat(1, 2), 89, 127)
    print "t2", "*" * 50
    t2.txtdump()
    soundcard.synth.play_track(t1, t2)

def test_2voice():
        t1 = soundcard.Track()
        t2 = soundcard.Track()
        t1.set_patch(44)
        t2.set_patch(42)
        t1.note(4, 60, 127)
        t2.note(4, 66, 127)
        soundcard.synth.play_track(t1, t2)

def test_3voice():
    t1 = soundcard.Track()
    t2 = soundcard.Track()
    t3 = soundcard.Track()
    t1.set_bpm(60)
    nlist = (60, 64, 67)
    instrument = (48, 127, 65, 127, 66, 127)
    t1.set_patch(instrument[0])
    t2.set_patch(instrument[2])
    t3.set_patch(instrument[4])
    # start notes
    t1.note(4, nlist[0], instrument[1])
    for notename in nlist[1:-1]:
        t2.start_note(notename, instrument[3])
    t2.notelen_time(4)
    for notename in nlist[1:-1]:
        t2.stop_note(notename, instrument[3])
    t3.note(4, nlist[-1], instrument[5])
    soundcard.synth.play_track(t1, t2, t3)

def test_percussion():
    t1 = soundcard.PercussionTrack()
    for n in range(10):
        t1.note(16, 60, 127)
        t1.note(16, 60, 127)
        t1.note(16, 60, 127)
        t1.note(16, 60, 127)
        t1.note(8, 54, 127)
        t1.note(8, 54, 127)
    soundcard.synth.play_track(t1)

test_percussion()
