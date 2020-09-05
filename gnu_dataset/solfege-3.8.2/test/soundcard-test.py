#!/usr/bin/python2.3
import sys
sys.path.insert(0, ".")
import soundcard

## This works nice with ALSA and OSS emulation with SB Live card:
import src.i18n
src.i18n.setup(".")
soundcard.initialise_external_midiplayer("/usr/bin/timidity", False)
#soundcard.initialise_devicefile("/dev/sequencer2", 2)


print """
You should here three major triads:

    G       G       G
  E       E       E_E
C       C       C_C_C   C
(piano) (flute) (strings ens)

Press enter when the sounds are finished.
"""

t = soundcard.Track()
t.set_bpm(120, 4)
t.note(4, 60, 100)
t.note(4, 64, 100)
t.note(4, 67, 100)
t.set_patch(73)
t.note(4, 60, 100)
t.note(4, 64, 100)
t.note(4, 67, 100)
t.set_patch(48)
t.start_note(60, 100)
t.notelen_time(4)
t.start_note(64, 100)
t.notelen_time(4)
t.start_note(67, 100)
t.notelen_time(4)
t.stop_note(60, 100)
t.stop_note(64, 100)
t.stop_note(67, 100)
t.note(2, 60, 100)
soundcard.synth.play_track(t)

import sys
print "press <enter>"
sys.stdin.readline()
#soundcard.synth.close()
