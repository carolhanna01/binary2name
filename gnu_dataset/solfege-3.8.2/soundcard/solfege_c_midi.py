# This file was created automatically by SWIG 1.3.29.
# Don't modify this file, modify the SWIG interface instead.
# This file is compatible with both classic and new-style classes.

import _solfege_c_midi
import new
new_instancemethod = new.instancemethod
def _swig_setattr_nondynamic(self,class_type,name,value,static=1):
    if (name == "thisown"): return self.this.own(value)
    if (name == "this"):
        if type(value).__name__ == 'PySwigObject':
            self.__dict__[name] = value
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    if (not static) or hasattr(self,name):
        self.__dict__[name] = value
    else:
        raise AttributeError("You cannot add attributes to %s" % self)

def _swig_setattr(self,class_type,name,value):
    return _swig_setattr_nondynamic(self,class_type,name,value,0)

def _swig_getattr(self,class_type,name):
    if (name == "thisown"): return self.this.own()
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError,name

def _swig_repr(self):
    try: strthis = "proxy of " + self.this.__repr__()
    except: strthis = ""
    return "<%s.%s; %s >" % (self.__class__.__module__, self.__class__.__name__, strthis,)

import types
try:
    _object = types.ObjectType
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0
del types


STDC_HEADERS = _solfege_c_midi.STDC_HEADERS
HAVE_FCNTL_H = _solfege_c_midi.HAVE_FCNTL_H
HAVE_LINUX_AWE_VOICE_H = _solfege_c_midi.HAVE_LINUX_AWE_VOICE_H
HAVE_SYS_IOCTL_H = _solfege_c_midi.HAVE_SYS_IOCTL_H
HAVE_UNISTD_H = _solfege_c_midi.HAVE_UNISTD_H
seq_bender = _solfege_c_midi.seq_bender
seq_start_note = _solfege_c_midi.seq_start_note
seq_stop_note = _solfege_c_midi.seq_stop_note
seq_set_patch = _solfege_c_midi.seq_set_patch
seq_delta_time = _solfege_c_midi.seq_delta_time
seq_start_timer = _solfege_c_midi.seq_start_timer
awe_set_channel_mode = _solfege_c_midi.awe_set_channel_mode
awe_drum_channels = _solfege_c_midi.awe_drum_channels
seqbuf_dump = _solfege_c_midi.seqbuf_dump
sndctl_seq_nrsynths = _solfege_c_midi.sndctl_seq_nrsynths
sndctl_seq_reset = _solfege_c_midi.sndctl_seq_reset
sndctl_tmr_timebase = _solfege_c_midi.sndctl_tmr_timebase
sndctl_tmr_tempo = _solfege_c_midi.sndctl_tmr_tempo
sndctl_seq_gettime = _solfege_c_midi.sndctl_seq_gettime
get_synth_name = _solfege_c_midi.get_synth_name
get_synth_nr_voices = _solfege_c_midi.get_synth_nr_voices

cvar = _solfege_c_midi.cvar

