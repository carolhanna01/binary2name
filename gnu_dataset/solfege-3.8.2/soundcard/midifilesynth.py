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

import sys, os, tempfile, signal
import textwrap
from mpd.track import MidiEventStream

def ms_win_kill(pid):
    import win32api
    handle = win32api.OpenProcess(1, 0, pid)
    return (0 != win32api.TerminateProcess(handle, 0))

class MidiFileSynth(object):
    NUM_CHANNELS = 16
    def __init__(self, cmd, verbose_init):
        self.m_type_major = "Midifile"
        self.m_cmd = cmd
        self.m_tmpfilename = tempfile.mkstemp(".mid")[1]
        self.error_report_cb = None
        self.m_pidfile = tempfile.mkstemp()[1]
        if sys.platform == 'win32':
            self.play_midieventstream = self._play_midieventstream_system
        else:
            self.play_midieventstream = self._play_midieventstream_fork_execl
        if verbose_init:
            print "Solfege will use an external midiplayer program."
            print "cmdline:", self.m_cmd
            print "tmpfile:", self.m_tmpfilename
        self.__child_pid = None
    def close(self):
        try:
            if os.path.exists(self.m_tmpfilename):
                os.remove(self.m_tmpfilename)
        except OSError:
            pass
            # We ignore this error because it seems to be easiest right now.
            # FIXME
    def join_cmd_and_filename(self, cmd, filename):
        if "%s" in cmd:
            return cmd % filename
        else:
            return " ".join((cmd, filename))
    def play_track(self, *tracks):
        self.play_midieventstream(MidiEventStream(*tracks))
    def play_midieventstream(self, midieventstream):
        if sys.platform == 'win32':
            self._play_midieventstream_system(*midieventstream)
        else:
            self._play_midieventstream_fork_execl(*midieventstream)
    def _play_midieventstream_system(self, midieventstream):
        midieventstream.create_midifile(self.m_tmpfilename)
        os.system(self.join_cmd_and_filename(self.m_cmd, self.m_tmpfilename))
        # spawnl(os.P_DETACH, 'c:\\python20\\python', 'python', 'script.py')
        # #P_DETACH'ed can be killed
    def _play_midieventstream_fork_execl(self, midieventstream):
        midieventstream.create_midifile(self.m_tmpfilename)
        if not os.path.isfile(self.m_cmd.split()[0]):
            msg = _("""Failed to play music because the selected midi player program '%s' does not exist. You have to change the sound setup from the preferences window (Ctrl-F12).""") % self.m_cmd.split()[0]
            if self.error_report_cb:
                self.error_report_cb(msg)
            else:
                print >> sys.stderr, "\n".join(textwrap.wrap(msg))
            return
        if self.__child_pid:
            try:
                os.kill(self.__child_pid, signal.SIGKILL)
            except OSError:
                pass
            os.wait()
            self.__child_pid = None
        pid = os.fork()
        if pid == 0:
            v = self.join_cmd_and_filename(self.m_cmd, self.m_tmpfilename).split(" ")
            v = v[:1] + [""] + v[1:]
            try:
                os.execlp(*v)
            except OSError, x:
                print "OSError while trying to invoke external midiplayer:", x
                print "Please check your sound setup!"
                os._exit(-1)
        else:
            self.__child_pid = pid
    def stop(self):
        if sys.platform != 'win32':
            if self.__child_pid:
                try:
                    os.kill(self.__child_pid, signal.SIGKILL)
                except OSError:
                    pass
                os.wait()
                self.__child_pid = None

def test():
    synth = MidiFileSynth("/usr/bin/timidity %s", True)
    sys.path.insert(0, ".")
    import mpd
    from mpd.track import track
    from mpd.rat import Rat
    import src.i18n
    src.i18n.setup(".")
    t1 = track.Track()
    t1.set_bpm(520, 4)
    for i in range(70, 90):
        t1.start_note(i, 127)
        t1.notelen_time(Rat(1, 4))
        t1.stop_note(i, 127)
    t2 = track.Track()
    for i in reversed(range(80, 100)):
        t2.start_note(i, 127)
        t2.notelen_time(Rat(1, 4))
        t2.stop_note(i, 127)
    synth.play_track(track.MidiEventStream(t1, t2))

if __name__ == '__main__':
    test()

