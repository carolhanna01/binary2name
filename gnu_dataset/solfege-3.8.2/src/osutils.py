
import os
import sys
import signal

class ExecutableDoesNotExist(Exception):
    def __init__(self, v):
        Exception.__init__(self, 
          _("The executable '%s' does not exist. Please check your setup.") % v)


__all__ = ['run_external_program', 'ExecutableDoesNotExist']
__child_pid = None
def run_external_program(cmdstring, wdir, argument):
    global __child_pid
    if "%s" not in cmdstring:
        cmdline = "%s %s" % (cmdstring, argument)
    else:
        cmdline = cmdstring % argument
    wdir = os.path.normpath(wdir)
    cur_dir = os.getcwd()
    v = cmdline.split()
    v = v[:1] + v
    if not os.path.exists(os.path.join(wdir, v[0])):
        raise ExecutableDoesNotExist(v[0])
        return
    if sys.platform == 'win32':
        os.system(cmdline)
        return
    try:
        if __child_pid is not None:
            os.kill(__child_pid, signal.SIGKILL)
            os.wait()
        __child_pid = None
    except OSError:
        print "kill failed, OSError"
    except AttributeError:
        __child_pid = None
    pid = os.fork()
    if pid == 0:
        os.chdir(wdir)
        try:
            os.execlp(*v)
        except OSError, x:
            print >> sys.stderr, "OS Error:", x
            os._exit(-1)
            os.chdir(cur_dir)
        os.chdir(cur_dir)
    else:
        __child_pid = pid


