# Copyright (C) 1998,1999,2000 by the Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

"""Contains all the common functionality for msg bounce scanning API.

This module can also be used as the basis for a bounce detection testing
framework.  When run as a script, it expects two arguments, the listname and
the filename containing the bounce message.

"""

import traceback
from types import ListType

# testing kludge
if __name__ == '__main__':
    execfile('bin/paths.py')

from Mailman import mm_cfg
from Mailman import Errors
from Mailman.Logging.Syslog import syslog
from Mailman.pythonlib.StringIO import StringIO



# msg must be a mimetools.Message
def ScanMessages(mlist, msg, testing=0):
    pipeline = ['DSN',
                'Qmail',
                'Postfix',
                'Yahoo',
                'Caiwireless',
                'Smail',
                'Exim',
                'Netscape',
                'Compuserve',
                'Microsoft',
                'GroupWise',
                'SMTP32',
                'SimpleMatch',
                'Catchall',
                ]
    for modname in pipeline:
        mod = __import__('Mailman.Bouncers.'+modname)
        func = getattr(getattr(getattr(mod, 'Bouncers'), modname), 'process')
        addrs = func(msg)
        if addrs:
            for addr in addrs:
                # we found a bounce or a list of bounce addrs
                if not testing:
                    try:
                        mlist.RegisterBounce(addr, msg)
                    except Exception, e:
                        syslog('error', 'Bouncer exception: %s' % e)
                        s = StringIO()
                        traceback.print_exc(file=s)
                        syslog('error', s.getvalue())
                        return 0
                else:
                    print '%16s: detected address <%s>' % (modname, addr)
            # we saw some bounces
            return 1
        elif testing:
            print '%16s: no bounces detected' % modname
    # no bounces detected
    return 0



# for testing
if __name__ == '__main__':
    import mimetools
    from Mailman import MailList

    def usage(code, msg=''):
        print __doc__
        if msg:
            print msg
        sys.exit(code)

    if len(sys.argv) < 2:
        usage(1, 'required arguments: <file> [, <file> ...]')

    for filename in sys.argv[1:]:
        print 'scanning file', filename
        fp = open(filename)
        msg = mimetools.Message(fp)
        ScanMessages(None, msg, testing=1)
        fp.close()
