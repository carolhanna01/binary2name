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

"""Determine whether the message is approved for delivery.

This module only tests for definitive approvals.  IOW, this module only
determines whether the message is definitively approved or definitively
denied.  Situations that could hold a message for approval or confirmation are
not tested by this module.

"""

import string
import HandlerAPI
from Mailman import mm_cfg
from Mailman import Errors

# multiple inheritance for backwards compatibility
class LoopError(HandlerAPI.DiscardMessage, Errors.MMLoopingPost):
    """We've seen this message before"""



def process(mlist, msg, msgdata):
    # short circuits
    if msgdata.get('approved'):
        # digests, Usenet postings, and some other messages come
        # pre-approved.  TBD: we may want to further filter Usenet messages,
        # so the test above may not be entirely correct.
        return
    # See if the message has an Approved: header with a valid password
    passwd = msg.getheader('approved')
    if passwd and mlist.ValidAdminPassword(passwd):
        # TBD: should we definitely deny if the password exists but does not
        # match?  For now we'll let it percolate up for further
        # determination.
        msgdata['approved'] = 1
    # has this message already been posted to this list?
    beentheres = map(filterfunc, msg.getallmatchingheaders('x-beenthere'))
    if mlist.GetListEmail() in beentheres:
        raise LoopError



def filterfunc(s):
    return string.strip(string.split(s, ': ')[1])
