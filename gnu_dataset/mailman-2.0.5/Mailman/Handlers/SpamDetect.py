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

"""Do more detailed spam detection.

This module hard codes site wide spam detection.  By hacking the
KNOWN_SPAMMERS variable, you can set up more regular expression matches
against message headers.  If spam is detected the message is discarded
immediately.

TBD: This needs to be made more configurable and robust.
"""

import re
import HandlerAPI

class SpamDetected(HandlerAPI.DiscardMessage):
    """The message contains known spam"""


# This variable contains a list of 2-tuple of the format (header, regex) which
# this module uses to match against the current message.  If the regex matches
# the given header in the current message, then it is flagged as spam.  header
# can be None to indicate regex search of the body of the message.  Note that
# the more searching done, the slower this whole process gets.

KNOWN_SPAMMERS = []



def process(mlist, msg, msgdata):
    if msgdata.get('approved'):
        return
    for header, regex in KNOWN_SPAMMERS:
        cre = re.compile(regex, re.IGNORECASE)
        if header is None:
            text = msg.body
        else:
            text = msg.get(header)
            if not text:
                continue
        mo = cre.search(text)
        if mo:
            # we've detected spam, so throw the message away
            raise SpamDetected
