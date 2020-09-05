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


"""Shared mailman errors and messages."""


# exceptions for problems related to opening a list
class MMListError(Exception): pass
class MMUnknownListError(MMListError): pass
class MMCorruptListDatabaseError(MMListError): pass
class MMListNotReadyError(MMListError): pass
class MMListAlreadyExistsError(MMListError): pass


# XXX: These should be converted to new style class exceptions
MMBadUserError       = "MMBadUserError"


# Exception hierarchy for various authentication failures, can be
# raised from functions in SecurityManager.py
class MMAuthenticationError(Exception): pass
class MMBadPasswordError(MMAuthenticationError): pass
class MMPasswordsMustMatch(MMAuthenticationError): pass
class MMCookieError(MMAuthenticationError): pass
class MMExpiredCookieError(MMCookieError): pass
class MMInvalidCookieError(MMCookieError): pass

MMMustDigestError    = "MMMustDigestError"
MMCantDigestError    = "MMCantDigestError"
MMNotAMemberError    = "MMNotAMemberError"
MMNoSuchUserError    = "MMNoSuchUserError"
MMNeedApproval       = "MMNeedApproval"
MMSubscribeNeedsConfirmation = "MMSubscribeNeedsConfirmation"
MMBadConfirmation    = "MMBadConfirmation"
MMAlreadyAMember     = "MMAlreadyAMember"
MMAlreadyDigested    = "MMAlreadyDigested"
MMAlreadyUndigested  = "MMAlreadyUndigested"

MODERATED_LIST_MSG    = "Moderated list"
IMPLICIT_DEST_MSG     = "Implicit destination"
SUSPICIOUS_HEADER_MSG = "Suspicious header"
FORBIDDEN_SENDER_MSG  = "Forbidden sender"



# New style class based exceptions.  All the above errors should eventually be
# converted.

class MailmanError(Exception):
    """Base class for all Mailman exceptions."""
    pass


class MMLoopingPost(MailmanError):
    """Post already went through this list!"""
    pass


# Exception hierarchy for bad email address errors that can be raised from
# Utils.ValidateEmail()
class EmailAddressError(MailmanError):
    """Base class for email address validation errors."""
    pass

class MMBadEmailError(EmailAddressError):
    """Email address is invalid (empty string or not fully qualified)."""
    pass

class MMHostileAddress(EmailAddressError):
    """Email address has potentially hostile characters in it."""
    pass


# Exceptions for admin request database
class LostHeldMessage(MailmanError):
    """Held message was lost."""
    pass
