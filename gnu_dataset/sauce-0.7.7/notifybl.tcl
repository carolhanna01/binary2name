########### msgdata.tcl
# Code for notifying people that they have been blacklisted.
#
# This file is part of SAUCE, a very picky anti-spam receiver-SMTP.
# SAUCE is Copyright (C) 1997-2001 Ian Jackson
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
#
# $Id: notifybl.tcl,v 1.9 2001/03/21 23:53:14 ian Exp $

########## notifybl threads
#
# thread_start notifybl $desc $addr_to_notify $desc_what_blacklisted $why
#
# state variables:
# 
# lp            local part to receive notice
# dm            domain to receive notice
# toid          global timeout
# chan          local MTA channel
# whatbl        what is being blacklisted
# why           and why
# okcode        the response code from the local MTA

thread_typedefine notifybl {notifyaddr whatbl why} {
    global notifybl_timeout
    debug 1 [list notifybl $notifyaddr $whatbl $why]
    if {![regexp -nocase {^(.+)\@([-.0-9a-z]+)$} $notifyaddr all lp state(dm)]} {
	error "trying to notify address $notifyaddr"
    }
    set state(lp) [lp_quote $lp]
    set state(chan) [open |[list sendmail -bs -oem] r+]
    set state(whatbl) $whatbl
    set state(why) $why
    fconfigure $state(chan) -blocking false -translation {binary crlf}
    chanset_desc $state(chan) "$state(desc) / local"
    set state(toid) [thread_after notifybl $id $notifybl_timeout timedout]
    threadio_commandresponse notifybl $id $state(chan) {} {^220} greeting_ok {}
} {
    # ignore shutdown requests
} {
    catch { after cancel $state(toid) }
    catch { close $state(chan) }
}


thread_chainproc notifybl greeting_ok {data} {
    global canonical_hostname
    threadio_commandresponse notifybl $id $state(chan) "HELO $canonical_hostname" \
	    {^2[0-9][0-9]} helo_ok {}
}

thread_chainproc notifybl helo_ok {data} {
    global canonical_hostname notifybl_bounces
    set bffrom $notifybl_bounces
    if {[string length $bffrom]} { append bffrom @ $canonical_hostname }
    threadio_commandresponse notifybl $id $state(chan) \
	    "MAIL FROM:<$bffrom>" {^2[0-9][0-9]} mailfrom_ok {}
}

thread_chainproc notifybl mailfrom_ok {data} {
    threadio_commandresponse notifybl $id $state(chan) \
	    "RCPT TO:<$state(lp)@$state(dm)>" \
	    {^2[0-9][0-9]} rcptto_ok {}
}

thread_chainproc notifybl rcptto_ok {data} {
    threadio_commandresponse notifybl $id $state(chan) DATA {^354} data_ok {}
}

thread_chainproc notifybl data_ok {data} {
    global canonical_hostname notifybl_localpart blmessage
    chanset_hide $state(chan) 1 3
    threadio_puts notifybl $id $state(chan) \
"From: $notifybl_localpart@$canonical_hostname
To: Recipient.suppressed:;
Subject: $state(whatbl) has been blacklisted

$blmessage
.
" \
	message_ok {}
}

thread_chainproc notifybl message_ok {} {
    chanset_hide $state(chan) 1 1
    threadio_commandresponse notifybl $id $state(chan) {} {^250} finaldot_ok {}
}

thread_chainproc notifybl finaldot_ok {data} {
    regsub {^250[ \t]*} $data {} state(okcode)
    threadio_commandresponse notifybl $id $state(chan) QUIT {^221} quit_ok {}
}

thread_chainproc notifybl quit_ok {data} {
    thread_finish notifybl $id $state(okcode)
}

thread_chainproc notifybl timeout {} {
    thread_error notifybl $id "timed out"
}
