########### avfpool.tcl
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
# $Id: avfpool.tcl,v 1.8 2001/03/21 23:53:14 ian Exp $

########## avfpool thread
# thread_start avfpool $desc
#
# thread_crosscall avfpool $avfpoolid chan_retrieve $ipaddress
# => [list $channel $count]
#    channel     channel id, for which caller is now responsible
#    count       number of recipients for the current message:
#                  0 -> MAIL FROM not yet done, caller must
#                       discard channel if it fails
#                 >0 -> that many, can do at least one more
#                 -1 -> PIPELINING supported, MAIL FROM not yet
#                       done, caller must RSET after use, and
#                       discard channel if RSET fails
# OR
# => {}
#
# thread_crosscall avfpool $avfpoolid chan_place $ipaddress $channel $count
#    channel     channel id, as before.  avfpool is now responsible
#    count       count value, as got back from retreive.  avfpool
#                will increment this, so caller doesn't need to
#
# thread_crosscall avfpool $avfpoolid addr_checkcache $address
# => 1    -> address verified recently and is OK
# => 0    -> address not verified recently or not OK
#
# thread_crosscall avfpool $avfpoolid addr_addcache $address
# => {}
#  only call this if you have actually verified the address, not
#  if you got it out of the cache

# state variables:
# chl            list of channels, each entry is [list $chan $ipaddr $toid $count]
# chlrset        list of channels currently awaiting RSET completion,
#                each entry is [list $chan $ipaddr $toid]
# chlclosing     list of channels currently being closed, entry is [list $chan $toid]
#
# global variables:
# avfpool_eaddrcache  array indexed by email address; answer is timeout id for expiry

thread_typedefine avfpool {} {
    global avfpool_eaddrcache
    set state(chl) {}
    set state(chlrset) {}
    set state(chlclosing) {}
    catch {
	unset avfpool_eaddrcache
    }
} NO-CLEAN-SHUTDOWN {
    global avfpool_eaddrcache
    log error "avfpool died, clearing out !"
    foreach chlwhich {chl chlrset chlclosing} {
	catch {
	    foreach che $state($chlwhich) {
		manyset $che chan toid
		catch_close_cleardesc chan
		catch { after cancel $toid }
	    }
	}
    }
    catch {
	foreach toid [array names avfpool_eaddrcache] {
	    catch { after cancel $avfpool_eaddrcache($toid) }
	}
    }
    catch {
	unset avfpool_eaddrcache
    }
}

thread_chainproc avfpool chan_retrieve {ipaddress} {
    set ix 0
    foreach che $state(chl) {
	manyset $che chan toid ipaddr count
	if {"$ipaddr" == "$ipaddress"} {
	    after cancel $toid
	    set state(chl) [lreplace $state(chl) $ix $ix]
	    return [list $chan $count]
	}
	incr ix
    }
    return {}
}

thread_chainproc avfpool chan_place {ipaddress chan count} {
    global max_verify_rcpts verify_rset_timeout verify_reuse_timeout
    if {$count >= $max_verify_rcpts} {
	set toid [thread_after avfpool $id $verify_rset_timeout rset_timedout $chan {}]
	lappend state(chlrset) [list $chan $toid $ipaddress]
	threadio_commandresponse avfpool $id $chan rset {^2..} rset_ok rset_failed $chan
    } else {
	if {$count >= 0} {
	    incr count
	}
	set toid [thread_after avfpool $id $verify_reuse_timeout chanuse_timedout $chan]
	lappend state(chl) [list $chan $toid $ipaddress $count]
    }
}

thread_chainproc avfpool rset_ok {channel data} {
    global verify_reuse_timeout
    manyset [avfpool_retrdata chlrset $channel] chan toid ipaddr
    after cancel $toid
    set toid [thread_after avfpool $id $verify_reuse_timeout chanuse_timedout $chan]
    lappend state(chl) [list $chan $toid $ipaddr 0]
}

thread_chainproc avfpool rset_failed {channel why} {
    manyset [avfpool_retrdata chlrset $channel] chan toid ipaddr
    after cancel $toid
    catch_close_cleardesc chan
}

thread_chainproc avfpool rset_timedout {channel} {
    manyset [avfpool_retrdata chlrset $channel] chan toid ipaddr
    catch_close_cleardesc chan
}

thread_chainproc avfpool chanuse_timedout {channel} {
    global verify_quit_timeout
    manyset [avfpool_retrdata chl $channel] chan toid ipaddr count
    set toid [thread_after avfpool $id $verify_quit_timeout quit_timedout $chan]
    lappend state(chlclosing) [list $chan $toid]
    threadio_commandresponse avfpool $id $chan QUIT {^221} quit_done quit_done $chan
}

thread_chainproc avfpool quit_done {channel data} {
    manyset [avfpool_retrdata chlclosing $channel] chan toid
    after cancel $toid
    catch_close_cleardesc chan
}

thread_chainproc avfpool quit_timedout {channel} {
    manyset [avfpool_retrdata chlclosing $channel] chan toid
    catch_close_cleardesc chan
}

thread_subproc avfpool retrdata {chlwhich channel} {
    set ix 0
    foreach che $state($chlwhich) {
	if {"[lindex $che 0]" == "$channel"} {
	    set state($chlwhich) [lreplace $state($chlwhich) $ix $ix]
	    return $che
	}
	incr ix
    }
    error "channel $channel not found in $chlwhich $state($chlwhich)"
}

thread_chainproc avfpool addr_checkcache {address} {
    global avfpool_eaddrcache
    return [info exists avfpool_eaddrcache($address)]
}

thread_chainproc avfpool addr_addcache {address} {
    global avfpool_eaddrcache verify_cache_timeout
    if {[info exists avfpool_eaddrcache($address)]} {
	after cancel $avfpool_eaddrcache($address)
    }
    set avfpool_eaddrcache($address) [thread_after avfpool $id \
	    $verify_cache_timeout addr_timedout $address]
}

thread_chainproc avfpool addr_timedout {address} {
    global avfpool_eaddrcache
    unset avfpool_eaddrcache($address)
}
