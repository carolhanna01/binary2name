########## avf.tcl
#
# Address verification functions
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
# $Id: avf.tcl,v 1.21 2001/03/21 23:53:14 ian Exp $


########## avf threads
# thread_start avf $desc $localpart $domain
#
# success => 1 $message
# permanent failure => 0 $message
# temporary failure =>X

# state variables:
# dm            domain to be verified
# qaddr         quoted address to be verified
# tempfaillev   level of temp failure \  temp fail msgs with level>tempfaillev
# tempfail      temp failure message  /   will be stored in tempfail
# alltoid       overall timeout id
# dnsid         dns query sub-thread (unset => none)
# vc            channel for connection to remote SMTP
# vccount       see avfpool count argument/result
# caddr         IP address we're connected to (plus . if it's a fresh conn)
# mxhosts       list of MX record contents, including the preferences,
#               not including any that have already been tried, or whose
#               addresses are in mxaddrs, or which are currently being
#               looked up; set only in later stages of processing
# mxaddrs       list of addresses of MX's which we are currently trying;
#               set only in later stages of processing (first entry may
#               have . appended if we must try a fresh conn).
# mxerror       error encountered while trying to find the MX records
# toid          individual timeout (unset => none)
# conncurrently what we are currently doing on conn \ used to update tempfail
# conncurrentlylev error level if it doesn't work   / if things don't work out

thread_typedefine avf {localpart domain} {
    global verify_all_timeout avfpoolid
    if {![domain_ok $domain]} {
	thread_finish avf $id 0 "syntax error in domain"
	return
    }
    set state(dm) $domain
    set state(qaddr) [lp_quote $localpart]@$state(dm)
    if {[thread_crosscall avfpool $avfpoolid addr_checkcache $state(qaddr)]} {
	thread_finish avf $id 1 "Verified (cached)"
	return
    }
    set state(tempfaillev) 0
    set state(tempfail) "unknown failure"
    set state(alltoid) [thread_after avf $id $verify_all_timeout timeout]
    set state(dnsid) [thread_start dns $state(desc) $domain MX 0]
    thread_join avf $id dns $state(dnsid) mx_ok {}
} ERROR-ON-SHUTDOWN {
    catch { thread_cancel dns $state(dnsid) }
    catch { after cancel $state(alltoid) }
    catch { after cancel $state(toid) }
    catch_close_cleardesc state(vc)
}

thread_chainproc avf timeout {} {
    unset state(alltoid)
    thread_error avf $id "timed out - $state(tempfail)" {}
}

thread_chainproc avf mx_ok {answers emsgstr how} {
    unset state(dnsid)
    switch -exact -- $how {
	OK {
	    set state(mxhosts) [lsort -index 0 -integer $answers]
	    set state(mxaddrs) {}
	    avf_tempfail 10 "unable to find address for any mx for $state(dm)"
	    avf_tryaddrs
	}
	NXDOMAIN - MISCONFIG {
	    thread_finish avf $id 0 "$emsgstr"
	}
	NOTYPE {
	    set state(mxerror) $emsgstr
	    set state(dnsid) [thread_start dns $state(desc) $state(dm) A 0]
	    thread_join avf $id dns $state(dnsid) afallback_ok {}
	}
	default {
	    thread_error avf $id "$how" {}
	}
    }
}

thread_chainproc avf afallback_ok {answers emsgstr how} {
    unset state(dnsid)
    if {"$how" == "OK"} {
	set state(mxhosts) {}
	set state(mxaddrs) $answers
	avf_tempfail 20 "unable to contact (non-MX) host $state(dm)"
	avf_tryaddrs
    } else {
	thread_finish avf $id 0 "$state(mxerror); $emsgstr"
    }
}

thread_subproc avf tryaddrs {} {
    global remote_port verify_perconn_timeout
    global avfpoolid
    global avfchancounter

    while 1 {
	if {[llength $state(mxaddrs)]} {
	    set caddr [lindex $state(mxaddrs) 0]
	    set state(mxaddrs) [lreplace $state(mxaddrs) 0 0]
	    if {[regsub {\.$} $caddr {} caddr_addr]} {
		set state(vc) {}
	    } else {
		manyset [thread_crosscall avfpool $avfpoolid \
			chan_retrieve $caddr] \
			state(vc) state(vccount)
	    }
	    if {[string length $state(vc)]} {
		set state(caddr) $caddr_addr
		set state(toid) [thread_after avf $id \
			$verify_perconn_timeout conntimedout]
		avf_haveconnection
		return
	    } elseif {[catch {
		set state(vc) [socket -async $caddr_addr $remote_port]
		incr avfchancounter
		chanset_desc $state(vc) "verify $caddr:smtp $avfchancounter"
	    } emsg]} {
		avf_tempfail 40 "attempt connection to \[$caddr\]: $emsg"
		continue
	    } else {
                fconfigure $state(vc) \
			-translation {binary crlf} -blocking false
		set state(caddr) $caddr_addr.
		avf_conncurrently 43 "connect"
		set state(toid) [thread_after avf $id \
			$verify_perconn_timeout conntimedout]
		thread_fileevent avf $id $state(vc) writable connected
		return
	    }
	} elseif {[llength $state(mxhosts)]} {
	    set state(dnsid) [thread_start dns $state(desc) \
		    [lindex [lindex $state(mxhosts) 0] 1] A 0]
	    set state(mxhosts) [lreplace $state(mxhosts) 0 0]
	    thread_join avf $id dns $state(dnsid) a_ok a_err
	    return
	} else {
	    thread_error avf $id $state(tempfail) {}
	    return
	}
    }
}

thread_chainproc avf a_ok {answers emsgstr how} {
    unset state(dnsid)
    set state(mxaddrs) $answers
    if {![llength $answers]} { thread_finish avf $id 0 $emsgstr; return }
    avf_tempfail 30 "unable to contact any mail exchanger for $state(dm)"
    avf_tryaddrs
}    

thread_chainproc avf a_err {emsg} {
    unset state(dnsid)
    avf_tryaddrs
}

thread_chainproc avf conntimedout {} {
    catch_close_cleardesc state(vc)
    catch { unset state(vccount) }
    unset state(toid)
    avf_tempfail $state(conncurrentlylev) "$state(conncurrently) timed out"
    avf_tryaddrs
}

thread_chainproc avf connected {} {
    if {[catch {
	set state(lh) [lindex [fconfigure $state(vc) -sockname] 1]
    } emsg]} {
	thread_error avf $id \
 "get local host name for verification socket to \[$state(caddr)\]: $emsg" {}
    }
    fileevent $state(vc) writable {}
    avf_conncurrently 45 "greeting wait"
    threadio_commandresponse avf $id $state(vc) {} {^220} greeting_ok vc_err
}

thread_chainproc avf greeting_ok {data} {
    avf_conncurrently 50 "EHLO"
    threadio_commandresponse avf $id $state(vc) "EHLO $state(lh)" \
	    {^[25]..} ehlo_ok vc_err
}

thread_chainproc avf ehlo_ok {data} {
    global canonical_hostname
    set state(vccount) 0
    if {[regexp {^2[0-9][0-9]} $data]} {
	if {[regexp -nocase {(?m)^2[0-9][0-9][- ][ \t]*pipelining[ \t]*$} \
		"$data\n"]} {
	    set state(vccount) -1
	}
	avf_haveconnection
    } else {
	avf_conncurrently 50 "HELO"
	threadio_commandresponse avf $id $state(vc) \
		"HELO $canonical_hostname" {^2..} helo_ok vc_err
    }
}

thread_chainproc avf helo_ok {data} {
    avf_haveconnection
}

thread_subproc avf haveconnection {} {
    avf_conncurrently 60 "MAIL FROM:<>"
    if {$state(vccount) < 0} {
	threadio_commandresponse avf $id $state(vc) \
		"MAIL FROM:<>\nRCPT TO:<$state(qaddr)>\nRSET" \
		{^[25]..} mailfrom_ok_pipelining vc_err
    } elseif {$state(vccount) == 0} {
	threadio_commandresponse avf $id $state(vc) "MAIL FROM:<>" \
		{^[25]..} mailfrom_ok_synch vc_err
    } else {
	avf_rcptto_synch
    }
}

thread_chainproc avf mailfrom_ok_pipelining {data} {
    if {[avf_mailfrom_ok_but5xx $data]} return
    avf_conncurrently 70 "RCPT TO (for verify, pipelining)"
    threadio_commandresponse avf $id $state(vc) "" \
	    {^[245][0-9][0-9]} rcptto_done_pipelining vc_err
}

thread_chainproc avf rcptto_done_pipelining {data} {
    avf_conncurrently 80 "RSET (for verify, pipelining)"
    threadio_commandresponse avf $id $state(vc) "" \
	    {^2..} rset_ok_pipelining vc_err $data
}

thread_chainproc avf rset_ok_pipelining {rcptdata data} {
    avf_rcpt_process $rcptdata
}

thread_chainproc avf mailfrom_ok_synch {data} {
    if {[avf_mailfrom_ok_but5xx $data]} return
    avf_rcptto_synch
}

thread_subproc avf vc_kill {} {
    after cancel $state(toid)
    unset state(toid)
    catch { threadio_puts_throw $state(vc) "QUIT\r\n" }
    catch_close_cleardesc state(vc)
    catch { unset state(vccount) }
}

thread_subproc avf mailfrom_ok_but5xx {data} {
    if {[regexp {^2..} $data]} { return 0 }
    avf_vc_kill
    avf_result 0 "MAIL => $data"
    return 1
}

thread_subproc avf rcptto_synch {} {
    avf_conncurrently 70 "RCPT TO (for verify)"
    threadio_commandresponse avf $id $state(vc) "RCPT TO:<$state(qaddr)>" \
	    {^[245][0-9][0-9]} rcptto_done_synch vc_err
}

thread_chainproc avf rcptto_done_synch {data} {
    avf_rcpt_process $data
}

thread_subproc avf rcpt_process {data} {
    global avfpoolid
    if {[regexp {^2[0-9][0-9].*} $data text]} {
	thread_crosscall avfpool $avfpoolid addr_addcache $state(qaddr)
	set result 1
    } elseif {[regexp {^5[0-9][0-9].*} $data text]} {
	set result 0
    } else {
	avf_tempfail 90 "\[$state(caddr)\] $data"
	avf_vc_failed
	return
    }
    regsub {\.$} $state(caddr) {} caddr_addr
    thread_crosscall avfpool $avfpoolid chan_place \
	    $caddr_addr $state(vc) $state(vccount)
    unset state(vc)
    unset state(vccount)
    avf_result $result $text
}

thread_subproc avf result {ok text} {
    regsub -all {(?m)^(2[0-9][0-9]\-?[ \t]*)?} $text "\[$state(caddr)\] " text
    thread_finish avf $id $ok $text
}

thread_chainproc avf vc_err {emsg} {
    avf_tempfail $state(conncurrentlylev) "\[$state(caddr)\] $emsg"
    avf_vc_failed
}

thread_subproc avf vc_failed {} {
    avf_vc_kill
    if {![regexp {\.$} $state(caddr)]} {
	# try again with a new connection
	set state(mxaddrs) [concat $state(caddr). $state(mxaddrs)]
    }
    avf_tryaddrs
}

thread_subproc avf conncurrently {lev what} {
    append what " \[$state(caddr)\]"
    set state(conncurrently) $what
    set state(conncurrentlylev) [expr {$lev+1}]
    avf_tempfail $lev "$what failed"
}

thread_subproc avf tempfail {lev msg} {
    if {$state(tempfaillev) < $lev} {
	set state(tempfail) $msg
	set state(tempfaillev) $lev
    }
}
