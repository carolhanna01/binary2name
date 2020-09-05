########### msgdata.tcl
# Routines (part of main program) for dealing with DATA.
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
# $Id: msgdata.tcl,v 1.35 2001/03/21 23:53:14 ian Exp $

# While receiving the message data, we are either:
# * Receiving the header:
#     No DATA has been issued to the local MTA; the header info is
#     accumulating in our VM.  We may have address verification
#     threads outstanding.  In this state rej_*/defer_* variables
#     indicate whether we will want to accept or reject the message
#     when we've seen all the headers.  In this state
#     state(hdrscomplete) is unset.  Functions called in this state
#     have names starting hdr_...
# * Awaiting address verifications:
#     We've received and parsed the header, but we're waiting for
#     outstanding addresses from the header to finish verifying.  In
#     this state we're always calling msg_maincontrol.  In this state
#     state(hdrscomplete) is 1.
# * Receiving data, thinking we will accept the message:
#     DATA has been issued to the local MTA and the header and perhaps
#     some of the body have been sent to it.  Functions called in this
#     state have names starting body_...
# * Receiving the body, but we will reject or discard the message:
#     The local MTA has not been sent DATA.  We just throw away the
#     data, and we pass about the SMTP status reply to give to the
#     caller.  Functions called in this state have names starting
#     bodyrej_...
# Functions callable in any of these states have names starting msg_...

# Header parsing ...
#  a_kinds                 Can't    Do what       Pass     Response
#                          parse?   with addrs?   to MTA?  to final dot
#  lax                     Ignore   Ignore        Pass     250?
#  unchecked               Ignore   Ignore        Pass     250?
#  unchecked + lax         Ignore   Ignore        Pass     250?
#  normal                  550      Check         Pass     250?
#  normal + lax            550      Check         Pass     250?
#  bait                    Ignore   Blacklist     Discard  250
#  bait + lax              Ignore   Blacklist     Pass     250?
#  unchecked + bait        Ignore   Blacklist     Pass     250?
#  unchecked + bait + lax  Ignore   Blacklist     Pass     250?
#  normal + bait           Ignore   Blacklist     Discard  550
#  normal + bait + lax     Ignore   Blacklist     Discard  550

thread_subproc ic a_kind {kind} {
    return [expr {[lsearch -exact $kind $state(a_kinds)] >= 0}]
}

thread_subproc ic rej_bl {what inwhere} {
    ic_rej "Blacklisted $inwhere `[proto_quote $what]'"
}

thread_subproc ic rej {what} {
    if {[lsearch -exact $state(rej_msg) $what] == -1} {
	lappend state(rej_msg) $what
    }
}

thread_chainproc ic askfordata_done {} {
    set state(header) {}
    set state(currenthn) {}
    set state(currenthl) {}
    set state(resentmids) {}
    set orig {}
    if {[string length $state(mf_dm)]} {
	lappend orig $state(mf_lp)@$state(mf_dm)
    }
    set state(originators) $orig
    set state(originators_tochk) {}
    chanset_hide $state(chan) 3 1
    foreach k $state(a_kinds) { set ka($k) 1 }
    set state(a_kinds) [lsort [array names ka]]
    threadio_gets ic $id $state(chan) hdr_read {}
}

thread_chainproc ic hdr_read {data} {
    # This is the main event callback for reading header lines.
    
    global max_header_size blacklist_message require_messageid
    regexp {^.*} $data data
    ic_msg_checkeof
    if {"$data" == "."} { set eom 1 } else { set eom 0 }
    debug 3 "hdr_read >$data<"
    if {[regexp {^[ \t]} $data]} {
	if {![string length $state(currenthn)]} {
	    ic_hdr_bad 0 \
		    "first line of header was header field continuation"
	    return
	}
	append state(currenthl) "\n$data"
	if {[string length $state(currenthl)] > $max_header_size} {
	    ic_hdr_bad 0 \
 "continued header $state(currenthn) too large (>$max_header_size bytes)"
	    return
	}
    } elseif {[regexp {^([\041-\071\073-\176\241-\376]+)[ \t]*:} \
	              $data all newhn]} {
	ic_hdr_process1 $state(currenthn) $state(currenthl)
	set state(currenthn) $newhn
	set state(currenthl) $data
    } elseif {$eom || ![string length $data]} {
	# End of headers
	ic_hdr_process1 $state(currenthn) $state(currenthl)
	set state(currenthn) {}
	set state(currenthl) {}
	if {!$eom} { append state(header) "$data\n" }
	if {![llength $state(originators)]} {
	    ic_rej "No originators in envelope or body"
	} elseif {$require_messageid && ![info exists state(mid)]} {
	    ic_rej "No Message-ID header"
	} elseif {$require_messageid &&
		  [info exists state(resentany)] &&
		  ![llength $state(resentmids)]} {
	    ic_rej "Resent- header(s), but no Resent-Message-ID"
	}
	ic_hdr_endhdrs $eom
	return
    } else {
	ic_hdr_bad 0 "Header data malformed"
	return
    }
    threadio_gets ic $id $state(chan) hdr_read {}
}

thread_subproc ic hdr_bad {eom problem} {
    ic_rej $problem
    ic_hdr_endhdrs $eom
}

thread_subproc ic msg_checkeof {} {
    global canonical_hostname
    if {[eof $state(chan)]} {
	catch_close_cleardesc $state(mtachan)
	ic_commandfinalresponse major \
		"421 $canonical_hostname Connection dropped in message data"
	return -code return
    }
}

thread_subproc ic hdr_process1 {hn hl} {
    global blacklist_message max_header_size errorInfo header_reject_res
    global errorCode errorInfo
    debug 3 "hdr_process1 >$hn|$hl<"
    if {![string length $hn]} return
    append state(header) "$hl\n"
    set lowerhn [string tolower $hn]
    if {[string length $state(header)] > $max_header_size} {
	ic_rej "header $hn too large (>$max_header_size bytes)"
	return
    }
    regsub {^[^:]+:[ \t]*} $hl {} hl
    if {[regexp -nocase \
	    {^resent-(from|reply-to|sender|message-id|to|cc|bcc|date)$} \
	    $hn]} {
	set state(resentany) 1
    }
    if {[regexp -nocase {^message-id$} $hn]} {
	regsub -nocase {^message-id:[ \t\n]*} $hl {} thismid
	if {[info exists state(mid)]} {
	    ic_rej "Message-ID header appears twice"
	    append state(mid) "/$thismid"
	} else {
	    set state(mid) $thismid
	}
    } elseif {[regexp -nocase {^resent-message-id$} $hn]} {
	regsub -nocase "^resent-message-id:\[ \t\n\]*" $hl {} thismid
	lappend state(resentmids) $thismid
    }
    if {[regexp -nocase {^(resent-)?(from|sender|reply-to)$} $hn]} {
	if {[catch {
	    ic_hdr_recipients $hn $hl
	} emsg]} {
	    if {"$errorCode" == "SAUCE BADHDR"} {
		debug 3 "header error >$emsg|$errorInfo<"
		ic_rej "error in $hn header: [singleline $emsg]"
	    } else {
		error $emsg "$errorInfo\nin $hn header" $errorCode
	    }
	}
    }
    if {[info exists header_reject_res($lowerhn)]} {
	foreach re $header_reject_res($lowerhn) {
	    if {[info exists state(add_bl)]} break
	    if {[catch {
		if {[regexp -- $re $hl]} {
		    ic_rej "policy error in $hn header"
		}
	    } emsg]} {
		log error "header rejection regexp problem ($re): $emsg"
	    }
	}
    }
}

thread_subproc ic hdr_err {emsg} { error $emsg {} {SAUCE BADHDR} }

thread_subproc ic hdr_recipients {hn tf} {
    set lowerhn [string tolower $hn]
    debug 3 "hdr_recipients >$hn|$tf<"
    set colev 0
    set uq {}
    while {[string length $tf]} {
	debug 3 "hdr_recipients >$lowerhn|$tf|$uq<"
	if {[regexp {^[ \n\t]+(.*)$} $tf all tf]} {
	} elseif {[regexp {^\((.*)$} $tf all tf]} {
	    incr colev
	} elseif {$colev} {
	    if {[regexp {^\)(.*)$} $tf all tf]} {
		incr colev -1
	    } elseif {[regexp {^[^\\\n()]+(.*)$} $tf all tf]} {
	    } elseif {[regexp {^\\.(.*)$} $tf all tf]} {
	    } elseif {[regexp {^\n(.*)$} $tf all tf]} {
	    } else {
		ic_hdr_err "invalid text in comment"
	    }
	} elseif {[regexp \
 {^([\055\041\043-\047\051-\053\057-\071\075\077\101-\132\136-\176\200-\376]+)(.*)} \
                   $tf all xt tf]} {
	    binary scan $xt H* xt
            append uq $xt
	} elseif {[regexp {^([][()<>@,;:\.])(.*)} $tf all xt tf]} {
            append uq $xt
	} elseif {[regexp {^"(.*)$} $tf all tf]} {
	    while {[regexp {^([^"\\\n]+)(.*)$} $tf all qt tf] || \
		   [regexp {^\\(.)(.*)$} $tf all qt tf] || \
		   [regexp {^\\(\n)(.*)$} $tf all qt tf]} {
	        binary scan $qt H* qt
	        append uq $qt
	    }
	    if {![regexp {^\"(.*)$} $tf all tf]} {
		ic_hdr_err "missing end of quoted string"
	    }
	} else {
	    ic_hdr_err "lexical error"
	}
    }
    append uq ,
    while {[string length $uq]} {
	debug 3 "hdr_recipients >$uq<"
	if {[regsub {^[0-9a-f.]+:([][0-9a-f.@,<>]*);} $uq {\1,} uq]} {
	} elseif {[regexp {^[0-9a-f.]*<([][0-9a-f.@]+)>,(.*)} \
		          $uq all ras uq]} {
	    regsub {^(@[][0-9a-f.]:)*} $ras {} ras
	    ic_hdr_1recipient $hn $ras
	} elseif {[regexp {^([][0-9a-f.@]+),(.*)} $uq all ras uq]} {
	    ic_hdr_1recipient $hn $ras
	} elseif {[regexp {^,(.*)} $uq all uq]} {
	} else {
	    ic_hdr_err "syntax error"
	}
    }
}

thread_subproc ic hdr_1recipient {hn ras} {
    debug 3 "hdr_1recipient >$hn|$ras<"
    if {![regexp {^([0-9a-f.]+)@([][0-9a-f.]+)$} $ras all lp dm]} {
	ic_hdr_err "invalid address"
    }
    regsub -all {\.} $lp 2e lp
    regsub -all {\.} $dm 2e dm
    set lp [binary format H* $lp]
    set dm [binary format H* $dm]

    set addr $lp@$dm
    lappend state(originators) $addr
    if {[ic_a_kind normal] && ![ic_a_kind bait]} {
	lappend state(originators_tochk) [list $lp $dm $state(currenthn)]
    }
    if {"[ds_get addr-list $addr]" == "black"} {
	ic_rej_bl $addr $hn
    }
    ic_msg_maincontrol X
}

thread_subproc ic hdr_endhdrs {eom} {
    # Called when we want to stop header parsing and go on to either
    # accept the data into our MTA, or junk it.
    # eom==1 means we've had final dot already.
    
    set state(hdrscomplete) 1
    ic_msg_maincontrol $eom
}

thread_subproc ic msg_maincontrol {eom} {
    global blacklist_message

    # This function is called during header processing, when it
    # handles avf replies wrt one originator and simply queues the
    # next, if none have already been queued.  It is also called at
    # the end of header parsing, to check if more verifies need to be
    # waited for, and in that case (ie, if state(hdrscomplete) is
    # set), is responsible for passing the main flow of control.
    
    debug 3 "originators_tochk >$state(originators_tochk)<"

    while 1 {
	if {[llength $state(rej_msg)]} {
	    if {[info exists state(avfid)]} {
		thread_cancel avf $state(avfid)
		unset state(avfid)
	    }
	    set state(originators_tochk) {}
	}
	if {[info exists state(avfid)]} return
	if {![llength $state(originators_tochk)]} break

	manyset [lindex $state(originators_tochk) 0] lp dm hn
	set addr $lp@$dm
	set state(originators_tochk) [lreplace $state(originators_tochk) 0 0]
	if {![llength state(rej_msg)]} {
	    switch -exact -- [ds_get addr-list $addr] {
		white - whitesoon {
		}
		black {
		    ic_rej_bl $addr $hn
		}
		default {
		    set state(avfid) [thread_start avf \
			    "$state(desc) / verify $hn $lp@$dm" $lp $dm]
		    thread_join ic $id avf $state(avfid) msg_origverify_ok \
			    msg_origverify_err $eom $lp@$dm $hn
		    return
		}
	    }
	}
    }
    debug 3 "originators >$state(originators)<"
    # No originators left to check and no such check in progress
    if {![info exists state(hdrscomplete)]} return

    debug 2 "originators verified, all complete $state(originators)"
    # OK, we have whole headers.  There is no pending timeout or verification
    # thread, and are no unchecked originators.  We can accept or reject it !

    ic_msg_maybeblacklist

    set rej_this [concat $state(rej_conn) $state(rej_msg)]
    smtp_prefix_response [join $rej_this "\n"] 550 rej_response
    switch -exact $state(a_kinds) {
	{lax} - {unchecked} - {lax unchecked}
	- {bait lax} - {bait unchecked} - {bait lax unchecked} {
	    ic_body $eom $rej_this
	}
	{normal} - {bait normal}
	- {lax normal} - {bait lax normal} {
	    if {[llength $rej_this]} {
		ic_bodyrej $eom $rej_response
	    } else {
		# Due to ic_msg_maybeblacklist we can be sure no bait if here
		ic_body $eom {}
	    }
	}
	{bait} {
	    ic_bodyrej $eom "250 [ic_transactionid]b"
	}
	default {
	    error "internal error - kinds $state(a_kinds)"
	}
    }
}

thread_chainproc ic msg_origverify_ok {eom addr hn ok message} {
    unset state(avfid)
    if {!$ok} {
	ic_rej "$hn address `[proto_quote $addr]': [singleline $message]"
    }
    ic_msg_maincontrol $eom
}

thread_chainproc ic msg_origverify_err {eom addr hn message} {
    unset state(avfid)
    ic_msg_maincontrol $eom
}

# Accepting the message body ...

thread_subproc ic body {eom rejwarnings} {
    threadio_commandresponse ic $id $state(mtachan) data {} \
	    body_data_ok {} $eom $rejwarnings
}

thread_chainproc ic body_data_ok {eom rejwarnings data} {
    global add_received add_warnings canonical_hostname
    if {![regexp {^3[0-9][0-9]} $data]} {
	ic_bodyrej $eom $data
	return
    }
    chanset_hide $state(mtachan) 1 3
    set hdrdata {}
    if {$add_warnings} {
	foreach w $rejwarnings {
	    append hdrdata "X-SAUCE-Warning: ($canonical_hostname) $w\n"
	}
    }
    if {$add_received} {
	regsub {^.Name\: } {$Name: debian_version_0_7_7 $} {} rcsinfo
	if {![regexp {^[ $]*$} $rcsinfo]} {
	    regsub {^debian_version_} $rcsinfo v rcsinfo
	    regsub -all _ $rcsinfo . rcsinfo
	} else {
	    regsub {^.Revision\: } {$Revision: 1.35 $} r rcsinfo
	}
	regsub {[ $]*$} $rcsinfo {} rcsinfo
	set date [date_822]
	if {[string length $state(ident)]} {
	    set ident " ident $state(ident)"
	} else {
	    set ident ""
	}
	append hdrdata \
"Received: from $state(rh) (\[$state(ra)\])$ident
	  by $state(lh) (SAUCE $rcsinfo)
          with $state(smtpstyle) id [ic_transactionid]; $date\n"
    }
    append hdrdata $state(header)
    threadio_puts ic $id $state(mtachan) $hdrdata body_copy {} $eom
}

thread_chainproc ic body_copy {eom} {
    if {!$eom} {
	threadio_gets ic $id $state(chan) body_read {}
    } else {
	ic_body_eom
    }
}

thread_chainproc ic body_read {data} {
    ic_msg_checkeof
    if {"$data" == "."} {
	ic_body_eom
    } else {
	threadio_puts ic $id $state(mtachan) "$data\n" body_copy {} 0
    }
}

# Now we have final dot

thread_subproc ic body_eom {} {
    chanset_hide $state(mtachan) 1 1
    threadio_commandresponse ic $id $state(mtachan) "." {} body_finish_ok {}
}

thread_chainproc ic body_finish_ok {data} {
    global addr_whitelist_delay addr_whitelist_timeout addr_verified_timeout
    global site_whitelist_delay site_whitelist_timeout site_verified_timeout

    if {![regexp -- {^250[- ](.*)} $data dm realdata]} {
	ic_commandresponse major $data
    } else {
	set mid [ic_body_mid]
	set minfo [ic_body_minfo $mid]

	# We don't make whitelist entries as a result of messages we
	# wanted to reject.  Instead we verify them every time.  Sorry
	# folks !
	set wouldreject 0
	foreach rd {rej defer} {
	    foreach cm {conn msg} {
		if {[llength $state(${rd}_${cm})]} { set wouldreject 1 }
	    }
	}
	if {!$wouldreject} {
	    foreach as {addr site} \
		    itlist [list $state(originators) [list $state(ra)]] {
		foreach it $itlist {
		    set st [ds_get $as-list $it]
		    switch -exact -- $st {
			unknown - verified {
			    set sl [list \
				    whitesoon [set ${as}_whitelist_delay] \
				    white [set ${as}_whitelist_timeout]]
			}
			white {
			    set sl [list white [set ${as}_whitelist_timeout]]
			}
			default { set sl {} }
		    }
		    if {![llength $sl]} continue
		    if {[catch {
			if {"[lindex $sl 0]" != "$st"} {
			    eval [list setstate $as $it "$mid $data"] $sl
			} else {
			    eval [list ds_set $as-list $it] $sl
			}
		    } emsg]} {
			log error \
 "cannot create whitelist entry for $as $it: $emsg"
		    }
		}
	    }
	}
	ic_commandresponse delivery \
		"250 [ic_transactionid] [singleline $realdata]"
	manyset [ic_getsiteannoy 0] cannoy cannoydesc
	log notice \
"accepted $minfo via $state(rh) [string tolower $cannoydesc]=${cannoy}ms $data"
    }
    ic_msg_resetvars
}

thread_subproc ic transactionid {} {
    return sauce-$id-[expr {[clock seconds]/1000}]-$state(rcptcounter)
}

thread_subproc ic body_mid {} {
    if {[llength $state(resentmids)]} {
	return [lindex $state(resentmids) 0]
    } elseif {[info exists state(mid)]} {
	return $state(mid)
    } else {
	return "(No Message-ID)"
    }
}

thread_subproc ic body_minfo {mid} {
    set sender "$state(mf_lp)@$state(mf_dm)"
    if {"$sender" == "@"} { set sender {<>} }
    return "$mid from $sender"
}    

# Alternatively, discard the body and RSET the sub-MTA

thread_subproc ic bodyrej {eom why} {
    ic_msg_resethdrvars
    if {!$eom} {
	threadio_gets ic $id $state(chan) bodyrej_read {} $why
    } else {
	threadio_commandresponse ic $id $state(mtachan) rset {^2..} \
		bodyrej_resetmta_ok {} $why
    }
}

thread_chainproc ic bodyrej_read {why data} {
    ic_msg_checkeof
    if {"$data" == "."} {
	ic_bodyrej 1 $why
    } else {
	threadio_gets ic $id $state(chan) bodyrej_read {} $why
    }
}

thread_chainproc ic bodyrej_resetmta_greeting_ok {what data} {
    threadio_commandresponse ic $id $state(mtachan) \
	    "$state(helocmd) $state(helostring)" {^2..} \
	    bodyrej_resetmta_ok {} $what
}

thread_chainproc ic bodyrej_resetmta_ok {what data} {
    ic_commandresponse major $what
    ic_msg_resetvars
}

# Check if we want to blacklist

thread_subproc ic blacklist_thing {sa value tellwho valdesc} {
    upvar #0 ${sa}_blacklist_timeout bl_to
    set st [ds_get $sa-list $value]
    set cd [ic_transactionid]
    set add_bl [join $state(add_bl) "; "]
    switch -exact -- $st {
	unknown - whitesoon - verified {
	    log notice "$cd ... $sa $value ... blacklisting"
	    set thread [thread_start notifybl \
		    "$state(desc) / notify-bl $sa $value" \
		    $tellwho $valdesc $state(add_bl)]
	    thread_join {} {} notifybl $thread addbl_done addbl_err \
		    $state(desc) $tellwho "$sa $value"
	    setstate $sa $value $add_bl black $bl_to
	    return 1
	}
	black {
	    log notice "$cd ... $sa $value ... refreshing blacklist"
	    setstate addr $value $add_bl black $bl_to
	    return 1
	}
	white {
	    log notice "$cd ... $sa $value ... is whitelisted"
	    return 0
	}
	default {
	    log error "$cd ... $sa $value ... unknown state $st"
	    return 0
	}
    }
}

thread_subproc ic blacklist_site {} {
    ic_blacklist_thing site $state(ra) \
	    "postmaster@$state(cmdomain)" \
	    "Calling IP address `$state(ra)'"
}

thread_subproc ic msg_maybeblacklist {} {
    global blacklist_all_addresses always_blacklist_site
    global chan_desc blacknone_message

    if {![info exists state(add_bl)]} return

    if {![llength $state(rej_msg)] && ![llength $state(rej_conn)]} {
	ic_rej $blacknone_message
	# If we had some other reason to reject it, use that.
    }
    
    set anyadded 0
    set cd [ic_transactionid]
    log notice "$cd blacklisting because [join $state(add_bl) {; }]"
    foreach try $state(originators) {
	if {[ic_blacklist_thing addr $try $try \
		"Originator address `$try'"]} {
	    set anyadded 1
	    if {!$blacklist_all_addresses} break
	}
    }
    if {!$anyadded} {
	log notice "$cd ... unable to blacklist by originator ..."
    }
    if {!$anyadded || $always_blacklist_site} {
	if {[ic_blacklist_site]} {
	    set anyadded 1
	}
    }
    if {!$anyadded} {
	log notice "$cd ... unable to blacklist !"
	debug 1 "ra $state(ra) originators $state(originators) $state(header)"
    }
}

proc addbl_done {desc what where okcode} {
    log notice "$desc: blacklist notification sent to $where: $what: $okcode"
}

proc addbl_err {desc what where msg} {
    log notice "$desc: blacklist notification to $where failed: $what: $msg"
}

# General cleanup functions ...

thread_subproc ic msg_resetvars {} {
    catch { thread_cancel avf $state(avfid) }
    ic_msg_resethdrvars
    foreach x {mf_lp mf_dm mf_parms mid hdrscomplete} {
	catch { unset state($x) }
    }
    foreach x {rej_msg defer_msg delay_msg att_rcpts a_kinds resentmids} {
	set state($x) {}
    }
    foreach x {resentany} {
	set state($x) 0
    }

}

thread_subproc ic msg_resethdrvars {} {
    foreach x {header currenthn currenthl} { catch { unset state($x) } }
}
