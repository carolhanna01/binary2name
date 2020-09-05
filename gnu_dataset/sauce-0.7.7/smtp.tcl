########### smtp.tcl
#
# Main SMTP protocol implementation
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
# $Id: smtp.tcl,v 1.6 2001/03/21 23:53:15 ian Exp $


########## connection threads
# thread_start $chan $desc $remoteaddr $remoteport
#
# errors/results ignored
#
# state variables:
#
# Always set:
# chan              incoming SMTP connection
# ra                calling IP address
# rp                calling port
# rh                calling hostname (or IP-literal), available only after HELO
# la                called (local) address
# lh                called (local) canonical hostname for interface
# lp                called (local) port
# rcptcounter       counter of total no of RCPTs issued in session, for logging
# smtpcmd           SMTP command we are processing, or empty string
# smtpreaderr       Last system error reading from incoming SMTP connection
# rej_conn          list of connection-wide rejection reasons (or {})
# defer_conn        deferral reasons (defer_* takes precedence over rej_*)
#                    none of these reasons may contain "\n"
# smtperrors        count of SMTP error responses
# quitting          set => we have been asked to shut down

# Generally useful
# avfid             ID of current address verification thread (unset => none)
# dnsid             thread for forward DNS lookups (unset => none)
# dnsptrid          thread for reverse DNS lookup (unset => none)
# lastchal          last challenge sent on this conn for SAUCEADMIN
# rbl_hits          list of RBL domains we've hit

# Used during connection setup, but not changed afterwards:
# cmdomain          calling host's mail domain to report to
# ident             result of ident lookup (whether informative or error)
# ichan             reverse ident lookup connection (unset => none)
# itoid             timeout for ident lookup or incoming command (unset=>none)

# Only set after HELO
# helostring        set => we have had helo/ehlo, and this was what they said
#                   this implies mtachan being open and connected
# smtpstyle         `smtp' or `esmtp'.
# mtachan           channel to local MTA
# helocmd           the command helo or ehlo that they used

# Only set after MAIL FROM; unset/reset for new messages:
# rej_msg           list of reasons why we're rejecting this message
# defer_msg         list of reasons why we're deferring this message
#                    (only ever set before we get to DATA to avoid late defer)
# delay_msg         set => we want to defer because of an RBL or newness &c
# att_rcpts         list of recipients they've asked for (perhaps not got)
#		     each entry is list: $class $rcpt $respcoode
# mf_lp             set => we have had MAIL FROM, and this is the local part
# mf_dm             MAIL FROM domain, may be garbage or unset if mf_lp not set
# a_kinds           list of recip kinds we've accepted (unchecked/normal/bait)
#                    we only allow ourselves to get into the following states:
#                     none unchecked normal bait unchecked+bait normal+bait
#                     and the same +lax (but avoid accepting normal after bait)
#                    for this var, nodelay addrs are normal.  Repeats allowed.
# mf_parms          parameters to MAIL FROM, if any

# add_bl            list of reasons to blacklist everyone to do with this msg
#                    none may contain newline

# Used/set while processing message body ([re]set by ic_msg_resetvars)
# header            accumulated header text
# currenthn         name of header field we're currently in
# currenthl         value of header field we're currently in, including name
# mid               value of Message-ID header field
# resentmids        list of Resent-Message-ID's ({} => none)
# resentany         1 => we have had a Resent- field, 0 => we haven't
# hdrscomplete      we have received and stored all the headers (that we
#                   are going to parse); now finish verifs & do body (unset/1)
# originators       List of origs we've seen (incl. env & headers) & checked
# originators_tochk Originators seen but not yet checked (with avf)
#                    (both in dequoted form), as [list $lp $dm $headername]

# Recipient delay:
# We can delay if we get mail from unknown senders or unknown hosts.
# The delay period can be increased if the sending host is RBLd.
# So: both sender and host on whitelist => no delay, no RBL lookup.
# Otherwise, do RBL lookup.  Sending host RBLd for reject => reject.
# Otherwise, delay until BOTH
#  first contact from this site was at least minimum site delay ago
#  first contact from this sender was at least minimum sender delay ago
# (minimum is calculated across all applicable RBLs, and new
# site or sender delay).
# 
# mf_message        proposed success message for MAIL FROM
# rblids            RBL lookups in progress (or empty list)
# minsiteage        min time (secs) since first contact with site, or we delay
# minaddrage        min time (secs) since first contact with addr, or we delay
# mf_lp, mf_dm      set

thread_typedefine ic {chan lalhlp ra rp} {
    global ident_port ident_timeout chan_desc
    set state(chan) $chan
    set state(ra) $ra
    set state(rp) $rp
    manyset $lalhlp state(la) state(lh) state(lp)
    set state(smtpcmd) {}
    set state(smtpreaderr) {}
    set state(rej_conn) {}
    set state(defer_conn) {}
    set state(smtperrors) 0
    set state(rcptcounter) 0

    set state(rej_msg) {}
    set state(defer_msg) {}
    set state(delay_msg) {}
    set state(att_rcpts) {}
    set state(rbl_hits) {}

    set state(rblids) {}
    set state(resentmids) {}
    set state(dnsptrid) \
	    [thread_start dnsptr "$state(desc) / reverse lookup" $ra]
    if {[catch {
	set state(ichan) [socket -myaddr $state(la) -async \
		                 $state(ra) $ident_port]
	chanset_desc $state(ichan) "$state(desc) / ident"
    } emsg]} {
	log notice "ident error connecting to $ra: $emsg"
	ic_ident_done {}
    } else {
	fconfigure $state(ichan) -translation {binary crlf} -blocking false
	set state(itoid) [thread_after ic $id $ident_timeout ident_timeout]
	thread_fileevent ic $id $state(ichan) writable ident_connected
    }
} {
    global canonical_hostname
    set state(quitting) 1
    if {![info exists state(header)] && ![info exists state(sofar)]} {
	catch_close_cleardesc state(mtachan)
	ic_commandfinalresponse immed "421 $canonical_hostname shutting down"
    }
} {
    ic_kill_subthreads
    catch { fileevent $state(chan) readable {} }
    catch { fileevent $state(chan) writable {} }
    catch_close_cleardesc state(mtachan)
}

thread_subproc ic kill_subthreads {} {
    foreach thr $state(rblids) { catch { thread_cancel dns $thr } }
    set state(rblids) {}
    catch_close_cleardesc state(ichan)
    catch { thread_cancel dnsptr $state(dnsptrid) }
    catch { thread_cancel avf $state(avfid) }
    catch { after cancel $state(itoid) }
    catch { unset state(itoid) }
    catch { after cancel $state(ptoid) }
    catch { unset state(ptoid) }
}

thread_chainproc ic ident_timeout {} {
    catch_close_cleardesc state(ichan)
    unset state(itoid)
    log notice "ident timeout on $state(ra)"
    ic_ident_done {}
}

thread_chainproc ic ident_connected {} {
    threadio_putsgets ic $id $state(ichan) "$state(rp) , $state(lp)\n" \
	    ident_rx_ok ident_rx_err
}

thread_chainproc ic ident_rx_ok {data} {
    after cancel $state(itoid)
    unset state(itoid)
    set eof [eof $state(ichan)]
    catch_close_cleardesc state(ichan)
    regexp {^.*} $data data
    if {$eof} {
	log notice "ident eof on $state(ra)"
	ic_ident_done {}
    } elseif {[regexp -nocase {^[ \t]*[0-9]+[ \t]*,[ \t]*[0-9]+[ \t]*:[ \t]*userid[ \t]*:[^:]*:([^:]*)$} $data all userid]} {
	ic_ident_done [string trim $userid]
    } elseif {[regexp -nocase {^[ \t]*[0-9]+[ \t]*,[ \t]*[0-9]+[ \t]*:[ \t]*error[ \t]*:(.*)$} $data all error]} {
	log notice "ident remote error on $state(ra): [string trim $error]"
	ic_ident_done {}
    } else {
	log notice "ident gave garbage on $state(ra): [string trim $data]"
	ic_ident_done {}
    }
}

thread_chainproc ic ident_rx_err {emsg} {
    log debug "ident failed on $state(ra): $emsg"
    ic_ident_done {}
}

thread_subproc ic ident_done {ident} {
    catch { after cancel $state(itoid) }
    catch { unset state(itoid) }
    set state(ident) $ident
    thread_join ic $id dnsptr $state(dnsptrid) remotedns_ok remotedns_err
}

thread_chainproc ic remotedns_ok {answers emsgstr} {
    global require_reverse_dns
    if {[llength $answers]} {
	ic_remotedns_done [join $answers]
    } else {
	if {$require_reverse_dns} {
	    lappend state(rej_conn) "reverse DNS: $state(ra): $emsgstr"
	}
	ic_remotedns_done "\[$state(ra)\]"
    }
}

thread_chainproc ic remotedns_err {emsg} {
    global require_reverse_dns
    if {$require_reverse_dns} {
	lappend state(defer_conn) "reverse DNS: $state(ra): [singleline $emsg]"
    }
    ic_remotedns_done "\[$state(ra)\]"
}

thread_subproc ic remotedns_done {dnsresult} {
    global canonical_hostname
    unset state(dnsptrid)
    set state(rh) $dnsresult
    ic_commandresponse_maybefinal greeting -1 \
	    "220 $canonical_hostname sauce-smtpd ESMTP ready"
}

thread_subproc ic commandresponse {evtype response} {
    ic_commandresponse_maybefinal $evtype 0 $response
}

proc intern_getsiteannoy {ra change} {
    global annoy_halflife annoy_grudge_max annoy_love_max
    global annoy_grumpy annoy_actout_max local_interface

    if {[info exists local_interface($ra)]} { return {0 Submissive} }

    set ca [ds_get site-annoy $ra]
    set now [clock seconds]
    if {"$ca" == "unknown"} {
	set cv 0
    } else {
	manyset [string map {a { } m -} $ca] ct cv
	set newcv [expr {
	    round( floor(
	    $cv * pow( 0.5, double($now-$ct)/$annoy_halflife )
	    ))
	}]
	debug 2 cv=$cv now=$now ct=$ct hl=$annoy_halflife newcv=$newcv
	set cv $newcv
    }
    incr cv $change
    if {$cv > $annoy_grudge_max} { set cv $annoy_grudge_max }
    if {$cv < -$annoy_love_max} { set cv -$annoy_love_max }
    ds_set site-annoy $ra \
	    [string map {{ } a - m} [list $now $cv]] \
	    [expr {$now + 3*$annoy_halflife}]
    if {$cv <= -$annoy_love_max/2 && $cv <= -($annoy_grumpy+$annoy_actout_max)} {
	set irritamt Ecstatic
    } elseif {$cv <= 0} {
	set irritamt Pleased
    } elseif {$cv <= $annoy_grumpy} {
	set irritamt Irritated
    } elseif {$cv <= $annoy_grumpy+$annoy_actout_max} {
	set irritamt Angry
    } else {
	set irritamt Furious
    }
    return [list $cv $irritamt]
}

thread_subproc ic getsiteannoy {change} {
    return [intern_getsiteannoy $state(ra) $change]
}

thread_subproc ic commandresponse_maybefinal {evtype final response} {
    # final==-1 means initial (used because then we have to prefix
    # each stalling line); final==0 means normal; final==1 means final.
    global max_smtp_errors canonical_hostname
    global annoy_actout_max annoy_grumpy annoy_partrespevery annoy_actout_nopartresp
    global pleasure_command pleasure_delivery annoyance_major annoyance_minor

    if {"$evtype" != "immed" && [ic_check_quitting]} { return }

    switch -exact $evtype {
	command - delivery { set annoychange -[set pleasure_$evtype] }
	nopartresp { set annoychange -$pleasure_command }
	major - minor { set annoychange [set annoyance_$evtype] }
	rcpt-defer { set annoychange $annoyance_minor }
	immed - greeting { set annoychange 0 }
	default { error "$evtype ?" }
    }
    manyset [ic_getsiteannoy $annoychange] cv irritamt
    
    switch -exact $evtype {
	major - minor - rcpt-defer {
	    set delay $cv
	    incr state(smtperrors)

	    if {$final<=0 && $state(smtperrors) > $max_smtp_errors} {
		log notice "too many errors from $state(rh), closing channel (annoy=$cv)"
		ic_commandfinalresponse major \
			"421 $canonical_hostname $response \[too many errors\]"
		return
	    }
	}
	command { set delay [expr {$cv - $annoy_grumpy}] }
	nopartresp - greeting {
	    set delay [expr {$cv - $annoy_grumpy}]
	    if {$delay > $annoy_actout_nopartresp} {
		set delay $annoy_actout_nopartresp
	    }
	}
	delivery - immed { set delay 0 }
	default { error "$evtype ?" }
    }
    if {$delay > $annoy_actout_max} { set delay $annoy_actout_max }
    switch -exact $evtype {
	major {
	    logreject reject state command $response [string tolower $irritamt] ${cv}ms
	}
	minor {
	    if {$delay > 0} {
		logreject notice state delay $response [string tolower $irritamt] ${cv}ms
	    }
	}
	rcpt-defer {
	    logreject notice state rcpt-defer {} [string tolower $irritamt] ${cv}ms
	}
	command - delivery - nopartresp - greeting - immed { }
	default { error "$evtype ?" }
    }

    if {[string length $response]} {
	if {$delay > 0 || "$evtype" == "greeting"} {
	    regsub {(?m)$} $response " \[[irrit_present $irritamt]\]" response
	}
    } else {
	set delay 0
    }
    ic_commandresponsedelay $delay [expr {"$evtype" == "nopartresp"}] \
	    $final $response 0
}

thread_subproc ic commandresponsedelay {delay nopartresp final response f_ix} {
    global command_timeout annoy_partrespevery

    if {$final<=0 && [ic_check_quitting]} { return }

    if {!$nopartresp && $delay > $annoy_partrespevery} {
        incr delay -$annoy_partrespevery
	set state(ptoid) [thread_after ic $id $annoy_partrespevery \
		commandresponsedelay_part $delay $final $response $f_ix]
	return
    } elseif {$delay > 0} {
	set state(ptoid) [thread_after ic $id $delay \
		commandresponsedelay_after $final $response]
	return
    }
	
    chanset_hide $state(chan) 1 1
    if {[string length $response]} {
	append response "\n"
    }
    set state(smtpcmd) {}
    set state(smtpreaderr) {}
    if {$final>0} {
	chanset_hide $state(chan) 1 1
	threadio_puts ic $id $state(chan) $response tellquit_done tellquit_done
    } else {
	set state(itoid) [thread_after ic $id $command_timeout timedout]
	threadio_putsgets ic $id $state(chan) $response command_ok command_err
    }
}

thread_chainproc ic commandresponsedelay_part {delay final response f_ix} {
    global canonical_hostname fill_msgs

    if {[regexp {^([0-9][0-9][0-9]\-[^\n]*)\n(.*)$} $response dummy \
	    thisline remainder]} {
    } elseif {[regexp {^([0-9][0-9][0-9])\s([^\n]*)} $response dummy \
	    code rhs]} {
	set thisline "$code-$rhs"
	set remainder "$code "
	if {$final!=0} { append remainder "$canonical_hostname " }
	append remainder [lindex $fill_msgs $f_ix]
	set f_ix [expr {($f_ix+1) % [llength $fill_msgs]}]
    } else {
	error "incomprehensible commandresponsedelay_part response $response"
    }
    unset state(ptoid)
    threadio_puts ic $id $state(chan) "$thisline\n" \
	    commandresponsedelay_ok commandresponsedelay_err \
	    $delay 0 $final $remainder $f_ix
}

thread_chainproc ic commandresponsedelay_err \
	{delay nopartresp final response f_ix emsg} {
    ic_command_err $emsg
}

thread_chainproc ic commandresponsedelay_ok \
	{delay nopartresp final response f_ix} {
    ic_commandresponsedelay $delay $nopartresp $final $response $f_ix
}

thread_chainproc ic commandresponsedelay_after {final response} {
    unset state(ptoid)
    ic_commandresponsedelay 0 1 $final $response 0
}

thread_chainproc ic tellquit_done {args} {
    thread_finish ic $id
}

thread_subproc ic commandfinalresponse {evtype message} {
    ic_kill_subthreads
    if {[info exists state(mtachan)]} {
	threadio_commandresponse ic $id $state(mtachan) quit \
		{} mtaquit_done mtaquit_done $evtype $message
    } else {
	ic_commandresponse_maybefinal $evtype 1 $message
    }
}

thread_chainproc ic mtaquit_done {evtype message args} {
    catch_close_cleardesc state(mtachan)
    ic_commandresponse_maybefinal $evtype 1 $message
}

thread_chainproc ic timedout {} {
    global canonical_hostname

    fileevent $state(chan) readable {}
    ic_commandfinalresponse minor \
	    "421 $canonical_hostname Timed out waiting for command"
}

thread_chainproc ic command_err {emsg} {
    ic_command_err $emsg
}

thread_subproc ic command_err {emsg} {
    global annoyance_minor
    
    manyset [ic_getsiteannoy $annoyance_minor] cv irritamt
    set state(smtpreaderr) $emsg
    logreject notice state dropped {} [string tolower $irritamt] ${cv}ms
    thread_finish ic $id
}

thread_subproc ic commandnorhs {rhs} {
    if {[string length $rhs]} {
	ic_commandresponse major "501 No parameters allowed"
	return -code return
    }
}

thread_subproc ic check_quitting {} {
    global canonical_hostname
    if {![info exists state(quitting)]} { return 0 }
    ic_commandfinalresponse immed "421 $canonical_hostname Shutting down"
    return 1
}

thread_chainproc ic command_ok {cmd} {
    global canonical_hostname blacklist_message bland_message
    global admin_chal_timeout always_blacklist_site
    global adminsecret blacksite_message allow_saucestate mixedkinds_message
    after cancel $state(itoid)
    unset state(itoid)
    regexp {^.*} $cmd cmd
    set state(smtpcmd) $cmd
    set state(smtpreaderr) {}
    if {![string length $cmd]} { set state(smtpreaderr) {Empty command} }
    set state(whyreject) {}
    if {[ic_check_quitting]} {
	return
    } elseif {[eof $state(chan)]} {
	set state(smtpreaderr) EOF
	ic_commandfinalresponse major ""
	return
    } elseif {![regexp -nocase -- {^([a-z0-9]+)[ \t]*(.*)$} $cmd all verb rhs]} {
	ic_commandresponse major "500 Syntax error"
	return
    } else {
	set verb [string tolower $verb]
	switch -exact -- $verb {
	    quit {
		ic_commandnorhs $rhs
		ic_commandfinalresponse command \
			"221 $canonical_hostname goodbye"
	    }
	    helo {
		ic_helo helo smtp $rhs
	    }
	    ehlo {
		ic_helo ehlo esmtp $rhs
	    }
	    mail {
		if {![info exists state(helostring)]} {
		    ic_commandresponse major "503 need HELO or EHLO before MAIL"
		} elseif {[info exists state(mf_lp)]} {
		    ic_commandresponse major "503 MAIL already issued"
		} elseif {[regexp -nocase \
			{^from:[ \t]*<(.+)@([^@]+)>[ \t]*(.*)$} \
			$rhs all lp dm parms]} {
		    ic_msg_resetvars
		    set state(mf_lp) $lp
		    set state(mf_dm) $dm
		    set state(mf_parms) $parms
		    if {[regexp {^\[.*\]$} $state(mf_dm)]} {
			ic_mailfrom_fail "550 Domain-literal senders not allowed"
		    } elseif {[catch { address_dequote state(mf_lp) state(mf_dm) } \
			    emsg]} {
			ic_mailfrom_fail "501 Syntax error in sender ($emsg)"
		    } else {
			set str "$state(mf_lp)@$state(mf_dm)"
			set as [ds_get addr-list $str]
			set ss [ds_get site-list $state(ra)]

			if {"$as" == "white" && "$ss" == "white"} {
			    set state(mf_message) "You are on the whitelist"
			    ic_mailfrom_ok
			} elseif {"$as" == "black"} {
			    set state(mf_message) "You are on the blacklist"
			    ic_mailfrom_ok
			} elseif {"$as" == "unknown"} {
			    set state(avfid) [thread_start avf \
				    "$state(desc) / verify $str" \
				    $state(mf_lp) $state(mf_dm)]
			    thread_join ic $id avf $state(avfid) \
				    mailfrom_avf_ok mailfrom_avf_err
			} elseif {"$as" == "verified"} {
			    set state(mf_message) "You were verified previously"
			    ic_rbl
			} else {
			    set state(mf_message) "You are on the greylist"
			    ic_rbl
			}
		    }
		} elseif {[regexp -nocase \
			{^from:[ \t]*<>[ \t]*(.*)$} \
			$rhs all parms]} {
		    ic_msg_resetvars
		    set state(mf_lp) {}
		    set state(mf_dm) {}
		    set state(mf_parms) $parms
		    set ss [ds_get site-list $state(ra)]
		    if {"$ss" == "white"} {
			set state(mf_message) "Bounce is from whitelisted site"
			ic_mailfrom_ok
		    } else {
			set state(mf_message) "Ready to receive a bounce"
			ic_rbl
		    }
		} else {
		    ic_commandresponse major "501 Syntax error in parameter to MAIL"
		}
	    }
	    vrfy {
		ic_commandresponse command "252 VRFY not supported by SAUCE."
	    }
	    rcpt {
		incr state(rcptcounter)
		if {![info exists state(mf_lp)]} {
		    ic_commandresponse minor "503 need MAIL before RCPT"
		} elseif {[regexp -nocase -- \
			{^to:[ \t]*<(.+)@([^@]+)>[ \t]*$} \
			$rhs all lp dm]} {
		    set str "$lp@$dm"
		    if {[catch { address_dequote lp dm } emsg]} {
			ic_rcptresponse major badsyntax $str \
				"501 Syntax error in recipient ($emsg)"
		    } else {
			set rtcmd "rcpt to:<[lp_quote $lp]@$dm>"
			set atype [addr_classify $lp $dm state]
			regexp {^[0-9a-z]+} $atype atype_summ
			set kind $atype
			switch -glob $atype_summ {
			    unchecked { set notafter normal }
			    lax { set notafter {} }
			    nodelay - normal {
				set kind normal; set notafter {bait unchecked}
			    }
			    bait { set notafter {} }
			    [45]* {
				ic_rcptresponse major $atype_summ $str $atype
				return
			    }
			    default { error "internal error - atype $atype" }
			}
			set delay_this {}
			if {"$atype" == "normal" && \
				[info exists state(delay_msg)]} {
			    lappend delay_this $state(delay_msg)
			}
			if {"[ds_get site-list $state(ra)]" == "black"} {
			    ic_rej_bl "\[$state(ra)\]" site
			}
			set mf $state(mf_lp)@$state(mf_dm)
			if {"[ds_get addr-list $mf]" == "black"} {
			    ic_rej_bl $mf "return path"
			}
			set rej_this [concat $state(rej_conn) $state(rej_msg)]
			set defer_this \
				[concat $state(defer_conn) $state(defer_msg)]
			switch -exact $atype {
			    bait {
				lappend state(add_bl) \
					"Sent mail to bait address $lp@$dm"
				if {$always_blacklist_site} ic_blacklist_site
				ic_rcptresponse command $atype_summ $str \
					"250 $bland_message"
				lappend state(a_kinds) $kind
				return
			    }
			    normal - nodelay {
				foreach {varname code badness} {
				    rej_this   550 major
				    defer_this 451 major
				    delay_this 450 rcpt-defer
				} {
				    set resp [join [set $varname] "\n"]
				    if {[string length $resp]} {
					smtp_prefix_response $resp $code resp
					ic_rcptresponse $badness $atype_summ \
						$str $resp
					return
				    }
				}
			    }
			    unchecked - lax {
			    }
			    default {
				error "atype ? $atype"
			    }
			}
			foreach k $notafter {
			    if {[ic_a_kind $k]} {
				ic_rcptresponse command $atype_summ $str \
					"450 $mixedkinds_message"
				return
			    }
			}
			threadio_commandresponse ic $id $state(mtachan) \
				$rtcmd {} mta_rcpt_ok {} $kind $atype_summ $str
		    }
		} else {
		    ic_commandresponse major \
			    "501 Syntax error in parameter to RCPT"
		}
	    }
	    data {
		ic_commandnorhs $rhs
		if {![llength $state(a_kinds)]} {
		    ic_commandresponse minor "503 No recipients specified"
		} else {
		    threadio_puts ic $id $state(chan) \
			    "354 Send text\n" askfordata_done command_err
		}
	    }
	    sauceadmin {
		if {![string length $rhs]} {
		    set chal [exec -keepnewline \
			    dd if=/dev/urandom bs=1 count=8 2>/dev/null]
		    binary scan $chal H* chal
		    if {[string length $chal] != 16} {
			error "urandom failed `$chal'"
		    }
		    append chal [format %08lx [clock seconds]]
		    set state(lastchal) $chal
		    ic_commandresponse immed "393 $chal"
		} elseif {![info exists state(lastchal)]} {
		    ic_commandresponse major \
			    "503 Need SAUCEADMIN on its own first"
		} else {
		    set waschal $state(lastchal)
		    log notice "$state(desc): ATTEMPTING SWITCH TO ADMIN MODE"
		    if {![regexp \
           {^([0-9a-f]{16})([0-9a-f]{8})[ \t]+([0-9a-f]{32})$} \
			    $rhs all chal wasdate resp]} {
			ic_commandresponse major "501 \\x{24} \\x{32} please"
		    } elseif {"$chal$wasdate" != "$waschal"} {
			ic_commandresponse immed "490 challenge overwritten"
		    } elseif "[clock seconds] - 0x$wasdate \
			    > $admin_chal_timeout" {
			ic_commandresponse immed "491 challenge timed out"
		    } elseif {![string length $adminsecret]} {
			ic_commandresponse immed "495 admin secret missing"
		    } elseif {"$resp" != \
 "[exec <<"[binary format H* $waschal]$adminsecret" md5sum]"} {
                        ic_commandresponse immed "492 incorrect response"
                        unset state(lastchal)
                    } else {
			log notice "$state(desc): switch to admin mode ok"
			threadio_puts ic $id $state(chan) "294 yes master\n" \
				yesmaster_outdone command_err
		    }
		}
	    }
	    saucestate {
		if {$allow_saucestate} {
		    set op "100-\n"
		    foreach x [lsort [array names state]] {
			append op "100-[list $x $state($x)]\n"
		    }
		    append op "100"
		    ic_commandresponse immed $op
		} else {
		    ic_commandresponse immed "504 SAUCESTATE not available."
		}
	    }
	    help {
		ic_commandnorhs $rhs
		ic_commandresponse command \
{214-
214 QUIT HELP NOOP HELO EHLO MAIL RCPT DATA QUIT RSET VRFY}
	    }
	    noop {
		ic_commandnorhs $rhs
		ic_commandresponse command "250 NOOP OK"
	    }
	    rset {
		ic_commandnorhs $rhs
		if {[info exists state(mtachan)]} {
		    threadio_commandresponse ic $id $state(mtachan) rset \
			    {^2[0-9][0-9]} mta_rset_ok {}
		} else {
		    ic_msg_resetvars
		    ic_commandresponse command "250 OK"
		}
	    }
	    default {
		ic_commandresponse major "502 Command unrecognised"
	    }
	}
    }
}

thread_subproc ic helo {helocmd smtpstyle rhs} {
    global forbid_helo_ipliteral require_reverse_dns canonical_hostname
    set state(helocmd) $helocmd
    set state(smtpstyle) $smtpstyle
    if {[info exists state(helostring)]} {
	ic_commandresponse major "503 HELO or EHLO already specified"
    } elseif {[regexp {^\[(\d+\.\d+\.\d+\.\d+)\]$} $rhs all ipliteral]} {
	if {$forbid_helo_ipliteral} {
	    lappend state(rej_conn) \
		    "IP literal ($rhs) in HELO forbidden by adminstrator"
	}
	ic_find_maildomain $state(rh) $rhs
    } elseif {![domain_ok $rhs]} {
	if {[regexp -nocase {[^-_.+@/<>0-9a-z]} $rhs]} {
	    ic_commandresponse major "501 Invalid characters in HELO domain"
	} else {
	    lappend state(rej_conn) "Syntax error in HELO domain `$rhs'"
	    ic_find_maildomain $state(rh) $rhs
	}
    } else {
	if {"[string tolower $rhs]" == "[string tolower $state(rh)]"} {
	    ic_find_maildomain $rhs $rhs
	} elseif {"$state(ra)" == "127.0.0.1"} {
	    ic_set_maildomain $canonical_hostname $rhs
	} elseif {"[ds_get site-list $state(ra)]" == "white"} {
	    ic_set_maildomain "\[$state(ra)\]" $rhs
	} else {
	    set state(dnsid) [thread_start dns "$state(desc) / HELO lookup" $rhs A 1]
	    thread_join ic $id dns $state(dnsid) helodns_ok helodns_err $rhs
	}
    }
}

thread_chainproc ic helodns_ok {hs answers emsgstr how} {
    global check_helo_name require_callingmaildomain_dnsok
    unset state(dnsid)
    if {[llength $answers]} {
	if {[lsearch -exact $answers $state(ra)] != -1} {
	    ic_find_maildomain $hs $hs
	} else {
	    if {$check_helo_name} {
		lappend state(rej_conn) \
			"HELO name $hs has no address matching $state(ra)"
	    }
	    ic_find_maildomain $state(rh) $hs
	}
    } else {
	if {"$how" == "MISCONFIG" && $require_callingmaildomain_dnsok} {
	    lappend state(rej_conn) \
		    "HELO name lookup revealed misconfiguration: $emsgstr"
	} elseif {$check_helo_name} {
	    lappend state(rej_conn) "HELO name incorrect: $emsgstr"
	}
	ic_find_maildomain $state(rh) $hs
    }
}

thread_chainproc ic helodns_err {hs emsg} {
    unset state(dnsid)
    lappend state(defer_conn) "HELO name lookup failed: [singleline $emsg]"
    ic_find_maildomain $state(rh) $hs
}

thread_subproc ic find_maildomain {chstart hs} {
    global require_callingmaildomain_name require_reverse_dns
    if {![string match {\[*\]} $chstart]} {
	ic_findmore_maildomain $chstart $chstart $hs
    } else {
	if {$require_callingmaildomain_name && !$require_reverse_dns} {
	    lappend state(rej_conn) \
		    "Cannot find $state(ra) host name via reverse DNS or HELO"
	}
	ic_set_maildomain $chstart $hs
    }
}

thread_subproc ic findmore_maildomain {chstart chnow hs} {
    if {[llength [split $chnow .]] == 1} {
	ic_set_maildomain $chstart $hs
    } else {
	set state(dnsid) [thread_start dns "$state(desc) / maildomain lookup" \
	                  $chnow MX 0]
	thread_join ic $id dns $state(dnsid) fch_ok fch_err $chstart $chnow $hs
    }
}

thread_chainproc ic fch_ok {chstart chnow hs answers emsgstr how} {
    global require_callingmaildomain_dnsok
    unset state(dnsid)
    if {[llength $answers]} {
	ic_set_maildomain $chnow $hs
    } else {
	if {"$how" == "MISCONFIG" && $require_callingmaildomain_dnsok} {
	    lappend state(rej_conn) "While finding mail domain: $emsgstr"
	}
	regsub {^[^.]+\.} $chnow {} chnow
	ic_findmore_maildomain $chstart $chnow $hs
    }
}

thread_chainproc ic fch_err {chstart chnow hs emsg} {
    global require_callingmaildomain_dnsok
    unset state(dnsid)
    if {$require_callingmaildomain_dnsok} {
	lappend state(defer_conn) \
		"Problem finding mail domain: [singleline $emsg]"
    }
    ic_set_maildomain $chstart $hs
}

thread_subproc ic set_maildomain {ch hs} {
    set state(cmdomain) $ch
    set state(helostring) $hs
    ic_mtachan_open
    threadio_commandresponse ic $id $state(mtachan) {} {} mta_greeting_ok {}
}

thread_subproc ic mtachan_open {} {
    set lcmd [list open |[list sendmail -bs -oem \
	    -oMa $state(ra) -oMr $state(smtpstyle)-sauce \
	    -oMs $state(rh) -oMt $state(ident)] r+]
    debug 2 "running sendmail: $lcmd"
    set state(mtachan) [eval $lcmd]
    fconfigure $state(mtachan) -blocking false -translation {binary crlf}
    chanset_desc $state(mtachan) "$state(desc) / MTA"
}

thread_chainproc ic mta_greeting_ok {data} {
    if {![regexp {^220} $data]} {
	ic_mta_greethelo_err $data
	return
    }	
    threadio_commandresponse ic $id $state(mtachan) \
	    "$state(helocmd) $state(helostring)" {} mta_helo_ok {}
}

thread_subproc ic mta_greethelo_err {emsg} {
    global canonical_hostname
    
    regsub -nocase {^[0-9]* ?[-+.:0-9a-z]* *} $emsg {} emsg
    ic_commandfinalresponse major "421 $canonical_hostname $emsg"
}

thread_chainproc ic mta_helo_ok {data} {
    global canonical_hostname
    if {![regexp {^2[0-9][0-9]} $data]} {
	ic_mta_greethelo_err $data
	return
    }
    set str "$canonical_hostname hello $state(ident)@$state(rh)"
    append str " (postmaster@$state(cmdomain)?)"
    if {"$state(helocmd)" == "helo"} {
	set op "250 $str"
    } elseif {"$state(helocmd)" == "ehlo"} {
	set op "250-$str\n"
	foreach l [lrange [split $data "\n"] 1 end] {
	    if {[regexp -nocase {^250[- ]([-a-z0-9]+)(.*)$} $l all keyword params]} {
		set params [string trim $params]
		switch -exact -- [string tolower $keyword] {
		    8bitmime - size {
			append op "250-[string toupper $keyword] $params\n"
		    }
		}
	    }
	}
	append op "250 PIPELINING"
    } else {
	error "internal error - ugh? $helocmd"
    }
    ic_commandresponse nopartresp $op
}

thread_subproc ic mailfrom_fail {message} {
    unset state(mf_lp)
    unset state(mf_dm)
    unset state(mf_parms)
    catch {
	unset state(mf_message)
    }
    catch {
	unset state(minsiteage)
	unset state(minaddrage)
    }
    ic_commandresponse major $message
}

thread_chainproc ic mailfrom_avf_ok {ok message} {
    unset state(avfid)
    if {!$ok} {
	set sl [singleline "invalid MAIL-FROM: $message"]
	ic_rej $sl
	set message "Warning! Rejection likely: $sl"
    }
    set state(mf_message) $message
    ic_rbl
}

thread_chainproc ic mailfrom_avf_err {message} {
    unset state(avfid)
    set sl [singleline "problematic MAIL-FROM: $message"]
    lappend state(defer_msg) $sl
    set state(mf_message) "Warning! Deferral likely: $sl"
    ic_rbl
}
	
thread_subproc ic rbl {} {
    global rbls new_addr_defer new_site_defer new_addr_message new_site_message

    set state(minaddrage) 0
    set state(minsiteage) 0
    ic_rbl_minage addr $state(mf_lp)@$state(mf_dm) $new_addr_defer $new_addr_message
    ic_rbl_minage site $state(ra) $new_site_defer $new_site_message
	
    set tolookup {}
    foreach dq [split $state(ra) .] { set tolookup $dq.$tolookup }

    foreach rbl $rbls {
	manyset $rbl dm maa msa rblmsg
	regsub -all {%d} $rblmsg $dm rblmsg
	set thread [thread_start dns "$state(desc) / rbl $dm" $tolookup$dm TXT 1]
	lappend state(rblids) $thread
	thread_join ic $id dns $thread rbl_done rbl_err \
		$thread $dm $maa $msa $rblmsg
    }
    ic_rbl_checkdone
}

thread_chainproc ic rbl_done {thread dm maa msa rblmsg answers emsgstr etype} {
    ic_rbl_rmthread $thread
    switch -exact -- $etype {
	NXDOMAIN - NOTYPE {
	}
	MISCONFIG {
	    log notice "RBL misconfigured! $emsgstr"
	}
	OK {
	    if {![llength $answers]} { error "no answers, no error" }
	    lappend state(rbl_hits) $dm
	    set l {}
	    foreach a $answers {
		regsub {^\"} $a {} a
		regsub {\"$} $a {} a
		set a [proto_quote $a]
		lappend l $a
	    }
	    regsub -all {%m} $rblmsg [join $l ", "] rblmsg
	    regsub -all {%p} $rblmsg {%} rblmsg
	    if {![string length $maa]} {
		lappend state(rej_conn) [singleline $rblmsg]
	    } else {
		ic_rbl_minage addr $state(mf_lp)@$state(mf_dm) $maa $rblmsg
		ic_rbl_minage site $state(ra) $msa $rblmsg
	    }
	}
	default {
	    error "[list $etype $answers $emsgstr] ?"
	}
    }
    ic_rbl_checkdone
}

thread_chainproc ic rbl_err {thread dm maa msa rblmsg emsg} {
    ic_rbl_rmthread $thread
    ic_rbl_checkdone
}

thread_subproc ic rbl_rmthread {thread} {
    set ntl {}
    foreach t $state(rblids) {
	if {"$t" != "$thread"} { lappend ntl $t }
    }
    set state(rblids) $ntl
}

thread_subproc ic rbl_minage {what key newminage msg} {
    upvar #0 remember_${what}_defer remember_defer

    debug 2 rbl_minage $what $key $newminage $msg
    if {!$newminage} return
    if {"$key" == "@"} return
    set whatstate [ds_get $what-list $key]
    if {"$whatstate" == "white" || "$whatstate" == "whitesoon"} return
    if {$state(min${what}age) >= $newminage} return
    set state(min${what}age $newminage

    set now [clock seconds]
    set firstcontact [ds_get $what-seen $key]
    if {"$firstcontact" == "unknown"} {
	set firstcontact $now
	ds_set $what-seen $key $now $remember_defer
	debug 2 rbl_minage ... firstcontact $now
    }
    if {$now < $firstcontact+$newminage} {
	set state(delay_msg) $msg
	debug 2 rbl_minage ... defer $now $firstcontact+$newminage
    }
}

thread_subproc ic rbl_checkdone {} {
    if {[llength $state(rblids)]} return
    ic_mailfrom_ok
}

thread_subproc ic mailfrom_ok {} {
    global max_smtpparms_size
    if {[string length $state(mf_parms)] > $max_smtpparms_size} {
	ic_mailfrom_fail "503 MAIL FROM parameter string too long"
    } else {
	set addr "[lp_quote $state(mf_lp)]@$state(mf_dm)"
	if {"$addr" == "@"} { set addr {} }
	threadio_commandresponse ic $id $state(mtachan) \
		"mail from:<$addr> $state(mf_parms)" {} mta_mailfrom_ok {}
    }
}

thread_chainproc ic mta_mailfrom_ok {data} {
    if {[regexp {^2[0-9][0-9]} $data]} {
	smtp_prefix_response $state(mf_message) 250 message
	ic_commandresponse command $message
	unset state(mf_message)
	unset state(mf_parms)
	catch {
	    unset state(minsiteage)
	    unset state(minaddrage)
	}
    } else {
	ic_mailfrom_fail $data
    }
}

thread_subproc ic rcptresponse {evtype atype_summ rcpt response} {
    regexp {^[0-9][0-9][0-9]} $response code
    lappend state(att_rcpts) [list $atype_summ $rcpt $code]
    ic_commandresponse $evtype $response
}

thread_chainproc ic mta_rcpt_ok {kind atype_summ rcpt data} {
    if {[regexp {^2[0-9][0-9]} $data]} {
	lappend state(a_kinds) $kind
	set evtype command
    } else {
	set evtype major
    }
    ic_rcptresponse $evtype $atype_summ $rcpt $data
}

thread_chainproc ic mta_rset_ok {data} {
    catch { unset state(mf_lp) }
    ic_commandresponse command "250 OK"
}
