########### library.tcl
# Utility routines, eg, for munging addresses (RFC821) etc.
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
# $Id: library.tcl,v 1.22 2001/03/21 23:53:14 ian Exp $


set lg_outs                {fatal error reject dbreasons notice debug}
			    
set lg_sev_outs(debug)     {                                    debug}
set lg_sev_outs(notice)    {                             notice debug}
set lg_sev_outs(dbreasons) {                   dbreasons notice debug}
set lg_sev_outs(reject)    {            reject           notice debug}
set lg_sev_outs(error)     {      error reject           notice debug}
set lg_sev_outs(fatal)     {fatal error reject           notice debug}

foreach ll $lg_outs { set lg_chan($ll) stderr }

proc reopenlogs {} {
    global lg_outs log_dir log_mode log_mode_debug lg_chan log_stderr
    if {$log_stderr} { return }
    set ofiles {}
    set mode $log_mode_debug
    foreach ll $lg_outs {
	catch { unset of }
	if {[info exists lg_chan($ll)]} { set of $lg_chan($ll) }
	set f [open $log_dir/$ll.log {APPEND WRONLY CREAT} $mode]
	if {[catch { fconfigure $f -buffering line } emsg]} {
	    catch { close $f }
	    error $emsg
	}
	catch {
	    if {"$of" == "stderr"} {
		flush $of
	    } else {
		close $of
	    }
	}
	set lg_chan($ll) $f
	set mode $log_mode
    }
    log notice "logfiles (re)opened"
}

proc debugn {rep lev argl} {
    global debug_level
    if {$debug_level >= $lev} {
	log debug "[string repeat {  } $rep][eval concat $argl]"
    }
}

proc debug {lev args} { debugn [expr {[info level]-1}] $lev $args }
proc debug0 {lev args} { debugn 0 $lev $args }

proc log {severity msg} {
    global lg_sev_outs current_bigerr lg_chan log_stderr
    if {[catch {
	set datestr [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S %Z}]
    } emsg]} {
	set datestr "($emsg)"
    }
    set llhs {}
    regsub -all -nocase {\\} $msg {\\\\} msg
    regsub -all -nocase {\n} $msg "\\n" msg
    while {[regexp -nocase {^([ -~\t]*)([^ -~\t])(.*)$} $msg all lhs here msg]} {
	binary scan $here H* here
	append llhs $lhs "\\x$here"
    }
    append llhs $msg
    set lfile {}
    foreach ll $lg_sev_outs($severity) {
	if {[info exists lg_chan($ll)] && "$lfile" == "$lg_chan($ll)"} { continue }
	if {[catch {
	    puts $lg_chan($ll) "$datestr: $severity: $llhs"
	    set lfile $lg_chan($ll)
	} emsg]} {
	    set current_bigerr "$ll log error: $emsg"
	    catch {
		puts $lg_chan(fatal) "$datestr: fatal: error logging for $ll: $emsg"
	    }
	}
    }
}

proc logreject_val {varname key val} {
    upvar $varname var
    if {![string length $val]} return
    append var " $key="
    if {[regexp -nocase {[^-+.0-9a-z@%_$*:<>]} $val]} {
	regsub -all {"} $val {&&} val
	append var "\"$val\""
    } else {
	append var $val
    }
}

proc logreject {severity statevar when saidwhat args} {
    global annoy_grumpy annoy_actout_max
    upvar $statevar state
    
    set m $when
    if {[info exists state(rh)]} {
	logreject_val m host $state(rh)
    } else {
	logreject_val m addr $state(ra)
    }
    if {[info exists state(mf_lp)]} {
	set sender $state(mf_lp)@$state(mf_dm)
	if {"$sender" == "@"} { set sender {<>} }
	logreject_val m from $sender
    }
    logreject_val m cmd $state(smtpcmd)
    logreject_val m readerr $state(smtpreaderr)
    logreject_val m resp $saidwhat
    foreach cm {conn msg} {
	foreach rd {rej defer} {
	    foreach reas $state(${rd}_${cm}) {
		logreject_val m ${rd}-${cm} $reas
	    }
	}
    }
    if {[llength $state(resentmids)]} {
	foreach i $state(resentmids) { logreject_val m resent-id $i }
    } elseif {[info exists state(mid)]} {
	logreject_val m id $state(mid)
    }
    foreach x $state(att_rcpts) {
	manyset $x t r c
	if {"$t" == "$c"} { set tc $t } else { set tc $t-$c }
	logreject_val m rcpt-$tc $r
    }
    foreach x $state(rbl_hits) {
	logreject_val m rbl $x
    }
    foreach {key val} $args {
	logreject_val m $key $val
    }
    log $severity $m
}

proc manyset {list args} {
    foreach val $list var $args {
	upvar 1 $var my
	set my $val
    }
}

########## general utility functions

proc smtp_prefix_response {message_in code message_out_var} {
    upvar 1 $message_out_var message_out
    regsub -all {(?m)^} $message_in "$code-" message
    regsub "$code-(\[^\\n\]*)\$" $message "$code \\1" message_out
}

proc domain_ok {domain} {
    return [expr {
	![regexp -nocase {[^-0-9a-z.]} $domain] &&
	[regexp -nocase {^[-0-9a-z]+(\.[-0-9a-z]+)*$} $domain] &&
        [string length $domain] <= 255
    }]
}

proc lp_quote {lp} {
    regsub -all {([^-.!#$%&'*+/0-9=?A-Z^_`a-z{|}~])} $lp {\\\1} lp
    return $lp
}

proc proto_quote {msg} {
    regsub -all {\n} $msg " // " msg
    regsub -all {[^\t\040-\176]} $msg ? msg
    return $msg
}

proc date_822 {} {
    clock format [clock seconds] -gmt true -format {%d %b %Y %T +0000 (GMT)}
}

proc singleline {manylines_onestring} {
    return [join [split $manylines_onestring "\n"] "; "]
}

proc ia2value {dottedquad minbitlen} {
    set l [split $dottedquad .]
    if {[llength $l] > 4} { error "too many bytes in IP address/mask" }
    if {![llength $l]} { error "empty IP address/mask" }
    if {[llength $l]*8 < $minbitlen} {
	error "IP address/mask too short (need $minbitlen bits)"
    }
    set ac 0
    set sh 24
    foreach v $l {
	if {$v > 255} { error "IP address byte out of range" }
	incr ac [expr {$v << $sh}]
	incr sh -8
    }
    return $ac
}

proc address_dequote {lpvar domvar} {
    upvar 1 $lpvar lp
    upvar 1 $domvar dm
    if {![domain_ok $dm]} { error "invalid domain" }
    while {[regexp {^@([^:,]+)[:,](.+)$} $lp all adm rhs]} {
	if {![domain_ok $adm]} { error "invalid source route syntax" }
	set lp $rhs
    }
    if {![regexp {^\"((?:[^\"\\]|\\.)+)\"$} $lp all lp] &&
        ![regexp {^([-!#$%&'*+/0-9=?A-Z^_`a-z{|}~]|\\.)+(\.([-!#$%&'*+/0-9=?A-Z^_`a-z{|}~]|\\.)+)*$} $lp]} {
	error "invalid local-part syntax"
    }
    regsub -all {\\(.)} $lp {\1} lp
}
