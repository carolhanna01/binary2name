#!/usr/bin/tclsh8.2
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
# $Id: rcpt-policy.tcl,v 1.3 2001/03/21 23:53:14 ian Exp $

# userv service.  Input is new user policy file, lines like:
#  <sending-site-pat>  <sending-addr-pat>  <receiving-addr-pat>  <result>
# (and one line `.')  Blank lines are permitted.  Lines starting with #
# are comments.
#
# <sending-site-pat> is one of
#   [<ip-address>]
#   [<ip-address-mask>/<prefix-len>]
#   <domain-name-glob>
#
# <foo-addr-pat> is one of
#   <local-part-glob>@
#   <address-glob>  (not ending in @)
# (To match `<>' when specified as envelope sender, match `@' instead.)
#
# globs may contain no whitespace.  They support [...], ? and *
# and \-escapes.  There is no way to specify patterns including
# whitespace.
#
# <result> is one of
#   450|451|452|550|552|553 <message-string>
#   normal|lax|nodelay|bait|unchecked

proc log {lev msg} {
    puts stderr $msg
}

set @@@readlibs@@@ readlibs.tcl
set sauce_libraries {
    readconf
    library
    sconfig
}
source ${@@@readlibs@@@}

proc readconfig_posthook {args} {}

readconfig

if {[string length $current_bigerr]} {
    log fatal "configuration errors, stopping"
    exit 3
}

cd $var_dir/policies

proc fail {emsg} {
    puts stderr $emsg
    exit 1
}

switch -exact [llength $argv] {
    0 {
	set policy $env(USERV_USER)
	if {[info exists env(USERV_U_SUBPOLICY)]} {
	    set subpolicy $env(USERV_U_SUBPOLICY)
	    if {![string length $subpolicy]} {
	    } elseif {[regexp -nocase {^\:[-+_.%$0-9a-z?*]{0,188}$} \
		    $subpolicy subpolicy]} {
		append policy $subpolicy
	    } else {
		fail \
 "subpolicy must be : followed by 0-127 alphanumerics or - + _ . % $ ? *"
	    }
	}
    }
    1 {
	set policy [lindex $argv 0]
	if {[regexp {/} $policy]} { fail "policy name may not contain /" }
	if {[regexp {^\.} $policy]} { fail "policy name may not start with ." }
    }
    default { fail "specify only one policy to set" }
}

set lno 0

set outtxt {}
proc out {s} { global outtxt; append outtxt $s "\n" }

out "proc acuser_proc/$policy {} {"

proc syxerr {emsg} {
    global lno errorInfo
    fail "rcpt-policy: policy line $lno: error: $emsg"
}

proc out_once {text} {
    upvar #0 outonce_done($text) d
    if {[info exists d]} return
    out $text
    set d 1
}

set encc 0

proc encvarn {thing} {
    upvar #0 enc_scope($thing) enc
    if {![info exists enc]} {
	global encc
	regsub -all {[^a-z]+} $thing _ p
	regsub -all {_+$} $p {} p
	regsub {^_} $p {U} p
	set enc "${p}_X[incr encc]"
    }
    return $enc
}

proc scope {thing} {
    set enc [encvarn $thing]
    out_once "    upvar 1 $thing $enc"
    return "\$$enc"
}

proc condkind {kind argl rbody} {
    set body    "    upvar 1 \${condname}pat pat\n"
    append body "    if {\"\$pat\" == \"*\"} return\n"
    append body $rbody
    proc cond_add_$kind [concat condname $argl] $body
}

condkind site {} {
    set len 32
    if {[regexp {\[([0-9][0-9.]+)\]} $pat dummy mask] || \
	    [regexp {\[([0-9][0-9.]*)/([0-9]+)\]} $pat dummy mask len]} {
	out_once "    set ra_v \[ia2value [scope state(ra)] 32\]"
	if {[catch { set re_v [ia2value $mask $len] } emsg]} {
	    syxerr "invalid address: $emsg"
	}
	if {$len > 32} { syxerr "prefix length >32" }
	set ma_v [expr {$len == 0 ? 0 : (0xffffffff<<(32-$len))}]
	if {$re_v & ~$ma_v} { syxerr "mask is non-zero beyond prefix" }
	cond_add "(\$ra_v & [format 0x%08x $ma_v]) == [format 0x%08x $re_v]"
    } elseif {[string match "\[*" $pat]} {
	syxerr "invalid address mask"
    } else {
	cond_add_glob $pat [scope state(rh)]
    }
}

proc cond_add_glob {pat valstring} {
    regexp {(.*)} $pat npat
    if {[catch { string match $npat foobar } emsg]} {
	syxerr "invalid glob pattern: $emsg"
    }
    cond_add "\[string match [list $npat] $valstring\]"
}

proc cond_add {cond} {
    global conds
    lappend conds $cond
}

condkind addr {lpv dmv} {
    if {[regexp {^(.*)\@$} $pat dummy lpat]} {
	cond_add_glob $lpat [scope $lpv]
    } else {
	set enc [encvarn $lpv@$dmv]
	out_once "    set $enc [scope $lpv]@[scope $dmv]"
	cond_add_glob $pat \$$enc
    }
}

set condjoin " &&\n        "
set any 0

while 1 {
    if {[gets stdin line] < 0} { syxerr "missing final line `.'" }
    incr lno
    set line [string trim $line]
    if {![string length $line]} continue
    if {"$line" == "."} break
    if {[string match #* $line]} continue
    if {![regexp {^(\S+)\s+(\S+)\s+(\S+)\s+(\S.*\S)$} $line dummy \
	    sspat sapat rapat result]} { syxerr "syntax error" }
    set conds {}
    cond_add_site ss
    cond_add_addr sa state(mf_lp) state(mf_dm)
    cond_add_addr ra lp dm
    if {[llength $conds]} {
	out "    if {[join $conds $condjoin]} \{"
    }
    if {![regexp \
	    {^(45[012]|55[023]) \S.*$|^(unchecked|lax|nodelay|normal|bait)$} \
	    $result]} {
	syxerr "invalid result"
    }
    if {[regexp -nocase {[^ -~]} $result]} { syxerr "invalid char in result" }
    out "        [list return $result]"
    set any 1
    if {[llength $conds]} {
	out "    \}"
    }
}
out "    return {}\n}"

if {$any} {
    set out [open n$policy w]
    puts -nonewline $out $outtxt
    close $out
    file rename -force n$policy p$policy
    puts "ok - new SAUCE policy $policy installed"
} else {
    file delete p$policy
    puts "ok - any SAUCE policy $policy removed"
}
