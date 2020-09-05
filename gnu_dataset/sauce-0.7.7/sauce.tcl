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
# $Id: sauce.tcl,v 1.103 2001/03/21 23:53:14 ian Exp $

# wishlist: blacklist even if spam-message aborted
# wishlist: use adns client instead of host
# wishlist: limit number of simultaneous connections from one host or /24

# set tcl_traceExec 1

set @@@readlibs@@@ readlibs.tcl
set sauce_libraries {
    readconf
    library
    thread
    dns
    avf
    avfpool
    smtp
    msgdata
    notifybl
    datastate
    yesmaster
    sconfig
}
source ${@@@readlibs@@@}

proc readconfig_posthook {} {
    global fill_msgs canonical_hostname

    if {![info exists fill_msgs]} {
	set fill_msgs [list \
 "Your system has had many errors while trying to send us mail." \
 "Teergrube (spam countermeasure) triggered: stalling SMTP responses." \
 "Please consult your system administrator or postmaster." \
 "Check your retry configuration, and look for spam in your queue." \
 "Queries ?  Contact postmaster@$canonical_hostname." \
 ]
    }
    addr_patterns_compile

    return {}
}

proc addr_patterns_compile {} {
    global addr_patterns

    set donelocal 0
    set el {}
    set preproc    "    upvar 1 \$statevar state\n"
    append preproc "    set dm \[string tolower \$dm\]\n"
    set proc {}
    foreach le $addr_patterns {
	manyset $le at ap
	set cond {}
	if {[regexp {(.*)\@$} $ap dm ap]} {
	    if {!$donelocal} {
		append preproc \
			"    global local_domain\n" \
			"    set islocal \[info exists local_domain(\$dm)\]\n"
		set donelocal 1
	    }
	    append cond {$islocal && }
	    set matchag {$lp}
	} else {
	    set matchag {$lp@$dm}
	}
	append cond "\[[list regexp "^$ap$"]"
	append cond " $matchag dummy d1\]"
	set subpolicy {}
	if {[regexp {^(user|policy)\=(.+)$} $at dummy up subpolicy] || \
		[regexp {^(user|policy)$} $at dummy up]} {
	    set policy {}
	    if {"$up" == "policy"} {
		set policy [list :$subpolicy]
	    } elseif {[string length $subpolicy]} {
		set policy [list $subpolicy]
	    } else {
		set policy {[addr_classify_safed1 $d1]}
	    }
	    if {[regexp {\:$} $subpolicy]} {
		append policy {[addr_classify_safed1 $d1]}
	    }
	    append cond " && \[addr_classify_policy $policy\]"
	    set at \$at
	}
	append proc "    if [list $cond] {\n"
	append proc "        return $at\n"
	append proc "    }\n"
    }
    append proc "    return normal\n"
    proc addr_classify {lp dm statevar} $preproc$proc
}

proc addr_classify_safed1 {d1} {
    regsub -nocase -all {[^-+_.%$0-9a-z]} $d1 ? d1
    regexp -nocase {^([-+_.%$0-9a-z?]{0,126})(.*)$} $d1 dummy d1 rhs
    if {[string length $rhs]} { append d1 * }
    return $d1
}

proc addr_classify_policy {user} {
    foreach v {state lp dm at} { upvar 1 $v $v }
    global policy_file policies_dir errorCode policy_file

    if {[regexp {/} $user] || [regexp {^\.} $user]} {
	error "policy name contains / or starts with .: `$user'"
    }

    set filename $policies_dir/p$user
    upvar #0 acuser_dit($user) dit
    if {[catch {
	file stat $filename statinfo
	set newdit "$statinfo(dev) $statinfo(ino) $statinfo(ctime)"
	if {![info exists dit] || "$dit" != "$newdit"} {
	    source $filename
	    set dit $newdit
	}
    } emsg]} {
	manyset $errorCode posix enoent string
	catch { unset dit }
	if {"$posix" != "POSIX"} {
	    error "$emsg (unexpected error code $errorCode)"
	}
	if {"$enoent" != "ENOENT"} {
	    set at [list 451 "unable to check user $user policy: $string"]
	    return 1
	}
	return 0
    }
    set nat [acuser_proc/$user]
    if {![string length $nat]} { return 0 }
    set at $nat
    return 1
}

set nconns 0

readconfig

if {![file isdirectory $var_dir]} {
    error "database directory $var_dir is not an existing directory"
}

set avfchancounter 0
if {[info exists asynch_appdebug]} {
    set debug_level $asynch_appdebug
}

########## controlling stuff

proc shutdown {} {
    global force_shutdown_delay
    log notice "shutdown request received"
    after $force_shutdown_delay {
	thread_forceshutdown
    }
    thread_sysshutdown {
	log notice "shutting down now"
	exit
    }
}

proc conn_done {chan ra lh args} {
    global nconns
    catch_close_cleardesc chan
    incr nconns -1
}

proc conn_err {chan ra lh printwhat emsg} {
    global canonical_hostname fail_send_timeout
    if {[catch {
	set resp "421 $canonical_hostname $printwhat, try later"
	set m failed
	logreject_val m addr $ra
	logreject_val m resp $resp
	logreject_val m why $emsg
	log reject $m
	set toid [after $fail_send_timeout conn_done $chan $ra $lh]
 	threadio_puts {} {} $chan "$resp\r\n" conn_err_done conn_err_done \
		$chan $ra $lh $toid
    }]} {
	conn_done $chan $ra $lh
    }
}

proc conn_err_done {chan ra lh toid args} {
    after cancel $toid
    conn_done $chan $ra $lh
}

proc new_conn {chan ra rp} {
    global ipaddr_phase_proportion ipaddr_phase_offset local_interface current_bigerr
    global nconns conns_max annoy_love_max annoy_grudge_max annoyance_minor

    if {[catch {
	if {$ipaddr_phase_proportion < 256 && \
	    ![info exists local_interface($ra)]} {
	    set cp $ipaddr_phase_offset
	    foreach tb [split $ra .] fc {3 23 73 131} {
		set cp [expr {($cp+$tb*$fc)%256}]
	    }
	    if {$cp < $ipaddr_phase_proportion} {
		log notice "$ra connected, phase $cp < $ipaddr_phase_proportion"
	    } else {
 log notice "$ra connected, phase $cp >= $ipaddr_phase_proportion, twisting"
		fconfigure $chan -blocking true
		exec <@ $chan >@ $chan sh -c { sendmail -bs <&1 & }
		return
	    }
	}
	set lalhlp [fconfigure $chan -sockname]
	fconfigure $chan -translation {binary crlf} -blocking false
    } emsg]} {
	if {[string length $emsg]} { log error "get local address: $emsg" }
	catch { close $chan }
    } else {
	set lh [lindex $lalhlp 1]
	set desc "$lh-$ra:$rp"
	if {$ipaddr_phase_proportion == 256} { debug0 1 "$desc connected" }
	incr nconns
	chanset_desc $chan $desc
	if {[thread_shuttingdown]} {
	    conn_err $chan $ra $lh "Shutting down" {}
	} elseif {[string length $current_bigerr]} {
	    conn_err $chan $ra $lh $current_bigerr {}
	    set current_bigerr {}
	    readconfig
	    reopenlogs
	} else {
	    manyset [intern_getsiteannoy $ra 0] annoyval annoytype
	    if {$annoyval > 0} {
		set tmax [expr {int(
		    double($conns_max) *
		    pow(0.25, sqrt(double($annoyval) / double($annoy_grudge_max)))
		)}]
		if {$nconns > $tmax} {
		    manyset [intern_getsiteannoy $ra $annoyance_minor] annoyval annoytype
		    if {$annoyval == $annoy_grudge_max} { bff_add $ra }
		    conn_err $chan $ra \
 $lh "Too busy ($nconns/$tmax $annoyval) \[[irrit_present $annoytype]\]" {}
		    return
		}
	    }
	    set thread [thread_start ic $desc $chan $lalhlp $ra $rp]
	    thread_join {} {} ic $thread conn_done conn_err $chan $ra $lh "Internal error"
	}
    }
}

proc bff_log {addr how} {
    log reject "firewall addr=$addr $how"
}

proc bff_add {addr} {
    global busyfury_firewall busyfury_firewall_time bff_addrs
    if {!$busyfury_firewall || !$busyfury_firewall_time} return
    if {[info exists bff_addrs($addr)]} return
    set bff_addrs($addr) [after $busyfury_firewall_time bff_expire $addr]
    bff_log $addr deny
    bff_setup
}

proc bff_expire {addr} {
    global bff_addrs
    unset bff_addrs($addr)
    bff_log $addr accept
    bff_setup
}

proc bff_setup {} {
    global busyfury_firewall bff_addrs firewall_command
    if {!$busyfury_firewall} return
    set addrs [array names bff_addrs]
    set cmd [concat [list $firewall_command < /dev/null] $addrs]
    if {[catch {
	set result [eval exec $cmd]
    } emsg]} {
	log error "unable to set firewall state: $emsg"
    }
    if {[llength $result]} { debug 1 "set firewall: $result" }
}

proc globalavfpool_start {} {
    global avfpoolid
    set avfpoolid [thread_start avfpool avfpool]
    thread_join {} {} avfpool $avfpoolid globalavfpool_done globalavfpool_done
}

proc globalavfpool_done {args} {
    log error "avfpool done !  report: $args"
    globalavfpool_start
}

proc setstate {type entry why args} {
    global var_dir errorInfo errorCode
    log dbreasons [list $type $entry [lindex $args 0] $why]
    eval [list ds_set $type-list $entry] $args
}

proc databases_init {} {
    global var_dir
    global initdb_file

    foreach what {addr site} \
	    re {{^(black|white|whitesoon|verified)$} {^(black|white|whitesoon)$}} {
	ds_bind $what-list $var_dir/db.$what-list $re
	ds_bind $what-seen $var_dir/db.$what-seen {^\d+$}
    }
    ds_bind site-annoy $var_dir/db.site-annoy {^\d+am?\d+$}

    set f [open $initdb_file r]
    set lno 0
    while {[gets $f l] != -1} {
	incr lno
	if {[regexp {^\#} $l]} { continue }
	if {![regexp {^(site|addr)\s+(white|black)\s+(.*\S)\s*$} \
		     $l dummy type state keyquoted]} {
	    error "$filename:$lno:bad format in blacklist/whitelist config"
	}
	set key [subst -nocommands -novariables $keyquoted]
	ds_setforever $type-list $key $state
    }
    close $f
}

if {[catch {
    if {[llength $real_argv]} {
	error "please supply no non-option arguments"
    }
    reopenlogs
    if {[file writable /]} {
	error "do not run sauce as root; use something like authbind instead"
    }
    databases_init
} emsg]} {
    if {![string length $current_bigerr]} {
	log fatal "error starting up: $emsg ($errorInfo)"
	set current_bigerr "Error starting up"
    }
}

if {[string length $current_bigerr]} {
    log fatal "fatal errors, stopping ($current_bigerr)";
    exit 1
}

set adminsecret {}
globalavfpool_start

if {[catch {
    if {[array size local_interface]} {
	foreach li [array names local_interface] {
	    socket -myaddr $li -server new_conn $port
	}
    } else {
	socket -server new_conn $port
    }

    thread_start adminsecret admin-secret
    bff_setup
    log notice started

    if {![info exists asynch_script]} {
	vwait quit_now
    }
} emsg]} {
    log fatal "main program returned error: $emsg, $errorInfo"
}
