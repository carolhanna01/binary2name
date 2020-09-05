########### dns.tcl
# DNS lookup code, using `host'
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
# $Id: dns.tcl,v 1.17 2001/03/21 23:53:14 ian Exp $


########## dns threads
#
# thread_start dns $desc $domain $type $cnameok
#
# success => $answers {} OK
# permanent failure (domain unknown) => {} $emsgstr NXDOMAIN
# permanent failure (type unknown) => {} $emsgstr NOTYPE
# permanent failure (misconfigured) => {} $emsgstr MISCONFIG
# temporary failure =>X
# $emsgstr is always a single line

# state variables:
# chan      channel onto `host' (unset => closed)
# domain    query domain
# type      query type (class is always IN)
# permfail  0 => no permanent failure detected yet; 1 => has been
# errs      error message(s) found so far
# answers   answer value(s) so far
# toid      timeout id

set rrdata_adnstype(MX) mx-
set rrdata_adnstype(A) a
set rrdata_adnstype(TXT) txt
set rrdata_adnstype(PTR) ptr-

thread_typedefine dns {domain type cnameok} {
    global dns_timeout var_dir rrdata_adnstype
    set domain [string tolower $domain]
    set adnsargs [expr {$cnameok ? "-Cs" : "-Cf"}]
    lappend adnsargs -t $rrdata_adnstype($type)
    lappend adnsargs - $domain
    set state(chan) [open [concat |adnshost -Fa +Do +Dt +Dc $adnsargs 2>@stderr] r]
    set cnokstr [expr {$cnameok ? "~" : "!"}]
    chanset_desc $state(chan) "$state(desc) / $type$cnokstr $domain"
    set state(domain) $domain
    set state(type) $type
    set state(toid) [thread_after dns $id $dns_timeout timeout]
    threadio_gets dns $id $state(chan) readline1 {}
    return $id
} ERROR-ON-SHUTDOWN {
    catch { after cancel $state(toid) }
    catch_close_cleardesc state(chan)
}

thread_subproc dns fault {msg} {
    error "Internal error: DNS lookup failed: $msg"
}

thread_subproc dns results {answers emsgstr how} {
    catch { close $state(chan) }
    unset state(chan)
    thread_finish dns $id $answers $emsgstr $how
}

thread_chainproc dns readline1 {data} {
    if {![string length $data] && [eof $state(chan)]} {
	dns_fault "unexpected immediate EOF"
    }
    if {![regexp -nocase {^0 ([0-9]+) [a-z]+ ([0-9]+) ([a-z]+) \"(.*)\"$} \
	    $data all nrrs statusnum statusstr emsg]} {
	dns_fault "bad response: $data"
    }
    set emsgstr "Error during DNS $state(type) lookup for $state(domain): $emsg"
    if {$nrrs} {
	threadio_gets dns $id $state(chan) readline2 {} {} $nrrs
	return
    } elseif {$statusnum <= 99} {
	thread_error dns $id $emsgstr {}
    } elseif {$statusnum <= 199} {
	dns_results {} $emsgstr MISCONFIG
    } elseif {$statusnum <= 299 || "$statusstr" == "nxdomain"} {
	dns_results {} $emsgstr NXDOMAIN
    } elseif {"$statusstr" == "nodata"} {
	dns_results {} $emsgstr NOTYPE
    } else {
	dns_fault "unexpected status: $statusnum $statusstr"
    }
}
	
thread_chainproc dns readline2 {sofar nrrs_remain data} {
    lappend sofar $data
    incr nrrs_remain -1
    if {$nrrs_remain <= 0} {
	dns_results $sofar {} OK
    } else {
	threadio_gets dns $id $state(chan) readline2 {} $sofar $nrrs_remain
    }
}	

thread_chainproc dns timeout {} {
    append state(errs) " lookup timed out"
    thread_error dns $id [string trim $state(errs)] {}
}

########## dnsptr threads
#
# thread_start dnsptr $desc $ipaddr
#
# success => $ipaddr {}
# permanent failure => {} $error
# temporary failure =>X
# $error is a single line string

# state variables:
# ipaddr    address for which PTR is requested
# dnsid     id of DNS query subthread (unset => none)
# remain    list of unchecked returns from PTR in-addr lookup (unset until DNS finishes)
# errs      list of hard error message(s)

thread_typedefine dnsptr {ipaddr} {
    set state(ipaddr) $ipaddr
    set ptr in-addr.arpa
    foreach octet [split $ipaddr .] {
	set ptr $octet.$ptr
    }
    set state(dnsid) [thread_start dns $state(desc) $ptr PTR 1]
    thread_join dnsptr $id dns $state(dnsid) dns_rvok dns_rverr
} ERROR-ON-SHUTDOWN {
    catch { thread_cancel $state(dnsid) }
}

thread_chainproc dnsptr dns_rvok {answers emsgstr how} {
    unset state(dnsid)
    if {[llength $answers]} {
	set state(remain) $answers
	set state(errs) {}
	dnsptr_continue
    } else {
	thread_finish dnsptr $id {} $emsgstr
    }
}

thread_chainproc dnsptr dns_rverr {emsg} {
    unset state(dnsid)
    thread_error dnsptr $id $emsg {}
}

thread_subproc dnsptr continue {} {
    if {![llength $state(remain)]} {
	thread_finish dnsptr $id {} \
		"$state(ipaddr) -> [join $state(errs) {; }]"
	return
    }
    set remain $state(remain)
    set try [lindex $remain 0]
    set state(remain) [lreplace $remain 0 0]
    set state(dnsid) [thread_start dns $state(desc) $try A 0]
    thread_join dnsptr $id dns $state(dnsid) dns_fwok dns_fwerr $try
}

thread_chainproc dnsptr dns_fwok {try answers emsgstr how} {
    unset state(dnsid)
    if {![string length $answers]} {
	lappend state(errs) "$try -> $emsgstr"
    } else {
	foreach ans $answers {
	    if {"$ans"=="$state(ipaddr)"} {
		thread_finish dnsptr $id $try {}
		return
	    }
	}
	lappend state(errs) "$try -> [join $answers {, }]"
    }
    dnsptr_continue
}

thread_chainproc dnsptr dns_fwerr {try emsg} {
    unset state(dnsid)
    thread_error dnsptr $id "$try -> $emsg" {}
}
