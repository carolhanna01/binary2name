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
# $Id: sauceadmin.tcl,v 1.15 2001/03/21 23:53:15 ian Exp $

proc log {lev msg} {
    puts stderr $msg
}

set @@@readlibs@@@ readlibs.tcl
set sauce_libraries {
    readconf
    sconfig
}
source ${@@@readlibs@@@}

proc readconfig_posthook {args} {}

readconfig

if {[string length $current_bigerr]} {
    log fatal "configuration errors, stopping"
    exit 3
}

set interfaces [array names local_interface]
if {![llength $interfaces]} { set interfaces 127.0.0.1 }

set tries [llength $interfaces]
if {$tries < 4} { set tries 4 }

set connected 0
set errorred 0
while {!$connected} {
    if {![llength $interfaces]} {
	if {!$errorred} { exit 1 }
	puts stderr "no interfaces left to try"; exit 2
    }
    set if [lindex $interfaces 0]
    set interfaces [lrange $interfaces 1 end]
    if {[catch { set sock [socket $if $port] } emsg]} {
	if {$sauceadmin_connrefused_ok &&
	    "[lindex $errorCode 0]" == "POSIX" &&
	    "[lindex $errorCode 1]" == "ECONNREFUSED"} {
            continue
	}
	set errored 1
	puts stderr "$if: $emsg"
	continue
    }
    if {[catch {
	fconfigure $sock -buffering line -translation {crlf crlf}
	gets $sock banner
	if {![regexp {^220} $banner]} { error "banner => $banner" }
	puts $sock SAUCEADMIN
	gets $sock chalstr
	if {![regexp {^393[ \t]+([0-9a-f]+)\r?$} $chalstr all chal]} {
	    error "SAUCEADMIN => $chalstr"
	}
	set sfile [open $var_dir/adminsecret r]
	set secret [read $sfile]
	close $sfile; unset sfile
	set resp [exec <<"[binary format H* $chal]$secret" md5sum]
	puts $sock "SAUCEADMIN $chal $resp"
	gets $sock okstr
	if {![regexp {^294|^4[0-9][0-9]} $okstr]} {
	    error "SAUCEADMIN <response> => $okstr"
	}
    } emsg]} {
	set errored 1
	puts stderr "$if: $emsg"
	catch { close $sfile; unset sfile }
	catch { close $sock; unset sock }
	continue
    }
    if {[regexp {^294} $okstr]} {
	set connected 1
    } else {
	set errored 1
	puts stderr "$if: SAUCEADMIN <response> => $okstr"
	incr tries -1
	if {$tries <= 0} { puts stderr "too many retries"; exit 2 }
	exec sleep 4
	lappend interfaces $if
    }
}

if {[llength $real_argv]} {
    fconfigure $sock -buffering none
    puts $sock ";"
    gets $sock x
    if {"$x" != "% "} { puts stderr "startup: `$x'"; exit 2 }
    foreach cmd $real_argv {
	gets $sock x
	if {"$x" != "EOP"} { puts stderr "before $cmd: `$x'"; exit 2 }
	puts $sock "$cmd\n;"
	set op "\n"
	while 1 {
	    gets $sock x
	    if {"$x" == "EOP"} break
	    if {![string length $x] && [eof $sock]} {
		if {"$cmd" == "shutdown"} { exit 0 }
		puts stderr "after $cmd: eof"
		exit 2
	    }
	    append op "$x\n"
	}
	regsub {\n\% \n} $op "\n" op
	regsub {^\n} $op {} op
	puts -nonewline $op
	puts $sock ";"
	gets $sock x
	if {"$x" != ""} { puts stderr "after $cmd: `$x'"; exit 2 }
    }
    exit 0
}

proc copydata {in out} {
    set d [read $in]
    if {![string length $d] && [eof $in]} {
	puts " disconnected"
	exit 0
    }
    puts -nonewline $out $d
}

fconfigure stdin -blocking false -buffering none
fconfigure stdout -blocking false -buffering none
fconfigure $sock -blocking false -buffering none
fileevent stdin readable [list copydata stdin $sock]
fileevent $sock readable [list copydata $sock stdout]
    
vwait unused
