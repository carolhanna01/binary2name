#!/usr/bin/tclsh8.2

# This [sub]program is Copyright (C) 1997-2001 Ian Jackson
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
# $Id: asynch.tcl,v 1.10 2001/03/21 23:53:14 ian Exp $
 
proc asynch_startcmd {} {
    global asynch_sofar
    set asynch_sofar {}
    puts -nonewline "% "
    flush stdout
}

proc morecmd {} {
    global asynch_sofar asynch_result asynch_code errorInfo
    set r [read -nonewline stdin]
    if {[eof stdin]} {
	fconfigure stdin -blocking true
	puts -nonewline "\n"
	exit 0
    }
    append asynch_sofar $r
    if {[info complete $asynch_sofar]} {
	uplevel #0 {set asynch_code [catch $asynch_sofar asynch_result]}
	if {$asynch_code} {
	    puts "** $errorInfo"
	    flush stdout
	} elseif {[string length $asynch_result]} {
	    puts "=> $asynch_result"
	}
	flush stdout
	asynch_startcmd
    }
}

if {[llength $argv] && "[lindex $argv 0]" == "-d"} {
    set tcl_traceExec 1
    set argv [lreplace $argv 0 0]
} elseif {[llength $argv] && "[lindex $argv 0]" == "-dd"} {
    set tcl_traceExec 2
    set argv [lreplace $argv 0 0]
}

if {[llength $argv] &&
    [regexp {^-a(\d+)$} [lindex $argv 0] asynch_dummy asynch_appdebug]} {
    set argv [lreplace $argv 0 0]
}

if {[llength $argv]} {
    set asynch_script [lindex $argv 0]
    set argv [lreplace $argv 0 0]
    source $asynch_script
}

asynch_startcmd
fileevent stdin readable morecmd
fconfigure stdin -blocking false
vwait asynch_quitting
