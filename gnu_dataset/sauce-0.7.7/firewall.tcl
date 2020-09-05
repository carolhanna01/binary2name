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
# $Id: firewall.tcl,v 1.4 2001/03/21 23:53:14 ian Exp $

# usage: .../firewall <chain> <target> -- [<ip-address> ...]
# invoked by SAUCE from userv using with-lock

set ipchains ipchains

set chain [lindex $argv 0]
set target [lindex $argv 1]
if {"[lindex $argv 2]" != "--"} { error "bad delimiter" }
set addrs [lrange $argv 3 end]

set lchan [open |[list $ipchains -n -L $chain] r]
set ix 0
while {[gets $lchan l] >= 0} {
    switch -regexp $l {
	{^(DENY|REJECT)\s+\w+\s+\-+\s+[.0-9]+\s+0\.0\.0\.0/0\s+n/a$} {
	    set b_now([lindex $l 3]) [incr ix]
	}
	{^Chain \w+ \(.*\)\:$} { }
	{^target\s+prot\s+opt\s+source\s+destination\s+ports$} { }
	default { error "unknown $l" }
    }
}
close $lchan

foreach a $addrs {
    if {![regexp {^(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})$} $a ma]} {
	error "bad address $a"
    }
    set b_want($ma) 1
}

proc modify_chain {what addr} {
    global ipchains chain target
    exec $ipchains $what $chain -j $target -s $addr >@ stderr 2>@ stderr
}

foreach x [array names b_now] {
    if {[info exists b_want($x)]} continue
    modify_chain -D $x
}

foreach x [array names b_want] {
    if {[info exists b_now($x)]} continue
    modify_chain -A $x
}
