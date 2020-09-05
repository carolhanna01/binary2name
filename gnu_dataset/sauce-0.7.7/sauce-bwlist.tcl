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
# $Id: sauce-bwlist.tcl,v 1.6 2001/03/21 23:53:14 ian Exp $

set @@@var_dir@@@ .
set @@@sbin_dir@@@ .
set @@@username@@@ ian

proc usage {} {
    puts stdout \
 {usage: sauce-bwlist [--force] --black|--white|--unknown --addr|--site <entry> [<reason>]}
}

proc usageerr {emsg} {
    puts stderr "usage error: $emsg"
    usage
    exit 255
}

proc nextarg {} {
    global argv
    set a [lindex $argv 0]
    set argv [lrange $argv 1 end]
    return $a
}

set force 0
set reasonprefix {}
set type unset
set newst unset

while {[regexp {^--} [lindex $argv 0]]} {
    set a [nextarg]
    switch -exact -- $a {
	-- { break }
	--force { set force 1 }
	--addr { set type addr }
	--site { set type site }
	--black { set newst black }
	--unknown { set newst unknown }
	--white { set newst white }
	--userv {
	    set type arg
	    set newst arg
	    set reasonprefix "$env(USERV_USER): "
	}
	default { usageerr "unknown option: $a" }
    }
}

proc checkarg {vn list} {
    upvar #0 $vn var

    if {"$var" == "arg"} {
	set a [nextarg]
	foreach x $list { if {"$a" == "$x"} { set var $x; return } }
	error "? $vn $a"
    } elseif {"$var" == "unset"} {
	usageerr "must specify --XYZ where XYZ is one of $list"
    }
}

checkarg type {addr site}
checkarg newst {black white unknown}

switch -exact [llength $argv] {
    0 { usageerr "no entry given" }
    1 { set reason "no reason given" }
    2 { regexp {.*} [lindex $argv 1] reason }
    default { usageerr "too many arguments" }
}

regexp {.*} [lindex $argv 0] entry

set reason $reasonprefix$reason

if {[regexp {^[0-9]*[-&|@<>]} $entry meta]} {
    usageerr "entry contains metachars: $meta"
}

if {[catch {
    set f [open ${@@@var_dir@@@}/adminsecret r]
} emsg]} {
    if {"[lrange $errorCode 0 1]" != "POSIX EACCES"} {
	error $emsg $errorInfo $errorCode
    }
    if {$force} { usageerr "privilege required to --force" }
    if {[catch {
	exec userv ${@@@username@@@} sauce-userblacklist \
		$type $newst $entry " $reason" >@ stdout
    } emsg]} { puts stderr "requesting service via userv: $emsg"; exit 255 }
} else {
    close $f
    set cmd [list userblacklist $type $newst $entry $force $reason]
    if {[catch {
	exec ${@@@sbin_dir@@@}/sauceadmin $cmd >@ stdout
    } emsg]} { puts stderr "executing sauceadmin: $emsg"; exit 255 }
}
