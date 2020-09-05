########## datastate.tcl
# This file contains core routines for handling the persistent
# data with timeouts.
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
# $Id: datastate.tcl,v 1.11 2001/03/21 23:53:14 ian Exp $

# This routine maintains permanent database(s) of information about
# key(s).  Each database maps a key to a state value.  The state
# values are defined by the caller.  The states may time out, and be
# replaced by other states, as defined by the caller.

# The variables used internally are:
#  ds__array.DB.(KEY) -> [list TIMEOUT STATE NEXT_TIMEOUT NEXT_STATE ...]
# Ie, the array element contains a list of successive timeouts and
# states.  TIMEOUT may be * to mean forever.  Empty lists should not
# occur.  DB should consist of alphanumerics, _ and -, start
# with a letter, and end with an alphanumeric.
#  ds__prefix.DB     Contains the filename prefix used for binding.
#  ds__logfile.DB    File handle for log (incremental) file;
#                    may not be set if there is no log data.
#  ds__loglen.DB     Number of entries written to log file

proc ds_get {db key} {
    # Returns the current value in DB of KEY.  If the key is not
    # found (or has expired), returns `unknown'.
    upvar #0 ds__array.$db.($key) elem
    if {![info exists elem]} {
	debug 3 ds_get $db $key => not found
	return unknown
    }
    set telem $elem
    debug 3 ds_get $db $key => $telem ?
    if {[ds__proctimeouts telem [clock seconds]]} {
	if {[string length $telem]} {
	    set elem $telem
	} else {
	    unset elem
	    return unknown
	}
    }
    set value [lindex $telem 1]
    ds__checkvalue $db $value
    return $value
}

proc ds__checkvalue {db value} {
    upvar #0 ds__regexp.$db regexp
    if {![regexp -- $regexp $value]} { error "bad db value $value for $db" }
}

proc ds_set {db key args} {
    # Sets, in DB, the value of KEY.  The remaining ARGS should come
    # in pairs VALUE TIMEOUT, where VALUE is the value, and TIMEOUT is
    # the duration in seconds for which the value should hold.  VALUEs
    # should consist of alphanumerics.

    upvar #0 ds__array.$db. array
    upvar #0 ds__array.$db.($key) elem
    upvar #0 ds__prefix.$db prefix
    upvar #0 ds__logfile.$db logfile
    upvar #0 ds__loglen.$db loglen
    set telem {}
    set now [clock seconds]

    debug 3 ds_set $db $key := $args at $now
    foreach {value timeout} $args {
	ds__checkvalue $db $value
	incr timeout $now
	lappend telem $timeout $value
    }
    debug 3 ds_set $db $key := $telem ?
    ds__proctimeouts telem $now
    if {[string length $telem]} {
	set elem $telem
    } else {
	catch { unset elem }
    }
    ds__ensurelog $db
    ds__writerecord $logfile $key $telem
    if {[incr loglen] > [array size array]+20} {
	ds__consolidate $db $now
    }
}

proc ds__ensurelog {db} {
    global errorInfo errorCode
    upvar #0 ds__prefix.$db prefix
    upvar #0 ds__logfile.$db logfile
    if {[info exists logfile]} return
    debug 3 ds__ensurelog $db ...
    set fh [open $prefix.log {WRONLY CREAT EXCL} 0666]
    if {[catch {
	fconfigure $fh -buffering line
    } emsg]} {
	set einfo $errorInfo
	set ecode $errorCode
	catch { close $fh }
	error $emsg $einfo $ecode
    }
    set logfile $fh
}


proc ds_bind {db fileprefix regexp} {
    # Binds the database DB to files with prefix FILEPREFIX.
    # Initially, this will load the database, and it will also cause
    # updates to be recorded there.  The files used are
    # FILEPREFIX.main, FILEPREFIX.log and FILEPREFIX.new.
    # Values must match REGEXP (though ds_get may also return `unknown').
    upvar #0 ds__prefix.$db prefix
    upvar #0 ds__loglen.$db loglen
    upvar #0 ds__regexp.$db record_regexp

    set prefix $fileprefix
    set now [clock seconds]
    set record_regexp $regexp
    debug 3 ds_bind $db $fileprefix at $now
    set loglen 0
    ds__readfile $db $prefix.main $now
    ds__readfile $db $prefix.log $now
    ds__consolidate $db $now
}

proc ds_setforever {db key value} {
    # Sets, in DB, the value of KEY to VALUE, forever.  This is not
    # recorded in any database files - it is assumed to be the
    # result of static configuration.
    upvar #0 ds__array.$db. array

    debug 3 ds_setforever $db $key $value
    ds__checkvalue $db $value
    set array($key) [list * $value]
}

proc ds__readfile {db filename now} {
    global errorCode errorInfo
    upvar #0 ds__array.$db. array

    if {[catch {
	set fh [open $filename r]
    } emsg]} {
	if {[lrange $errorCode 0 1] == "POSIX ENOENT"} {
	    debug 3 ds__readfile $db $filename $now nonexistent
	    return 0
	}
	error $emsg $errorInfo $errorCode
    }
    debug 3 ds__readfile $db $filename $now ok
    set lno 0
    if {[catch {
	while {[gets $fh l] != -1} {
	    incr lno
	    if {![regexp {\;$} $l]} {
		log notice "$filename:$lno:incomplete records in database"
		continue
	    }
	    if {![regexp -nocase \
		    {^\:([-=_+@.%0-9a-z\\]*)( (\d+) ([0-9a-z][0-9a-z]*))+\;$} \
		    $l dummy keyquoted telem]} {
		log error "$filename:$lno:bad format in database:$l"
		continue
	    }
	    set key {}
	    while {[regexp -nocase {^(.*)\\x([0-9a-f][0-9a-f])(.*)$} $keyquoted \
		    dummy l hex keyquoted]} {
		append key $l [binary format H* $hex]
	    }
	    append key $keyquoted
	    regsub {^ } $telem {} telem
	    debug 3 ds__readfile $db $filename element $key $telem ?
	    ds__proctimeouts telem $now
	    if {[string length $telem]} {
		set array($key) $telem
	    } else {
		catch { unset array($key) }
	    }
	}
    } emsg]} {
	set einfo $errorInfo
	set ecode $errorCode
	catch { close $fh }
	error $emsg $einfo $ecode
    }
    close $fh
    return 1
}

proc ds__proctimeouts {telemvar now} {
    upvar 1 $telemvar telem
    set changed 0

    while {[llength $telem] && "[lindex $telem 0]" != "*" && [lindex $telem 0] < $now} {
	debug 3 ds__proctimeouts $now $telem dropping
	set telem [lrange $telem 2 end]
	set changed 1
    }
    debug 3 ds__proctimeouts $now $telem !
    return $changed
}

proc ds__consolidate {db now} {
    global errorInfo errorCode
    upvar #0 ds__array.$db. array
    upvar #0 ds__prefix.$db prefix
    upvar #0 ds__logfile.$db logfile
    upvar #0 ds__loglen.$db loglen
    
    debug 3 ds__consolidate $db $now ...
    if {[info exists logfile]} {
	set e [catch { close $logfile } emsg]
	unset logfile
	if {$e} { error $emsg $info $ecode }
    }
    ds__writefile $db $prefix.new $now
    file rename -force -- $prefix.new $prefix.main
    file delete -- $prefix.log
    set loglen 0
}

proc ds__writefile {db filename now} {
    global errorInfo errorCode
    upvar #0 ds__array.$db. array

    debug 3 ds__writefile $db $filename $now
    set fh [open $filename {WRONLY TRUNC CREAT} 0666]
    if {[catch {
	foreach key [array names array] {
	    set telem $array($key)
	    if {[lindex $telem 0] == "*"} { continue }
	    ds__proctimeouts telem $now
	    if {[llength $telem]} {
		ds__writerecord $fh $key $telem
	    }
	}
	close $fh
    } emsg]} {
	set einfo $errorInfo
	set ecode $errorCode
	catch { close $f }
	catch { file delete -- $filename }
	error $emsg $einfo $ecode
    }
}

proc ds__writerecord {fh key telem} {
    debug 3 ds__writerecord $key [list $telem]
    set keyquoted {}
    while {[regexp -nocase {^([-=_+@.%0-9a-z]*)([^-=_+@.%0-9a-z])(.*)$} \
            $key dummy l ch key]} {
	binary scan $ch H* hex
	append keyquoted $l {\x} $hex
    }
    append keyquoted $key
    debug 3 ds__writerecord ... $keyquoted
    puts $fh ":$keyquoted $telem;"
}
