########## thread.tcl
# `Thread'-handling functions
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
# $Id: thread.tcl,v 1.20 2001/03/21 23:53:15 ian Exp $

proc thrdbg_threads {type} {
    foreach x [lsort [info globals]] {
	if {[string match $type/* $x]} { puts $x }
    }
}

proc thrdb_state {type/id} {
    upvar #0 [set type/id] state
    foreach n [lsort [array names state]] {
	puts [list set $n $state($n)]
    }
}

proc thread_typedefine {type formarglist startbody shutdownbody cleanbody} {
    upvar #0 $type/nid nid
    set nid 0
    if {"$shutdownbody" == "ERROR-ON-SHUTDOWN"} {
	set shutdownbody "\n    thread_error $type \$id {shut down} {}\n"
    } elseif {"$shutdownbody" == "NO-CLEAN-SHUTDOWN"} {
	set shutdownbody "\n    global threads\n    unset threads(\[list $type \$id\])\n"
    }
    proc $type/start [concat id $formarglist] "    upvar #0 $type/\$id state\n$startbody"
    proc $type/shutdown {id} "    upvar #0 $type/\$id state\n$shutdownbody"
    proc $type/clean {id} "    upvar #0 $type/\$id state\n$cleanbody"
}

proc thread_chainproc {type name formarglist body} {
    proc $type//$name [concat id $formarglist] "    upvar #0 $type/\$id state\n$body"
}

proc thread_subproc {type name formarglist body} {
    proc "${type}_$name" $formarglist "    upvar id id\n    upvar state state\n$body"
}

proc thread_start {type desc args} {
    global errorInfo threads
    upvar #0 $type/nid id
    incr id
    upvar #0 $type/$id state
    debug0 2 [list $type $id start] $args
    if {[catch {
	if {[info exists state]} { error "$type/$id already exists" }
	set state(desc) $desc
	eval [list $type/start $id] $args
	set threads([list $type $id]) 1
    } emsg]} {
	log error "Tcl error (startup) $type $args:\n $errorInfo"
	set state(r_emsg) $emsg
	$type/clean $id
    }
    return $id
}

proc thread_shuttingdown {} {
    global terminating
    return [info exists terminating]
}    

proc thread__shutdowncheck {} {
    global terminating threads
    debug0 2 shutdowncheck [thread_shuttingdown] [array names threads]
    if {![thread_shuttingdown]} return
    if {[array size threads]} return
    eval $terminating
}

proc thread_sysshutdown {script} {
    global terminating threads errorInfo
    set terminating $script
    thread__shutdowncheck
    foreach ti [array names threads] {
	manyset $ti type id
	if {[catch {
	    eval $type/shutdown $id
	} emsg]} {
	    thread_error $type $id $emsg \
		    "Tcl error (shutting down) $type $id:\n $errorInfo"
	}
    }
    thread__shutdowncheck
}

proc thread_forceshutdown {} {
    global threads
    foreach ti [array names threads] {
	manyset $ti type id
	upvar #0 $type/$id state
	if {[info exists state(desc)]} {
	    set desc $state(desc)
	} else {
	    set desc {<description not available>}
	}
	debug0 1 "thread still running: $type/$id $desc"
    }
    log notice "shutdown tidy timed out, quitting now"
    exit 1
}

proc thread__cleanup {type id} {
    global threads
    debug0 2 cleanup $type $id
    catch { unset threads([list $type $id]) }
    $type/clean $id
    thread__shutdowncheck
}

proc thread__docancel {type id} {
    global threads
    upvar #0 $type/$id state
    if {![info exists state(r_emsg)] && ![info exists state(r_result)]} {
	thread__cleanup $type $id
    }
    unset state
}

proc thread_cancel {type id} {
    debug0 2 [list $type $id cancel]
    thread__docancel $type $id
}

proc thread_chain {type func args} {
    upvar id id
    thread__eval $type $id $func $args {}
}

proc thread_crosscall {type id func args} {
    thread__eval $type $id $func $args {}
}

proc thread__eval {type id func argsl1 argsl2} {
    global errorInfo
    if {![string length $type]} {
	eval $func $argsl1 $argsl2
    } elseif {![string length $func]} {
	set emsg [lindex $argsl2 0]
	debug0 2 [list propagated-error $type $id $emsg $argsl1 $argsl2]
	thread_error $type $id $emsg {}
    } elseif {[catch {
	eval $type//$func $id $argsl1 $argsl2
    } emsg]} {
	thread_error $type $id $emsg \
		"Tcl error $type $id $func $argsl1 $argsl2:\n $errorInfo"
    } else {
	return $emsg
    }
    return {}
}

proc thread__checkdone {} {
    upvar type type
    upvar state state
    upvar id id
    if {[info exists state(r_emsg)] && [info exists state(ca_onerrf)]} {
	set emsg $state(r_emsg)
	set ca_type $state(ca_type)
	set ca_id $state(ca_id)
	set ca_onerrf $state(ca_onerrf)
	set ca_xargs $state(ca_xargs)
	debug0 2 [list $type $id error $emsg]
	thread__docancel $type $id
	thread__eval $ca_type $ca_id $ca_onerrf $ca_xargs [list $emsg]
    } elseif {[info exists state(r_result)] && [info exists state(ca_onokf)]} {
	set values $state(r_result)
	set ca_type $state(ca_type)
	set ca_id $state(ca_id)
	set ca_onokf $state(ca_onokf)
	set ca_xargs $state(ca_xargs)
	debug0 2 [list $type $id finish] $values
	thread__docancel $type $id
	thread__eval $ca_type $ca_id $ca_onokf $ca_xargs $values
    }
}

proc thread_error {type id emsg loginfo} {
    global threads
    upvar #0 $type/$id state
    if {[string length $loginfo]} {
	log error $loginfo
    }
    set state(r_emsg) $emsg
    thread__cleanup $type $id
    thread__checkdone
}

proc thread_finish {type id args} {
    global threads
    upvar #0 $type/$id state
    set state(r_result) $args
    thread__cleanup $type $id
    thread__checkdone
}

proc thread_join {mytype myid type id ca_onokf ca_onerrf args} {
    upvar #0 $type/$id state
    set state(ca_type) $mytype
    set state(ca_id) $myid
    set state(ca_onokf) $ca_onokf
    set state(ca_onerrf) $ca_onerrf
    set state(ca_xargs) $args
    thread__checkdone
}

proc thread_fileevent {type id chan what func args} {
    fileevent $chan $what [list thread__eval $type $id $func $args {}]
}

proc thread_after {type id timeout func args} {
    return [after $timeout [list thread__eval $type $id $func $args {}]]
}

proc bgerror {msg} {
    global errorCode errorInfo
    log error "sauce: untrapped error: $msg ($errorCode $errorInfo)"
}

########## threadio functions

proc chan_debug {lev desc dirn str} {
    foreach le [split $str "\n"] {
	if {[string length $le]} {
	    debug0 $lev "$desc $dirn $le"
	}
    }
}

proc threadio__edesc {emsg} {
    global errorCode
    set ecode $errorCode
    switch -exact [lindex $ecode 0] {
	POSIX { return [lindex $ecode 2] }
	CHILDKILLED { return "subprocess failed: [lindex $ecode 3]" }
	CHILDSTATUS { return "subprocess failed with exit status [lindex $ecode 2]" }
	default { return "$ecode -- $emsg" }
    }
}

proc threadio_gets {type id chan onokf onerrf args} {
    fileevent $chan readable \
	    [list threadio__gets_ready $type $id $chan $onokf $onerrf $args]
}

proc threadio__gets_ready {type id chan onokf onerrf argsl} {
    global chan_desc
    manyset $chan_desc($chan) cd cdhi cdho
    if {[catch {
	gets $chan str
    } emsg]} {
	set edesc [threadio__edesc $emsg]
	fileevent $chan readable {}
	debug0 1 "$cd ** reading: $edesc $emsg"
	thread__eval $type $id $onerrf $argsl [list $edesc]
    } elseif {[fblocked $chan]} {
	return
    } elseif {[eof $chan]} {
	fileevent $chan readable {}
	debug0 1 "$cd <<\$\$"
	thread__eval $type $id $onokf $argsl {{}}
    } else {
	regsub {\r$} $str {} str
	fileevent $chan readable {}
	chan_debug $cdhi $cd {<<} $str
	thread__eval $type $id $onokf $argsl [list $str]
    }
}

proc threadio__puts_debug {chan data} {
    global chan_desc
    manyset $chan_desc($chan) cd cdhi cdho
    chan_debug $cdho $cd {>>} $data
}

proc threadio__puts_puts {chan data} {
    puts -nonewline $chan $data
}

proc threadio_puts_throw {chan data} {
    threadio__puts_debug $chan $data
    threadio__puts_puts $chan $data
}

proc threadio_puts {type id chan data onokf onerrf args} {
    threadio__puts_debug $chan $data
    fileevent $chan writable [list threadio__puts_ready $type $id \
	    $chan $data $onokf $onerrf $args]
}

proc threadio__puts_ready {type id chan data onokf onerrf argsl} {
    global chan_desc
    manyset $chan_desc($chan) cd cdhi cdho
    if {[catch {
	threadio__puts_puts $chan $data
	flush $chan
    } emsg]} {
	set edesc [threadio__edesc $emsg]
	fileevent $chan writable {}
	debug0 1 "$cd ** writing: $edesc $emsg"
	thread__eval $type $id $onerrf $argsl [list $edesc]
    } elseif {[fblocked $chan]} {
	return	
    } else {
	fileevent $chan writable {}
	thread__eval $type $id $onokf $argsl {}
    }
}

proc threadio__putsgets_puts_ok {type id chan onokf onerrf argsl} {
    eval [list threadio_gets $type $id $chan $onokf $onerrf] $argsl
}

proc threadio__putsgets_puts_err {type id chan onokf onerrf argsl emsg} {
    thread__eval $type $id $onerrf $argsl [list $emsg]
}

proc threadio_putsgets {type id chan data onokf onerrf args} {
    threadio_puts {} {} $chan $data \
	    threadio__putsgets_puts_ok threadio__putsgets_puts_err \
	    $type $id $chan $onokf $onerrf $args
}

proc threadio__commandresponse_line_ok \
	{type id chan re onokf onerrf sofar argsl what data} {
    regexp {^.*} $data data
    if {[eof $chan]} {
	thread__eval $type $id $onerrf $argsl [list "${what}connection dropped"]
    } elseif {[regexp {^[0-9][0-9][0-9]-} $data]} {
	append sofar $data "\n"
	threadio_gets {} {} $chan \
		threadio__commandresponse_line_ok threadio__commandresponse_line_err \
		$type $id $chan $re $onokf $onerrf $sofar $argsl $what
    } elseif {[regexp {^[0-9][0-9][0-9]} $data]} {
	append sofar $data
	if {[regexp -- $re $data]} {
	    thread__eval $type $id $onokf $argsl [list $sofar]
	} else {
	    thread__eval $type $id $onerrf $argsl [list "$what$sofar"]
	}
    } else {
	thread__eval $type $id $onerrf $argsl [list "${what}invalid $data"]
    }
}

proc threadio__commandresponse_line_err \
	{type id chan re onokf onerrf sofar argsl what emsg} {
    thread__eval $type $id $onerrf $argsl [list $emsg]
}

proc threadio_commandresponse {type id chan cmd re onokf onerrf args} {
    if {[string length $cmd]} {
	append cmd "\n"
	regexp {^[^ \t\n]*} $cmd what
	set what "[string trim [string toupper $what]] => "
    } else {
	set what ""
    }
    threadio_putsgets {} {} $chan $cmd \
	    threadio__commandresponse_line_ok threadio__commandresponse_line_err \
	    $type $id $chan $re $onokf $onerrf {} $args $what
}

proc chanset_desc {chan msg} {
    global chan_desc
    set chan_desc($chan) [list $msg 1 1]
}

proc chanset_hide {chan in out} {
    upvar #0 chan_desc($chan) cde
    set cde [lreplace $cde 1 2 $in $out]
}

proc catch_close_cleardesc {chanvar} {
    # closes and cleans up the channel whose name is stored
    # in the variable named by chanvar.  Idempotent (unsets
    # the variable).
    global chan_desc
    catch {
	upvar 1 $chanvar chan
	set ochan $chan
	unset chan
	catch { fileevent $ochan writable {} }
	catch { fileevent $ochan readable {} }
	catch { debug0 1 "[lindex $chan_desc($ochan) 0] >>\$\$" }
	catch { close $ochan }
	catch { unset chan_desc($ochan) }
    }
}
