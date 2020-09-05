########### msgdata.tcl
# Routines (part of main program) for dealing with SAUCEADMIN.
#
# This file is part of SAUCE, a very picky anti-spam receiver-SMTP.
# SAUCE is Copyright (C) 1997-2001 Ian Jackson.
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
# $Id: yesmaster.tcl,v 1.15 2001/03/21 23:53:15 ian Exp $

# state variables used during tcl commands processing:
# sofar             partially received command

set yesmaster_shutdowns {}

thread_chainproc ic yesmaster_outdone {} {
    ic_yesmaster_startcmd
}

thread_subproc ic yesmaster_startcmd {} {
    set state(sofar) {}
    threadio_putsgets ic $id $state(chan) "% " yesmaster_gotdata yesmaster_err
}

thread_chainproc ic yesmaster_gotdata {data} {
    global errorInfo yesmaster_shutdowns threads
    if {![string length $data] && [eof $state(chan)]} {
	thread_finish ic $id
	return
    }
    append state(sofar) $data
    if {"$state(sofar)" == ";"} {
	set state(sofar) {}
	threadio_putsgets ic $id $state(chan) "\nEOP\n" yesmaster_gotdata yestmaster_err
	return
    }
    if {"[string trim $state(sofar)]" == "shutdown"} {
	unset threads([list ic $id])
	shutdown
    } elseif {[info complete $state(sofar)]} {
	set code [catch [list uplevel #0 $state(sofar)] result]
	if {$code} {
	    set output "** $errorInfo\n"
	} elseif {[string length $result]} {
	    set output "=> $result\n"
	} else {
	    set output {}
	}
	threadio_puts ic $id $state(chan) $output yesmaster_outdone yesmaster_err
    } else {
	threadio_gets ic $id $state(chan) yesmaster_gotdata yesmaster_err
    }
}

thread_chainproc ic yesmaster_err {emsg} {
    log notice "$state(desc): error during admin: $emsg"
    thread_finish ic $id
}

########## adminsecret thread
#
# thread_start adminsecret $desc
#
# never returns

# state variables:
# toid      timeout id

thread_typedefine adminsecret {} {
    adminsecret_refresh
} NO-CLEAN-SHUTDOWN {
    global adminsecret
    set adminsecret {}
    catch { after cancel $state(toid) }
}

thread_chainproc adminsecret timeout {} {
    adminsecret_refresh
}

thread_subproc adminsecret refresh {} {
    global adminsecret admin_secret_length admin_secret_refresh var_dir
    set adminsecret {}
    set chan {}
    if {[catch {
	set new [exec -keepnewline dd if=/dev/urandom bs=1 \
		count=[format %d $admin_secret_length] 2>/dev/null]
	if {[string length $new] == $admin_secret_length} {
	    if {[file exists $var_dir/adminsecret] &&
	        [file size $var_dir/adminsecret] <= $admin_secret_length} {
		set mode {WRONLY CREAT}
	    } else {
		set mode w
	    }
	    set chan [open $var_dir/adminsecret $mode 0600]
	    puts -nonewline $chan $new
	    close $chan
	    unset chan
	    set adminsecret $new
	    log notice "new admin secret set"
	} else {
	    error "admin secret wrong length"
	}
    } emsg]} {
	log error "unable to make new admin secret: $emsg"
    }
    thread_after adminsecret $id $admin_secret_refresh timeout
}

########## helper and command functions for sauceadmin
#

proc show {args} {
    return [join $args]
}

proc help {} {
    show {Some useful commands:
  readconfig                reread config files
  reopenlogs                reopen log files
  show <value>              like puts, but goes where you want it
  exit                      stop SAUCE immediately
  set debug_level <number>  set debugging level
  set <config_var> <canon>  reconfigure - but be careful, no checking !
  userblacklist <force> <reason> addr|site <entry>}
}

proc ? {} { help }

proc userblacklist {type newst entry force why} {
    set st [ds_get $type-list $entry]
    if {"$newst" == "unknown"} {
	set newst whitesoon
	set to -1
    } else {
	upvar #0 ${type}_blacklist_timeout to
    }
    switch -exact -- $st {
	whitesoon - black - verified {
	}
	default {
	    if {!$force} {
		return "$type state is $st: $entry"
	    }
	}
    }
    setstate $type $entry $why $newst $to
    return "$type ${newst}listed: $entry"
}
