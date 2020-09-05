########### readconf.tcl
# Routines (part of controlling main program) for reading config file.
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
# $Id: readconf.tcl,v 1.15 2001/03/21 23:53:14 ian Exp $

set current_bigerr {}

proc config_var {vn def type args} {
    global configvars
    set configvars($vn) [list $type $args]
    config_setvar $type $vn $def $args
}

proc config_nd {vn type args} {
    global configvars
    set configvars($vn) [list $type $args]
}

proc config_raw {vn body} {
    global configvars configvarsset
    set configvars($vn) [list raw $vn]
    proc config_rawset_$vn {value} $body
    set configvarsset($vn) 1
}

proc config_setvar {type vn vv argl} {
    global configvarsset
    upvar #0 $vn var
    switch $type {
	raw {
	    config_rawset_[lindex $argl 0] $vv
	}
	hlist {
	    set subtype [lindex $argl 0]
	    set subargl [lrange $argl 1 end]
	    foreach le $vv {
		set var($le) [eval [list config_normalise_$subtype $le] $subargl]
	    }
	}
	default {
	    set var [eval [list config_normalise_$type $vv] $argl]
	}
    }
    set configvarsset($vn) 1
}

proc config_normalise_port {vv} {
    if {[regexp {^0*([0-9]+)$} $vv dummy vr]} {
	return $vr
    } elseif {[regexp {^[a-z][-0-9a-z]*$} $vv]} {
	return $vv
    } else {
	error "invalid port"
    }
}

proc config_normalise_boolean {vv} {
    if {[regexp -nocase {^f|^n|^0$|^off} $vv]} {
	return 0
    } elseif {[regexp -nocase {^t|^y|^1$|^on} $vv]} {
	return 1
    } else {
	error "invalid boolean"
    }
}

proc config_normalise_filemode {vv} {
    if {[regexp -nocase {^[0-7][0-7][0-7]$} $vv]} { return [format 0%o 0$vv] }
    error "invalid file mode"
}

set unitlist_interval(ms) 1
set unitlist_interval(s) 1000
set unitlist_interval(m) 60000
set unitlist_interval(h) 3600000
set unitlist_interval(d) 86400000

set unitlist_elapsed(s) 1
set unitlist_elapsed(m) 60
set unitlist_elapsed(h) 3600
set unitlist_elapsed(d) 86400
set unitlist_elapsed(wk) 604800
set unitlist_elapsed(mth) 2629800
set unitlist_elapsed(yr) 31557600

set unitlist_size(b) 1
set unitlist_size(kb) 1024
set unitlist_size(mb) 1048576

proc cfnu_bare {vv} {
    upvar ul ul
    if {![regexp {^([.0-9]+)([a-z]+)$} $vv dummy mag un]} {
	error "value $vv not in form <amount><units>"
    }
    if {![info exists ul($un)]} {
	error "unit $un unknown; allowed are [lsort [array names ul]]"
    }
    set mag [expr $mag]
    set uv "$ul($un).0"
    return [expr {$mag*$uv}]
}

proc config_normalise_file {vv} {
    return $vv
}

proc config_normalise_printable {vv} {
    regexp {^"(.*)"[ \t]*$} $vv all vv
    return $vv
}

proc config_normalise_localpart {vv} {
    if {[regexp -nocase {[^-._+=%$0-9a-z]} $vv forb]} {
	error "localpart $vv contains forbidden character $forb"
    }
    return $vv
}

proc config_normalise_domain {vv} {
    if {[regexp -nocase {[^-.0-9a-z]} $vv forb]} {
	error "domain $vv contains forbidden character $forb"
    }
    return [string tolower $vv]
}

proc config_normalise_nicelocalpart {vv} {
    if {[regexp {[^-+_.0-9a-z]} $vv forb]} {
	error "local part $vv contains forbidden character $forb"
    }
    return [string tolower $vv]
}

proc config_normalise_number {vv min max} {
    if {![regexp {^[.0-9]+$} $vv mag]} {
	error "value $vv is not a correctly-formatted number"
    }
    return [expr {$mag+0}]
}

proc config_normalise_units {vv dim min max} {
    upvar #0 unitlist_$dim ul
    set actv [cfnu_bare $vv]
    set minv [cfnu_bare $min]
    set maxv [cfnu_bare $max]
    if {$actv < $minv || $actv > $maxv} { error "value $vv not in range $min..$max" }
    return [expr {round($actv)}]
}

proc config_err {emsg} {
    global current_bigerr
    log fatal $emsg
    set current_bigerr "Configuration error"
}

proc config_procsetvar {vn vv} {
    global configvars
    if {[info exists configvars($vn)]} {
	set cfv $configvars($vn)
	set type [lindex $cfv 0]
	set argl [lindex $cfv 1]
	config_setvar $type $vn $vv $argl
    } else {
	error "unknown configuration directive $vn"
    }
}

proc config_args {argv} {
    global real_argv
    set ix 0
    set real_argv {}
    foreach ta $argv {
	if {![regexp -- {^-} $ta]} {
	    set real_argv [lrange $argv $ix end]
	    return
	}
	incr ix
	if {![regexp -- {^--([^=]+)=?(.*)$} $ta dummy vn vv]} {
	    config_err "unknown option format $ta"
	    continue
	}
	regsub -all -- - $vn _ vn
	if {[catch { config_procsetvar $vn $vv } emsg]} {
	    config_err "command line:$ta:$emsg"
	}
    }
}

proc config_read {cf} {
    if {[catch { set fh [open $cf r] } emsg]} {
	config_err "unable to read config file: $emsg"
	return
    }
    if {[catch {
	set ln 0
	while {[gets $fh l] >= 0} {
	    incr ln
	    if {[regexp {^#} $l] || ![regexp {[^ \t]} $l]} continue
	    if {![regexp {^[ \t]*([_a-zA-Z]+)[ \t]*(.*)$} $l dummy vn vv]} {
		config_err "$cf:$ln:could not find valid config variable name"
		continue
	    }
	    if {[catch { config_procsetvar $vn [string trimright $vv] } emsg]} {
		config_err "$cf:$ln:$emsg"
		continue
	    }
	}
    } emsg]} {
	config_err "$cf:reading:$emsg"
    }
    catch { close $fh }
}

proc config_checkmissing {} {
    global configvarsset config_file configvars
    foreach vn [array names configvars] {
	if {![info exists configvarsset($vn)]} {
	    config_err "$config_file:no setting for $vn"
	}
    }
}
