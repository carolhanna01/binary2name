########### subst.tcl
# Run with two args: settingsfile, filename.ext
# Then copies input file <filename>.<ext> to output file <filename>,
# making substitutions as specified in <settingsfile>
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
# $Id: subst.tcl,v 1.7 2001/03/21 23:53:15 ian Exp $

source instlib.tcl

if {[llength $argv] != 2} { error "need exactly two args" }
source [lindex $argv 0]
set infilename [lindex $argv 1]
regexp {^(.+)\.([^.]+)$} $infilename dummy filename ext

switch -exact $ext {
    tcl {
	set hashpling 1
	set removeset 1
    }
    in {
	set hashpling 0
	set removeset 0
    }
    default {
	error $ext
    }
}

set in [open $infilename r]

set mode 0666
if {[file executable $infilename]} { set mode 0777 }

set out [open $filename.new w $mode]
set line1 1

while {[gets $in line] >= 0} {
    if {$line1 && $hashpling && [regexp {^\#\!.*tclsh} $line]} {
	set line "#! $_tclsh"
    } elseif {$removeset && [regexp {^set[ \t]+\@\@\@[a-z_]+\??\@\@\@[ \t]} $line]} {
	continue
    } else {
	while {[regexp {^(.*)\$\{\@\@\@([a-z_]+)(\??)\@\@\@\}(.*)$} \
		$line all lhs vn allow0 rhs]} {
	    set v [set $vn$allow0]
	    set v [string_quote $v [string length $allow0]]
	    set line $lhs$v$rhs
	}
	regsub -all {\$\{\@\@\@\}} $line @@@ line
    }
    puts $out $line
    set line1 0
}

close $in
close $out
file rename -force $filename.new $filename
