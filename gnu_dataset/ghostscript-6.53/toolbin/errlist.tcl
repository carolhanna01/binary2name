#!/bin/sh
# hack to restart using tclsh \
exec tclsh "$0" "$@"

#  Copyright (C) 2000 artofcode LLC.  All rights reserved.
#
#  This file is part of GNU Ghostscript.
# 
#  GNU Ghostscript is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
#  to anyone for the consequences of using it or for whether it serves any
#  particular purpose or works at all, unless he says so in writing.  Refer
#  to the GNU General Public License for full details.
# 
#  Everyone is granted permission to copy, modify and redistribute GNU
#  Ghostscript, but only under the conditions described in the GNU General
#  Public License.  A copy of this license is supposed to have been given
#  to you along with GNU Ghostscript so you can know your rights and
#  responsibilities.  It should be in a file named COPYING.  Among other
#  things, the copyright notice and this notice must be preserved on all
#  copies.

# $Id: errlist.tcl,v 1.3.2.1 2001/11/02 23:05:44 giles Exp $

# Usage:
#	errlist < compiler-output-log > interleaved-listing

set inname ""
while {[gets stdin line] >= 0} {
    if {![regexp {^([./0-9a-zA-Z_]+):([0-9]+):} $line skip fname lno]} {continue}
    if {$fname != $inname} {
	if {$inname != ""} {close $infile}
	set infile [open $fname]
	set inname $fname
	set inlno 1
    }
    puts $line
    while {$inlno < $lno} {
	gets $infile
	incr inlno
    }
    while {$inlno <= $lno} {
	if {[gets $infile inline] >= 0} {
	    puts $inline
	}
	incr inlno
    }
    puts ""
}
if {$inname != ""} {
    close $infile
}
