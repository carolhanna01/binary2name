# ----------------------------------------------------------------------------
#  progressbar.tcl
#  This file is part of Unifix BWidget Toolkit
# ----------------------------------------------------------------------------
#  Index of commands:
#     - ProgressBar::create
#     - ProgressBar::configure
#     - ProgressBar::cget
#     - ProgressBar::_destroy
#     - ProgressBar::_modify
# ----------------------------------------------------------------------------

# BWidget ToolKit
# Copyright (c) 1998-1999 UNIFIX. 
# Copyright (c) 2001-2002 ActiveState Corp. 

# The following terms apply to all files associated with the software
# unless explicitly disclaimed in individual files.

# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.

# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.

# GOVERNMENT USE: If you are acquiring this software on behalf of the
# U.S. government, the Government shall have only "Restricted Rights"
# in the software and related documentation as defined in the Federal 
# Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
# are acquiring the software on behalf of the Department of Defense, the
# software shall be classified as "Commercial Computer Software" and the
# Government shall have only "Restricted Rights" as defined in Clause
# 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
# authors grant the U.S. Government and others acting in its behalf
# permission to use and distribute the software in accordance with the
# terms specified in this license. 

namespace eval ProgressBar {
    Widget::declare ProgressBar {
        {-type        Enum       normal     0 {normal incremental infinite nonincremental_infinite}}
        {-maximum     Int        100        0 "%d >= 0"}
        {-background  TkResource ""         0 frame}
        {-foreground  TkResource blue       0 label}
        {-borderwidth TkResource 2          0 frame}
        {-troughcolor TkResource ""         0 scrollbar}
        {-relief      TkResource sunken     0 label}
        {-orient      Enum       horizontal 1 {horizontal vertical}}
        {-variable    String     ""         0}
        {-idle        Boolean    0          0}
        {-width       TkResource 100        0 frame}
        {-height      TkResource 4m         0 frame}
        {-bg          Synonym    -background}
        {-fg          Synonym    -foreground}
        {-bd          Synonym    -borderwidth}
    }

    Widget::addmap ProgressBar "" :cmd {-background {} -width {} -height {}}
    Widget::addmap ProgressBar "" .bar {
	-troughcolor -background -borderwidth {} -relief {}
    }

    variable _widget

    proc ::ProgressBar { path args } {
	return [eval ProgressBar::create $path $args]
    }
    proc use {} {}
}


# ----------------------------------------------------------------------------
#  Command ProgressBar::create
# ----------------------------------------------------------------------------
proc ProgressBar::create { path args } {
    variable _widget

    array set maps [list ProgressBar {} :cmd {} .bar {}]
    array set maps [Widget::parseArgs ProgressBar $args]
    eval frame $path $maps(:cmd) -class ProgressBar -bd 0 \
	    -highlightthickness 0 -relief flat
    Widget::initFromODB ProgressBar $path $maps(ProgressBar)

    set c  [eval canvas $path.bar $maps(.bar) -highlightthickness 0]
    set fg [Widget::cget $path -foreground]
    if { ![string compare [Widget::cget $path -orient] "horizontal"] } {
        $path.bar create rectangle -1 0 0 0 -fill $fg -outline $fg -tags rect
    } else {
        $path.bar create rectangle 0 1 0 0 -fill $fg -outline $fg -tags rect
    }

    set _widget($path,val) 0
    set _widget($path,dir) 1
    set _widget($path,var) [Widget::cget $path -variable]
    if {$_widget($path,var) != ""} {
        GlobalVar::tracevar variable $_widget($path,var) w \
		[list ProgressBar::_modify $path]
        set _widget($path,afterid) \
	    [after idle [list ProgressBar::_modify $path]]
    }

    bind $path.bar <Destroy>   [list ProgressBar::_destroy $path]
    bind $path.bar <Configure> [list ProgressBar::_modify $path]

    rename $path ::$path:cmd
    proc ::$path { cmd args } \
	    "return \[eval ProgressBar::\$cmd [list $path] \$args\]"

    return $path
}


# ----------------------------------------------------------------------------
#  Command ProgressBar::configure
# ----------------------------------------------------------------------------
proc ProgressBar::configure { path args } {
    variable _widget

    set res [Widget::configure $path $args]

    if { [Widget::hasChangedX $path -variable] } {
	set newv [Widget::cget $path -variable]
        if { $_widget($path,var) != "" } {
            GlobalVar::tracevar vdelete $_widget($path,var) w \
		    [list ProgressBar::_modify $path]
        }
        if { $newv != "" } {
            set _widget($path,var) $newv
            GlobalVar::tracevar variable $newv w \
		    [list ProgressBar::_modify $path]
	    if {![info exists _widget($path,afterid)]} {
		set _widget($path,afterid) \
		    [after idle [list ProgressBar::_modify $path]]
	    }
        } else {
            set _widget($path,var) ""
        }
    }

    foreach {cbd cor cma} [Widget::hasChangedX $path -borderwidth \
	    -orient -maximum] break

    if { $cbd || $cor || $cma } {
	if {![info exists _widget($path,afterid)]} {
	    set _widget($path,afterid) \
		[after idle [list ProgressBar::_modify $path]]
	}
    }
    if { [Widget::hasChangedX $path -foreground] } {
	set fg [Widget::cget $path -foreground]
        $path.bar itemconfigure rect -fill $fg -outline $fg
    }
    return $res
}


# ----------------------------------------------------------------------------
#  Command ProgressBar::cget
# ----------------------------------------------------------------------------
proc ProgressBar::cget { path option } {
    return [Widget::cget $path $option]
}


# ----------------------------------------------------------------------------
#  Command ProgressBar::_destroy
# ----------------------------------------------------------------------------
proc ProgressBar::_destroy { path } {
    variable _widget

    if {[info exists _widget($path,afterid)]} {
	after cancel $_widget($path,afterid)
	unset _widget($path,afterid)
    }
    if {[info exists _widget($path,var)]} {
	if {$_widget($path,var) != ""} {
	    GlobalVar::tracevar vdelete $_widget($path,var) w \
		[list ProgressBar::_modify $path]
	}
	unset _widget($path,var)
    }
    unset _widget($path,dir)
    Widget::destroy $path
    rename $path {}
}


# ----------------------------------------------------------------------------
#  Command ProgressBar::_modify
# ----------------------------------------------------------------------------
proc ProgressBar::_modify { path args } {
    variable _widget

    catch {unset _widget($path,afterid)}
    if { ![GlobalVar::exists $_widget($path,var)] ||
	 [set val [GlobalVar::getvar $_widget($path,var)]] < 0 } {
        catch {place forget $path.bar}
    } else {
	place $path.bar -relx 0 -rely 0 -relwidth 1 -relheight 1
	set type [Widget::getoption $path -type]
	if { $val != 0 && $type != "normal" && \
		$type != "nonincremental_infinite"} {
	    set val [expr {$val+$_widget($path,val)}]
	}
	set _widget($path,val) $val
	set max [Widget::getoption $path -maximum]
	set bd  [expr {2*[$path.bar cget -bd]}]
	set w   [winfo width  $path.bar]
	set h   [winfo height $path.bar]
	if {$type == "infinite" || $type == "nonincremental_infinite"} {
	    # JDC: New infinite behaviour
	    set tval [expr {$val % $max}]
	    if { $tval < ($max / 2.0) } {
		set x0 [expr {double($tval) / double($max) * 1.5}]
	    } else {
		set x0 [expr {(1.0-(double($tval) / double($max))) * 1.5}]
	    }
	    set x1 [expr {$x0 + 0.25}]
	    if {[Widget::getoption $path -orient] == "horizontal"} {
		$path.bar coords rect [expr {$x0*$w}] 0 [expr {$x1*$w}] $h
	    } else {
		$path.bar coords rect 0 [expr {$h-$x0*$h}] $w \
			[expr {$x1*$h}]
	    }
	} else {
	    if { $val > $max } {set val $max}
	    if {[Widget::getoption $path -orient] == "horizontal"} {
		$path.bar coords rect -1 0 [expr {$val*$w/$max}] $h
	    } else {
		$path.bar coords rect 0 [expr {$h+1}] $w \
			[expr {$h*(1.0 - double($val)/$max)}]
	    }
	}
    }
    if {![Widget::cget $path -idle]} {
	update idletasks
    }
}
