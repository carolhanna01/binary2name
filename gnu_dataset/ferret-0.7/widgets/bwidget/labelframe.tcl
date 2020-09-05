# ------------------------------------------------------------------------------
#  labelframe.tcl
#  This file is part of Unifix BWidget Toolkit
#  $Id: labelframe.tcl,v 1.1.1.1 2003/10/09 09:31:55 jemarch Exp $
# ------------------------------------------------------------------------------
#  Index of commands:
#     - LabelFrame::create
#     - LabelFrame::getframe
#     - LabelFrame::configure
#     - LabelFrame::cget
#     - LabelFrame::align
# ------------------------------------------------------------------------------

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

namespace eval LabelFrame {
    BWLabel::use

    Widget::bwinclude LabelFrame BWLabel .l \
        remove     {
            -highlightthickness -highlightcolor -highlightbackground
            -takefocus -relief -borderwidth
            -cursor
            -dragenabled -draginitcmd -dragendcmd -dragevent -dragtype
            -dropenabled -droptypes -dropovercmd  -dropcmd} \
        initialize {-anchor w}

    Widget::declare LabelFrame {
        {-relief      TkResource flat 0 frame}
        {-borderwidth TkResource 0    0 frame}
        {-side        Enum       left 1 {left right top bottom}}
        {-bd          Synonym    -borderwidth}
    }

    Widget::addmap LabelFrame "" :cmd {-background {}}
    Widget::addmap LabelFrame "" .f   {-background {} -relief {} -borderwidth {}}

    Widget::syncoptions LabelFrame Label .l {-text {} -underline {}}

    bind BwLabelFrame <FocusIn> {BWLabel::setfocus %W.l}
    bind BwLabelFrame <Destroy> {Widget::destroy %W; rename %W {}}

    proc ::LabelFrame { path args } { return [eval LabelFrame::create $path $args] }
    proc use {} {}
}


# ------------------------------------------------------------------------------
#  Command LabelFrame::create
# ------------------------------------------------------------------------------
proc LabelFrame::create { path args } {
    Widget::init LabelFrame $path $args

    set path  [eval frame $path [Widget::subcget $path :cmd] \
	    -relief flat -bd 0 -takefocus 0 -highlightthickness 0 \
	    -class LabelFrame]

    set label [eval BWLabel::create $path.l [Widget::subcget $path .l] \
                   -takefocus 0 -highlightthickness 0 -relief flat -borderwidth 0 \
                   -dropenabled 0 -dragenabled 0]
    set frame [eval frame $path.f [Widget::subcget $path .f] \
                   -highlightthickness 0 -takefocus 0]

    switch  [Widget::getoption $path -side] {
        left   {set packopt "-side left"}
        right  {set packopt "-side right"}
        top    {set packopt "-side top -fill x"}
        bottom {set packopt "-side bottom -fill x"}
    }

    eval pack $label $packopt
    pack $frame -fill both -expand yes

    bindtags $path [list $path BwLabelFrame [winfo toplevel $path] all]

    rename $path ::$path:cmd
    proc ::$path { cmd args } "return \[eval LabelFrame::\$cmd $path \$args\]"

    return $path
}


# ------------------------------------------------------------------------------
#  Command LabelFrame::getframe
# ------------------------------------------------------------------------------
proc LabelFrame::getframe { path } {
    return $path.f
}


# ------------------------------------------------------------------------------
#  Command LabelFrame::configure
# ------------------------------------------------------------------------------
proc LabelFrame::configure { path args } {
    return [Widget::configure $path $args]
}


# ------------------------------------------------------------------------------
#  Command LabelFrame::cget
# ------------------------------------------------------------------------------
proc LabelFrame::cget { path option } {
    return [Widget::cget $path $option]
}


# ------------------------------------------------------------------------------
#  Command LabelFrame::align
#  This command align label of all widget given by args of class LabelFrame
#  (or "derived") by setting their width to the max one +1
# ------------------------------------------------------------------------------
proc LabelFrame::align { args } {
    set maxlen 0
    set wlist  {}
    foreach wl $args {
        foreach w $wl {
            if { ![info exists Widget::_class($w)] } {
                continue
            }
            set class $Widget::_class($w)
            if { ![string compare $class "LabelFrame"] } {
                set textopt  -text
                set widthopt -width
            } else {
                upvar 0 Widget::${class}::map classmap
                set textopt  ""
                set widthopt ""
                set notdone  2
                foreach {option lmap} [array get classmap] {
                    foreach {subpath subclass realopt} $lmap {
                        if { ![string compare $subclass "LabelFrame"] } {
                            if { ![string compare $realopt "-text"] } {
                                set textopt $option
                                incr notdone -1
                                break
                            }
                            if { ![string compare $realopt "-width"] } {
                                set widthopt $option
                                incr notdone -1
                                break
                            }
                        }
                    }
                    if { !$notdone } {
                        break
                    }
                }
                if { $notdone } {
                    continue
                }
            }
            set len [string length [$w cget $textopt]]
            if { $len > $maxlen } {
                set maxlen $len
            }
            lappend wlist $w $widthopt
        }
    }
    incr maxlen
    foreach {w widthopt} $wlist {
        $w configure $widthopt $maxlen
    }
}
