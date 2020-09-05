# ------------------------------------------------------------------------------
#  titleframe.tcl
#  This file is part of Unifix BWidget Toolkit
# ------------------------------------------------------------------------------
#  Index of commands:
#     - TitleFrame::create
#     - TitleFrame::configure
#     - TitleFrame::cget
#     - TitleFrame::getframe
#     - TitleFrame::_place
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


namespace eval TitleFrame {
    Widget::declare TitleFrame {
        {-relief      TkResource groove 0 frame}
        {-borderwidth TkResource 2      0 frame}
        {-font        TkResource ""     0 label}
        {-foreground  TkResource ""     0 label}
        {-state       TkResource ""     0 label}
        {-background  TkResource ""     0 frame}
        {-text        String     ""     0}
        {-ipad        Int        4      0 "%d >=0"}
        {-side        Enum       left   0 {left center right}}
        {-baseline    Enum       center 0 {top center bottom}}
        {-fg          Synonym    -foreground}
        {-bg          Synonym    -background}
        {-bd          Synonym    -borderwidth}
    }

    Widget::addmap TitleFrame "" :cmd {-background {}}
    Widget::addmap TitleFrame "" .l   {-background {} -foreground {} -text {} -font {}}
    Widget::addmap TitleFrame "" .l   {-state {}}
    Widget::addmap TitleFrame "" .p   {-background {}}
    Widget::addmap TitleFrame "" .b   {-background {} -relief {} -borderwidth {}}
    Widget::addmap TitleFrame "" .b.p {-background {}}
    Widget::addmap TitleFrame "" .f   {-background {}}

    proc ::TitleFrame { path args } { return [eval TitleFrame::create $path $args] }
    proc use {} {}
}


# ------------------------------------------------------------------------------
#  Command TitleFrame::create
# ------------------------------------------------------------------------------
proc TitleFrame::create { path args } {
    Widget::init TitleFrame $path $args

    set frame  [eval frame $path [Widget::subcget $path :cmd] \
	    -class TitleFrame -relief flat -bd 0 -highlightthickness 0]

    set padtop [eval frame $path.p [Widget::subcget $path :cmd] \
	    -relief flat -borderwidth 0]
    set border [eval frame $path.b [Widget::subcget $path .b] -highlightthickness 0]
    set label  [eval label $path.l [Widget::subcget $path .l] \
                    -highlightthickness 0 \
                    -relief flat \
                    -bd     0 -padx 2 -pady 0]
    set padbot [eval frame $border.p [Widget::subcget $path .p] \
	    -relief flat -bd 0 -highlightthickness 0]
    set frame  [eval frame $path.f [Widget::subcget $path .f] \
	    -relief flat -bd 0 -highlightthickness 0]
    set height [winfo reqheight $label]

    switch [Widget::getoption $path -side] {
        left   { set relx 0.0; set x 5;  set anchor nw }
        center { set relx 0.5; set x 0;  set anchor n  }
        right  { set relx 1.0; set x -5; set anchor ne }
    }
    set bd [Widget::getoption $path -borderwidth]
    switch [Widget::getoption $path -baseline] {
        top    { set htop $height; set hbot 1; set y 0 }
        center { set htop [expr {$height/2}]; set hbot [expr {$height/2+$height%2+1}]; set y 0 }
        bottom { set htop 1; set hbot $height; set y [expr {$bd+1}] }
    }
    $padtop configure -height $htop
    $padbot configure -height $hbot

    set pad [Widget::getoption $path -ipad]
    pack $padbot -side top -fill x
    pack $frame  -in $border -fill both -expand yes -padx $pad -pady $pad

    pack $padtop -side top -fill x
    pack $border -fill both -expand yes

    place $label -relx $relx -x $x -anchor $anchor -y $y

    bind $label <Configure> "TitleFrame::_place $path"
    bind $path  <Destroy>   {Widget::destroy %W; rename %W {}}

    rename $path ::$path:cmd
    proc ::$path { cmd args } "return \[eval TitleFrame::\$cmd $path \$args\]"
    
    return $path
}


# ------------------------------------------------------------------------------
#  Command TitleFrame::configure
# ------------------------------------------------------------------------------
proc TitleFrame::configure { path args } {
    set res [Widget::configure $path $args]

    if { [Widget::hasChanged $path -ipad pad] } {
        pack configure $path.f -padx $pad -pady $pad
    }
    if { [Widget::hasChanged $path -borderwidth val] |
         [Widget::hasChanged $path -font        val] |
         [Widget::hasChanged $path -side        val] |
         [Widget::hasChanged $path -baseline    val] } {
        _place $path
    }

    return $res
}


# ------------------------------------------------------------------------------
#  Command TitleFrame::cget
# ------------------------------------------------------------------------------
proc TitleFrame::cget { path option } {
    return [Widget::cget $path $option]
}


# ------------------------------------------------------------------------------
#  Command TitleFrame::getframe
# ------------------------------------------------------------------------------
proc TitleFrame::getframe { path } {
    return $path.f
}


# ------------------------------------------------------------------------------
#  Command TitleFrame::_place
# ------------------------------------------------------------------------------
proc TitleFrame::_place { path } {
    set height [winfo height $path.l]
    switch [Widget::getoption $path -side] {
        left    { set relx 0.0; set x 10;  set anchor nw }
        center  { set relx 0.5; set x 0;   set anchor n  }
        right   { set relx 1.0; set x -10; set anchor ne }
    }
    set bd [Widget::getoption $path -borderwidth]
    switch [Widget::getoption $path -baseline] {
        top    { set htop $height; set hbot 1; set y 0 }
        center { set htop [expr {$height/2}]; set hbot [expr {$height/2+$height%2+1}]; set y 0 }
        bottom { set htop 1; set hbot $height; set y [expr {$bd+1}] }
    }
    $path.p   configure -height $htop
    $path.b.p configure -height $hbot

    place $path.l -relx $relx -x $x -anchor $anchor -y $y
}




