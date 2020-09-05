# ----------------------------------------------------------------------------
#  font.tcl
#  This file is part of Unifix BWidget Toolkit
# ----------------------------------------------------------------------------
#  Index of commands:
#     - SelectFont::create
#     - SelectFont::configure
#     - SelectFont::cget
#     - SelectFont::_draw
#     - SelectFont::_destroy
#     - SelectFont::_modstyle
#     - SelectFont::_update
#     - SelectFont::_getfont
#     - SelectFont::_init
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


namespace eval SelectFont {
    Dialog::use
    LabelFrame::use
    ScrolledWindow::use

    Widget::declare SelectFont {
        {-title		String		"Font selection" 0}
        {-parent	String		"" 0}
        {-background	TkResource	"" 0 frame}

        {-type		Enum		dialog        0 {dialog toolbar}}
        {-font		TkResource	""            0 label}
	{-families	String		"all"         1}
	{-querysystem	Boolean		1             0}
	{-styles	String		"bold italic underline overstrike" 1}
        {-command	String		""            0}
        {-sampletext	String		"Sample Text" 0}
        {-bg		Synonym		-background}
    }

    proc ::SelectFont { path args } { 
	return [eval SelectFont::create $path $args] 
    }
    proc use {} {}

    variable _families
    variable _styleOff
    array set _styleOff [list bold normal italic roman]
    variable _sizes     {4 5 6 7 8 9 10 11 12 13 14 15 16 \
	    17 18 19 20 21 22 23 24}
    
    # Set up preset lists of fonts, so the user can avoid the painfully slow
    # loadfont process if desired.
    if { [string equal $::tcl_platform(platform) "windows"] } {
	set presetVariable [list	\
		7x14			\
		Arial			\
		{Arial Narrow}		\
		{Lucida Sans}		\
		{MS Sans Serif}		\
		{MS Serif}		\
		{Times New Roman}	\
		]
	set presetFixed    [list	\
		6x13			\
		{Courier New}		\
		FixedSys		\
		Terminal		\
		]
	set presetAll      [list	\
		6x13			\
		7x14			\
		Arial			\
		{Arial Narrow}		\
		{Courier New}		\
		FixedSys		\
		{Lucida Sans}		\
		{MS Sans Serif}		\
		{MS Serif}		\
		Terminal		\
		{Times New Roman}	\
		]
    } else {
	set presetVariable [list	\
		helvetica		\
		lucida			\
		lucidabright		\
		{times new roman}	\
		]
	set presetFixed    [list	\
		courier			\
		fixed			\
		{lucida typewriter}	\
		screen			\
		serif			\
		terminal		\
		]
	set presetAll      [list	\
		courier			\
		fixed			\
		helvetica		\
		lucida			\
		lucidabright		\
		{lucida typewriter}	\
		screen			\
		serif			\
		terminal		\
		{times new roman}	\
		]
    }
    array set _families [list \
	    presetvariable	$presetVariable	\
	    presetfixed		$presetFixed	\
	    presetall		$presetAll	\
	    ]
		
    variable _widget
}


# ----------------------------------------------------------------------------
#  Command SelectFont::create
# ----------------------------------------------------------------------------
proc SelectFont::create { path args } {
    variable _families
    variable _sizes
    variable $path
    upvar 0  $path data

    # Initialize the internal rep of the widget options
    Widget::init SelectFont "$path#SelectFont" $args

    if { [Widget::getoption "$path#SelectFont" -querysystem] } {
        loadfont [Widget::getoption "$path#SelectFont" -families]
    }

    set bg [Widget::getoption "$path#SelectFont" -background]
    set _styles [Widget::getoption "$path#SelectFont" -styles]
    if { [Widget::getoption "$path#SelectFont" -type] == "dialog" } {
        Dialog::create $path -modal local -default 0 -cancel 1 -background $bg \
            -title  [Widget::getoption "$path#SelectFont" -title] \
            -parent [Widget::getoption "$path#SelectFont" -parent]

        set frame [Dialog::getframe $path]
        set topf  [frame $frame.topf -relief flat -borderwidth 0 -background $bg]

        set labf1 [LabelFrame::create $topf.labf1 -text "Font" -name font \
                       -side top -anchor w -relief flat -background $bg]
        set sw    [ScrolledWindow::create [LabelFrame::getframe $labf1].sw \
                       -background $bg]
        set lbf   [listbox $sw.lb \
                       -height 5 -width 25 -exportselection false -selectmode browse]
        ScrolledWindow::setwidget $sw $lbf
        LabelFrame::configure $labf1 -focus $lbf
	if { [Widget::getoption "$path#SelectFont" -querysystem] } {
	    set fam [Widget::getoption "$path#SelectFont" -families]
	} else {
	    set fam "preset"
	    append fam [Widget::getoption "$path#SelectFont" -families]
	}
        eval [list $lbf insert end] $_families($fam)
        set script "set SelectFont::$path\(family\) \[%W curselection\]; SelectFont::_update $path"
        bind $lbf <ButtonRelease-1> $script
        bind $lbf <space>           $script
        pack $sw -fill both -expand yes

        set labf2 [LabelFrame::create $topf.labf2 -text "Size" -name size \
                       -side top -anchor w -relief flat -background $bg]
        set sw    [ScrolledWindow::create [LabelFrame::getframe $labf2].sw \
                       -scrollbar vertical -background $bg]
        set lbs   [listbox $sw.lb \
                       -height 5 -width 6 -exportselection false -selectmode browse]
        ScrolledWindow::setwidget $sw $lbs
        LabelFrame::configure $labf2 -focus $lbs
        eval [list $lbs insert end] $_sizes
        set script "set SelectFont::$path\(size\) \[%W curselection\]; SelectFont::_update $path"
        bind $lbs <ButtonRelease-1> $script
        bind $lbs <space>           $script
        pack $sw -fill both -expand yes

        set labf3 [LabelFrame::create $topf.labf3 -text "Style" -name style \
                       -side top -anchor w -relief sunken -bd 1 -background $bg]
        set subf  [LabelFrame::getframe $labf3]
        foreach st $_styles {
            set name [lindex [BWidget::getname $st] 0]
            if { $name == "" } {
                set name "[string toupper [string index $name 0]][string range $name 1 end]"
            }
            checkbutton $subf.$st -text $name \
                -variable   SelectFont::$path\($st\) \
                -background $bg \
                -command    "SelectFont::_update $path"
            bind $subf.$st <Return> break
            pack $subf.$st -anchor w
        }
        LabelFrame::configure $labf3 -focus $subf.[lindex $_styles 0]

        pack $labf1 -side left -anchor n -fill both -expand yes
        pack $labf2 -side left -anchor n -fill both -expand yes -padx 8
        pack $labf3 -side left -anchor n -fill both -expand yes

        set botf [frame $frame.botf -width 100 -height 50 \
                      -bg white -bd 0 -relief flat \
                      -highlightthickness 1 -takefocus 0 \
                      -highlightbackground black \
                      -highlightcolor black]

        set lab  [label $botf.label \
                      -background white -foreground black \
                      -borderwidth 0 -takefocus 0 -highlightthickness 0 \
                      -text [Widget::getoption "$path#SelectFont" -sampletext]]
        place $lab -relx 0.5 -rely 0.5 -anchor c

        pack $topf -pady 4 -fill both -expand yes
        pack $botf -pady 4 -fill x

        Dialog::add $path -name ok
        Dialog::add $path -name cancel

        set data(label) $lab
        set data(lbf)   $lbf
        set data(lbs)   $lbs

        _getfont $path

        proc ::$path { cmd args } "return \[eval SelectFont::\$cmd $path \$args\]"

        return [_draw $path]
    } else {
	if { [Widget::getoption "$path#SelectFont" -querysystem] } {
	    set fams [Widget::getoption "$path#SelectFont" -families]
	} else {
	    set fams "preset"
	    append fams [Widget::getoption "$path#SelectFont" -families]
	}
        frame $path -relief flat -borderwidth 0 -background $bg
        bind $path <Destroy> "SelectFont::_destroy $path"
        set lbf [ComboBox::create $path.font \
                     -highlightthickness 0 -takefocus 0 -background $bg \
                     -values   $_families($fams) \
                     -textvariable SelectFont::$path\(family\) \
                     -editable 0 \
                     -modifycmd "SelectFont::_update $path"]
        set lbs [ComboBox::create $path.size \
                     -highlightthickness 0 -takefocus 0 -background $bg \
                     -width    4 \
                     -values   $_sizes \
                     -textvariable SelectFont::$path\(size\) \
                     -editable 0 \
                     -modifycmd "SelectFont::_update $path"]
        pack $lbf -side left -anchor w
        pack $lbs -side left -anchor w -padx 4
        foreach st $_styles {
            button $path.$st \
                -highlightthickness 0 -takefocus 0 -padx 0 -pady 0 -bd 2 \
                -background $bg \
                -image  [Bitmap::get $st] \
                -command "SelectFont::_modstyle $path $st"
            pack $path.$st -side left -anchor w
        }
        set data(label) ""
        set data(lbf)   $lbf
        set data(lbs)   $lbs
        _getfont $path

        rename $path ::$path:cmd
        proc ::$path { cmd args } "return \[eval SelectFont::\$cmd $path \$args\]"
    }

    return $path
}


# ----------------------------------------------------------------------------
#  Command SelectFont::configure
# ----------------------------------------------------------------------------
proc SelectFont::configure { path args } {
    set _styles [Widget::getoption "$path#SelectFont" -styles]

    set res [Widget::configure "$path#SelectFont" $args]

    if { [Widget::hasChanged "$path#SelectFont" -font font] } {
        _getfont $path
    }
    if { [Widget::hasChanged "$path#SelectFont" -background bg] } {
        switch -- [Widget::getoption "$path#SelectFont" -type] {
            dialog {
                Dialog::configure $path -background $bg
                set topf [Dialog::getframe $path].topf
                $topf configure -background $bg
                foreach labf {labf1 labf2} {
                    LabelFrame::configure $topf.$labf -background $bg
                    set subf [LabelFrame::getframe $topf.$labf]
                    ScrolledWindow::configure $subf.sw -background $bg
                    $subf.sw.lb configure -background $bg
                }
                LabelFrame::configure $topf.labf3 -background $bg
                set subf [LabelFrame::getframe $topf.labf3]
                foreach w [winfo children $subf] {
                    $w configure -background $bg
                }
            }
            toolbar {
                $path configure -background $bg
                ComboBox::configure $path.font -background $bg
                ComboBox::configure $path.size -background $bg
                foreach st $_styles {
                    $path.$st configure -background $bg
                }
            }
        }
    }
    return $res
}


# ----------------------------------------------------------------------------
#  Command SelectFont::cget
# ----------------------------------------------------------------------------
proc SelectFont::cget { path option } {
    return [Widget::cget "$path#SelectFont" $option]
}


# ----------------------------------------------------------------------------
#  Command SelectFont::loadfont
# ----------------------------------------------------------------------------
proc SelectFont::loadfont {{which all}} {
    variable _families

    # initialize families
    if {![info exists _families(all)]} {
	set _families(all) [lsort -dictionary [font families]]
    }
    if {[regexp {fixed|variable} $which] \
	    && ![info exists _families($which)]} {
	# initialize families
	set _families(fixed) {}
	set _families(variable) {}
	foreach family $_families(all) {
	    if { [font metrics [list $family] -fixed] } {
		lappend _families(fixed) $family
	    } else {
		lappend _families(variable) $family
	    }
	}
    }
    return
}


# ----------------------------------------------------------------------------
#  Command SelectFont::_draw
# ----------------------------------------------------------------------------
proc SelectFont::_draw { path } {
    variable $path
    upvar 0  $path data

    $data(lbf) selection clear 0 end
    $data(lbf) selection set $data(family)
    $data(lbf) activate $data(family)
    $data(lbf) see $data(family)
    $data(lbs) selection clear 0 end
    $data(lbs) selection set $data(size)
    $data(lbs) activate $data(size)
    $data(lbs) see $data(size)
    _update $path

    if { [Dialog::draw $path] == 0 } {
        set result [Widget::getoption "$path#SelectFont" -font]
    } else {
        set result ""
    }
    unset data
    Widget::destroy "$path#SelectFont"
    destroy $path
    return $result
}


# ----------------------------------------------------------------------------
#  Command SelectFont::_destroy
# ----------------------------------------------------------------------------
proc SelectFont::_destroy { path } {
    variable $path
    upvar 0  $path data

    unset data
    Widget::destroy "$path#SelectFont"
    rename $path {}
}


# ----------------------------------------------------------------------------
#  Command SelectFont::_modstyle
# ----------------------------------------------------------------------------
proc SelectFont::_modstyle { path style } {
    variable $path
    upvar 0  $path data

    if { $data($style) == 1 } {
        $path.$style configure -relief raised
        set data($style) 0
    } else {
        $path.$style configure -relief sunken
        set data($style) 1
    }
    _update $path
}


# ----------------------------------------------------------------------------
#  Command SelectFont::_update
# ----------------------------------------------------------------------------
proc SelectFont::_update { path } {
    variable _families
    variable _sizes
    variable _styleOff
    variable $path
    upvar 0  $path data

    set type [Widget::getoption "$path#SelectFont" -type]
    set _styles [Widget::getoption "$path#SelectFont" -styles]
    if { [Widget::getoption "$path#SelectFont" -querysystem] } {
	set fams [Widget::getoption "$path#SelectFont" -families]
    } else {
	set fams "preset"
	append fams [Widget::getoption "$path#SelectFont" -families]
    }
    if { $type == "dialog" } {
        set curs [$path:cmd cget -cursor]
        $path:cmd configure -cursor watch
    }
    if { [Widget::getoption "$path#SelectFont" -type] == "dialog" } {
        set font [list [lindex $_families($fams) $data(family)] \
		[lindex $_sizes $data(size)]]
    } else {
        set font [list $data(family) $data(size)]
    }
    foreach st $_styles {
        if { $data($st) } {
            lappend font $st
        } elseif {[info exists _styleOff($st)]} {
	    # This adds the default bold/italic value to a font
	    #lappend font $_styleOff($st)
	}
    }
    Widget::setoption "$path#SelectFont" -font $font
    if { $type == "dialog" } {
        $data(label) configure -font $font
        $path:cmd configure -cursor $curs
    } elseif { [set cmd [Widget::getoption "$path#SelectFont" -command]] != "" } {
        uplevel \#0 $cmd
    }
}


# ----------------------------------------------------------------------------
#  Command SelectFont::_getfont
# ----------------------------------------------------------------------------
proc SelectFont::_getfont { path } {
    variable _families
    variable _sizes
    variable $path
    upvar 0  $path data

    array set font [font actual [Widget::getoption "$path#SelectFont" -font]]
    set data(bold)       [expr {![string equal $font(-weight) "normal"]}]
    set data(italic)     [expr {![string equal $font(-slant)  "roman"]}]
    set data(underline)  $font(-underline)
    set data(overstrike) $font(-overstrike)
    set _styles [Widget::getoption "$path#SelectFont" -styles]
    if { [Widget::getoption "$path#SelectFont" -querysystem] } {
	set fams [Widget::getoption "$path#SelectFont" -families]
    } else {
	set fams "preset"
	append fams [Widget::getoption "$path#SelectFont" -families]
    }
    if { [Widget::getoption "$path#SelectFont" -type] == "dialog" } {
        set idxf [lsearch $_families($fams) $font(-family)]
        set idxs [lsearch $_sizes    $font(-size)]
        set data(family) [expr {$idxf >= 0 ? $idxf : 0}]
        set data(size)   [expr {$idxs >= 0 ? $idxs : 0}]
    } else {
        set data(family) $font(-family)
        set data(size)   $font(-size)
        foreach st $_styles {
            $path.$st configure -relief [expr {$data($st) ? "sunken":"raised"}]
        }
    }
}

