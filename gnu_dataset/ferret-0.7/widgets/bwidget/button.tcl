# ------------------------------------------------------------------------------
#  button.tcl
#  This file is part of Unifix BWidget Toolkit
# ------------------------------------------------------------------------------
#  Index of commands:
#   Public commands
#     - Button::create
#     - Button::configure
#     - Button::cget
#     - Button::invoke
#   Private commands (event bindings)
#     - Button::_destroy
#     - Button::_enter
#     - Button::_leave
#     - Button::_press
#     - Button::_release
#     - Button::_repeat
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

namespace eval Button {
    set remove [list -command -relief -text -textvariable -underline -state]
    if {[info tclversion] > 8.3} {
	lappend remove -repeatdelay -repeatinterval
    }
    Widget::tkinclude Button button :cmd remove $remove

    Widget::declare Button {
        {-name            String "" 0}
        {-text            String "" 0}
        {-textvariable    String "" 0}
        {-underline       Int    -1 0 "%d >= -1"}
        {-armcommand      String "" 0}
        {-disarmcommand   String "" 0}
        {-command         String "" 0}
        {-state           TkResource "" 0 button}
        {-repeatdelay     Int    0  0 "%d >= 0"}
        {-repeatinterval  Int    0  0 "%d >= 0"}
        {-relief          Enum   raised  0 {raised sunken flat ridge solid groove link}}
    }

    DynamicHelp::include Button balloon

    Widget::syncoptions Button "" :cmd {-text {} -underline {}}

    variable _current ""
    variable _pressed ""

    bind BwButton <Enter>           {Button::_enter %W}
    bind BwButton <Leave>           {Button::_leave %W}
    bind BwButton <ButtonPress-1>   {Button::_press %W}
    bind BwButton <ButtonRelease-1> {Button::_release %W}
    bind BwButton <Key-space>       {Button::invoke %W; break}
    bind BwButton <Return>          {Button::invoke %W; break}
    bind BwButton <Destroy>         {Widget::destroy %W; rename %W {}}

    proc ::Button { path args } { return [eval Button::create $path $args] }
    proc use {} {}
}


# ------------------------------------------------------------------------------
#  Command Button::create
# ------------------------------------------------------------------------------
proc Button::create { path args } {
    array set maps [list Button {} :cmd {}]
    array set maps [Widget::parseArgs Button $args]
    eval [concat [list button $path] $maps(:cmd)]
    Widget::initFromODB Button $path $maps(Button)

    # Do some extra configuration on the button
    set relief [Widget::getMegawidgetOption $path -relief]
    if { ![string compare $relief "link"] } {
        set relief "flat"
    }
    set var [Widget::getMegawidgetOption $path -textvariable]
    set st [Widget::getMegawidgetOption $path -state]
    if {  ![string length $var] } {
        set desc [BWidget::getname [Widget::getMegawidgetOption $path -name]]
        if { [llength $desc] } {
            set text  [lindex $desc 0]
            set under [lindex $desc 1]
            Widget::configure $path [list -text $text]
            Widget::configure $path [list -underline $under]
        } else {
            set text  [Widget::getMegawidgetOption $path -text]
            set under [Widget::getMegawidgetOption $path -underline]
        }
    } else {
        set under -1
        set text  ""
        Widget::configure $path [list -underline $under]
    }

    $path configure -relief $relief -text $text -underline $under \
	    -textvariable $var -state $st
    bindtags $path [list $path BwButton [winfo toplevel $path] all]

    set accel1 [string tolower [string index $text $under]]
    set accel2 [string toupper $accel1]
    if { $accel1 != "" } {
        bind [winfo toplevel $path] <Alt-$accel1> "Button::invoke $path"
        bind [winfo toplevel $path] <Alt-$accel2> "Button::invoke $path"    
    }

    DynamicHelp::sethelp $path $path 1

    rename $path ::$path:cmd
    proc ::$path { cmd args } "return \[eval Button::\$cmd $path \$args\]"

    return $path
}


# ------------------------------------------------------------------------------
#  Command Button::configure
# ------------------------------------------------------------------------------
proc Button::configure { path args } {
    set oldunder [$path:cmd cget -underline]
    if { $oldunder != -1 } {
        set oldaccel1 [string tolower [string index [$path:cmd cget -text] $oldunder]]
        set oldaccel2 [string toupper $oldaccel1]
    } else {
        set oldaccel1 ""
        set oldaccel2 ""
    }
    set res [Widget::configure $path $args]

    # Extract all the modified bits we're interested in
    foreach {cr cs cv cn ct cu} [Widget::hasChangedX $path \
	    -relief -state -textvariable -name -text -underline] break
    if { $cr || $cs } {
	set relief [Widget::cget $path -relief]
	set state  [Widget::cget $path -state]
        if { ![string compare $relief "link"] } {
            if { ![string compare $state "active"] } {
                set relief "raised"
            } else {
                set relief "flat"
            }
        }
        $path:cmd configure -relief $relief -state $state
    }

    if { $cv || $cn || $ct || $cu } {
	set var		[Widget::cget $path -textvariable]
	set text	[Widget::cget $path -text]
	set under	[Widget::cget $path -underline]
        if {  ![string length $var] } {
            set desc [BWidget::getname [Widget::cget $path -name]]
            if { [llength $desc] } {
                set text  [lindex $desc 0]
                set under [lindex $desc 1]
            }
        } else {
            set under -1
            set text  ""
        }
        set top [winfo toplevel $path]
        if { $oldaccel1 != "" } {
            bind $top <Alt-$oldaccel1> {}
            bind $top <Alt-$oldaccel2> {}
        }
        set accel1 [string tolower [string index $text $under]]
        set accel2 [string toupper $accel1]
        if { $accel1 != "" } {
            bind $top <Alt-$accel1> "Button::invoke $path"
            bind $top <Alt-$accel2> "Button::invoke $path"
        }
        $path:cmd configure -text $text -underline $under -textvariable $var
    }
    DynamicHelp::sethelp $path $path

    set res
}


# ------------------------------------------------------------------------------
#  Command Button::cget
# ------------------------------------------------------------------------------
proc Button::cget { path option } {
    Widget::cget $path $option
}


# ------------------------------------------------------------------------------
#  Command Button::invoke
# ------------------------------------------------------------------------------
proc Button::invoke { path } {
    if { [string compare [$path:cmd cget -state] "disabled"] } {
	$path:cmd configure -state active -relief sunken
	update idletasks
	set cmd [Widget::getMegawidgetOption $path -armcommand]
        if { $cmd != "" } {
            uplevel \#0 $cmd
        }
	after 100
        set relief [Widget::getMegawidgetOption $path -relief]
        if { ![string compare $relief "link"] } {
            set relief flat
        }
	$path:cmd configure \
            -state  [Widget::getMegawidgetOption $path -state] \
            -relief $relief
	set cmd [Widget::getMegawidgetOption $path -disarmcommand]
        if { $cmd != "" } {
            uplevel \#0 $cmd
        }
	set cmd [Widget::getMegawidgetOption $path -command]
        if { $cmd != "" } {
            uplevel \#0 $cmd
        }
    }
}


# ------------------------------------------------------------------------------
#  Command Button::_enter
# ------------------------------------------------------------------------------
proc Button::_enter { path } {
    variable _current
    variable _pressed

    set _current $path
    if { [string compare [$path:cmd cget -state] "disabled"] } {
        $path:cmd configure -state active
        if { $_pressed == $path } {
            $path:cmd configure -relief sunken
        } elseif { ![string compare [Widget::cget $path -relief] "link"] } {
            $path:cmd configure -relief raised
        }
    }
}


# ------------------------------------------------------------------------------
#  Command Button::_leave
# ------------------------------------------------------------------------------
proc Button::_leave { path } {
    variable _current
    variable _pressed

    set _current ""
    if { [string compare [$path:cmd cget -state] "disabled"] } {
        $path:cmd configure -state [Widget::cget $path -state]
        set relief [Widget::cget $path -relief]
        if { $_pressed == $path } {
            if { ![string compare $relief "link"] } {
                set relief raised
            }
            $path:cmd configure -relief $relief
        } elseif { ![string compare $relief "link"] } {
            $path:cmd configure -relief flat
        }
    }
}


# ------------------------------------------------------------------------------
#  Command Button::_press
# ------------------------------------------------------------------------------
proc Button::_press { path } {
    variable _pressed

    if { [string compare [$path:cmd cget -state] "disabled"] } {
        set _pressed $path
	$path:cmd configure -relief sunken
	set cmd [Widget::getMegawidgetOption $path -armcommand]
        if { $cmd != "" } {
            uplevel \#0 $cmd
	    set repeatdelay [Widget::getMegawidgetOption $path -repeatdelay]
	    set repeatint [Widget::getMegawidgetOption $path -repeatinterval]
            if { $repeatdelay > 0 } {
                after $repeatdelay "Button::_repeat $path"
            } elseif { $repeatint > 0 } {
                after $repeatint "Button::_repeat $path"
	    }
        }
    }
}


# ------------------------------------------------------------------------------
#  Command Button::_release
# ------------------------------------------------------------------------------
proc Button::_release { path } {
    variable _current
    variable _pressed

    if { $_pressed == $path } {
        set _pressed ""
        set relief [Widget::getMegawidgetOption $path -relief]
        if { ![string compare $relief "link"] } {
            set relief raised
        }
        $path:cmd configure -relief $relief
	set cmd [Widget::getMegawidgetOption $path -disarmcommand]
        if { $cmd != "" } {
            uplevel \#0 $cmd
        }
        if { $_current == $path &&
             [string compare [$path:cmd cget -state] "disabled"] && \
	     [set cmd [Widget::getMegawidgetOption $path -command]] != "" } {
            uplevel \#0 $cmd
        }
    }
}


# ------------------------------------------------------------------------------
#  Command Button::_repeat
# ------------------------------------------------------------------------------
proc Button::_repeat { path } {
    variable _current
    variable _pressed

    if { $_current == $path && $_pressed == $path &&
         [string compare [$path:cmd cget -state] "disabled"] &&
         [set cmd [Widget::getMegawidgetOption $path -armcommand]] != "" } {
        uplevel \#0 $cmd
    }
    if { $_pressed == $path &&
         ([set delay [Widget::getMegawidgetOption $path -repeatinterval]] >0 ||
          [set delay [Widget::getMegawidgetOption $path -repeatdelay]] > 0) } {
        after $delay "Button::_repeat $path"
    }
}

