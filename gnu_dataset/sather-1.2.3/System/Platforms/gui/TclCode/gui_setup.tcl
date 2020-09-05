#------------------------------>  Tcl - script  <-----------------------------#
#- Copyright (C) 199x by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

# This code is called after the gui has been initialized in order to 
# set up polling and to initialize other tcl code for some of the widgets

proc debugPuts { file msg } {
#    puts "${file}: ${msg}"
}
########################################################################
#            Code to Poll Sather at Regular Intervals
########################################################################
set gPollInterval 2
after 1000 "firstPoll"

# It is ok to use these "callbacks" here sincee they are not really callbacks
# They are interpreted at the server end
sather server_debug false
sather server_tcldebug false

# By default, it turns off all the debugging flags, which are normally
# turned on in the sather code.  Unfortunately, since these debugging
# flags are only read *after* the sockets have been initialized, it is
# necessary to turn them on in sather in order to detect socket
# initialization bugs.  However, if they are turned off here, not much
# debugging output is generated

proc firstPoll { } {
    global gPollInterval
    after ${gPollInterval} "poll"
}

proc poll { } {
    global gPollInterval
    sather poll
    after ${gPollInterval} "poll"
}

########################################################################
#            Loading Tcl Source Code for Widgets
########################################################################
set gCurrentlyLoading ""
set gHome ""
proc loadFile { gname} {
    global gCurrentlyLoading
    global gHome
    global env
    set gCurrentlyLoading ${gname}
    debugPuts "gui-setup.tcl" "Sourcing ${gname}..."
    set gGuiHomeDefined [array names env GUI_HOME]
    if {[string length $gGuiHomeDefined] != 0} {
	set gHome $env(GUI_HOME)
    } else {
	set gHome $env(SATHER_HOME)/Gui
    }
    debugPuts "gui-setup.tcl" "Gui home: ${gHome}"
    set res [catch {uplevel #0 {source ${gHome}/Config/TclCode/${gCurrentlyLoading}}} srcErr]
    if { ${res} != 0 } { 
	puts "Error loading ${gname}"
	puts ${srcErr} 
    }
    debugPuts "gui-setup.tcl" "Done sourcing ${gname}..."
}


debugPuts "canvas.tcl" "Starting to source file..."

proc canvasBindItem { frame event tag action } {
    ${frame} bind ${tag} "${event}" "canvasCallback ${frame} ${action} %b %x %y "
}

proc canvasCallback { frame action b x y } {
    set curpos [${frame} coords current]
    puts "in callback:"
    sather  ${frame} ${action} $b $x $y [lindex ${curpos} 0] [lindex ${curpos} 1]
}


proc motionBinding { cname action triggerTag moveTag } {
    # Bind items with tags "tagName" on the cavas with name "cname"
    # A callback associated with "action" will be called when the
    # motion is completed.
    ${cname} bind ${triggerTag} <1> "beginMove ${cname} %x %y ${moveTag} "
    ${cname} bind ${triggerTag} <B1-Motion> "contMovement ${cname} %x %y "
    ${cname} bind ${triggerTag} <B1-ButtonRelease> "endMove ${cname} ${action} %b %x %y"

}

# Local details to handle movement of canvas objects locally
global move(X) 0
global move(Y) 0

proc beginMove { canvasName x y  moveTag  } {
    global move
    ${canvasName} dtag selected
    ${canvasName} addtag selected withtag current
    ${canvasName} addtag selected withtag ${moveTag}
    ${canvasName} raise selected
    set move(X) ${x}
    set move(Y) ${y}
}

proc contMovement { canvasName x y } {
    global move
    ${canvasName} move selected [expr $x - $move(X)] [expr $y - $move(Y)]
    set move(X) $x
    set move(Y) $y
}
    
proc endMove { canvasName action b x y } {
    global move
    ${canvasName} lower selected current
    ${canvasName} dtag selected
    set curpos [${canvasName} coords current]
    sather ${canvasName} ${action} $b $x $y [lindex ${curpos} 0] [lindex ${curpos} 1]
}


debugPuts "gui_setup.tcl" "Done dealing with canvas stuff..."


proc textInsert { frame  at text } {
    ${frame} insert ${at} ${text}
}

proc queryVariable { name } {
    global $name
    return [expr $$name]
}

debugPuts "gui_setup.tcl" "Done dealing with text stuff..."

debugPuts "gui-setup.tcl" "Finished gui-setup.tcl"
