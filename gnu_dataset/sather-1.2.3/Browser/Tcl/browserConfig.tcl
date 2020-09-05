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

# Configuration of the browser, and routines to save browser's configuration

# Determines the configuration mode MultiWindow or SingleWindow
set gConfigMode "MultiWindow"
# Determines whether the user will be asked to confirm a quit
set gQueryQuit 1
# Fraction of overall window devoted to top half
set gTopFraction 0.5

################################## Configuration Menu #########################
proc configAddMenu { cm } {

    $cm add check -label "Multi-Window Configuration" \
	    -variable gConfigMode \
	    -onvalue "MultiWindow" -offvalue "SingleWindow"\
	    -command configToggleMultiWindow 
    $cm add separator 
    $cm add command -label "Equal Split"   -command { configSetTopFraction 0.5 }
    $cm add command -label "Hide Top"  -command {configSetTopFraction 0.05}
    $cm add command -label "Full Top"  -command {configSetTopFraction 1.0}
    $cm add command -label "Increase Top"  -command {configChangeTopFraction 0.1}
    $cm add command -label "Decrease Top"  -command {configChangeTopFraction -0.1}

}

proc configure { } {
    # Main configuration procedure
    global gBackgroundColor gDeepBackgroundColor
    global gGraphFrame gTextDisplay gFeatureListFrame 
    global gClassListFrame gInformFrame gMenuFrame
    global gHome
    global gConfigMode
    global gTopFraction
    global gHistoryAtStartup gHistoryFrame

    wm minsize . 1 1
    set gHistoryFrame .topHistory

         debugPuts "browserConfig.tcl" "Making graph ...."
    . configure -background $gDeepBackgroundColor

    frame .top 
    frame .bottom 
    .bottom configure -background $gDeepBackgroundColor
    frame .bottom.upperbottom 
    frame .bottom.lowerbottom -relief flat 

    if {${gConfigMode} == "MultiWindow"} {
	debugPuts "browserConfig.tcl" "Multiwindow configuration chosen: ${gConfigMode}"
	toplevel .toplevelgraph
	set gGraphFrame .toplevelgraph.gGraphFrame
	set gTextDisplay .top.textDisplay
	wm title .toplevelgraph "Browser Graph"
	wm iconmask .toplevelgraph "@${gHome}/Browser/Tcl/sathermask.xbm"
	wm iconbitmap .toplevelgraph "@${gHome}/Browser/Tcl/sather.xbm"	
    } else {
	set gGraphFrame .top.gGraphFrame
	set gTextDisplay .bottom.upperbottom.textDisplay
    }
	
    frame ${gGraphFrame} -relief flat -borderwidth 5 -bg $gDeepBackgroundColor
    graphCreate 1000c 500c 1000c 500c  

    set gFeatureListFrame .bottom.upperbottom.gFeatureListFrame
    frame ${gFeatureListFrame} -relief flat -borderwidth 5 -bg $gDeepBackgroundColor
    featureListCreate 


    set gClassListFrame .bottom.upperbottom.gClassListFrame
    frame ${gClassListFrame} -relief flat -borderwidth 5 -bg $gDeepBackgroundColor
    classListCreate 


    frame ${gTextDisplay} -borderwidth 5 -relief flat -bg $gDeepBackgroundColor
    textCreate 

    set gTopFrame .top.gTopFrame
    frame $gTopFrame -bg $gDeepBackgroundColor

         debugPuts "browserConfig.tcl" "Making menubar ...."
    set  gMenuFrame .top.gTopFrame.gMenuFrame
    frame ${gMenuFrame} -bg $gDeepBackgroundColor -relief flat -bd 2
    menuBarCreate ${gMenuFrame}
    
    set gButtonFrame .top.gTopFrame.gButtonFrame
    frame  ${gButtonFrame} -bg $gDeepBackgroundColor
    button ${gButtonFrame}.quit -text "Quit" \
	    -command "quitBrowser"\
	    -relief raised  -bg ${gBackgroundColor}
    button ${gButtonFrame}.help -text "Help" -command "menuShowHelp help"

    pack ${gButtonFrame}.quit -side left -padx 5



    pack ${gButtonFrame} -side right
    pack ${gMenuFrame} -side left

         debugPuts "browserConfig.tcl" "Making inform ..."
    set gInformOutline .bottom.lowerbottom
    set gInformFrame ${gInformOutline}.gInformFrame
    frame ${gInformFrame} 
    informCreate ${gInformFrame}

    set top ${gTopFraction}
    set bottom [expr 1.0 - $top]
         debugPuts "browserConfig.tcl" "Packing ...."
    place .bottom -relx 0.0 -rely $top -relh $bottom -relw 1.0
    place .top -relx 0.0 -rely 0.0 -relh $top -relw 1.0

    pack ${gTopFrame} -side top -fill x 

    pack ${gGraphFrame} -side top -fill y -expand yes

    pack ${gInformOutline} -side bottom -expand yes -fill y
    pack ${gInformFrame} -side bottom -expand yes -fill x
    pack ${gClassListFrame} -side left -expand yes -fill both 
    if {$gConfigMode == "MultiWindow"} {
	pack ${gFeatureListFrame} -side right -expand yes -fill both 
    }  else {
	pack ${gFeatureListFrame} -side left -expand yes -fill both 
    }
    pack ${gTextDisplay} -side right -expand yes -fill both 

    pack .bottom.upperbottom -side top -expand yes -fill both
    pack .bottom.lowerbottom -side bottom -expand no -fill x 
    # took off -expand yes 

    wm geometry . 900x800
    set browsername "Sather Browser"
    if { $gConfigMode=="MultiWindow"} {
	wm geometry .toplevelgraph 900x600
	wm minsize .toplevelgraph 1 1
	# wm iconify .toplevelgraph
	wm title .toplevelgraph "Graph for ${browsername}"
    }
    wm title . ${browsername}    
    wm iconmask . "@${gHome}/Browser/Tcl/sathermask.xbm"
    wm iconbitmap . "@${gHome}/Browser/Tcl/sather.xbm"
    historyCreateAtStartup
}

proc configSave { } {
    global gTopFraction 

    global gNoShow_C_Classes gNoShow_Test_Classes
    global gConfigMode gQueryQuit
    global gConfigFileName
    global gNoShow_Test_Classes gNoShow_C_Classes

    if { [file exists ${gConfigFileName}] } {
	if { [promptGetNonConfirmation "Writing file ${gConfigFileName}" \
		"File ${gConfigFileName} exists. Overwrite (usually ok)?"] } {
	    return
	}
    }
    set f [open ${gConfigFileName} "w"]
    puts $f "# This file is GENERATED BY THE SATHER BROWSER."
    puts $f "# Put your own customization in .bsCustom.tcl rather than here"
    puts $f "# so that it will not be lost the next time you save"
    puts $f "# your configuration."
    puts $f "set gNoShow_C_Classes ${gNoShow_C_Classes}"
    puts $f "set gNoShow_Test_Classes ${gNoShow_Test_Classes}"
    puts $f "set gTopFraction ${gTopFraction}"
    puts $f "set gConfigMode ${gConfigMode}"
    puts $f "set gQueryQuit ${gQueryQuit}"
    puts $f "set gNoShow_Test_Classes ${gNoShow_Test_Classes}"
    puts $f "set gNoShow_C_Classes ${gNoShow_C_Classes}"
    historySaveState ${f}
    textSaveState ${f}
    graphSaveState ${f}
    classListSaveState ${f}
    featureListSaveState ${f}
    close $f

}


# Private
proc configSetTopFraction { val } {
    # Set the fraction of the window devoted to the top half to "val"
    global gTopFraction
    set gTopFraction ${val}
    configChangeTopFraction 0.0
}

# Private
proc configChangeTopFraction { val } {
    # Change the fraction of the window devoted to the top half by "val"
    global gTopFraction
    set top [expr ${val} + ${gTopFraction}]
    if { ${val} < 0.0 } {
	if {$top < 0.1} {   return }
    } 
    if { ${val} > 0.0} {
	if { $top > 0.9 } { return   }
    }

    set gTopFraction ${top}
    set bottom [expr 1.0 - $top]
    place .bottom -relx 0.0 -rely $top -relh $bottom -relw 1.0
    # Add some logic to make sure that the menu never disappears....
    place .top -relx 0.0 -rely 0.0 -relh $top -relw 1.0
     debugPuts "browserConfig.tcl" "before configure..."
}

# Private
proc configToggleMultiWindow { } {
    # Switch between single window and mbk's multi-window configuration
    global gConfigMode

    if {[promptGetNonConfirmation "Saving configuration" \
	    "You will need to restart the browser to see the change"]} {
	# Toggle it back if no confirmation
	if {$gConfigMode == "MultiWindow" } {
	    set gConfigMode "SingleWindow"
	} else {
	    set gConfigMode "MultiWindow"
	}
	return
    }
    configSave
    
}

