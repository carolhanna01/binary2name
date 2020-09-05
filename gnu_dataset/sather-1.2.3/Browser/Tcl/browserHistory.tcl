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

# Create and manage a history of what locations have been viewed
# Globals
# Indicates whether a history window should be opened at startup
set gHistoryAtStartup false

# Indicates number of history items stored
set gHistoryLimit 20

# Private: 
set gHistoryFrame ""
# Private:
set gHistoryList ""

# Create the history frame at startup if needed
proc historyCreateAtStartup { } {
    global gHistoryAtStartup
    if {${gHistoryAtStartup}} {
	historyCreate
    }
}

# Create the history frame afresh
proc historyCreate { } {
    global gDeepBackgroundColor
    global gBackgroundColor
    global gFeatureListTitleColor
    global gFeatureListTitleRev
    global gFeatureListSelectionColor
    global gFeatureListSelectionRev
    global gTitleFont
    global gHistoryFrame
    global gHome

    toplevel ${gHistoryFrame}
    wm title ${gHistoryFrame} "History"
    wm iconmask .topHistory "@${gHome}/Browser/Tcl/sathermask.xbm"
    wm iconbitmap .topHistory "@${gHome}/Browser/Tcl/sather.xbm"
    raise ${gHistoryFrame} .
    set topName ${gHistoryFrame}

    listbox ${topName}.l \
	    -yscroll "${topName}.vscroll set" \
	    -xscroll "${topName}.hscroll set" \
	    -selectmode single
    frame ${topName}.topFrame 
    button ${topName}.quit -text "Quit" \
	    -command "historyQuit"\
	    -relief raised  -bg ${gBackgroundColor}
    ${topName}.l configure -bg $gBackgroundColor -relief raised
    ${topName}.l configure -selectbackground $gFeatureListSelectionColor
    ${topName}.l configure -selectforeground $gFeatureListSelectionRev
    
    scrollbar ${topName}.vscroll -bg $gDeepBackgroundColor -command   \
	    "${topName}.l yview"
    scrollbar ${topName}.hscroll -bg $gDeepBackgroundColor \
	    -orient horizontal -command  "${topName}.l xview"

    pack ${topName}.quit -side bottom
    pack ${topName}.vscroll -side right -fill y
    pack ${topName}.hscroll -side bottom -fill x
    
    pack ${topName}.topFrame -side top

    pack ${topName}.l -expand yes -fill both
    bind ${topName}.l  <B1-ButtonRelease> "historySelectBind %x %y "
    bind ${topName}  <Any-Enter> \
	    {inform " \[History Window\] LEFT button: Show history item"}
    historyInit
}

proc historyInit { } {
    global gHistoryList gHistoryFrame

    foreach helt ${gHistoryList} {
	${gHistoryFrame}.l insert 0 ${helt}
    }
}

proc historyRaise { } {
    global gHistoryFrame
   raise ${gHistoryFrame}
}

proc historyQuit { } {
    global gHistoryFrame
    destroy ${gHistoryFrame}
}

proc historySaveState { f } {
    global gHistoryLimit gHistoryAtStartup
    puts $f "set gHistoryAtStartup ${gHistoryAtStartup}"
    puts $f "set gHistoryLimit ${gHistoryLimit}"
} 

proc historyAddMenu { cm } {
    ${cm} add command -label "View History" -command "historyCreate"

    set cm2 "${cm}.historysize"
    menu ${cm2}
    ${cm} add cascade -label "History Items" -menu "${cm2}"
    ${cm2} add radio -label "10" -variable gHistoryLimit -value 10
    ${cm2} add radio -label "20" -variable gHistoryLimit -value 20
    ${cm2} add radio -label "50" -variable gHistoryLimit -value 50
    ${cm2} add radio -label "100" -variable gHistoryLimit -value 100
    # ${cm} add separator
    $cm add check -label "Startup with History" \
	    -command {promptInformBox "History" "Toggles whether the browser starts up with the history window. Save configuration for this change to take effect"}\
	    -variable gHistoryAtStartup -onvalue 1 -offvalue 0 
#    ${cm} add command -label "Close History" -command "historyQuit"
#    ${cm} add command -label "Raise History" -command "historyRaise"
}

proc historySelectBind { x y } {
    global gHistoryFrame

    . config -cursor watch
    update
    set ind [${gHistoryFrame}.l curselection]
    set sel [${gHistoryFrame}.l get ${ind}]
    # puts "Selection ${sel}"
    if { $sel == ""} { return "" }
    # Update the current view to show the right class and file position
    # for the history item "sel"
    set sellSingle ${sel}
    set sell [split ${sellSingle} "\t"]
    set class [lindex ${sell} 0]  
    set feat [lindex ${sell} 1]   
    set file [lindex ${sell} 2]   
    set line [lindex ${sell} 3]   
    # puts "File: ${file} Line: ${line} Class: ${class} "
    updateForClass ${class} false
    # puts "Updated non text"
    if {${feat} != "class"} {
	textUpdateFeature ${feat} ${file} ${line}
    } 
    . config -cursor arrow
    update
}


proc historyAdd { className featName fileName lineNum } {
    # Add an element to the history stack
    global gHistoryFrame
    global gHistoryList
    global gHistoryLimit

    set hs [llength ${gHistoryList}]
    if {${hs} > ${gHistoryLimit}} {
	set gHistoryList [lreplace ${gHistoryList} 20 end]
    }
    set hist "${className}\t${featName}\t${fileName}\t${lineNum} "
    set gHistoryList [concat ${gHistoryList} [list ${hist}]]
    # set exists [lsearch ${gHistoryList} ${hist}]
    
    if {[winfo exists ${gHistoryFrame}]} {
	${gHistoryFrame}.l insert 0 $hist
	${gHistoryFrame}.l yview 0
    } else {
	return false
    }

}

