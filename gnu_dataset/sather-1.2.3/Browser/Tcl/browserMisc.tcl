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

proc startWait { } {
    global gTextDisplay gClassListFrame gGraphFrame
    global gIsWaiting
    ${gTextDisplay}.t config -cursor watch
    ${gClassListFrame}.l config -cursor watch
    ${gClassListFrame}.vscroll config -cursor watch
    ${gGraphFrame}.c config -cursor watch
    set gIsWaiting true
    update
}

proc endWait { } {
   global gTextDisplay gClassListFrame gGraphFrame
    global gIsWaiting
    ${gTextDisplay}.t config -cursor left_ptr
    ${gClassListFrame}.l config -cursor left_ptr
    ${gClassListFrame}.vscroll config -cursor  left_ptr
    ${gGraphFrame}.c config -cursor left_ptr
    set gIsWaiting false
    update
}
########################################################################
#                          MENU BAR 
# Make the menu bar for the graph display
# Arguments: frameName - name of the frame in which to create the widget
# Creates a menu bar with the following buttons
#######################################################################
proc menuBarCreate { frameName } {
    set cm [menuAdd ${frameName} file "File"]
    fileAddMenu ${cm}
    set cm [menuAdd ${frameName} config "Configuration"]
    configAddMenu ${cm}
    set cm [menuAdd ${frameName} graph "Graph"]
    graphAddMenu ${cm}
    set cm [menuAdd ${frameName} text "Text"]
    textAddMenu ${cm}
    set cm [menuAdd ${frameName} history "History"]
    historyAddMenu ${cm}
    set cm [menuAdd ${frameName} help "Help"]
    helpAddMenu ${cm}
    set cm [menuAdd ${frameName} feature "Features"]
    featureListAddMenu ${cm}
    pack ${frameName}.file -side left -padx 10
    pack ${frameName}.config -side left -padx 5    
    pack ${frameName}.graph -side left -padx 5    
    pack ${frameName}.feature -side left -padx 5
    pack ${frameName}.text -side left -padx 5
    pack ${frameName}.history -side left -padx 5
    pack ${frameName}.help -side left -padx 10

    debugPuts "browser.tcl" "Packed menu items ...."

    bind ${frameName}  <Any-Enter> {inform "LEFT button: Select Menu MIDDLE button drag: tear off menu (click on menu bar to unpost the menu)"}
}

proc menuAdd { frameName menuName title } {
    global gMenuBackgroundColor gMenuForegroundColor gTitleFont
    set cm ${frameName}.${menuName}.menu
    menubutton ${frameName}.${menuName} -text ${title} -menu ${cm} \
	    -bg $gMenuBackgroundColor -fg $gMenuForegroundColor \
	    -font ${gTitleFont}
    menu ${cm} 
    return ${cm}
    debugPuts "browser.tcl" "Made ${title} menu"
}

proc fileAddMenu { cm } {
    $cm add check -label "Quit Confirmation"  -variable gQueryQuit \
	    -onvalue 1 -offvalue 0
    $cm add separator 
    $cm add command -label "Save Configuration"   -command configSave
    $cm add command -label "Dump All State" -command dumpState
    ${cm} add command -label "Quit" -command quitBrowser
}


################### INTERFACE TO SATHER END ########################
proc satherGetFeatureElementsFor { cname } {
    set cdef [tkkit_cb getClassInfo ${cname}] 
    set features [lindex $cdef 2]
    return ${features}
}

################  AUXILLIARY FUNCTIONS ############################
#########################################################################
# Auxilliary general utitlity user interface routines that the browser uses
#########################################################################

# Get all the text in the specified file and return it
proc auxGetAllFile { fname } {
    set ret ""
    if { ${fname} == ""} {
	inform "Internal Non-fatal ERROR! Asked to open void file"
	return
    }
    if { [file exists ${fname}] } {
	# puts "File is ${fname}"
	set f [open ${fname} r]
	set ret [read $f]
	close $f
    } else {
	inform "No such file: $fname"
	set ret "No such file: ${fname}"
    }
    return $ret
}

# Silly inverted result (should be changed), since I normally test for the 

########################################################################
#                          Generic Sather routines
########################################################################
proc satherIsAbstract { nodeName } {
    set abs [string first "\$" ${nodeName}]
    # possible to return abs != -1 itself?
    if { $abs != -1 } {
	return true
    } else {
	return false
    }
    
}

proc satherCleanName { nodeName } {
    # Replace all the dangerous characters that sather uses
    # in class names
    regsub {\$} ${nodeName} {dol} newName
    regsub {\{} ${newName} {LB} newName2
    regsub {\}} ${newName2} {RB} newName3
    regsub {\}} ${newName2} {RB} newName3
    regsub {\\} ${newName3}  slash newName4
    regsub {\.} ${newName4} dot newName5
    return ${newName5}
}

 # Return true if the name ends with a .module
proc satherIsModule { nodeName } {
    set match [regexp {[A-Za-z_0-9]*.(module|com)} $nodeName ]
    if { $match == 1 } {
	return true
    } else {
	return false
    }
}

proc satherGetModuleName { fullName } {
    # Return the file portion of the full path name
    set match [regexp {[A-Za-a_/0-9]*/([A-Za-z_0-9]*).(module|com)} $fullName full modname ]
    if { $match == 1 } {
	return $modname
    } else {
	set match [regexp {([A-Za-z_0-9]*).(module|com)} $fullName full modname ]
	if { $match == 1} {
	    return $modname
	} else {
	    return ""
	    #mbk
	}
    }
}

 # Return the comment that follows a class/feature definition
debugPuts "browser.tcl" "before getCommentFollowing..."
proc satherGetCommentAfter { fileName routineLoc isAbs } {
    set fileText [auxGetAllFile $fileName]
    set lines [split $fileText "\n"]
    set firstLine [lindex $lines $routineLoc]
    set hasIs [string first " is" ${firstLine}]
    set hasAttr [string first  "attr" ${firstLine}]
    set hasShared [string first "shared" ${firstLine}]
    if { ${isAbs} } {
	set isAbs 1
    } else {
	set isAbs 0
    }
    # puts "First: ${firstLine} HasIs: ${hasIs} hasAttr: ${hasAttr} ${isAbs}"
    if { [expr (((${hasIs} >= 0) || (${hasAttr} >= 0)) || (( ${hasShared} >= 0 ) || ${isAbs} )) ] }  {
	# Look for comments on subsequent lines
	# Does the first line have a comment
	set retLines ${firstLine}
	incr routineLoc
	set nextLine [lindex $lines $routineLoc]
	set hasCom [string first "--" $nextLine]
	set hasInc [string first "include" $nextLine]
	while { [expr (($hasCom >= 0) || ($hasInc >= 0))] } {
	    incr routineLoc
	    # puts "First Next line: $nextLine $hasCom $hasInc"
	    set retLines "$retLines\n$nextLine"
	    set nextLine [lindex $lines $routineLoc]
	    set hasCom [string first "--" $nextLine]
	    set hasInc [string first "include" $nextLine]
	}
	return $retLines
    }   else {
	set retLines ${firstLine}
	# Get all text till next "is"
	set hasIs -1
	while { $hasIs < 0  } {
	    incr routineLoc
	    set nextLine [lindex $lines $routineLoc]
	    set hasIs [string first " is" $nextLine]
	    set retLines "$retLines\n$nextLine"
	}
	return $retLines
    }
}

proc updateForClass { newClassName commentDef } {
    # Basic update routine for a new class that always update history
    # to reflect a new class being selected
    global gCurNodeName gCurNodeFullName gFreezeClass

    # The new class name might be a module name (in which case
    # it will be the full module file name
    if { ${gFreezeClass} } {
	promptInformBox "Trying to change class" \
		"The focus class is currently frozen to ${gCurNodeName}.\
      To change focus to another class, first choose unfreeze in the Graph Menu"
	return;
    }
    startWait
    update
    # For modules, set the gCurNodeName to the truncated name
    if {[satherIsModule ${newClassName}] } {
	set gCurNodeName [satherGetModuleName ${newClassName}]
	if { ${gCurNodeName} != "CommandLine" } {
	    # Don't change current if = commandline.module (has no graph ...)
	    tkkit_cb setCurClassName ${newClassName}
	}
    } else {
	set gCurNodeName ${newClassName}
	tkkit_cb setCurClassName ${newClassName}
    }
    set gCurNodeFullName ${newClassName}
    
    classListUpdate 
    graphUpdate
    textUpdateModuleOrClass $commentDef
    endWait
}

proc quitBrowser { } {
    global gQueryQuit
    if {$gQueryQuit} {
	if { [promptGetNonConfirmation "Quitting Browser" \
		"Are you sure you wish to quit?" ]} {
	    return
	}
    }
    textSaveTextIfNecc
    destroy .
}


proc emacsStart { } {
    # Start up an emacs process with a running server for emacs-client
    exec  emacs -f server-start >& /dev/null & 
}


############### Menu items ######################################
# These lists of classes are initialized in classListInit, and
# here they are merely used to toggle the display of classes in the
# graph (by activating/deactivating them)
proc toggleNoShowCClasses { } {
    global gNoShow_C_Classes gCClasses
    satherCBtoggleViewClasses $gNoShow_C_Classes ${gCClasses}
    graphUpdate
}
 

proc toggleNoShowTestClasses { } {
    global gNoShow_Test_Classes  gTestClasses
    satherCBtoggleViewClasses $gNoShow_Test_Classes ${gTestClasses}
    graphUpdate
}

# Flush
proc satherCBFlushCache { } {
    tkkit_cb flushGraph
}

# Depending on the value of controlFlag, either activate or deactivate the
# classes indicated in classList
proc satherCBtoggleViewClasses { controlFlag classList } {
    satherCBFlushCache
    if {$controlFlag} {
	tkkit_cb deactivateClasses ${classList}
	return 0
    } else {
	tkkit_cb activateClasses ${classList}
	return 1
    }

}


