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

# Functions pertaining to the list of classes and modules


# Private globals
# List of all current classes/modules in the order in which they appear in
# the classListFrame
set gClassListElements {}


########################################################################
# Create a list box out of a text pane with the following subpanes
# .l listbox itself
# .vscroll vertical scroller
# .hscroll horizontal scroller
#######################################################################
proc classListCreate { } {
    global gFeatureListTitleColor
    global gClassListTitleColor gClassListTitleRev
    global gDeepBackgroundColor
    global gBackgroundColor
    global gTitleFont
    global gClassListFrame
    
    set topName ${gClassListFrame}
    set title "Modules and Classes"
    text ${topName}.l \
	    -yscrollcommand "${topName}.vscroll set" -width 25 \
	    -xscrollcommand "${topName}.hscroll set" -width 25 \
	    -wrap none 
    ${topName}.l configure -bg $gBackgroundColor -relief raised -bd 2 
    ${topName}.l configure -selectbackground $gFeatureListTitleColor

    scrollbar ${topName}.vscroll -bg $gDeepBackgroundColor \
	    -command "${topName}.l yview"
    scrollbar ${topName}.hscroll -bg $gDeepBackgroundColor \
	    -orient horizontal -command "${topName}.l xview"
    pack ${topName}.vscroll -side right -fill y 
    if {${title} != ""} {
	label ${topName}.title -text ${title} -bg $gClassListTitleColor \
		-fg $gClassListTitleRev -relief raised -bd 2 \
		-font ${gTitleFont}
	pack ${topName}.title -fill x
    }
    pack ${topName}.hscroll -side bottom -fill x
    pack ${topName}.l -expand yes -fill both
    bind ${topName}.l <B1-Motion> " break"
    bind ${topName}.l <B2-Motion> " break"
    bind ${topName}  <Any-Enter> {inform "LEFT: Show source MIDDLE: Show class documentation/Toggle module expansion"; classListArrowCursor }
}

##########################################################################    
#                         Class List box
# Initialize the list of all classes. Gather together classes under
# their modules, tag the text appropriately
# See the file Help/class-list for info on the tags
##########################################################################    
proc classListInit { } {
    global gClassListElements
    global gFontSizes
    global gClassListFrame
    global gSubtypeColor gIncludeColor gModuleColor
    global gModToClasses gClassListModuleFont gClassListClassFont
    global gUnexpandedModules

    ${gClassListFrame}.l configure -state normal
    ${gClassListFrame}.l delete 1.0 end 
    set classesRet [tkkit_cb allClasses]
    set unsorted [lindex ${classesRet} 0]
    # puts "Unsorted:$unsorted"
    # First go through and just fill in empty entries into gModToClasses, or it
    # won't work later on
    # gModToClasses is a mapping from modulename -> list of classes
    set commandLineModuleName "CommandLine.module"
    foreach class $unsorted {
	set mod [lindex $class 3]
	if { ${mod} == "" } {
	    set gModToClasses($commandLineModuleName) {}
	} else {
	    set gModToClasses($mod) {}
	}
    }
    # Put each class under its appropriate module in gModToClasses 
    # (should really be module table)
    foreach class $unsorted {
	set cnm [lindex $class 0]
	set fileloc [lindex $class 1]
	set mod [lindex $class 3]
	if { $mod == "" } {   set mod $commandLineModuleName }
	set clist $gModToClasses($mod)
	# puts "$cnm $fileloc $mod Classes:$clist"
	if { $clist == {} } {
	    set gModToClasses($mod) [list $cnm ]
	} else {
	    set clist2 [concat $clist [list $cnm]]
	    set gModToClasses($mod) $clist2
	}
    }
    set gClassListElements { }
    set unsortedModules [array names gModToClasses]
         debugPuts "browserClassList.tcl" "Going through unsorted ... $unsortedModules"
    # Note that this sorts the _full_ path names of the modules, 
    # not just the module names
    set sortedModules [lsort -increasing -ascii -command moduleSortCommand \
	                              $unsortedModules ]
    debugPuts "browserClassList.tcl" "Going through modules ... $sortedModules"
    set line 1
    # Go through each of the modules
    foreach module $sortedModules  {
	set textNameForModule [satherGetModuleName $module]
	if {[lsearch ${gUnexpandedModules} ${textNameForModule}] >= 0 } {
	    classListInsertModule ${module} ${line} true
	    incr line
	} else {
	    classListInsertModule ${module} ${line} false
	    incr line
	    set classes [lsort -increasing -ascii $gModToClasses($module)]
	    # If this is one of the collapsed modules, continue
	    # Add all the classes in the current module to the class list
	    foreach cl $classes { 
		classListInsertClass ${cl} ${line}
		incr line
	    }
	    set gClassListElements [concat $gClassListElements $classes]
	}

    }
         debugPuts "browserClassList.tcl" "Done with modules ..."
    
    ${gClassListFrame}.l configure -state disabled
    classListInitC_TEST_Lists 
}

proc classListHideClasses { } {
    # Tell sather about whether classes are visibile or not
    global gNoShow_C_Classes  gNoShow_Test_Classes  gCClasses gTestClasses
    satherCBtoggleViewClasses $gNoShow_C_Classes ${gCClasses}
    satherCBtoggleViewClasses $gNoShow_Test_Classes ${gTestClasses}
}

proc classListUpdate { } {
    # Update the class list display based on the currently selected class
    global gCurNodeName gCurNodeFullName
    global gClassListFrame
    global gClassListElements
    global gClassListSelectColor gClassListSelectRev
    
    if {[satherIsAbstract ${gCurNodeName}]} {
	set linenum  [ expr [lsearch $gClassListElements ${gCurNodeName}] + 1]
    } else {
	set linenum  [ expr [lsearch $gClassListElements [list ${gCurNodeFullName}]] + 1]
    }
    inform "Selecting ${gCurNodeName}"

    ${gClassListFrame}.l tag delete current
    ${gClassListFrame}.l tag add current "${linenum}.0 linestart" "${linenum}.0 lineend"
    ${gClassListFrame}.l tag configure current -foreground $gClassListSelectColor -background $gClassListSelectRev
    set nl [expr ${linenum} - 1]
    ${gClassListFrame}.l yview -pickplace "0.0 + ${nl} lines"

}


proc classListSaveState { f } {
    # Whether C_ classes and TEST_ classses are visible in the graph
    global gUnexpandedModules
    
    puts $f "set gUnexpandedModules \{ ${gUnexpandedModules} \}"
}

# Private
proc classListArrowCursor { } {
    global gClassListFrame
    global gIsWaiting
    if { ${gIsWaiting} } {
    } else {
	${gClassListFrame}.l config -cursor left_ptr
    }
}

# Private
#  Add the specified module to the class list
proc classListInsertModule { module line isCompressed } {
    # If isCompressed is true, add a trailing ...
    global gClassListElements gModuleColor gClassListModuleFont
    global gClassListFrame

    # Tag each module line with "module" and with the name of the module
    debugPuts "browserClassList.tcl" "Next module $module"
    set textNameForModule [satherGetModuleName $module]
    set gClassListElements [concat $gClassListElements [list ${module}]]
    # Adding the module text
    if {$isCompressed} {
	${gClassListFrame}.l insert end \
		"${textNameForModule}...                   "
    } else {
	${gClassListFrame}.l insert end \
		"${textNameForModule}                      "
    }
    # Tagging the text with "module" and the text name of the module
    ${gClassListFrame}.l tag add module "$line.0" "$line.0+20 chars"
    ${gClassListFrame}.l tag add ${textNameForModule} \
	    "$line.0" "$line.0+20 chars"

    # Configuring the module text with properties
    ${gClassListFrame}.l tag configure ${textNameForModule} \
	    -foreground ${gModuleColor} \
	    -font ${gClassListModuleFont}
    # Set up button binding
    ${gClassListFrame}.l tag bind ${textNameForModule} <1> \
	    "updateForClass {${module}} false"
    ${gClassListFrame}.l tag bind ${textNameForModule} <2> \
	    "classListToggleModuleExpansion {${textNameForModule}} ${line}"
    ${gClassListFrame}.l insert end "\n"

}

# Private
#  Add the specified module to the class list
proc classListInsertClass { cl line } {
    #Insert the text into the class list pane for a single class name
    # Bind the mouse buttons to select the appropriate file location
    # Set the colors appropriately
    global gClassListFrame gSubtypeColor gIncludeColor
    global gClassListClassFont

    ${gClassListFrame}.l insert end "   ${cl}                      " 
    # Add the class tags, the <class name> and the word "class"
    ${gClassListFrame}.l tag add ${cl} "$line.0" "$line.0 lineend"
    ${gClassListFrame}.l tag add class "$line.0" "$line.0 lineend"
    ${gClassListFrame}.l insert end "\n"
    
    # Set the button bindings
    ${gClassListFrame}.l tag bind ${cl} <1> "updateForClass {${cl}} false"
    ${gClassListFrame}.l tag bind ${cl} <2> "updateForClass {${cl}} true"
    # set the zoom center
    set c [string index ${cl} 0]
    if { $c == "\$" } {
	${gClassListFrame}.l tag configure ${cl} \
		-foreground $gSubtypeColor -font ${gClassListClassFont}
    } else {
	${gClassListFrame}.l tag configure ${cl} \
		-foreground $gIncludeColor -font ${gClassListClassFont}
    }

}

#Private
proc classListToggleModuleExpansion { module line } {
    # If the module was not visible before, now make it visible and
    # vice versa.
    global gUnexpandedModules gClassListFrame


    set lpos [lsearch ${gUnexpandedModules} ${module}]
    if { ${lpos} >= 0} {
	set gUnexpandedModules [lreplace ${gUnexpandedModules} ${lpos} ${lpos}]
    } else {
	set gUnexpandedModules [linsert ${gUnexpandedModules} 1 ${module}]
    }
    classListInit
    ${gClassListFrame}.l yview -pickplace ${line}
    ${gClassListFrame}.l xview moveto 0.0
}

# Ensure that commandline is always the first module and other modules
# are sorted by the actual module name (not by the complete path name!)
proc moduleSortCommand { mod1 mod2 } {
    set mod1Name [satherGetModuleName ${mod1}]
    set mod2Name [satherGetModuleName ${mod2}]
    if {${mod1Name} == "CommandLine" } {
	return -1
    } elseif { ${mod2Name} == "CommandLine" } {
	return 1
    } elseif { ${mod1Name} > ${mod2Name} } {
	return 1
    } else {
	return -1
    }
}
     debugPuts "browserClassList.tcl" "After reading classListInit"

# Initialization of the lists of classes that begin with C_ and TEST_ 
proc classListInitC_TEST_Lists { } {
    global gClassListElements gCClasses gTestClasses

    foreach c ${gClassListElements} {
	set isC [string first "C_" ${c}]
	if { ${isC} == 0 } {
	    set gCClasses [concat ${gCClasses} [list ${c}]]
	}
	set isTest [string first "TEST_" ${c}]
	if { ${isTest} == 0 } {
	    set gTestClasses [concat ${gTestClasses} [list ${c}]]
	}
    }
    debugPuts "browserClassList.tcl" "C_ ${gCClasses}"
    debugPuts "browserClassList.tcl" "TEST ${gTestClasses}"
}

