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

# Routines pertaining to the feature list box

# Globals with a counter-part in sather. See updatesathervars
set gShowPublic 1
set gShowPrivate 1
set gShowAttr 1
set gShowConst 1
set gShowShared 1

# Private globals
# List of routs for the currently selected class. Set in featureListInit
# Form is determined by tkkit_cb getClassInfo
set gFeatureListElements ""

# Please see Help/featureList for documentation of this global

########################################################################
#                          FeatureList Box
# Create a list box with the following subpanes
# .l listbox itself
# .vscroll vertical scroller
# .hscroll horizontal scroller
#######################################################################
proc featureListCreate { } {
    global gDeepBackgroundColor
    global gBackgroundColor
    global gFeatureListTitleColor
    global gFeatureListTitleRev
    global gFeatureListSelectionColor
    global gFeatureListSelectionRev
    global gTitleFont
    global gFeatureListFrame
    global gFeatureListFont

    set topName ${gFeatureListFrame}
    set title "Features"
    listbox ${topName}.l \
	    -yscroll "${topName}.vscroll set" \
	    -xscroll "${topName}.hscroll set" \
	    -selectmode single \
	    -font ${gFeatureListFont}

    ${topName}.l configure -bg $gBackgroundColor -relief raised
    ${topName}.l configure -selectbackground $gFeatureListSelectionColor
    ${topName}.l configure -selectforeground $gFeatureListSelectionRev

    scrollbar ${topName}.vscroll -bg $gDeepBackgroundColor -command   \
	    "${topName}.l yview"
    scrollbar ${topName}.hscroll -bg $gDeepBackgroundColor \
	    -orient horizontal -command  "${topName}.l xview"
    pack ${topName}.vscroll -side right -fill y
    pack ${topName}.hscroll -side bottom -fill x
    if {${title} != ""} {
	label ${topName}.title -text ${title} -bg $gFeatureListTitleColor -fg $gFeatureListTitleRev -relief raised -font ${gTitleFont}
	pack ${topName}.title -fill x
    }
    pack ${topName}.l -expand yes -fill both
    bind ${topName}.l  <B1-ButtonRelease> "featureListSelectBind ${topName} %x %y "
    bind ${topName}  <Any-Enter> \
	    {inform "LEFT button: Show feature definition"}
}

# Initialize the feature list to indicate the features for the currently
# selected node. 
# Please see documentation in Help/featureList to see the structure
# of the argument "elements"
#     list of elements, each of which is a list: 
#      { <routine name> <file name> <line number> <feature type>}
#	     0             1             2             3
# <feature type> is of the form:
# Field  : iter private attr_writer att_reader, sh_writer sh_reader const
# Index       0    1       2           3           4         5         6
# When true:  i    p       w           r           w         r         c
# when false: n    n       n           n           n         n         n
proc featureListInit { elements } {
    global gFeatureListElements
    global gFeatureListFrame
    global gCurNodeName gCurNodeFullName

    set gFeatureListElements [lsort -increasing -ascii ${elements}]
    ${gFeatureListFrame}.l delete 0 end	
    if { [satherIsModule ${gCurNodeFullName}]  == "true" } {
	# If the current class is not a module, set the title of
	# the feature list box to the classname
	${gFeatureListFrame}.title configure -text "--------"
	set gFeatureListElements {}
	return
    } else {
	${gFeatureListFrame}.title configure -text ${gCurNodeName}
    }

    foreach rout ${gFeatureListElements} {
	set routName [lindex $rout 0]
	# Please the head of this file (under callbacks) re: routineType
	# Routtype holds a string indicating the properties of "routName"
	set routType [lindex $rout 3]

	# ipwrwrc  vs. nnnnnnn
	set isPriv [string index ${routType} 1]
	if { ${isPriv} == "p"} {
	    ${gFeatureListFrame}.l insert end "\[pri\][lindex $rout 0]"
	} else {
	    ${gFeatureListFrame}.l insert end [lindex $rout 0]
	}
    }

}

proc featureListSaveState { f } {
    global gShowPublic gShowPrivate gShowAttr  gShowConst  gShowShared
    puts $f "set gShowPublic ${gShowPublic}"
    puts $f "set gShowPrivate ${gShowPrivate}"
    puts $f "set gShowAttr ${gShowAttr}"
    puts $f "set gShowConst ${gShowConst}"
    puts $f "set gShowShared ${gShowShared}"

}

proc featureListUpdateSatherVars { } {
    global gShowPublic gShowPrivate gShowAttr  gShowConst  gShowShared

    tkkit_cb setBoolVar gShowPublic ${gShowPublic} 
    tkkit_cb setBoolVar gShowPrivate ${gShowPrivate} 
    tkkit_cb setBoolVar gShowAttr ${gShowAttr} 
    tkkit_cb setBoolVar gShowConst ${gShowConst} 
    tkkit_cb setBoolVar gShowShared ${gShowShared} 
}

proc featureListAddMenu {cm } {
    $cm add check -label "Private Features" \
      -command {tkkit_cb setBoolVar gShowPrivate $gShowPrivate ; textUpdateModuleOrClass false}\
      -variable gShowPrivate -onvalue 1 -offvalue 0
    $cm add check -label "Public Features" \
      -command {tkkit_cb setBoolVar gShowPublic $gShowPublic ;textUpdateModuleOrClass false } \
      -variable gShowPublic -onvalue 1 -offvalue 0
    $cm add check -label "Attr Features" \
      -command { tkkit_cb  setBoolVar gShowAttr $gShowAttr ;textUpdateModuleOrClass false } \
      -variable gShowAttr -onvalue 1 -offvalue 0
    $cm add check -label "Shared Features" \
      -command { tkkit_cb  setBoolVar gShowShared $gShowShared ; textUpdateModuleOrClass false}\
      -variable gShowShared -onvalue 1 -offvalue 0
    $cm add check -label "Const Features" \
      -command { tkkit_cb  setBoolVar gShowConst $gShowConst ; textUpdateModuleOrClass false } \
      -variable gShowConst -onvalue 1 -offvalue 0

}


proc featureListGetDoc { isAbsClass } {
    # Returns a string representing the documentation for the features
    # in the current feature set
    # For the head of the class and for each routine, go through the
    # list of routines (gFeatureListElements) and find the comments 
    # following that def

    global gFeatureListElements

    set txt ""
    foreach rout ${gFeatureListElements} {
	set featureSig  [lindex ${rout} 0]	
	if { [string first "pri" $featureSig] >= 0 } {
	} else {
	    set featureFileName [lindex ${rout} 1]
	    set featureFileOffset [lindex ${rout} 2]
	    set featureType [lindex ${rout} 3]
	    set isAttrWriter  [string index ${featureType} 2]
	    set isSharedWriter  [string index ${featureType} 4]
	    if { $isAttrWriter == "w" } {
		# Don't append the code for attribute writer -
		# End up duplicating attributes
	    } elseif { ${isSharedWriter} == "w"} {
	    } else {
		set off   [expr ${featureFileOffset} - 1]
		set com [satherGetCommentAfter $featureFileName $off ${isAbsClass} ]
		set txt "${txt}\n${com}\n"
	    }
	}
    }
    return ${txt}
}

# Private
proc featureListSelectBind { routListFrame x y } {
    # When the user clicks on a particular routine, show the feature  as
    # selected and display the associated definition in the text window
    global gFeatureListElements 
    global gCurNodeName

    set ind [${routListFrame}.l nearest $y]
    ${routListFrame}.l selection clear 0 
    ${routListFrame}.l selection set $ind
    ${routListFrame} config -cursor watch
    set sel [${routListFrame}.l curselection]
    if { [llength ${sel}] > 1 } {
	set sel [lindex ${sel} 0]
    }
    if { $sel == ""} { return "" }
    set gl [lindex ${gFeatureListElements} ${sel}]
    textUpdateFeature [lindex ${gl} 0] [lindex ${gl} 1] [lindex ${gl} 2]
    ${routListFrame} config -cursor arrow
    update
    return
}

