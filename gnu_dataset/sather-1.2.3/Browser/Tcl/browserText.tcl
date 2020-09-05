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

# Functions that relate to the text pane
# Indicates whether the text window or emacs should be used
set gUseEmacs 0

# Files with more lines will not be highlighted on loading
set gHighlightThreshold 0

# Whether "class" should be highlighted
set gHighlightClasses 1

# Whether comments should be highlighted
set gHighlightComments 1

# Private: Default set of unexpanded modules
set gUnexpandedModules ""


########################################################################
#                          Text Display widget
# Make the text display widget
# Arguments: frameName - name of the frame in which to create the widget
#            title  - title displayed on top of the widget
# Creates a text widget with the following subparts
#  .t        text      : actual text window 
#  .yscroll  scrollbar : Vertical scroll
#  .title    lable     : Holds the frameName
#######################################################################
proc textCreate { } {
    global gTextDisplay
    global gBackgroundColor  gDeepBackgroundColor
    global gTextTitleColor gTextTitleRev
    global gTitleFont
    global gTextFont

    set frameName ${gTextDisplay}
    set title "Class Text"
    text ${frameName}.t -bd 2 \
	    -yscrollcommand "${frameName}.yscroll set" \
	    -xscrollcommand "${frameName}.xscroll set" \
	    -wrap none \
	    -font ${gTextFont} -relief raised -bg $gBackgroundColor
    scrollbar ${frameName}.yscroll  -command "${frameName}.t yview" \
	    -bg $gDeepBackgroundColor
    scrollbar ${frameName}.xscroll  -command "${frameName}.t xview" \
	    -orient horizontal -bg $gDeepBackgroundColor
    ${frameName}.t insert 0.0 {\
	    Begin browsing by clicking on a class. }
    ${frameName}.t mark set insert 0.0 
    pack ${frameName}.yscroll -side right -fill y
    pack ${frameName}.xscroll -side bottom -fill x
    frame ${frameName}.top
    button ${frameName}.top.saveButton -text "Save" -command "textSaveText" \
	    -bg $gBackgroundColor -fg "black" 
    pack ${frameName}.top -side top -fill x
    pack ${frameName}.top.saveButton -side right
    if { ${title} != ""} {
	label ${frameName}.top.title -text ${title} -borderwidth 2 \
	-bg $gTextTitleColor -fg $gTextTitleRev  -relief raised -font ${gTitleFont}
	pack ${frameName}.top.title -side left -expand yes -fill x
    } 
    pack ${frameName}.t -expand yes -fill both -side bottom
    bind ${frameName}  <Any-Enter> {inform "LEFT button: Start editing"}
    debugPuts "browserText.tcl" "Created text window"
}


proc textUpdateFeature { rSig fname offset } {
    # Update text window to indicate a visit to a particular feature
    global gCurNodeName

    # Find paren, if any
    set endPt [string first ( ${rSig})]
    if {$endPt == -1} {
	# Otherwise assume a :
	set endPt [string first : ${rSig} ]
    }
    set rname [string range ${rSig} 0 [expr ${endPt} - 1]]
    inform "Selected routine $rSig in file: $fname line:$offset"
    textReadFileLoadText "${gCurNodeName}::${rname}"  ${fname} ${offset} true
    historyAdd ${gCurNodeName} ${rSig} ${fname} ${offset}
}

proc textUpdateModuleOrClass { commentDef } {
    # Update the text display after a new class/module has been selected
    # If commentDef is true, show the comment version of the class, rather
    # than the full text
    global gCurNodeName gCurNodeFullName

    set nm ${gCurNodeFullName}
    startWait
    if { [satherIsModule ${nm}] } {
	textReadFileLoadText $nm $nm 1 true
    } else {
	if { $commentDef } {
	    textShowClassDocs
	} else {
	    textShowClassDef 
	}
    }
    endWait
}

proc textSaveState { f } {
    global gHighlightClasses gHighlightThreshold gHighlightComments
    global gUseEmacs 

    puts $f "set gHighlightThreshold ${gHighlightThreshold}"
    puts $f "set gHighlightClasses ${gHighlightClasses}"
    puts $f "set gHighlightComments ${gHighlightComments}"
    puts $f "set gUseEmacs ${gUseEmacs}"
    debugPuts "browserText.tcl" "Saving text state"
}



proc textAddMenu { cm } {
    global gUseEmacs gHighlightThreshold gHighlightClasses gHighlightComments
    $cm add radio -label "Highlight files with <= 0 lines" \
	    -variable gHighlightThreshold -value 0
    $cm add radio -label "Highlight files with <= 500 lines" \
	    -variable gHighlightThreshold -value 500
    $cm add radio -label "Highlight files with <= 1000 lines" \
	    -variable gHighlightThreshold -value 1000
    $cm add radio -label "Highlight files with <= 2000 lines" \
	    -variable gHighlightThreshold -value 2000
    $cm add radio -label "Highlight files with <= 5000 lines" \
	    -variable gHighlightThreshold -value 5000
    $cm add radio -label "Highlight files with <= 20000 lines" \
	    -variable gHighlightThreshold -value 20000
    $cm add separator
    $cm add check -label "Highlight class name" \
	    -variable gHighlightClasses -onvalue 1 -offvalue 0
    $cm add check -label "Highlight comments" \
	    -variable gHighlightComments -onvalue 1 -offvalue 0
    ${cm} add separator
    $cm add command -label "Save Text" -command textMenuSaveToFile
    ${cm} add separator
    $cm add check -label "Use Emacs" -command emacsStart \
	    -variable gUseEmacs -onvalue 1 -offvalue 0
    ${cm} add separator

# Old version offered the choice of using emacs in an existing buffer
#    $cm add check -label "Use Emacs" \
#	    -command {promptInformBox "Using Emacs" "To use Emacs instead of the text window, run M-x server-start in an existing emacs, or choose the Text menu item: Start Server Emacs."}\
#	    -variable gUseEmacs -onvalue 1 -offvalue 0 
#   $cm add command -label "Start Server Emacs" -command emacsStart


}

# Goto the fileName, with the highlight on line "lineNumber"
# "isModifiable" indicates whether save should be set or not
#  gCurFile is set to the new file name, and the previous
# file is saved if it has been changed (user is queried)
proc textReadFileLoadText { title fileName lineNumber isModifiable } {
    global gCurFile
    global gTextDisplay
    global gUseEmacs

    inform "Loading text from file ${fileName} ${lineNumber} "
    if {${gUseEmacs}} {
	exec emacsclient +${lineNumber} ${fileName} >& /dev/null &
    } elseif { $gCurFile != $fileName } {
	# puts "Old file: ${gCurFile} NewFile: ${fileName}"
	textSaveTextIfNecc
	set gCurFile $fileName
	set txt [auxGetAllFile ${fileName}]
	if { [file writable ${fileName} ] } {
	    textLoadText "$title in ${fileName}" $txt  $isModifiable
	} else {
	    textLoadText "$title in ${fileName}" $txt  false
	}
	textGotoLine $lineNumber
    } else {
	inform "Staying in same file...."
	${gTextDisplay}.top.title configure -text "${title} in ${gCurFile}"
	textGotoLine $lineNumber
    }

    textChangeTitleColor
}

proc textSaveTextIfNecc { } {
    # Save the current file's text if it has been modified. Query user.
    global gCurFile
    global gTextDisplay
    if { $gCurFile != "" } {
	set oldText  [ string trimright [auxGetAllFile ${gCurFile}]]
	set curText [string trimright [${gTextDisplay}.t get "1.0" "end" ]]
	if {  ${oldText} != ${curText}  } {
	    set dialog [tk_dialog .dialog "Unsaved changes" \
		    "Current text is different from file ${gCurFile}"\
		    "" 0 "Save" "Discard"]
	    if {$dialog == 0} { 
		textSaveText
	    }
	}
    } 
}

proc textMenuSaveToFile { } {
    # Save the currently displayed text to a user specified file
    global gTextDisplay gUseEmacs
    global gCurNodeName
    promptInformBox "Saving text" "This saves whatever is currently in the text pane"
    set txt [ ${gTextDisplay}.t get "0.0" "end" ]
    set userFileName  [ promptReadFileFromUser "${gCurNodeName}.bs.save"]
    if { [lindex ${userFileName} 0 ] } {
	set fileNm [lindex  ${userFileName} 1]
	if { [file exists ${fileNm} ] } {
	    set conf [promptGetNonConfirmation \
		    "File ${fileNm} exists" "Overwrite?"]
	    if { ${conf} } {
		return 
	    } else {
		set file [open ${fileNm} w]
		puts ${file} ${txt}
		close ${file}
		inform "Wrote text to file ${file}"
	    }
	} else {
	    return 
	}
    }
}

######    END OF PUBLIC INTERFACE ####
# Show the list of features that constitute the class
# in the routine window and the class definition in the text window
# Private to text
proc textShowClassDef { } {
    global gCurNodeName

    set cname ${gCurNodeName}
    if { ${cname} == "" } {
	return
    }
    set cdef [tkkit_cb getClassInfo ${cname}] 
    set fname [lindex ${cdef} 0]
    set lineoffset [lindex ${cdef} 1] 
    textReadFileLoadText ${cname} ${fname} ${lineoffset} true
    set features [lindex $cdef 2]
    historyAdd ${gCurNodeName} "class" ${fname} ${lineoffset}
    featureListInit ${features}
}

# Change the color of the title of the text window to indicate
# whether a module, concrete class or abstract class was chosen
# Private to text pane, called after reading in text
proc textChangeTitleColor { } {
    global gModuleColor gModuleRev gSubtypeColor gSubtypeRev
    global gIncludeColor gIncludeRev
    global gTextDisplay
    global gCurNodeName gCurNodeFullName

        # mbk logic
    set title ${gCurNodeName}
    if { [satherIsModule ${gCurNodeFullName}] == "true" } {
	set col $gModuleColor
	set colr $gModuleRev
    } else {
	if { [string index $title 0] == "\$" } {
	    set col $gSubtypeColor
	    set colr $gSubtypeRev
	} else {
	    set col $gIncludeColor
	    set colr $gIncludeRev 
	}
    }
    ${gTextDisplay}.top.title configure -bg $col -fg $colr
}

# Private procedure to text window. Tags comments/classes and highlights
# them
proc textTagComments { } {
    global gTextDisplay
    global gHighlightClasses gClassHighlightColor gClassHighlightFont
    global gHighlightThreshold gCommentFont gCommentColor 
    global gHighlightComments

    set txt [$gTextDisplay.t get 1.0 end]
    set txtl [ split $txt "\n" ]
    set txtlsize [ llength $txtl ] 
    if { $txtlsize < $gHighlightThreshold } {
	inform "Highlighting comments (may be slow for large files)..."
	update
	set lineno 0
	while { $lineno < $txtlsize } {
	    set thisline [ lindex $txtl $lineno ]
	    incr lineno
	    if {$gHighlightClasses} {
		set cl [string first "class " $thisline]
		if { $cl >= 0 } {
		    ${gTextDisplay}.t tag add class \
			    "$lineno.$cl wordend + 1 chars" \
			    "$lineno.$cl wordend + 2 chars wordend"
		}
		set cl [string first "type " $thisline]
		if { $cl >= 0 } {
		    ${gTextDisplay}.t tag add class \
			    "$lineno.$cl wordend + 1 chars" \
			    "$lineno.$cl wordend + 2 chars wordend "
		}
	    }
	    if {$gHighlightComments} {
		set from [ string first "--" $thisline ]
		if { $from >= 0 } {
		    #puts "Line: $lineno  From: $from"
		    ${gTextDisplay}.t tag add comment "$lineno.$from" \
			    "$lineno.$from lineend"
		}
	    }
	}
	${gTextDisplay}.t tag configure comment \
		-font $gCommentFont \
		-foreground $gCommentColor
	${gTextDisplay}.t tag configure class \
		-font $gClassHighlightFont \
		-foreground $gClassHighlightColor
	inform "Done highlighting ..."
	update
    } else {
	inform "File length exceeds the highlight limit (see Text menu)"
	update
    }
}


 # Save the text in the current display out to the file.
proc textSaveText { } {
    global gCurFile
    global gTextDisplay

    if { $gCurFile == "" } {
	inform "No current file"
	return
    }
    set txt [${gTextDisplay}.t get "1.0"  "end"]
    set orig [open ${gCurFile} w]
    puts $orig $txt
    close ${orig}

}


# Loading a file consists of textLoadText and textGotoLine
# Load the text into the text window, based on whether the
# text is modifiable etc.
proc textLoadText { title txt isModifiable } {
    global gTextDisplay

    ${gTextDisplay}.t configure -state normal
    ${gTextDisplay}.top.title configure -text "$title"

    ${gTextDisplay}.t delete "1.0" "end "
    ${gTextDisplay}.t insert 0.0 $txt
    
    if { $isModifiable } {
	${gTextDisplay}.top.saveButton configure -state normal
	${gTextDisplay}.t configure -state normal
	inform "Text is writable"
    } else {
	${gTextDisplay}.top.saveButton configure -state disabled
	${gTextDisplay}.t configure -state disabled
	inform "Text is not writable"
    }
    textTagComments
}

# Goto a particular line in the current text display, puyt the
# cursor there and highlight it appropriately
# private to text pane
proc textGotoLine { lineNumber } {
    global gTextDisplay
    global gTextSelectionColor
    global gTextSelectionRev

    ${gTextDisplay}.t  yview -pickplace "0.0 + ${lineNumber} lines"
    ${gTextDisplay}.t tag delete activeLine

    ${gTextDisplay}.t tag configure activeLine \
	    -foreground $gTextSelectionColor \
	    -background $gTextSelectionRev
    # Changed this for 4.0 - apparently lines are now counted from 0?
    set line [expr ${lineNumber} - 1]
    ${gTextDisplay}.t mark set insert "0.0 + ${line} lines"
    ${gTextDisplay}.t tag add activeLine "insert linestart" "insert lineend"
    set tags [${gTextDisplay}.t tag ranges activeLine]
}

########################################################################
#                          Finding class documentation
########################################################################
# Show the comments associated with a given class.
# signature.  Create a piece of text for signature+routines and
# display the text in the text window.
# Private to text window
proc textShowClassDocs { } {
    global gCurNodeName
    global gTextDisplay
    global gCurFile
    # gCurFile is set to "" to indicate no file is involved
    

    set cdef [tkkit_cb getClassInfo ${gCurNodeName}] 
    inform "Obtained class info. Processing..."

    
    set features [lindex $cdef 2]
    set classFileName [lindex ${cdef} 0]
    set classLineoffset [lindex ${cdef} 1] 

    featureListInit ${features}
    set isAbs [satherIsAbstract ${gCurNodeName}]
    set classComment \
	    [satherGetCommentAfter $classFileName [expr $classLineoffset - 1] ${isAbs} ]


    set featureTxt [featureListGetDoc ${isAbs}]
    set txt "${classComment}\n${featureTxt}"
    set gCurFile ""
    textLoadText "Doc for ${gCurNodeName}" ${txt} false
    textGotoLine 1
    inform "Displaying documented form of class ${gCurNodeName}"
    . config -cursor arrow
    update
}

