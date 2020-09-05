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

#  See Help/graph for info on node and edge tags


# Indicates whether the initial graph should be of a fixed size
set gFixedSizegraphUpdate true

# Determines whether the graph is laid out horizontally or vertically
set gHorizontalGraph false

# Whether the browser will look for a layout file when viewing a graph
set gGraphLookForLayout true

# gGraphUpdate  True if graph should not be updated any further
set gGraphUpdate 1

# Experimental. Also need to change the node type to record
# in dotty-header for this to work
set gDottyShowFeatures false

# These variables have counterparts in browser.sa See updateSatherVars
# which commmunicates the values set here to the sather part
set gShowConcrete 1
set gShowAbstract 1
set gShowModule 1
set gShowConcreteEdges 1
set gShowAbstractEdges 1
set gShowModuleEdges 1
set gShowSingletons 1


# Private globals
# This holds the previous value of the of the graph scale so that
# we can determine the scale ratio whenever the scale changes
set gGraphOldScale 1.0      

# Scale at which the graph fits entirely in the graph. Zero, don't center 
set gGraphFitsScale  10     

########################################################################
#                          CANVAS/Graph Related
########################################################################
proc graphCreate { xsz ysz viewxsz viewysz } {
    # xsz and ysz indicate the boundary of the actual canvas
    # and viewxsz and viewysz are the boundaries of the window view
    global gDeepBackgroundColor
    global gBackgroundColor gGraphBackgroundColor
    global gIsColor
    global gGraphFrame gClassFreeze
    global gGraphButtonFont

    # Create a scrollable canvas under the frame topName
    # The items will be topName.c, topName.vscroll and topName.hscroll
    set topName ${gGraphFrame}
    frame ${topName}.bottomBar
    button ${topName}.bottomBar.featureToggle -text "Features" \
	    -command "graphToggleAllFeatures" -relief raised \
	    -font ${gGraphButtonFont} -bg ${gGraphBackgroundColor}
    button ${topName}.bottomBar.classFreeze -text "Freeze" \
	    -command "graphToggleClassFreeze" -relief raised \
	    -font ${gGraphButtonFont}  -bg ${gGraphBackgroundColor}

    canvas ${topName}.c -relief raised -height $viewysz -width $viewxsz -bd 2\
      -scrollregion [list 0 0 $xsz $ysz] -yscrollcommand "${topName}.vscroll set"\
      -xscrollcommand "${topName}.bottomBar.hscroll set" -bg $gGraphBackgroundColor 
    scrollbar ${topName}.vscroll -relief flat \
	    -command  "${topName}.c yview" -bg $gGraphBackgroundColor
    scrollbar ${topName}.bottomBar.hscroll -relief flat -orient horiz \
	    -command  "${topName}.c xview" -bg $gGraphBackgroundColor
    scale ${topName}.scale -orient vertical -command "graphZoom" \
	    -from 100 -to 10 -bg $gGraphBackgroundColor -relief raised

    ${topName}.scale configure -showvalue 0
    ${topName}.scale set 100

    pack ${topName}.scale -side left -fill y -anchor w -padx 5 
    frame ${topName}.separator -width 10 -relief flat -bg $gDeepBackgroundColor
    pack ${topName}.separator -side left -fill y

    pack ${topName}.vscroll -side right -fill y
    pack ${topName}.bottomBar -side bottom -fill x
    pack ${topName}.bottomBar.featureToggle -side right
    pack ${topName}.bottomBar.classFreeze -side right
    pack ${topName}.bottomBar.hscroll -side bottom -fill x



    pack ${topName}.c -side top -fill both 

    bind ${topName}  <Any-Enter> {inform "LEFT button: Select class   MIDDLE drag: Move class  RIGHT: Toggle View Features"}

    ${topName}.c bind node <1> "graphSelectNodeBind  %x %y"
    ${topName}.c bind node <2> "graphNodeBeginMoveBind  ${topName} %x %y"
    bind ${topName}.c <Shift-2> "graphRecenter"
    ${topName}.c bind node <B2-Motion> \
	    "graphNodeContinueMoveBind ${topName} %x %y "
    ${topName}.c bind node <B2-ButtonRelease> \
	    "graphNodeEndMoveBind ${topName} %x %y "

    #  place ${topName}.c  -relwidth 1.0 -relheight 1.0
}

proc graphUpdate { } {
    # Perform the graph update and then look for a layout file
    # This function was separated out so that graphSaveLayout
    # can save a call to graphUpdateNoLayout and avoid recursively
    # looking for a layout
    global gGraphLookForLayout gCurNodeName
    graphUpdateNoLayout
    if {${gGraphLookForLayout}} {
	set cn [satherCleanName ${gCurNodeName}]
	if [file exists "${cn}.lay"] {
	    inform "Reading graph layout"
	    graphReadLayout 
	}
    }
}

proc graphUpdateNoLayout { } {
    # Initialize the graph from data from the sather end.
    # If the size is fixed, then the graph will be made to fit in the
    # current view.
    global gGraphFrame gClassListFrame
    global gGraphUpdate
    global gCurNodeName
    global gHorizontalGraph
    global gFixedSizegraphUpdate 
    global gGraphFitsScale
    global gGraphLookForLayout

    if {!${gGraphUpdate}} { return    }
    graphClear
    inform "Updating graph"
    update
    set layoutInfo [tkkit_cb getRestrictedLayout " "]
    graphSetScale 100
    #   debugPuts "browserGraph.tcl" "Layout ${layoutInfo}"
    set graphSize [lindex $layoutInfo 0]
    # Class locations  
    set classes [lindex $layoutInfo 1]  
    # Edge information
    set edges [lindex $layoutInfo 2]    
    set singletons [lindex $layoutInfo 3]    
    set width [lindex ${graphSize} 1]
    set height [lindex ${graphSize} 0]
    set newScale 70
    if { $gFixedSizegraphUpdate } {
	# Graph must fit in window
	if { $width > 3 } {  set newScale  70   }
	if { $width > 7 } { set newScale 50   }
	if { $width > 15 } { set newScale 30  }
	if { $width > 17 } { set newScale 20 }
	if { $width > 20 } { set newScale 10 }
	set xFactor [expr 800.0 / $newScale  ]
	set yFactor [expr 250.0 / $newScale ]
    } else {
	set xFactor [expr ${width}]
	set yFactor [expr ${height}]
    }
    set gGraphFitsScale ${newScale}
    foreach class $classes {
	set className [lindex ${class} 0]
	if {$gHorizontalGraph} {
	    set rawX  [lindex ${class} 2]
	    set rawY  [lindex ${class} 1]
	    set classX [expr ${rawX} * $xFactor +50.0 ]
	    set classY [expr ${rawY} * $yFactor + 20.0 ]
	} else {
	    set rawX  [lindex ${class} 1]
	    set rawY  [lindex ${class} 2]
	    set classX [expr ${rawX} * $xFactor ]
	    set classY [expr ${rawY} * $yFactor + 60.0 ]
	}

	#puts "rawX: $rawX rawY $rawY ClassX: $classX classY $classY"
	set fullNodeText $className
	if { [satherIsModule $className] } {
	    set nodeText [satherGetModuleName $className]
	} else {
	    set nodeText $className
	}
	graphMakeTextNodeAt ${classX} ${classY} ${nodeText} ${fullNodeText}
    }
    foreach edge $edges {
	graphConnect  ${gGraphFrame} [lindex $edge 0] [lindex $edge 1]
    }

    ${gGraphFrame}.c addtag selected withtag ${gCurNodeName}
    graphSetScale ${newScale}
    graphHighlight
    ${gGraphFrame}.c bind node <Enter> {
	set tags [${gGraphFrame}.c gettags current]
	set nodeName [lindex $tags 0]
	inform "Over node ${nodeName}"
    }
    ${gGraphFrame}.c bind node <Leave> { inform ""  }
    #    ${gGraphFrame}.c itemconfigure abstract -fill $gSubtypeColor
    graphSetFonts
    inform "Done with graph initialization"
}


proc graphUpdateSatherVars { } {
    # Update the sather variables that correspond to tcl variables.
    # This should be called whenever the tcl variables change
    global gShowConcrete gShowAbstract gShowModule
    global gShowConcreteEdges  gShowAbstractEdges  gShowModuleEdges
    global gShowSingletons
    tkkit_cb setBoolVar gShowConcrete ${gShowConcrete} 
    tkkit_cb setBoolVar gShowAbstract ${gShowAbstract} 
    tkkit_cb setBoolVar gShowModule ${gShowModule} 
    tkkit_cb setBoolVar gShowConcreteEdges ${gShowConcreteEdges} 
    tkkit_cb setBoolVar gShowAbstractEdges ${gShowAbstractEdges} 
    tkkit_cb setBoolVar gShowModuleEdges ${gShowModuleEdges} 
    tkkit_cb setBoolVar gShowSingletons ${gShowSingletons} 
}

proc graphSaveState { f } {
    global gHorizontalGraph
    global gShowConcrete gShowAbstract gShowModule 
    global gShowConcreteEdges gShowAbstractEdges gShowModuleEdges
    global gShowSingletons 
    global gGraphUpdate 
    global gGraphLookForLayout

    puts $f "set gGraphUpdate ${gGraphUpdate}"
    puts $f "set gGraphLookForLayout ${gGraphLookForLayout}"
    puts $f "set gHorizontalGraph ${gHorizontalGraph}"
    puts $f "set gShowConcrete ${gShowConcrete}"
    puts $f "set gShowAbstract ${gShowAbstract}"
    puts $f "set gShowModule ${gShowModule}"
    puts $f "set gShowConcreteEdges ${gShowConcreteEdges}"
    puts $f "set gShowAbstractEdges ${gShowAbstractEdges}"
    puts $f "set gShowModuleEdges ${gShowModuleEdges}"
    puts $f "set gShowSingletons ${gShowSingletons}"
}

proc graphAddMenu { cm } {
    global gGraphLookForLayout
    
    ${cm} add command -label "View Features" -command graphToggleAllFeatures

    $cm add separator
    $cm add check -label "Show Concrete Classes" \
	    -command {tkkit_cb setBoolVar gShowConcrete $gShowConcrete ; \
	    graphUpdate }  \
	    -variable gShowConcrete -onvalue 1 -offvalue 0
    $cm add check -label "  Hide TEST_* Classes" \
	    -command "toggleNoShowTestClasses" \
	    -variable gNoShow_Test_Classes -onvalue 1 -offvalue 0
    $cm add check -label "  Hide C_* Classes"  -command "toggleNoShowCClasses"\
	    -variable gNoShow_C_Classes -onvalue 1 -offvalue 0
    $cm add check -label "Show Abstract Classes" \
    -command {tkkit_cb setBoolVar gShowAbstract $gShowAbstract ; graphUpdate }\
    -variable gShowAbstract -onvalue 1 -offvalue 0
    $cm add check -label "Show Modules" \
	    -command {tkkit_cb setBoolVar gShowModule $gShowModule ;\
	    graphUpdate }  \
	    -variable gShowModule -onvalue 1 -offvalue 0
    
         debugPuts "browserGraph.tcl" "Made part graph menu"
    $cm add check -label "Show Include Edges" \
      -command {tkkit_cb setBoolVar gShowConcreteEdges $gShowConcreteEdges ; \
               graphUpdate }\
      -variable gShowConcreteEdges -onvalue 1 -offvalue 0
    $cm add check -label "Show Subtype Edges" \
      -command {tkkit_cb setBoolVar gShowAbstractEdges $gShowAbstractEdges ; \
                graphUpdate }\
      -variable gShowAbstractEdges -onvalue 1 -offvalue 0
    $cm add check -label "Show Module Edges" \
      -command {tkkit_cb setBoolVar gShowModuleEdges $gShowModuleEdges ; \
                graphUpdate }\
      -variable gShowModuleEdges -onvalue 1 -offvalue 0
    $cm add separator
    $cm add check -label "Show Singleton Nodes" \
	    -command {tkkit_cb setBoolVar gShowSingletons ${gShowSingletons};\
	    graphUpdate }\
	    -variable gShowSingletons -onvalue 1 -offvalue 0
    $cm add check -label "Layout Left to Right "  -command graphUpdate \
	    -variable gHorizontalGraph -onvalue "true" -offvalue "false"
    $cm add check -label "Try to Fit Graph"  -command graphUpdate \
	    -variable gFixedSizegraphUpdate -onvalue "true" -offvalue "false"
    $cm add separator
    $cm add check -label "Update Graph"  \
	    -variable gGraphUpdate -onvalue "1" -offvalue "0"
    $cm add separator
    $cm add check -label "Freeze Class Focus" -command graphToggleClassFreeze \
	    -variable gClassFreeze

    # $cm add separator
    # ${cm} add command -label "Merge Partials" -command graphMergePartials

    $cm add separator
    $cm add command -label "Save Layout" -command "graphSaveLayout"
    $cm add command -label "Read Layout" -command "graphReadLayout"
    $cm add check -label "Always Read Layout" \
	    -variable gGraphLookForLayout -onvalue "true" -offvalue "false"\
	    -command graphUpdate

    $cm add separator
    $cm add command -label "Save as Postscript (eps)" \
	    -command "graphDumpPS false"
    $cm add command -label "Print Graph " \
	    -command "graphDumpPS true"
    $cm add separator
    $cm add command -label "Dot: Save Graph" \
	    -command "graphDumpCurGraphForDot false"
    $cm add command -label "Dot: Print Postscript" \
	    -command "graphPrintCurGraphForDot false"
    $cm add command -label "Dot: Save Complete Graph" \
	    -command "graphDumpFullGraphForDot"
    $cm add command -label "Dotty: Invoke on Graph" \
	    -command "graphDumpCurGraphForDot true"

}

proc graphToggleClassFreeze { } {
    global gFreezeClass
    global gGraphFrame
    if { ${gFreezeClass} } {
	inform "Unfreezing: You can now change the focus to another class"
	${gGraphFrame}.bottomBar.classFreeze configure -text "Freeze"
	set gFreezeClass 0
    } else {
	inform "Freezing: The focus is fixed to the current classes"
	${gGraphFrame}.bottomBar.classFreeze configure  -text "Unfreeze"
	set gFreezeClass 1
    }
}

proc graphToggleAllFeatures { } {
   # Show features of all currently visible nodes
    global gGraphFrame
    set curNodes [${gGraphFrame}.c find withtag "node"]
    foreach node ${curNodes} {
	set tags [${gGraphFrame}.c gettags ${node}]
	set nodeName [lindex ${tags} 0]
	graphToggleFeatures ${nodeName}
    }
}

# Private
proc graphToggleFeatures { nodeText } {
    # Display the features of the selected node
    global gGraphFrame
    if { [satherIsModule ${nodeText}] } {
	return
    }
    # Find the location of the node
    set curFeat [${gGraphFrame}.c find withtag "featureText${nodeText}"]
    if { [ llength ${curFeat} ] > 0 } {
	${gGraphFrame}.c delete "featureText${nodeText}"
	inform "Deleting expansion of: ${nodeText}"
    } else {
	set nodes [${gGraphFrame}.c find withtag "node${nodeText}"]
	foreach node ${nodes} {
	    # Find the features
	    graphDisplayFeatureTextFor ${nodeText}
	    inform "Creating expansion of: ${nodeText}"
	}
    }
}

# Private
proc graphDisplayFeatureTextFor { className } {
    # See Help/featureList for documentation of the feature list structure
    global gGraphFrame gItalicFontSizes
    set featureList [satherGetFeatureElementsFor ${className}]
    set resultTxt ""
    foreach feature ${featureList} {
	set featureName [string trim [lindex ${feature} 0]]
	set resultTxt "${resultTxt}\n${featureName}"
    }
    set coods [${gGraphFrame}.c coords "node${className}"]
    set coodX [lindex ${coods} 0]
    set coodY [lindex ${coods} 1]
    ${gGraphFrame}.c  create text ${coodX} ${coodY} -text  ${resultTxt} \
	    -tags "featureText${className} featureText" -anchor "nw" 
    graphSetFonts
    update

}

######################### Graph Construction Procedures ###################
# Private
proc graphMakeTextNodeAt { classX classY nodeText fullNodeText } {
    #  See Help/graph for info on node and edge tags
    # Add the text object centered at x, y with text nodeText
    # Add the nodeText as the first tag of the node
    global gGraphFrame
    global gIsColor
    global gSubtypeColor

    set c [string index ${nodeText} 0]
    if { $c == "\$" } {
	if { $gIsColor } {
	    set thecolor $gSubtypeColor
	} else {
	    set thecolor "black"
	}
	set tag "${fullNodeText} node node${fullNodeText} ${nodeText} abstract"
	${gGraphFrame}.c  create text ${classX} ${classY}\
		-text ${nodeText} -fill ${thecolor} -anchor center \
		-tags ${tag}
	${gGraphFrame}.c  bind ${nodeText} <B3-ButtonRelease> "graphToggleFeatures \{${nodeText}\} ; break"
    } elseif { [regexp {[A-Za-z0-9./_]*.module} $fullNodeText ] } {
	${gGraphFrame}.c  create text ${classX} ${classY}\
		-text ${nodeText}  -anchor center \
		-tags "${fullNodeText} node node${fullNodeText} ${nodeText} module"

    } else {
	${gGraphFrame}.c  create text ${classX} ${classY}\
		-text ${nodeText} -anchor center \
		-tags "${fullNodeText} node node${fullNodeText} ${nodeText} concrete"
	${gGraphFrame}.c  bind ${nodeText} <B3-ButtonRelease> "graphToggleFeatures \{${nodeText}\}; break"
    }


}



# Private
proc graphConnect { frameName srcName destName } {
    #  See Help/graph for info on node and edge tags
    # Connect two nodes in the graph frame
    global gHorizontalGraph

    set srcCood [${frameName}.c coords ${srcName}]	
    set destCood [${frameName}.c coords ${destName} ]

    set sl  [llength ${srcCood}]
    set dl  [llength ${destCood}]
    # If either source or destination do not exist, return.
    if { ${sl}  == 0 } {
	return
    }
    if { $dl == 0 } {
	return
    }
    set  srcx [lindex $srcCood 0]
    set  srcy [lindex $srcCood 1]	
    set  destx [lindex $destCood 0]
    set desty [lindex $destCood 1]
    set yoff 10
    set xoff 50
    if { $gHorizontalGraph } {
	set srcBB [${frameName}.c bbox node${srcName}]
	set destBB [${frameName}.c bbox node${destName}]
	set sty [expr $srcy]
	set endy [expr $desty]
	if { $srcx > $destx } {
	    # A left to right arrow
	    set stx [lindex $srcBB 0]
	    set endx [lindex $destBB 2]
	} else {
	    set stx [lindex $srcBB 2]
	    set endx [lindex $destBB 0]
	}
    } else {
	# Vertical graph - does not need bounding boxes
	set stx [expr $srcx]
	set endx [expr $destx]
	if { $srcy > $desty } {
	    # An upward arrow
	    set sty [expr $srcy - $yoff]
	    set endy [expr $desty + $yoff]
	} else {
	    set sty [expr $srcy + $yoff]
	    set endy [expr $desty - $yoff]
	}
    }
    set srcTags [${frameName}.c gettags ${srcName}]
    set srcType [lindex $srcTags 4]
    if { $srcType == "abstract" } {
	${frameName}.c create line ${stx} ${sty} ${endx} ${endy} \
		-arrow last -width 1.0 \
		-tags "${srcName} ${destName} arrow${srcName} \
		arrow${destName} arrow subtype"
    } elseif { $srcType == "module" } {
	${frameName}.c create line ${stx} ${sty} ${endx} ${endy} \
		-arrow last -width 1.0 \
		-tags "${srcName} ${destName} arrow${srcName} \
		arrow${destName} arrow moduleContains "

    } else {
	${frameName}.c create line ${stx} ${sty} ${endx} ${endy} \
		-arrow last -width 1.0 \
		-tags "${srcName} ${destName} arrow${srcName} \
		arrow${destName} arrow include"

    }

}

######################### Graph Related Procedures ###################
# Private
proc graphZoom { ignored } {
    # Zoom the graph around the origin, and recenter/rezero if necessary
    # Called by the scale widget and when manually settinng the scale 
    # using graphSetScale
    global gGraphFrame
    global gCurNodeName
    global gGraphOldScale
    global gGraphFitsScale
    
    set scaleFac [${gGraphFrame}.scale get]
    set desiredScale [expr $scaleFac/100.0]
    set scaleFactor [expr $desiredScale/$gGraphOldScale]
    set gGraphOldScale $desiredScale
    graphSetFonts
    # Find coods of last selected item
    ${gGraphFrame}.c scale node 0.5 0.5 $scaleFactor $scaleFactor
    ${gGraphFrame}.c scale featureText 0.5 0.5 $scaleFactor $scaleFactor
    ${gGraphFrame}.c scale arrow 0.5 0.5 $scaleFactor $scaleFactor
    ${gGraphFrame}.c scale selectRect 0.5 0.5 $scaleFactor $scaleFactor
    # Find coods of last selected item
    set scl [${gGraphFrame}.scale get]
    if { ${scl} <= ${gGraphFitsScale}} {
	graphReZero
    } else {
	graphRecenter
    }
    return
}

# Private
proc graphSetFonts { } {
    # Set the fonts of nodes on the graph appropriately, according to
    # the current graph scale and making use of the tables 
    # gFontSizes and gBoldFontSizes.
    global gGraphFrame
    global gFontSizes
    global gBoldFontSizes
    global gItalicFontSizes
    global gIsColor 

    set scaleFac [${gGraphFrame}.scale get]
    set desiredScale [expr $scaleFac/100.0]
    set fontSz   [expr $desiredScale * 14]
    set fs  [format "%.0f" $fontSz]
    if {${fs} > 4 } {
	set smallFs [expr ${fs} - 1]
    } else {
	set smallFs ${fs}
    }
    set font $gFontSizes($fs)
    set boldFont $gBoldFontSizes($fs)
    set italicFont $gItalicFontSizes($fs)
    set smallItalicFont $gItalicFontSizes($smallFs)
    ${gGraphFrame}.c itemconfigure node -font ${font}
    ${gGraphFrame}.c itemconfigure featureText -font ${smallItalicFont}
    ${gGraphFrame}.c itemconfigure abstract -font ${boldFont}
    ${gGraphFrame}.c itemconfigure module -font ${italicFont}
}

# Private - 
# Set the scale of the graph. Called in graphUpdate to directly set
# the scale to a particular value
proc graphSetScale { scaleValue } {
    global gGraphFrame
    ${gGraphFrame}.scale set ${scaleValue}
    graphZoom "ignored"
}

# Private Delete all nodes and edges
proc graphClear { } {
    global gGraphFrame
    ${gGraphFrame}.c delete node
    ${gGraphFrame}.c delete arrow
    ${gGraphFrame}.c delete featureText
}

# Private - resets the graph to upper left corner
proc graphReZero { } {
    global gGraphFrame
    ${gGraphFrame}.c yview moveto 0.0
    ${gGraphFrame}.c xview moveto 0.0
}

# Private - recenters around selected node
proc graphRecenter { } {
    # GraphRecenter the graph around the current class
    # What a bloody pain! There must be a simpler way to do this.

    global gGraphFrame
    global gCurNodeName

    set sel [${gGraphFrame}.c find withtag ${gCurNodeName}]
    # If nothing has been selected, do nothing
    if { $sel == "" } { return    }
    # Find the coods of the current node
    set coods [${gGraphFrame}.c coords ${gCurNodeName}]

    set htl [${gGraphFrame}.c configure -height ]
    set ht [lindex ${htl} 4]
    set wdl [${gGraphFrame}.c configure -width ]
    set wd [lindex ${wdl} 4]
    set scregl [${gGraphFrame}.c configure -scrollregion]
    # This assumes that the screg starts at 0,0
    debugPuts "browserGraph.tcl" "Scrolling params ${scregl} $ht $wd "
    set screg [lindex ${scregl} 4]
    # Full canvas wd
    set actwd [lindex ${screg} 2]
    set actht [lindex ${screg} 3]

    # Strip centimeter (c) suffix
    set wdint [string range $wd 0 [expr ([string length $wd] - 2)]]
    set htint [string range $ht 0 [expr ( [string length $ht] - 2)]]
    # Centimeters to pixels?
    set wdint [expr $wdint * 80.0 ]
    set htint [expr $htint * 40.0 ]
     # set to center of display
    set cx [expr [lindex $coods 0] - ($wdint / 2.0)]
    set cy [expr [lindex $coods 1] - ($htint / 2.0)]
    # Convert to integers
    set icx [format %3.0f $cx]
    set icy [format %3.0f $cy]

    # Strip centimeter (c) suffix
    set actwdint [string range $actwd 0 [expr ([string length $actwd] - 2)]]
    set acthtint [string range $actht 0 [expr ( [string length $actht] - 2)]]
    # Centimeters to pixels?
    set actwdint [expr $actwdint * 80.0 ]
    set acthtint [expr $acthtint * 40.0 ]

    # Divide by scrollincrement
    ${gGraphFrame}.c yview moveto [expr ($icy / $actwdint )]
    ${gGraphFrame}.c xview moveto [expr ($icx / $acthtint )]
}

########################### Nodes ############################

###############################################################
#              Code to move around nodes
#  Moving a node begins when the correct binding fires
#  graphNodeBeginMoveBind ${graphFrame} %x %y
###############################################################
# Global variables that hold the coods of the object currently being moved
set gMovingNodePos(lastX) 0
set gMovingNodePos(lastY) 0
set gMovingNodeName ""
# Private
proc graphNodeBeginMoveBind { graphFrame xpos ypos } {
    global gMovingNodePos
    global gMovingNodeName
    # Delete the previously marked selection
    ${graphFrame}.c dtag selected
    # Name the current object the selected object
    ${graphFrame}.c addtag selected withtag current
    # Get the node name and the corresponding box
    set tags [${graphFrame}.c gettags selected]
    set gMovingNodeName [lindex $tags 0]
    # Move over other objects
    ${graphFrame}.c raise selected
    # Set the moving coods to those of the current (selected) object
    set gMovingNodePos(lastX) $xpos
    set gMovingNodePos(lastY) $ypos

}

# Private
proc graphNodeContinueMoveBind { graphFrame xpos ypos } {
    # As movement continues, update the position and redraw object
    # Don't redraw arrows
    global gMovingNodePos
    ${graphFrame}.c move selected \
	    [expr $xpos-$gMovingNodePos(lastX)] \
	    [expr $ypos-$gMovingNodePos(lastY)]
    set gMovingNodePos(lastX) $xpos
    set gMovingNodePos(lastY) $ypos
}

# Private
proc graphNodeEndMoveBind { graphFrame xpos ypos } {
    # At the end of the movement, lower the object, and move arrows
    # that were connected to it
    global gMovingNodePos
    ${graphFrame}.c lower selected
    # Get the first tag of the node, which is its name
    set itemTaggedCurrent [${graphFrame}.c find withtag current ]
    set tags [${graphFrame}.c gettags $itemTaggedCurrent]
    set nodeName [lindex $tags 0]
    set curFeat [${graphFrame}.c find withtag "featureText${nodeName}"]
    if { [ llength ${curFeat} ] > 0 } {
	graphToggleFeatures ${nodeName}
	graphToggleFeatures ${nodeName}
    }
    graphMoveArrowsTouching ${graphFrame} ${nodeName}
    ${graphFrame}.c dtag selected
    graphHighlight
}

#Private
proc graphMoveArrowsTouching { graphFrame nodeName } {
    #  Code to move arrows to follow a node
    # Find all arrows that have the moved node as the source or destination
    # Find the actual line and redraw it, using the node names 
    set touchingArrows [${graphFrame}.c find withtag "arrow${nodeName}"]
    foreach arrow ${touchingArrows} {
	# Arrow is the tagId of the arrow
	set tags [${graphFrame}.c gettags ${arrow}]
	graphConnect ${graphFrame} [lindex $tags 0] [lindex $tags 1] 
	${graphFrame}.c delete ${arrow}
    }
    graphHighlight
}
     debugPuts "browserGraph.tcl" "Done with node movement"

proc graphMoveNodeTo { nodeName x y } {
    global gGraphFrame
    set node [${gGraphFrame}.c find withtag "node${nodeName}"]
    ${gGraphFrame}.c coords ${node} ${x} ${y}
    set node [${gGraphFrame}.c find withtag "featureText${nodeName}"]
    ${gGraphFrame}.c coords ${node} ${x} ${y}
    graphMoveArrowsTouching ${gGraphFrame} ${nodeName}
}

proc graphMergePartials { } {
    # Move all concrete nodes with the same name as an abstract node
    # (except for the dollar) to the position of the abstract node
    global gGraphFrame
    inform "Starting to merge partials"
    update 
    startWait
    set ids [${gGraphFrame}.c  find withtag node]
    foreach id ${ids} {
	# Prefix a dollar to each name and see whether any other
	# node name is the same
        set tags [${gGraphFrame}.c gettags $id]
	set nm  [lindex $tags 0]	
	set coods [${gGraphFrame}.c coords ${id} ]
	set x  [lindex $coods 0]
	set y [lindex $coods 1]
	# Look through all concrete classes 
	if { [satherIsAbstract ${nm}] } {
	} elseif  { [ satherIsModule ${nm} ] } {
	} else {
	    # Must be concrete. Search for abstract lookalike
	    set mids [${gGraphFrame}.c  find withtag node]
	    set matchname "\$${nm}"
	    foreach mid ${mids} {
		set mtags [${gGraphFrame}.c gettags $mid]
		set mnm  [lindex $mtags 0]	
		if { ${mnm} == ${matchname} } {
		    set mcoods [${gGraphFrame}.c coords ${mid} ]
		    set mx  [lindex $mcoods 0]
		    set my [lindex $mcoods 1]
		    set moveString \
			 [format "graphMoveNodeTo  ${nm} %f %f " ${mx} ${my}]
		    eval ${moveString}
		    # graphMoveNodeTo ${id} ${mx} ${my}
		}
	    }
	}
    }
    inform "Finishing partial merge"
    update 
    endWait
}


# Private
proc graphHighlight { } {
    # Highlight the edges and nodes in the graph either with color or shading
    global gIsColor
    global gGraphFrame
    global gCurNodeName

    global gSubtypeColor gModuleColor gIncludeColor gFeatureListTitleColor
    if {$gIsColor} {
	# Color nodes
	${gGraphFrame}.c itemconfigure node -fill black
	${gGraphFrame}.c itemconfigure abstract -fill $gSubtypeColor
	${gGraphFrame}.c itemconfigure module -fill $gModuleColor
	# Color edges
	${gGraphFrame}.c itemconfigure subtype -fill $gSubtypeColor
	${gGraphFrame}.c itemconfigure moduleContains -fill $gModuleColor
	${gGraphFrame}.c itemconfigure include -fill $gIncludeColor
	# Highlight the selected node
	set sel [${gGraphFrame}.c find withtag node${gCurNodeName}]
	${gGraphFrame}.c itemconfigure ${sel} -fill $gFeatureListTitleColor 
    } else {
	${gGraphFrame}.c itemconfigure subtype -width 2 
	${gGraphFrame}.c itemconfigure moduleContains -width 2 -stipple gray50
	${gGraphFrame}.c itemconfigure include -width 1
	${gGraphFrame}.c itemconfigure node -fill black
    }
}

# Private
proc graphSelectNodeBind { xpos ypos } {
    # Handle clicking on a node in the graph
    # Find and highlight the appropriate entry in the list of classes,
    # thereby going to the correct file/class location.
    global gGraphFrame 
    set sel [${gGraphFrame}.c find withtag current]
    set ctags [${gGraphFrame}.c gettags $sel]
    set nm [lindex $ctags 0]
    updateForClass ${nm} false

}

proc graphDumpPS { shouldPrint } {
    global gGraphFrame
    global gCurNodeName
    global gFontSizes
    global gItalicFontSizes
    global gBoldFontSizes
    global gBolditalicFontSizes
    set inList  {1 2 3 4 5 6 7 8 9 10 11 12 13 14 } 
    set outList {4 4 4 4 5 6 7 8 9 10 13 13 13 14 }
    foreach i ${inList} {
	set resSize [lindex $outList [expr $i -1 ]]
	set printerFonts($gFontSizes($i)) [list helvetica ${resSize}]
	set printerFonts($gBoldFontSizes($i)) [list helvetica-bold ${resSize}]
	set printerFonts($gItalicFontSizes($i)) [list  helvetica-italic ${resSize}]
	set printerFonts($gBolditalicFontSizes($i)) [list helvetica ${resSize}]
    }
    if {${shouldPrint}} {
	set fileNm "/tmp/browserGraphDump.ps"
	${gGraphFrame}.c postscript  -fontmap printerFonts -file "${fileNm}"
	exec lpr ${fileNm} >& /dev/null &
    } else {
	set userFileName  [promptWriteFileFromUser "${gCurNodeName}.ps" ]
	if { [lindex ${userFileName} 0 ]} {
	    set fileNm [lindex  ${userFileName} 1]
	    ${gGraphFrame}.c postscript  -fontmap printerFonts -file "${fileNm}"
	} else {
	    return
	}
    }
}

# Private
proc graphReadLayout { } {
    global gGraphFrame
    global gCurNodeName
    global gGraphTmpFileName

    set cleanName [satherCleanName ${gCurNodeName}]
    set fileName "${cleanName}.lay"
    if [file exists ${fileName}] {
    } else {
	set userFileName  [promptReadFileFromUser "${cleanName}.lay"]
	if { [lindex ${userFileName} 0 ]} {
	    set fileName [lindex  ${userFileName} 1]
	} else {
	    return
	}
    }
    loadFilePlain ${fileName}
}

# private
proc graphSaveLayout { } {
    global gGraphFrame
    global gCurNodeName
    set cleanName [satherCleanName ${gCurNodeName}]
    set userFileName  [promptWriteFileFromUser "${cleanName}.lay"]
    if { [lindex ${userFileName} 0 ]} {
	set fileName [lindex  ${userFileName} 1]
    } else {
	return
    }
    set curScaleShows [${gGraphFrame}.scale get]
    
    inform "Dumping positions to ${fileName}"
    set posFile [open ${fileName} w ]
    puts $posFile "# Class graph layout description saved by the browser."
    puts $posFile "# See Sather/Browser/Tcl/browserGraph.tcl proc graphSaveLayout"
    puts $posFile "graphSetScale ${curScaleShows}"
    #graphSaveState ${posFile}
    #puts ${posFile} "graphUpdateSatherVars"
    #puts ${posFile} "graphUpdateNoLayout"
    set ids [${gGraphFrame}.c  find withtag node]
    set line 0
    foreach id ${ids} {
	incr line
        set tags [${gGraphFrame}.c gettags $id]
	set nm  [lindex $tags 0]	
	set coods [${gGraphFrame}.c coords ${id} ]
	set x  [lindex $coods 0]
	set y [lindex $coods 1]
	if { [satherIsAbstract ${nm}] } {
	    set fs [format "graphMoveNodeTo  \\${nm} %f %f " \
		    ${x} ${y}]
	} elseif  { [ satherIsModule ${nm} ] } {
	    set fs [format "graphMoveNodeTo  ${nm} %f %f " \
		    ${x} ${y}]
	} else {
	    set fs [format "graphMoveNodeTo  ${nm} %f %f " \
		    ${x} ${y}]
	    
	}
	# puts $posFile "puts \"Line ${line}\""
	puts $posFile $fs
     }
     close $posFile
 }

# Generates a version of the current graph in a form that is usable by "dot"
proc graphDumpCurGraphForDot { runDot } {
    global gCurClassName
    set layoutInfo [tkkit_cb getRestrictedLayout " "]
    graphDumpDotGraph  ${layoutInfo} ${runDot}
	
}

proc graphPrintCurGraphForDot { runDot } {
    global gCurClassName
    set layoutInfo [tkkit_cb getRestrictedLayout " "]
    set fileName [graphDumpDotGraph  ${layoutInfo} ${runDot}]
    inform "Generating postscript...."
    exec dot -Tps -odot-graph.ps ${fileName} >& /dev/null 
    inform "Printing dot-graph.ps"
    exec lpr dot-graph.ps >& /dev/null 
    inform "Done printing dotty postscript graph."
}

# Generates a version of the complete graph of classes in a form
# suitable for use by the graph layout program from AT&T called "dot"
# You can modify this to generate any format you please ...
proc graphDumpFullGraphForDot { } {
    set fulg [tkkit_cb getFullLayout ]
    graphDumpDotGraph ${fulg} false
}


proc graphDumpDotGraph { g runDot } {
    global gCurNodeName
    global gHome
    
    set cleanName [satherCleanName ${gCurNodeName}]
    if { ${runDot} } {
	set fileNm "/tmp/sather-browser${cleanName}.dot"
    } else {
	set userFileName  [promptWriteFileFromUser "${cleanName}.dot" ]
	if { [lindex ${userFileName} 0 ] } {
	    set fileNm [lindex  ${userFileName} 1]
	} else {
	    return
	}
    }
    set fl [open ${fileNm} w]
    set header [open "${gHome}/Browser/Tcl/dotty-header" r]
    set headerStr [read $header]
    close ${header}
    puts $fl ${headerStr}
    set edges [lindex ${g} 2]
    foreach edge ${edges} {
	set src [lindex ${edge} 0]
	set dest [lindex ${edge} 1]

	dottyPutNode $fl ${src}
	dottyPutNode $fl ${dest}
    }
    foreach edge ${edges} {
	set src [lindex ${edge} 0]
	set dest [lindex ${edge} 1]
	dottyPutEdge $fl  ${src}  ${dest}
    }
    puts ${fl} "\}"
    close $fl
    update
    if { ${runDot} } {
	exec dotty ${fileNm} >& /dev/null &
    }
    return ${fileNm}
}

proc dottyPutNode { fn name } {
    global gDottyShowFeatures

    set cn [satherCleanName ${name}]
    if { [satherIsModule ${name}] } {
	set name [satherGetModuleName ${name}]
	puts $fn " \"${cn}\"  \[ \n label = \"${name}\" \n \];"
    } else {
	if { $gDottyShowFeatures } {
	    set classInfo [tkkit_cb getClassInfo ${name}]
	    set classFileName [lindex ${classInfo} 0]
	    set classLineOffset [lindex ${classInfo} 1]
	    set classLine [expr ${classLineOffset} - 1 ]
	    set classEndLine [findClassEnd ${name} ${classFileName} ${classLineOffset} ]
	    set featureList [lindex ${classInfo} 2]
	    set name [dottyCleanName ${name}]
	    puts -nonewline $fn " \"${cn}\"  \[ \n \n label = \"{${name} "
	    foreach feature ${featureList} {
		set featureName [string trim [lindex ${feature} 0]]
		set featureFile [lindex ${feature} 1]
		set featureLine [lindex ${feature} 2]
		# Check to see if it is an included routine i.e.
		# whether or not its definition is in the class definiton
		if { ${featureFile} == ${classFileName} } {
		    if { [ expr ${featureLine} >= ${classLineOffset} ] } {
			if { [expr ${featureLine} <= ${classEndLine} ] } {
			    set featureName [dottyCleanName ${featureName}]
			    puts -nonewline $fn "| $featureName "
			}
		    }
		}
		
		
	    }
	    puts -nonewline $fn " }\"\n \];\n"
	} else {
	    # Don't show features
	    set name [dottyCleanName ${name}]
	    puts $fn " \"${cn}\"  \[ \n label = \"${name}\" \n \];"
	}
	
    }
}

proc dottyCleanName { name } {
    regsub {\{} ${name} {\\\{} name
    regsub {\}} ${name} {\\\}} name
    return ${name}
}

proc dottyPutEdge { fn src dest } {
    set cs [satherCleanName ${src}]
    set cd [satherCleanName ${dest}]
    puts $fn " \"${cs}\" -> \"${cd}\" \[ \n \];"
}

proc findClassEnd { class classFile classLine } {
    # Find and return the end of the class. 
    # If the class end is not found, an internal error will occur
    set fileTextLines [getAllFileLines ${classFile} ]
    set count ${classLine}
    set foundEnd 0
    set curLine [lindex ${fileTextLines} ${count} ]
    set endLoc [string first "end" ${curLine} ] 
    set classLoc [string first ${class} ${curLine} ] 
    # puts "Loc: ${classLoc} First line:${class} in ${curLine}"
    if { [expr ${classLoc} < 0 ] } {
	# puts "Going back one line..." 
	set count [expr ${count} - 1]
	set curLine [lindex ${fileTextLines} ${count} ]
	set endLoc [string first "end" ${curLine} ] 
	set classLoc [string first ${class} ${curLine} ]
	# puts "Loc: ${classLoc} First line:${class} in ${curLine}"
    } 
    set matchEnd [expr (${endLoc} >= 0 ) && (${classLoc} >= 0) ]
    set nlines [llength ${fileTextLines}]
    # An end anywhere on the first line
    while { ! ${matchEnd} } {
	incr count
	set curLine [lindex ${fileTextLines} ${count}]
	# An end at the beginning of some subsequent line
	set endLoc [string first "end" ${curLine} ] 
	set matchEnd [expr ${endLoc} == 0 ]
	if { [expr ${count} > (${nlines} + 2) ] } {
	    puts "Error: Could not find end of class. Assuming whole file.";
	    puts "Class: ${class} File:${classFile}"
	    set matchEnd 1
	}
	# puts "Next line:${endLoc} ${curLine}"
	# set matchEnd [regexp {^[ \t]*end} ${curLine}]
    }
    return ${count}
}

set gSeenFileNames {}

proc getAllFileLines { fname } {
    # Memoize the lines of the file
    global gSeenFileText
    global gSeenFileNames
    
    if { [lsearch ${gSeenFileNames} ${fname} ] >= 0 } {
	return $gSeenFileText($fname)
    } else {
	set fileText [getAllFile ${fname} ]
	set fileLines [split ${fileText} "\n"]
	set gSeenFileText(${fname}) ${fileLines}
	lappend gSeenFileNames ${fname}
	return ${fileLines}
    }
}


proc getAllFile { fname } {
    set ret ""
    if { [file exists ${fname}] } {
	set f [open ${fname} r]
	set ret [read $f]
	close $f
    } else {
	puts "No File found: $fname"
	set ret "No File Found:${fname}"
    }
    return $ret
}
