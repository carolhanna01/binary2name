#------------------------------>  Tcl - script  <-----------------------------#
#- Copyright (C) 1995 by International Computer Science Institute            -#
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

#                     The Sather Browser
# Author: Benedict Gomes <gomes@icsi.berkeley.edu>
# Contributors:
# David Stoutamire (many ideas, sather code for graph layout, 
#                   comment highlighting. Also provided compiler support)
# Matt Kennel (window layout, intelligent color consistency, event flushing, 
#             cursor changes, bugs)
# Holger Klawitter (sather bitmap, history suggestion)
# Yon Uriarte (changes to graph layout, portability)
#
# Making changes: Users are strongly encouraged to make enhancements.
# To this end, I've
# -  changed the code around to have a more consistent naming scheme
#    (most functions start with a prefix that indicates the pane the work on)
#  - split up the source so that functions for different panes are separate
#  - added an implementation section to the files in the Help/ subdirectory
# Despite these efforts, I fear that my initial discipline was not 
# strong enough (this was my first piece of tcl code and I had no idea how
# badly tcl programming would scale). I'll be more than happy to give you 
# information about how some particular aspect of the browser works.
# If you add anything fun, let me know and I'll try to incorporate it!
# Log History in Help/VersionHistory
#########################################################################
#######################  DEBUGGING FLAGS ################################
# Turn on sather level debugging, if needed. May want to set this
# flag earlier, in browserStartupWindow.tcl
#tkkit_cb debugOn

# Turn on Tcl debugging by uncommenting the "puts" line.
# Also consider uncommenting a similar line in "proc inform"
proc debugPuts { fileName a } {
#    puts "TCL debug ${fileName}: ${a}"
}

############### SET NAMES OF CONFIGURATION FILES, HOME DIR etc. #####
set gHome [tkkit_cb getHome]
# Name of configuration file
set gConfigFileName "~/.bsConfig.tcl"

# Name of (hand) customization file
set gCustomFileName "~/.bsCustom.tcl"
######################## SOURCING OTHER TCL FILES ######################
set gCurrentlyLoading ""
proc loadSourceFile { gname} {
    global gHome
    # Load the file "gname" in the directory Browser/Tcl
    loadFilePlain "${gHome}/Browser/Tcl/${gname}"
}

proc loadFilePlain { gname} {
    # Load a file name ${gname}
    global gCurrentlyLoading
    set gCurrentlyLoading ${gname}
    set res [catch { uplevel #0 {source "${gCurrentlyLoading}" } } srcErr]
    if { ${res} != 0 } { 
	puts "Error loading ${gname}"
	puts ${srcErr} 
	promptInformBox "Error reading ${gname}" "Source failed: ${srcErr}"
    }
}

loadSourceFile "browserMisc.tcl"
# Miscellaneous top level routines
loadSourceFile "browserFontsNColors.tcl"
# Source the colors and fonts
loadSourceFile "browserPrompters.tcl"
# Source the code for prompters
loadSourceFile "browserDump.tcl"
# Load in functions relation to dumping of postscript, Dot, html etc.
loadSourceFile "browserConfig.tcl"
# Load in auxilliary procedures for dumping configuration
loadSourceFile "browserHistory.tcl"
# Source the code to handle the history mechanism
loadSourceFile "browserText.tcl"
# Source the code to create the text pane
loadSourceFile "browserClassList.tcl"
# Source the code to create the class - module pane
loadSourceFile "browserClassList.tcl"
# Source the code to create the class - module pane
loadSourceFile "browserFeatureList.tcl"
# Source the code to create the feature
loadSourceFile "browserGraph.tcl"
# Source the code to create the graph
loadSourceFile "browserHelp.tcl"
# Source the code to deal with help functionality
################ OTHER DOCUMENTATION ############################
### TAGS
# Graph Tags: See Help/graph
# Tags for the text widget: See Help/text
### CALLBACKS 
# SATHER CALLBACKS ARE DOCUMENTED IN THE FILE Help/Callbacks 
#######################################################################

#                     Various frames (values set in configure)
#                      ---------------------------------------
# gGraphFrame:           Frame for canvas for class graphs. gGraphFrame.c
#                        is actual canvas. See graphCreate
# gTextDisplay :         gTextDisplay.t is the text widget. See textCreate
# gFeatureListFrame:     Scrolling listbox of features. See featureListCreate
# gInformFrame:          Bottom of window - information about actions.
# gMenuFrame:            Frame for the menubuttons. See menuBarCreate
# gClassListFrame:       Frame for list of class names. see makeListBox

############################################################################## 
#  CUSTOM GLOBALS (Saved by saveConfig[browserConfig.tcl] in ~/.bsConfig.tcl )
##############################################################################
# Whether C classes are visible or not
global gNoShow_C_Classes 1

# Whether TEST_ classes are visible or not
global gNoShow_Test_Classes 1

debugPuts "browser.tcl" "Sourcing implementation globals...."
#######################################################################
#                      IMPLEMENTATION GLOBALS
#######################################################################
# Internal variables - understand code before modifying.
#                    Browser Global Data structures
#                    ------------------------------

set gCurNodeName ""
# Current node selected. In the case of a module, holds the truncated
# name (without the .module extension or the full path)


set gCurNodeFullName ""
# If a module is currently selected, holds its full file name

# gModToClasses   
# Mapping from module names to liss of classes. set in initClasses

set gIsWaiting false
# Whether we are currently waiting for an operation to complete.
# Used for changing cursor to watch
# Can't rely  on this - not always set properly

set gFreezeClass 0
# If true, don't permit choice of another class. Useful
# when laying out a graph to prevent accidentally undoing all changes!

set gCClasses {} 
# List of Classes that begin with C_, set in classListInitCTest

set gTestClasses {}
# List of Classes that begin with TEST_

set gCurFile ""
# Name of current file - used by textSaveText

######################## MAIN RUN PROCEDURE ####################
proc run { } {
    # Run the whole thing

    debugPuts "browser.tcl" "Starting run. Updating graph  variables from tcl"
    graphUpdateSatherVars
    debugPuts "browser.tcl" "Updating feature list sather vars from tcl..."
    featureListUpdateSatherVars
    debugPuts "browser.tcl" "Configuring browser..."
    configure
    debugPuts "browser.tcl" "Initializing class list...."
    classListInit
    debugPuts "browser.tcl" "Hiding classes..."
    classListHideClasses
    debugPuts "browser.tcl" "After initialization hook..."
    afterInitHook
    debugPuts "browser.tcl" "Done with running..."
}

########################################################################
#                          INFORM WINDOW 
########################################################################
proc informCreate { frameName } {
    global gDeepBackgroundColor
    global gInformColor

    label ${frameName}.msg -foreground ${gInformColor} -background $gDeepBackgroundColor
    pack ${frameName}.msg -fill x -expand yes
}

proc inform { text } {
    global gInformFrame
    ${gInformFrame}.msg configure -text ${text}
    # debugPuts "browser.tcl" "Informing:${text}"
}


######################## USER CUSTOMIZATION HOOKS ###################
proc afterInitHook { } {
    # User can redefine this in gCustomFileName to do something useful
    # At this point, the browser tables like gModToClasses have been setup
    # Example usage:
    # Defining new customized menu items
    # Printing out some error messages, perhaps
}

if [file exists ${gConfigFileName} ] {
    # Sourcing of .bsConfig.tcl - user's configuration file
    # Written out by saveConfig
    
    set res [catch { source ${gConfigFileName} } configErr ]
    if { ${res} != 0 } {
	puts "ERROR in ${gConfigFileName}"
	puts "Try deleting that file and restarting"
	puts "You will have to reset your browser configuration and store"
	puts "it again."
	puts ${configErr}
    }
    
}


if [file exists ${gCustomFileName} ] {
    # Sourcing of user's hand written customization file
    # This file can:
    # - define new procedures
    # - redefine existing procedures which will over-ride the current procs
    # - In particular, redefine afterInitHook to add new menu items etc.
    set res [catch { source ${gCustomFileName} } customErr ]
    if { ${res} != 0 } {
	puts "ERROR in ${gCustomFileName}."
	puts "Try moving that file somewhere else and restarting the browser"
	puts "The browser will revert to its default configuration"
	puts ${customErr}
    }

}


debugPuts "browser.tcl" "after getCommentFollowing..."

########################## START IT ALL OFF ###################
run 

