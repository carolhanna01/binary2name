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

# Help related functions

proc helpAddMenu { cm }  {
    $cm add command -label "What's New" -command "helpShow NewStuff"
    $cm add separator
    $cm add command -label "Menu Options " -command "helpShow menuPane"
    $cm add command -label "Class Graph Pane " -command "helpShow graphPane"
    $cm add command -label "Class/Module Listing Pane" -command "helpShow classListPane"
    $cm add command -label "Routine Listing Pane " -command "helpShow featureListPane"
    $cm add command -label "Text Pane" -command "helpShow textPane"
    $cm add command -label "History Popup" -command "helpShow historyPane"
    $cm add separator
    $cm add command -label "Using Emacs" -command "helpShow emacs"
    $cm add separator
    $cm add command -label "Implementation" -command "helpShow implementation"
    $cm add command -label "Tcl/Sather Callback Interface" -command "helpShow tcl-sather-interface"
    $cm add command -label "Browser Speed" -command "helpShow speed"
    $cm add command -label "Customization" -command "helpShow customization"
    $cm add command -label "Bugs " -command "helpShow bugs"
    $cm add separator
    $cm add command -label "Acknowledgements" -command "helpShow acknowledgements"
    $cm add separator



}

proc helpShow { fileName } {
    global gHome
    textReadFileLoadText "Help" $gHome/Browser/Help/${fileName} 1 false
}
