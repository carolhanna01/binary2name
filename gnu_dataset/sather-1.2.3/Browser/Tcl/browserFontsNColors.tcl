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

# Font and color configuration for the browser

set gTitleFont "lucidasans-14"

set gCommentFont "-*-*-medium-o-*-*-12-*-*-*-m-*-*-*"
set gClassHighlightFont "-*-*-*-r-*-*-12-*-*-*-m-*-*-*"
set gTextFont "7x14"
set gFeatureListFont "7x14"

# Various colors stuff.  Improved by Matt Kennel, ORNL/UTK.
# Set colors to control display.
if { $gIsColor } {
    set gSubtypeColor "firebrick"
    set gSubtypeRev   "white"

    set gIncludeColor "black"
    set gIncludeRev "white"

    set gModuleColor "darkgreen"
    set gModuleRev "white"

    set gCommentColor "brown"
    set gClassHighlightColor "blue"

    set gTextTitleColor "white"
    set gTextTitleRev "black"
    #colors for text file window's title.

    # Color for main window border background
    set gDeepBackgroundColor "grey30"

    set gGraphBackgroundColor "bisque"

    # Color of inform text
    set gInformColor "white"

    # Color for contents window background
    set gBackgroundColor "ivory"

    set gMenuForegroundColor "white"
    set gMenuBackgroundColor "grey30"

    # Color for Module&class list title
    set gClassListTitleColor "steelblue"
    set gClassListTitleRev "white"

    #Next two pairs of values are tied to each other
    set gClassListSelectColor "white"
    set gClassListSelectRev   "steelblue"
    # Colors for Feature list and a highlighted class in class list
    set gFeatureListTitleColor ${gClassListSelectRev}
    set gFeatureListTitleRev ${gClassListSelectColor}
    
    # colors for selected features
    set gFeatureListSelectionColor "steelblue"
    set gFeatureListSelectionRev "black"

    set gTextSelectionColor "black"
    set gTextSelectionRev "steelblue"
 } else {
    # b&w choices
    set gDeepBackgroundColor "white"
    set gBackgroundColor "white"
    set gGraphBackgroundColor "white"
    set gSubtypeColor "black" 
    set gSubtypeRev "white" 
    set gIncludeColor "black"
    set gIncludeRev "white"

    set gModuleColor  "black"
    set gModuleRev    "white"
    set gCommentColor "black"
    set gClassHighlightColor "black"

    # Color of inform text
    set gInformColor "black"

    set gTextTitleColor "white"
    set gTextTitleRev "black"

    set gClassListTitleColor "black"
    set gClassListTitleRev "white"

    set gMenuForegroundColor "black"
    set gMenuBackgroundColor "white"

    #Next two pairs of values are tied to each other
    set gClassListSelectColor "white"
    set gClassListSelectRev   "black"

    # Colors for Feature list title tied to class list selection color
    set gFeatureListTitleColor ${gClassListSelectRev}
    set gFeatureListTitleRev ${gClassListSelectColor}

    set gFeatureListSelectionColor "black"
    set gFeatureListSelectionRev "white"

    set gTextSelectionColor "white"
    set gTextSelectionRev "black"
 }


# Set fonts used in various panes
 # Font used to display module names in the list of classes and
 # modules pane.
set gClassListModuleFont "-*-lucida-bold-r-normal-*-17-*-*-*-p-*-*-*"
 # "lucidasans-bolditalic-14"
set gClassListClassFont  "-*-lucida-medium-r-normal-*-14-*-*-*-p-*-*-*"
set gGraphButtonFont "lucidasans-10"

 # Global table of fonts for use by the graph viewer
set gFontSizes() ""
 # Selected font sized was intended to hold a set of larger fonts
set gBoldFontSizes() ""
set gItalicFontSizes() ""
set gBolditalicFontSizes() ""


    # See the file future.tcl for the old settings of these variables...

set gFontSizes(14) "lucidasans-14"
set gFontSizes(13) "lucidasans-14"
set gFontSizes(12) "lucidasans-14"
set gFontSizes(11) "lucidasans-14"
set gFontSizes(10) "lucidasans-14"
set gFontSizes(9)  "lucidasans-14"
set gFontSizes(8)  "lucidasans-12"
set gFontSizes(7)  "lucidasans-12"
set gFontSizes(6)  "lucidasans-10"
set gFontSizes(5)  "lucidasans-10"
set gFontSizes(4)  "lucidasans-8"
set gFontSizes(3)  "lucidasans-8"
set gFontSizes(2)  "lucidasans-8"
set gFontSizes(1) "nil2"

set gItalicFontSizes(14) "lucidasans-italic-14"
set gItalicFontSizes(13) "lucidasans-italic-14"
set gItalicFontSizes(12) "lucidasans-italic-14"
set gItalicFontSizes(11) "lucidasans-italic-14"
set gItalicFontSizes(10) "lucidasans-italic-14"
set gItalicFontSizes(9)  "lucidasans-italic-14"
set gItalicFontSizes(8)  "lucidasans-italic-12"
set gItalicFontSizes(7)  "lucidasans-italic-12"
set gItalicFontSizes(6)  "lucidasans-italic-10"
set gItalicFontSizes(5)  "lucidasans-italic-10"
set gItalicFontSizes(4)  "lucidasans-italic-8"
set gItalicFontSizes(3)  "lucidasans-italic-8"
set gItalicFontSizes(2)  "lucidasans-italic-8"
set gItalicFontSizes(1) "nil2"

set gBoldFontSizes(14) "lucidasans-bold-14"
set gBoldFontSizes(13) "lucidasans-bold-14"
set gBoldFontSizes(12) "lucidasans-bold-14"
set gBoldFontSizes(11) "lucidasans-bold-14"
set gBoldFontSizes(10) "lucidasans-bold-14"
set gBoldFontSizes(9)  "lucidasans-bold-14"
set gBoldFontSizes(8)  "lucidasans-bold-12"
set gBoldFontSizes(7)  "lucidasans-bold-12"
set gBoldFontSizes(6)  "lucidasans-bold-10"
set gBoldFontSizes(5)  "lucidasans-bold-10"
set gBoldFontSizes(4)  "lucidasans-bold-8"
set gBoldFontSizes(3)  "lucidasans-bold-8"
set gBoldFontSizes(2) "lucidasans-bold-8"
set gBoldFontSizes(1) "nil2"

set gBolditalicFontSizes(14) "lucidasans-bolditalic-14"
set gBolditalicFontSizes(13) "lucidasans-bolditalic-14"
set gBolditalicFontSizes(12) "lucidasans-bolditalic-14"
set gBolditalicFontSizes(11) "lucidasans-bolditalic-14"
set gBolditalicFontSizes(10) "lucidasans-bolditalic-14"
set gBolditalicFontSizes(9)  "lucidasans-bolditalic-14"
set gBolditalicFontSizes(8)  "lucidasans-bolditalic-12"
set gBolditalicFontSizes(7)  "lucidasans-bolditalic-12"
set gBolditalicFontSizes(6)  "lucidasans-bolditalic-10"
set gBolditalicFontSizes(5)  "lucidasans-bolditalic-10"
set gBolditalicFontSizes(4)  "lucidasans-bolditalic-8"
set gBolditalicFontSizes(3)  "lucidasans-bolditalic-8"
set gBolditalicFontSizes(2) "lucidasans-bolditalic-8"
set gBolditalicFontSizes(1) "nil2"


