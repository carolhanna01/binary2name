;# gui.tcl -- Gerwin
;#
;# GUI implementation
;#
;# Copyright (C) 2003-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:23:34 jemarch"

;# This program is free software; you can redistribute it and/or
;# modify it under the terms of the GNU General Public License as
;# published by the Free Software Foundation; either version 3 of
;# the License, or (at your option) any later version.

;# This program is distributed in the hope that it will be useful,
;# but WITHOUT ANY WARRANTY; without even the implied warranty of
;# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;# GNU General Public License for more details.

;# You should have received a copy of the GNU General Public License
;# along with this program.  If not, see
;# <http://www.gnu.org/licenses/>.


;#
;# gui_create_fonts 
;#
;# Creates the default fonts for Gerwin

proc gui_create_fonts {} {

    global gfonts


    ;# Default application font
    set gfonts(default) \
	[font create -family Helvetica -size 11]


    ;# Fonts for the ER Diagram
    set gfonts(ercanvas_default) \
	[font create -family Courier -size 8]
    set gfonts(ercanvas_titles) \
	[font create -family Courier -size 11]
    set gfonts(ercanvas_minilabels) \
	[font create -family Times -size 8]
}


;# 
;# gui_gcanvas_make_zoom CANVAS FACTOR
;#
;# Makes a zoom on CANVAS, by FACTOR
;# Manages fonts sizes
;#
;# CANVAS can be er or td

proc gui_gcanvas_make_zoom {what_canvas factor} {
    
    global gcanvas
    global gcanvas_td
    global gfonts

    switch $what_canvas {

	er {

	    ;# First of all, scale the canvas itself
	    ${gcanvas} scale all 0.0 0.0 $factor $factor

	    ;# Scale the fonts 
	    set fontfactor [expr ([font actual $gfonts(ercanvas_default) -size] \
				 * $factor)]

	    regsub {([0-9]+)\.[0-9]} $fontfactor "\\1" fontfactor
	    
	    font configure $gfonts(ercanvas_default) \
		-size $fontfactor

	    set fontfactor [expr ([font actual $gfonts(ercanvas_titles) -size] \
				 * $factor)]

	    regsub {([0-9]+)\.[0-9]} $fontfactor "\\1" fontfactor
	    
	    font configure $gfonts(ercanvas_titles) \
		-size $fontfactor

	    ;# Reconfigure all entities and relations dimensions
	    
	}

	td {


	}

    }
    

}



;#
;# gui_init WIDGET
;#
;# Initializes the gerwin widgets, into WIDGET

proc gui_init {widget} {

    global fontselwidget
    global gmainframe
    global gcanvas
    global gerwin_version
    global gerwin_editionarea
    global apparea

    set gmainframe ${widget}
    
    ;# Title
    wm title . "Gerwin $gerwin_version"

    ;# Create the fonts
    gui_create_fonts

    ;# Make the mainframe
    MainFrame ${gmainframe} -separator both \
	-textvariable gerwin_state -menu [gui_create_mainmenu]

    ${gmainframe} addindicator -text "GNU Ferret"
    ${gmainframe} addindicator -text $gerwin_version

    ;# Create the button-area
    set fbarea [${gmainframe} addtoolbar]
    gui_create_button_area $fbarea


    set userframe [${gmainframe} getframe]


    ;# Create the app area
    gui_create_app_area ${userframe}.apparea



    ;# Create the edition area
    ;# The edition area is a notebook widget, that contain frames.
    ;# The first frame is that one with the project properties
    
    ;#set gerwin_editionarea ${userframe}.editionarea
    ;#gui_create_edition_area ${userframe}.editionarea    

    ;# Launch the bindings
    ;#gui_launch_bindings

    ;#pack ${userframe}.editionarea -side top -fill x
    pack ${gmainframe} -fill both -expand true

    ;# Now there is not any project opened, so disable all the buttons
    ;# except the one with new-project and the one with load-project
    gui_no_project_opened

}


;#
;# gui_create_app_area WIDGET
;#
;# Create the app area with all the pages

proc gui_create_app_area {widget} {

    global gmainframe
    global apparea
    global apptree


    set userframe [${gmainframe} getframe]

    ;# Create the apptree
    ;# gui_create_apptree ${userframe}.apptree

    ;# Create the notebook
    NoteBook ${userframe}.apparea
    set apparea ${userframe}.apparea

    ;# Pack the apptree
    ;# pack ${apptree} -fill y -side left

    ;# Pack the apparea
    pack ${apparea} -fill both -expand true -side right

}


;#
;# gui_create_apptree PATH
;#
;# Create the apptree at PATH 
;#

proc gui_create_apptree {path} {
    
    global apptree

    set apptree $path

    ;# Create the Tree widget
    Tree ${path} -showlines true -dragenabled false

    ;##############################
    ;# Create the application nodes
    ;##############################

    ;# Gerwin -> Settings at right
    gui_apptree_insert 0 root Gerwin

}

;#
;# gui_apptree_insert INDEX PARENT NODE
;#
;# Insert NODe into the apptree

proc gui_apptree_insert {index parent node} {

    global apptree
    global gfonts

    ${apptree} insert $index $parent apptree-{$node} \
	-text $node -font $gfonts(default) -image [Bitmap::get save]


}


;#
;# gui_app_area_raise_Project_page 
;#
;# Raises the project page on the app area

proc gui_app_area_raise_Project_page {} {

    global apparea

    ${apparea} raise Project


}

;#
;# gui_app_area_destroy_ER_page
;#
;# Destroys the app area TD page

proc gui_app_area_destroy_ER_page {} {

    global apparea

    ${apparea} delete ER


}


;#
;# gui_app_area_destroy_TD_page
;#
;# Destroys the app area TD page

proc gui_app_area_destroy_TD_page {} {

    global apparea

    ${apparea} delete TD


}

;#
;# gui_app_area_destroy_Output_page
;#
;# Destroys the app area Output page

proc gui_app_area_destroy_Output_page {} {

    global apparea

    ${apparea} delete Output


}

;#
;# gui_app_area_destroy_Project_page
;#
;# Destroys the app area Project page

proc gui_app_area_destroy_Project_page {} {

    global apparea

    ${apparea} delete Project

}

;#
;# gui_app_area_create_Project_page 
;#
;# Creates the app area Project page, with page information

proc gui_app_area_create_Project_page {} {

    global apparea

    global gerwin_cproject_name
    global gerwin_cproject_file
    global gerwin_cproject_author

    ;# Create the new page, if it is not created
    ${apparea} insert 0 Project -text "Project"
    ${apparea} raise Project

    ;# Put here the project attributes
    set widget [${apparea} getframe Project]

    TitleFrame ${widget}.editframe -side center -text "Project Properties"

    set editwidget [${widget}.editframe getframe]
    
    ;# Project name 
    frame ${editwidget}.nameline 
    LabelEntry ${editwidget}.nameline.label -label "Project Name:" -labelwidth 16 \
	-textvariable gerwin_cproject_name \
	-helptext "Name of the current project" -width 50

    ;# Project file
    frame ${editwidget}.fileline
    LabelEntry ${editwidget}.fileline.label -label "Project File:" -labelwidth 16 \
	-textvariable gerwin_cproject_file \
	-helptext "Storage file of the current project" -editable false \
	-width 50

    Button ${editwidget}.fileline.button -text "select file" \
	-helptext "Select a file to store the project in" \
	-command "set tempfile \[gui_select_file_to_save\] ; \

                  if {\$tempfile != {}} then {
                      set gerwin_cproject_file \$tempfile
                      gerwin_save_project_file
                  } " \
	-relief groove


    ;# Project author
    frame ${editwidget}.authorline
    LabelEntry ${editwidget}.authorline.label -label "Project Author:" -labelwidth 16 \
	-textvariable gerwin_cproject_author \
	-helptext "Author of the current project" -width 50
    
    pack ${editwidget}.nameline.label -side left -anchor w
    pack ${editwidget}.nameline -side top -anchor w

    pack ${editwidget}.fileline.label -side left -anchor w
    pack ${editwidget}.fileline.button -side left
    pack ${editwidget}.fileline -side top -anchor w

    pack ${editwidget}.authorline.label -side left -anchor w
    pack ${editwidget}.authorline -side top -anchor w

    pack ${editwidget} -side left

    pack ${widget}.editframe -side left -fill both -expand true
    
    ;# Scale the notebook
    NoteBook::compute_size ${apparea}


}

;#
;# gui_app_area_raise_ER_page
;#
;# Raises the ER page

proc gui_app_area_raise_ER_page {} {

    global apparea

    ${apparea} raise ER

}

;#
;# gui_app_area_raise_TD_page
;#
;# Raises the TD page

proc gui_app_area_raise_TD_page {} {

    global apparea

    ${apparea} raise TD


}



;#
;# gui_app_area_raise_Output_page
;#
;# Raises the Output page

proc gui_app_area_raise_Output_page {} {

    global apparea

    ${apparea} raise Output

}


;#
;# gui_app_area_create_Domain_page
;#
;# Create the app area Domain page, with the Domain Manager

proc gui_app_area_create_Domain_page {} {

    global apparea
    global gcanvas

    ;# Create the new page
    ${apparea} insert 0 Domain -text "Domains and Types"


    ;# Include here the new 

}


;#
;# gui_app_area_destroy_Domain_page
;#
;# Delete the Domain page from the app area, and destroy its contents

proc gui_app_area_destroy_Domain_page {} {

    global apparea
    global gerwin_domains
    global gerwin_domain

    ;# Delete the page and destroy the contents
    ${apparea} delete Domain

    ;# Purge all domains on the system
    foreach d $gerwin_domains {

	gob_domain_delete $d

    }
    set gerwin_domains {}

}

;#
;# gui_app_area_create_ER_page
;# 
;# Create the app area ER page

proc gui_app_area_create_ER_page {} {

    global apparea
    global gcanvas
    global gerwin_editionarea
    
    ;# Buttons globals
    global gui_button_bbox_elements
    global gui_button_bbox_tools
    global gui_button_bbox_print_ER
    global gui_button_bbox_zoom_ER


    ;# Create the new page
    ${apparea} insert end ER -text "ER Diagram"

    ;# Now, the scrolled canvas and the editionarea into two paned windows
    set pw [${apparea} getframe ER].pw
    PanedWindow $pw -side left -weights extra

    ;# Frame for the ER toolbar and scrolled canvas
    ;# Create the toolbar for ER
    set erframe [${pw} add]
    frame ${erframe}.toolbar
    set bbox [ButtonBox ${erframe}.toolbar.tb2 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_elements $bbox

    $bbox add -text entity \
        -helptype balloon -helptext "Insert a new entity object" \
	-relief groove \
	-command {gerwin_set_state "Inserting entity"}

    $bbox add -text relation \
	-helptype balloon -helptext "Insert a new relation object" \
	-relief groove \
	-command {gerwin_set_state "Inserting relation begin"}

    Separator ${erframe}.toolbar.sep2 -orient vertical


    set bbox [ButtonBox ${erframe}.toolbar.tb3 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_tools $bbox

    $bbox add -text move \
	-helptype balloon -helptext "Move an object" \
	-relief groove \
	-command {gerwin_set_state "Moving"}

    $bbox add -text edit \
	-helptype balloon -helptext "Edit the properties of an object" \
	-relief groove \
	-command {gerwin_set_state "Editing"}

    $bbox add -text delete \
	-helptype balloon -helptext "Delete an object" \
	-command {gerwin_set_state "Deleting"} -relief groove
	;#-image [Bitmap::get cut] -relief groove \

    Separator  ${erframe}.toolbar.sep1 -orient vertical

    set bbox [ButtonBox ${erframe}.toolbar.tb7 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_print_ER $bbox

    $bbox add -text "print" -helptype balloon \
	-helptext "Print the ER diagram" \
	-relief groove -command gerwin_print_ER

#     set bbox [ButtonBox ${erframe}.toolbar.tb8 -spacing 0 -padx 1 -pady 1]
#     set gui_button_bbox_zoom_ER $bbox

#     $bbox add -text "+" -helptype balloon \
# 	-helptext "Positive zoom" \
# 	-relief groove -command [list gui_gcanvas_make_zoom er 1.1]
#     $bbox add -text "-" -helptype balloon \
# 	-helptext "Negative zoom" \
# 	-relief groove -command [list gui_gcanvas_make_zoom er 0.9]


#     Separator ${erframe}.toolbar.sep3 -orient vertical

    pack ${erframe}.toolbar.tb2 -side left -anchor w
    pack ${erframe}.toolbar.sep2 -side left -padx 4 -fill y
    pack ${erframe}.toolbar.tb3 -side left -anchor w
    pack ${erframe}.toolbar.sep1 -side left -padx 4 -fill y
    pack ${erframe}.toolbar.tb7 -side left -anchor w
#     pack ${erframe}.toolbar.sep3 -side left -padx 4 -fill y
#     pack ${erframe}.toolbar.tb8 -side left -anchor w

    gui_create_scrolled_canvas [frame ${erframe}.fcanvas] ;#-bg white
    set gcanvas ${erframe}.fcanvas.canvas

    pack ${erframe}.toolbar -side top -fill x
    pack ${erframe}.fcanvas -side top -fill both -expand true

    ;# Set the canvas enter and leave bindings
    bind ${gcanvas} <Enter> gui_enter_canvas_bind
    bind ${gcanvas} <Leave> gui_leave_canvas_bind

    ;# Set the canvas dimensions bindings
    bind . <Key-Right> gui_expand_canvas_to_right
    bind . <Key-Down> gui_expand_canvas_to_down
    bind . <Key-Left> gui_shrink_canvas_to_left
    bind . <Key-Up> gui_shrink_canvas_to_up

    ;# Create the editionarea
    set gerwin_editionarea [${pw} add].editionarea
    gui_create_edition_area ${gerwin_editionarea}
    

    ;# Pack it
    pack ${gerwin_editionarea} -side bottom -fill x

    ;# Pack it
    pack ${erframe} -fill both -expand true
    pack ${pw} -fill both -expand true

    ;# Update the notebook size
    NoteBook::compute_size ${apparea}
}

;#
;# gui_app_area_create_TD_page
;#
;# Create the app area TD page

proc gui_app_area_create_TD_page {} {

    global apparea
    global gcanvas_td

    ;# Buttons globals
    global gui_button_bbox_print_TD
    global gui_button_bbox_gen_TD

    ;# Create the new page
    ${apparea} insert end TD -text "Table Diagram"

    set bbutton_frame [frame [${apparea} getframe TD].toolbar]

    set bbox [ButtonBox ${bbutton_frame}.tb1 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_gen_TD $bbox

    $bbox add -text "Update" \
	-helptype balloon -helptext "Generate the table diagram" \
	-relief groove -command {gen_ER_to_TD}

    Separator  ${bbutton_frame}.sep1 -orient vertical

    set bbox [ButtonBox ${bbutton_frame}.tb2 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_print_TD $bbox

    $bbox add -text "print" -helptype balloon \
	-helptext "Print the Table diagram" \
	-relief groove -command gerwin_print_TD

    pack ${bbutton_frame}.tb1 -side left -anchor w
    pack ${bbutton_frame}.sep1 -side left -padx 4 -fill y
    pack ${bbutton_frame}.tb2 -side left -anchor w


    set erframe [frame [${apparea} getframe TD].fcanvas]
    gui_create_scrolled_canvas ${erframe}

    set gcanvas_td ${erframe}.canvas

    ;# Set the canvas enter and leave bindings
    bind ${gcanvas_td} <Enter> {. configure -cursor fleur}
    bind ${gcanvas_td} <Leave> {. configure -cursor ""}

    ;# Set the bind for marking
    ${gcanvas_td} bind all <Button-1> {gui_mark_td %x %y}

    ;# Pack it 
    pack ${bbutton_frame} -fill x
    pack ${erframe} -fill both -expand true

}

;#
;# gui_app_area_create_Output_page 
;#
;# Create the app area Output page

proc gui_app_area_create_Output_page {} {

    global apparea
    global gtext_output
    global gerwin_output_formats
    global gerwin_output_active_format
    global output_pages
    global output_pages_widget

    ;# Buttons globals
    global gui_button_bbox_gen_output

    ;# Create the new page
    ${apparea} insert end Output -text "Output"

    ;# Create the toolbar
    set bbutton_frame [frame [${apparea} getframe Output].toolbar]
    set bbox [ButtonBox ${bbutton_frame}.tb1 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_gen_output $bbox

    $bbox add -text "Create output" \
	-helptype balloon -helptext "Generate the selected output format" \
	-relief groove -command "gen_TD_to_Output \$gerwin_output_active_format"


    ;# Replace it with a ComboBox
    Label ${bbutton_frame}.label -text "output format: "
    ComboBox ${bbutton_frame}.formats -values $gerwin_output_formats -expand tab \
	-editable false \
	-modifycmd "set gerwin_output_active_format \[lindex \$gerwin_output_formats \[${bbutton_frame}.formats getvalue\]\]"


    pack ${bbutton_frame}.label -side left -anchor w
    pack ${bbutton_frame}.formats -side left 
    pack ${bbutton_frame}.tb1 -side left -anchor w -padx 4

    ;# Now, create the notebook with the several output gtexts
    set outputs [NoteBook [${apparea} getframe Output].nb]
    set output_pages_widget $outputs
    
    ;# Pack it
    pack ${bbutton_frame} -fill x
    pack ${outputs} -fill both -expand true
    ;#pack ${erframe} -fill both -expand true
}


;#
;# gui_destroy_output_page FORMAT
;#
;# Destroy the FORMAT output page

proc gui_destroy_output_page {format} {

    global output_pages
    global output_pages_widget
    global gtext_output

    ;# First of all, clean output_pages and gtext_output(FORMAT)
    set index [lsearch $output_pages $format]
    set output_pages [lreplace $output_pages $index $index]

    unset gtext_output($format)

    ;# Ok, then destroy the widget
    ${output_pages_widget} delete $format


}

;#
;# gui_create_output_page FORMAT
;#
;# Creates a new output page for FORMAT on the output_pages_widget notebook

proc gui_create_output_page {format} {

    global output_pages
    global output_pages_widget
    global gtext_output
    global gerwin_output_active_format


    ;# First of all, update output_pages
    lappend output_pages $format

    ;# Second, create the page into the notebook
    ${output_pages_widget} insert end $format -text $format

    ;# Good. Next, create the button bar
    set toolbar [frame [${output_pages_widget} getframe $format].toolbar]    

    ;# Update button
    set bbox [ButtonBox ${toolbar}.tb3 -spacing 0 -padx 1 -pady 1]
    
    $bbox add -text update \
	-helptype balloon -helptext "Update this output from the tables" \
	-relief groove \
	-command "set ftemp \$gerwin_output_active_format 
                  set gerwin_output_active_format $format
                  gen_TD_to_Output $format
                  set gerwin_output_active_format \$ftemp"

    Separator ${toolbar}.sep3 -orient vertical

    ;# Save and print buttons
    set bbox [ButtonBox ${toolbar}.tb1 -spacing 0 -padx 1 -pady 1]

    $bbox add -text save \
        -helptype balloon -helptext "Save this output to a file" \
	-relief groove \
	-command [list gerwin_save_output_file $format]

#     $bbox add -text print \
#         -helptype balloon -helptext "Print this output to a printer" \
# 	-relief groove \
# 	-command [list gerwin_print_output $format]
    
    ;# Close page button
    set bbox [ButtonBox ${toolbar}.tb2 -spacing 0 -padx 1 -pady 1]

    $bbox add -text close \
        -helptype balloon -helptext "Close this output page" \
	-relief groove \
	-command [list gui_destroy_output_page $format]


    pack ${toolbar}.tb3 -side left -anchor w
    pack ${toolbar}.sep3 -side left
    pack ${toolbar}.tb1 -side left -anchor w
    pack ${toolbar}.tb2 -side right -anchor w

    ;# Now create the scrolled text
    set stext [frame [${output_pages_widget} getframe $format].stext]

    gui_create_scrolled_text ${stext}

    ;# Pack it
    pack ${toolbar} -side top -fill x
    pack ${stext} -side top -fill both -expand true

    ;# Finally, raise the page and update gtext_output
    ${output_pages_widget} raise $format
    set gtext_output($format) ${stext}.text
    
    ;# Put the background
    $gtext_output($format) configure -bg white

    ;# Make the text widget not editable
    $gtext_output($format) configure -state disabled
}


;#
;# gui_enter_canvas_bind 
;#

proc gui_enter_canvas_bind {} {
    
    global gerwin_state

    ;# Change the cursor 
    switch $gerwin_state {

	"Inserting entity" {

	    . configure -cursor crosshair

	}
	"Inserting relation begin" {

	    . configure -cursor left_side

	}

	"Inserting relation end" {
	    
	    . configure -cursor right_side

	}

	Deleting {
	    
	    . configure -cursor pirate

	}
	
	Moving {

	    . configure -cursor fleur

	}

	Editing {

	    . configure -cursor hand1
	}

	
	default {
	    
	    . configure -cursor ""
	}

    }


}

;#
;# gui_leave_canvas_bind 
;#

proc gui_leave_canvas_bind {} {

    . configure -cursor ""

}



;#
;# gui_update_font FONT
;#
;# Updates the application global font

proc gui_update_font {newfont} {

    global gui_font
    global fontselwidget
    global gmainframe

    . configure -cursor watch 

    ;# Update the fonts of all widgets
    if { $gui_font != $newfont } then {
	
	$fontselwidget configure -font $newfont
	set gui_font $newfont
    }

    . configure -cursor ""

}

;# 
;# gui_create_edition_area WIDGET
;#
;# Creates the edition area

proc gui_create_edition_area {widget} {

    ;# Create the notebook
    NoteBook ${widget}

}




;#
;# gui_create_mainmenu 
;#
;# Create the main menu string

proc gui_create_mainmenu {} {


    ;# Prepare the menu
    set mainmenu {
	"&Gerwin" {} {} 0 {
	    {separator}
	    {command "&Exit Gerwin" {} "exit the application" {Ctrl e} -command gerwin_quit}
	}
         
	"&Project" {} {} 0 {
	    {command "&New Project" {} "new project" {Ctrl n} -command gerwin_new_project}
	    {command "&Open Project" {} "open a new project" {Ctrl o} -command gerwin_open_project}
	    {command "&Save Project" {} "save the current project" {Ctrl s} -command gerwin_save_project}
	    {command "&Close Project" {} "close the current project" {Ctrl c} -command gerwin_close_project}
	}
	
    }

    return $mainmenu

}

;#
;# gui_create_button_area WIDGET
;#
;# Create the button area widget
;#

proc gui_create_button_area {widget} {

    global gui_button_bbox_project

    global gerwin_output_formats

    ;# Project area
    set bbox [ButtonBox ${widget}.tb1 -spacing 0 -padx 1 -pady 1]
    set gui_button_bbox_project $bbox
    
    $bbox add -text "new" -helptype balloon \
	-helptext "New project" \
	-relief groove \
	-command "gerwin_new_project"

    $bbox add -text "open" -helptype balloon \
	-helptext "Open a project" \
	-relief groove \
	-command "gerwin_open_project"

    $bbox add -text "save" -helptype balloon \
	-helptext "Save the current project" \
	-relief groove \
	-command "gerwin_save_project"


    pack ${widget}.tb1 -side left -anchor w 


#     set bbox [ButtonBox ${widget}.tb5 -spacing 0 -padx 1 -pady 1]
#     set gui_button_bbox_ssql $bbox

#     $bbox add -text "save sql" \
# 	-helptype balloon -helptext "Saves the SQL list" \
# 	-relief groove -command {gerwin_save_sql}

}

;#
;# gui_create_scrolled_text FRAME ARGS
;#
;# Create a scrolled text widget

proc gui_create_scrolled_text {widget args} {


    ;# Create the text widget
    eval {text ${widget}.text \
	      -xscrollcommand [list ${widget}.xscroll set] \
	      -yscrollcommand [list ${widget}.yscroll set] \
	      -borderwidth 0} $args

    ;# Create the scroll widgets
    scrollbar ${widget}.xscroll -orient horizontal \
	-command [list ${widget}.text xview]
    scrollbar ${widget}.yscroll -orient vertical \
	-command [list ${widget}.text yview]
    
    ;# Pack the widgets
    grid ${widget}.text ${widget}.yscroll -sticky news
    grid ${widget}.xscroll -sticky ew
    grid rowconfigure ${widget} 0 -weight 1
    grid columnconfigure ${widget} 0 -weight 1

}


;#
;# gui_shrink_canvas_to_left
;#

proc gui_shrink_canvas_to_left {} {

    global gcanvas

    set sarea [${gcanvas} cget -scrollregion]

    ${gcanvas} configure -scrollregion \
	[list [lindex $sarea 0] [lindex $sarea 1] \
	     [expr [lindex $sarea 2] - 100] [lindex $sarea 3]]


}



;#
;# gui_expand_canvas_to_right 
;#

proc gui_expand_canvas_to_right {} {

    global gcanvas

    set sarea [${gcanvas} cget -scrollregion]

    ${gcanvas} configure -scrollregion \
	[list [lindex $sarea 0] [lindex $sarea 1] \
	     [expr [lindex $sarea 2] + 100] [lindex $sarea 3]]


}


;#
;# gui_shrink_canvas_to_up
;#

proc gui_shrink_canvas_to_up {} {

    global gcanvas
 
    set sarea [${gcanvas} cget -scrollregion]

    ${gcanvas} configure -scrollregion \
	[list [lindex $sarea 0] [lindex $sarea 1] \
	     [lindex $sarea 2] [expr [lindex $sarea 3] - 100]]


}


;#
;# gui_expand_canvas_to_down
;#

proc gui_expand_canvas_to_down {} {

    global gcanvas
 
    set sarea [${gcanvas} cget -scrollregion]

    ${gcanvas} configure -scrollregion \
	[list [lindex $sarea 0] [lindex $sarea 1] \
	     [lindex $sarea 2] [expr [lindex $sarea 3] + 100]]


}

;#
;# gui_create_scrolled_canvas FRAME ARGS
;#
;# Create a scrolled canvas widget

proc gui_create_scrolled_canvas { widget args } {


    ;# Create the canvas widget
    eval {canvas ${widget}.canvas \
	      -xscrollcommand [list ${widget}.xscroll set] \
	      -yscrollcommand [list ${widget}.yscroll set] \
	      -highlightthickness 0 \
	      -borderwidth 0 -bg white} $args

    ;# Create the scroll widgets
    scrollbar ${widget}.xscroll -orient horizontal \
	-command [list ${widget}.canvas xview] 
    scrollbar ${widget}.yscroll -orient vertical \
	-command [list ${widget}.canvas yview]

    ;# Pack the widgets (with grid)
    grid ${widget}.canvas ${widget}.yscroll -sticky news
    grid ${widget}.xscroll -sticky ew
    grid rowconfigure ${widget} 0 -weight 1
    grid columnconfigure ${widget} 0 -weight 1

}


;#
;# gui_change_state TEXT
;#
;# Change the state to TEXT
;# Now this is a nop

proc gui_change_state {text} {


}

;#
;# gui_yes_no_cancel TEXT
;#

proc gui_yes_no_cancel {text} {

    global gmainframe

    MessageDlg ${gmainframe}.notice -title "I am asking you, dude!" -justify left -icon info \
	-message $text -type yesnocancel

}


;# 
;# gui_notice TEXT
;#
;# Notice about something.

proc gui_notice {text} {

    global gmainframe

    MessageDlg ${gmainframe}.notice -title "Notice" -justify left -icon info \
	-message $text -type ok
}

;# 
;# gui_fatal TEXT
;#
;# Notice a fatal error and exit

proc gui_fatal {text} {

    global gmainframe

    toplevel ${gmainframe}.fatal -bg red

    label ${gmainframe}.fatal.label -text "Fatal Error: $text" \
	-bg red -fg white
    button ${gmainframe}.fatal.button -text "Quit Gerwin" \
	-command {exit}

    pack ${gmainframe}.fatal.label -side top
    pack ${gmainframe}.fatal.button -side top

    focus ${gmainframe}.fatal
}


;#
;# gui_project_opened
;#
;# Enables some buttons and canvas and text backgrounds

proc gui_project_opened {} {

    global gui_button_bbox_project
    global gui_button_bbox_elements
    global gui_button_bbox_tools
    global gui_button_bbox_gen_TD
    global gui_button_bbox_gen_output
    global gui_button_bbox_gen_GerwinML
    global gui_button_bbox_ssql
    global gui_button_bbox_print_ER
    global gui_button_bbox_print_TD

    global gcanvas
    global gcanvas_td
    global gtext_sql

    ;# Enable some buttons

#     $gui_button_bbox_project itemconfigure 0 -state active ;# New Project
#     $gui_button_bbox_project itemconfigure 1 -state active ;# Open Project
     $gui_button_bbox_project itemconfigure 2 -state active ;# Save project
#     ;#$gui_button_bbox_project itemconfigure 3 -state active ;# Close project
    
#     $gui_button_bbox_elements itemconfigure 0 -state active ;# Insert entity
#     $gui_button_bbox_elements itemconfigure 1 -state active ;# Insert relation
#     ;#$gui_button_bbox_elements itemconfigure 2 -state active ;# Insert label

#     $gui_button_bbox_tools itemconfigure  0 -state active ;# Move
#     $gui_button_bbox_tools itemconfigure  1 -state active ;# Edit properties
#     $gui_button_bbox_tools itemconfigure  2 -state active ;# Delete

#     $gui_button_bbox_gen_TD itemconfigure 0 -state active ;# Generate tables
#     $gui_button_bbox_gen_output itemconfigure 0 -state active ;# Generate Output
    
#     ;#$gui_button_bbox_ssql itemconfigure 0 -state active ;# Save SQL

#     $gui_button_bbox_print_ER itemconfigure 0 -state active ;# Print ER
#     $gui_button_bbox_print_TD itemconfigure 0 -state active ;# Print TD

#     ;# Change some backgrounds
#     ${gcanvas} configure -bg white
#     ${gcanvas_td} configure -bg white


    ;# Create the app area pages
    ;#gui_app_area_create_Domain_page
    gui_app_area_create_Project_page
    gui_app_area_create_ER_page
    gui_app_area_create_TD_page
    gui_app_area_create_Output_page
    
    ;# Launch the bindings
    gui_launch_bindings

    ;# Set the editing state
    gerwin_set_state Editing
}



;#
;# gui_no_project_opened
;#
;# Disables some buttons and the color of some objects

proc gui_no_project_opened {} {

    global gui_button_bbox_project
    global gui_button_bbox_elements
    global gui_button_bbox_tools
    global gui_button_bbox_gen_TD
    global gui_button_bbox_gen_output
    global gui_button_bbox_gen_GerwinML
    global gui_button_bbox_ssql
    global gui_button_bbox_print_ER
    global gui_button_bbox_print_TD

    global gcanvas
    global gcanvas_td
    global gtext_sql

    ;# Disable some buttons

    $gui_button_bbox_project itemconfigure 2 -state disabled ;# Save project
    ;#$gui_button_bbox_project itemconfigure 3 -state disabled ;# Close project
    
#     $gui_button_bbox_elements itemconfigure 0 -state disabled ;# Insert entity
#     $gui_button_bbox_elements itemconfigure 1 -state disabled ;# Insert relation
    ;#$gui_button_bbox_elements itemconfigure 2 -state disabled ;# Insert label

#     $gui_button_bbox_tools itemconfigure  0 -state disabled ;# Move
#     $gui_button_bbox_tools itemconfigure  1 -state disabled ;# Edit properties
#     $gui_button_bbox_tools itemconfigure  2 -state disabled ;# Delete

#     $gui_button_bbox_gen_TD itemconfigure 0 -state disabled ;# Gen TD
#     $gui_button_bbox_gen_output itemconfigure 0 -state disabled  ;# Gen Output

#     ;#$gui_button_bbox_ssql itemconfigure 0 -state disabled

#     $gui_button_bbox_print_ER itemconfigure 0 -state disabled ;# Print ER
#     $gui_button_bbox_print_TD itemconfigure 0 -state disabled ;# Print TD


#     ;# Change the color of the canvas and text widget to grey
#     ${gcanvas} configure -bg grey
#     ${gcanvas_td} configure -bg grey
    ;#${gtext_sql} configure -bg grey
}

;#
;# gui_launch_bindings
;#
;# Launch the bindings of the canvas

proc gui_launch_bindings {} {

    global gcanvas
    global gmainframe

    set userframe [${gmainframe} getframe]

    ;# The button-1 (mark) binding
    ${gcanvas} bind all <Button-1> {gui_mark %x %y}

}

;#
;# gui_drag XPOS YPOS
;#
;# Event to mouse-drag-1 on the canvas

proc gui_drag {xpos ypos} {

    global gerwin_state
    global gmainframe
    global gerwin_premov_x
    global gerwin_premov_y
    global gerwin_premov_object
    global gerwin_obj_moving
    global gerwin_obj_type_moving
    global gm_entity
    global gm_relation
    global gerwin_relation
    global gcanvas

    ;# See if we are really moving
    if {$gerwin_state != "Moving"} then {
	return
    }

    set userframe [${gmainframe} getframe]

    ;# Map from view coordinates to canvas coordinates
    set xpos [${gcanvas} canvasx $xpos]
    set ypos [${gcanvas} canvasy $ypos]

    
    ;# Move the current object
    set dx [expr $xpos - $gerwin_premov_x]
    set dy [expr $ypos - $gerwin_premov_y]

    ;# Move the widget
    ${gcanvas} move $gerwin_premov_object $dx $dy


    ;# Actualize premov coordinates
    set gerwin_premov_x $xpos
    set gerwin_premov_y $ypos
 
    ;# Actualize the widget coordinates
    switch $gerwin_obj_type_moving {


	"Relation" {
	    set gm_relation($gerwin_obj_moving,xpos) [lindex [${gcanvas} bbox \
								  $gm_relation($gerwin_obj_moving,grouptag)] 0]
	    set gm_relation($gerwin_obj_moving,ypos) [lindex [${gcanvas} bbox \
								  $gm_relation($gerwin_obj_moving,grouptag)] 1]


	    ;# Update the links
	    if {$gerwin_relation($gerwin_obj_moving,reflexive)} then {
		;# It is a reflexive relation
		gm_relation_update_links_reflexive $gerwin_obj_moving
	    } else {
		;# Non-reflexive relation
		gm_relation_update_links $gerwin_obj_moving
	    }
	 
	}

	"Entity" {
	    set gm_entity($gerwin_obj_moving,xpos) [lindex [${gcanvas} bbox \
								     $gm_entity($gerwin_obj_moving,grouptag)] 0]
	    set gm_entity($gerwin_obj_moving,ypos) [lindex [${gcanvas} bbox \
								     $gm_entity($gerwin_obj_moving,grouptag)] 1]

	    ;# Update all the links
	    foreach r [gob_entity_get_relations $gerwin_obj_moving] {

		if {$gerwin_relation($r,reflexive)} then {
		    ;# Reflexive relation
		    gm_relation_update_links_reflexive $r
		} else {
		    gm_relation_update_links $r
		}

	    }
	    
	}

    }
    
}


;#
;# gui_drag_td XPOS YPOS
;#
;# Event to mouse-drag-1 on the TD canvas

proc gui_drag_td {xpos ypos} {

    global gmainframe
    global gerwin_premov_x
    global gerwin_premov_y
    global gerwin_premov_object
    global gerwin_obj_moving
    global gerwin_obj_type_moving
    global gm_table

    global gcanvas_td

    ;# We are always moving here
    ;# (for now :P) [jemarch]

    set userframe [${gmainframe} getframe]

    ;# Map from view coordinates to canvas coordinates
    set xpos [${gcanvas_td} canvasx $xpos]
    set ypos [${gcanvas_td} canvasy $ypos]

    ;# Move the current object
    set dx [expr $xpos - $gerwin_premov_x]
    set dy [expr $ypos - $gerwin_premov_y]

    ;# Actualize premov coordinates
    set gerwin_premov_x $xpos
    set gerwin_premov_y $ypos

    ;# Move the widget
    ${gcanvas_td} move $gerwin_premov_object $dx $dy

    ;# Actualize the coordinates
    switch $gerwin_obj_type_moving {

	"Flkey" {
	    ;# Get the name of the two linked tables
	    regsub {(.+)-([^-]+)} $gerwin_obj_moving "\\1" rtable
	    regsub {(.+)-([^-]+)} $gerwin_obj_moving "\\2" table

	    ;# Update the position of the corresponding flkey
	    gm_flkey_set_xpos $rtable $table [lindex [${gcanvas_td} bbox \
							  "taggroup-Flkey-$rtable-$table"] 0]
	    gm_flkey_set_ypos $rtable $table [lindex [${gcanvas_td} bbox \
							  "taggroup-Flkey-$rtable-$table"] 1]

	    ;# Update the links of the flkey

	    ;# Delete old links
	    foreach t [gob_table_get_foreign_keys $table] {
	
		set rtable [lindex $t 1]
	
		;#${gcanvas_td} delete "taggroup-Flkey-$rtable-$table"
		${gcanvas_td} delete "taggroup-Flkey-link-$rtable-$table"

	    }

	    ;# For each foreign key
	    foreach t [gob_table_get_foreign_keys $table] {

		set nrtable [lindex $t 1]
		set rattribute [lindex $t 0]

		if {$nrtable == $rtable} then {
		    lappend rattributes($rtable) $rattribute
		}

	    }

	    gm_draw_link_td $table $rtable $rattributes($rtable)

	}


	"Table" {



	    ;# Actualize the table coordinates
	    set gm_table($gerwin_obj_moving,xpos) [lindex [${gcanvas_td} bbox \
							       $gm_table($gerwin_obj_moving,grouptag)] 0]
	    set gm_table($gerwin_obj_moving,ypos) [lindex [${gcanvas_td} bbox \
							       $gm_table($gerwin_obj_moving,grouptag)] 1]

	    ;# Update the table links
	    gm_table_update_links $gerwin_obj_moving
	}
    }


}


;# 
;# gui_mark_td XPOS YPOS
;#
;# Evento to mouse-pointer-1 on the td canvas

proc gui_mark_td {xpos ypos} {

    global gmainframe
    global gerwin_tables
    global gerwin_premov_x
    global gerwin_premov_y
    global gerwin_premov_object
    global gerwin_obj_moving
    global gerwin_obj_type_moving
    
    global gcanvas_td

    set userframe [${gmainframe} getframe]

    ;# Get the coords onto the widget
    set xpos [${gcanvas_td} canvasx $xpos]
    set ypos [${gcanvas_td} canvasy $ypos]

    ;# Now, figure out what WGOB we want to delete
    set wgob [${gcanvas_td} find closest $xpos $ypos]

    ;# Get the taggroup from the WGOB
    set wgobtags [${gcanvas_td} gettags $wgob]
    set index [lsearch -glob $wgobtags "taggroup-*"]
    set taggroup [lindex $wgobtags $index]
    
    ;# Get the object name
    regsub {taggroup-([^-]+)-.+} $taggroup "\\1" gm_type

    if {$gm_type == "Flkey"} then {
	
	regsub {taggroup-[^-]+-(.+)} $taggroup "\\1" gm_name

    } else {
	regsub {taggroup-.*-(.+)} $taggroup "\\1" gm_name
    }

    set gerwin_obj_type_moving $gm_type
    set gerwin_obj_moving $gm_name

    set gerwin_premov_x $xpos
    set gerwin_premov_y $ypos
    set gerwin_premov_object $taggroup
}



;#
;# gui_mark XPOS YPOS
;#
;# Event to mouse-pointer-1 on the canvas

proc gui_mark {xpos ypos} {

    global gerwin_state
    global gmainframe
    global gerwin_entities
    global gerwin_relations
    global gerwin_relation
    global gerwin_premov_x
    global gerwin_premov_y
    global gerwin_premov_object
    global gerwin_entity_seq
    global gerwin_relation_seq
    global gerwin_obj_moving
    global gerwin_obj_type_moving
    global gui_entity1
    global gui_entity2
    global gcanvas
    global gm_entity
    global earea_pages


    set userframe [${gmainframe} getframe]

    ;# Switch over the actual state
    switch $gerwin_state {

	"Inserting relation begin" {

	    ;# Get the coords onto the widget
	    set xpos [${gcanvas} canvasx $xpos]
	    set ypos [${gcanvas} canvasy $ypos]

	    ;# Figure out what WGOB is the first entity
	    set wgob [${gcanvas} find closest $xpos $ypos]

	    ;# Get the taggroup tag from the WGOB
	    set wgobtags [${gcanvas} gettags $wgob]
	    set index [lsearch -glob $wgobtags "taggroup-*"]
	    set taggroup [lindex $wgobtags $index]

	    ;# Get the object name and type from the taggroup
	    regsub {taggroup-([^-]+)-.+} $taggroup "\\1" gm_type
	    regsub {taggroup-[^-]+-(.+)} $taggroup "\\1" gm_name

	    ;# Do nothing if this is not an entity
	    if {$gm_type != "Entity"} then {

		return

	    }

	    ;# Set the first entity, and then switch to "Inserting relation end" state
	    set gui_entity1 $gm_name

	    gerwin_set_state "Inserting relation end"
	    
	    . configure -cursor right_side
	}

	"Inserting relation end" {

	    ;# Get the coords onto the widget
	    set xpos [${gcanvas} canvasx $xpos]
	    set ypos [${gcanvas} canvasy $ypos]

	    ;# Figure out what WGOB is the first entity
	    set wgob [${gcanvas} find closest $xpos $ypos]

	    ;# Get the taggroup tag from the WGOB
	    set wgobtags [${gcanvas} gettags $wgob]
	    set index [lsearch -glob $wgobtags "taggroup-*"]
	    set taggroup [lindex $wgobtags $index]

	    ;# Get the object name and type from the taggroup
	    regsub {taggroup-([^-]+)-.+} $taggroup "\\1" gm_type
	    regsub {taggroup-[^-]+-(.+)} $taggroup "\\1" gm_name

	    if {$gm_type != "Entity"} then {

		;# See if we are adding an entity to a n-relation
		if {$gm_type == "Relation"} then {

		    ;# If the relation is reflexive, abort inmediatly
		    if {$gerwin_relation($gm_name,reflexive)} then {
			gui_notice "You cannot add a new entity to a reflexive relation!"

			;# Goto Editing state
			gerwin_set_state "Editing"

			. configure -cursor hand1
			return 
		    }

		    ;# If the entity is already part of the relation, abort inmediatly
		    if {[lsearch -glob [gob_relation_get_entities $gm_name] "$gui_entity1 *"] != -1} then {
			gui_notice "Sorry, $gui_entity1 already have a link to $gm_name"
			
			;# Goto Editing state
			gerwin_set_state "Editing"

			. configure -cursor hand1
			return
		    }

		    ;# Add a link to gui_entity1 to the relation
		    ;# Cardinality is always _/N
		    gob_relation_add_entity $gm_name $gui_entity1 1 N

		    ;# Add the relation to the entity's relation list
		    gob_entity_add_relation $gui_entity1 $gm_name

		    ;# Now, all entities have cardinality 1/N
		    ;# The _ is a fake value
		    foreach e [gob_relation_get_entities $gm_name] {
			set e [lindex $e 0]

			gob_relation_set_entity_min_card $gm_name $e _
			gob_relation_set_entity_max_card $gm_name $e N


		    }

		    ;# Actualize the edition page for the relation
		    ;# But only if it visible
		    if {[lsearch $earea_pages $gm_name] != -1} then {
			gui_edition_area_delete_page $gm_name
			gui_edit_relation $gm_name
		    }

		    ;# Update the links in the screen
		    gm_relation_update_links $gm_name

		    ;# Goto Editing state
		    gerwin_set_state "Editing"

		    . configure -cursor hand1
		}

		return
	    }

	    ;# Set the second entity
	    set gui_entity2 $gm_name

	    ;# Create the new relation
	    set rname "relation$gerwin_relation_seq"
	    incr gerwin_relation_seq
	    gob_create_relation $rname {} {}
	    gob_relation_add_entity $rname $gui_entity1 1 1
	    gob_relation_add_entity $rname $gui_entity2 1 1

	    ;# Add the relations to the entities
	    if {$gui_entity1 == $gui_entity2} then {
		;# Reflexive relation
		set gerwin_relation($rname,reflexive) 1
		gob_entity_add_relation $gui_entity1 $rname
	    } else {
		;# Non-reflexive relation
		gob_entity_add_relation $gui_entity1 $rname
		gob_entity_add_relation $gui_entity2 $rname
	    }

	    ;# Draw it
	    ;# Find an approximative good starting point

	    if {$gui_entity1 != $gui_entity2} then {
		;# Non reflexive relation
		set bbox1 [$gcanvas bbox taggroup-Entity-${gui_entity1}]
		set bbox2 [$gcanvas bbox taggroup-Entity-${gui_entity2}]

		set rp1x [expr \
			      [lindex $bbox1 0] + \
			      (([lindex $bbox1 2] - [lindex $bbox1 0]) / 2)]
						 
		set rp1y [expr \
			      [lindex $bbox1 1] +  \
			      (([lindex $bbox1 3] - [lindex $bbox1 1]) / 2)]

		set rp2x [expr \
			      [lindex $bbox2 0] + \
			      (([lindex $bbox2 2] - [lindex $bbox2 0]) / 2)]
						 
		set rp2y [expr \
			      [lindex $bbox2 1] +  \
			      (([lindex $bbox2 3] - [lindex $bbox2 1]) / 2)]

		set spx \
		    [expr ($rp1x + (($rp2x - $rp1x) / 2))]
		set spy \
		    [expr ($rp1y + (($rp2y - $rp1y) / 2))]

	    } else {
		;#  Reflexive relation
		set spx [expr $gm_entity($gui_entity1,xpos) + 100]
		set spy [expr $gm_entity($gui_entity1,ypos) - 50]

	    }

	    gm_draw_relation $rname $spx $spy

	    if {$gui_entity1 == $gui_entity2} then {
		;# Reflexive relation
		gm_relation_update_links_reflexive $rname
	    } else {
		;# No reflexive relation
		gm_relation_update_links $rname
	    }

	    ;# Goto Editing state
	    gerwin_set_state "Editing"

	    . configure -cursor hand1
	}

	"Inserting entity" {
	    
	    ;# Get the coords onto the widget
	    set xpos [${gcanvas} canvasx $xpos]
	    set ypos [${gcanvas} canvasy $ypos]

	    ;# Create a new entity, with a sequential name and 
	    ;# Without any attribute, and with the key empty.
	    set ename "entity$gerwin_entity_seq"
	    incr gerwin_entity_seq
	    gob_create_entity $ename {} {} {}

	    ;# Draw it
	    gm_draw_entity $ename $xpos $ypos

	    ;# Goto Editing state
	    gerwin_set_state "Editing"

	    . configure -cursor hand1	    

	}

	"Moving" {
	    
	    ;# Get the coords onto the widget
	    set xpos [${gcanvas} canvasx $xpos]
	    set ypos [${gcanvas} canvasy $ypos]

	    ;# Now, figure out what WGOB we want to delete
	    set wgob [${gcanvas} find closest $xpos $ypos]
			

	    ;# Get the taggroup tag from the WGOB
	    set wgobtags [${gcanvas} gettags $wgob]
	    set index [lsearch -glob $wgobtags "taggroup-*"]
	    set taggroup [lindex $wgobtags $index]

	    ;# Get the object name and type from the taggroup
	    regsub {taggroup-([^-]+)-.+} $taggroup "\\1" gm_type
	    regsub {taggroup-[^-]+-(.+)} $taggroup "\\1" gm_name

	    set gerwin_obj_moving $gm_name
	    set gerwin_obj_type_moving $gm_type

	    set gerwin_premov_x $xpos
	    set gerwin_premov_y $ypos
	    set gerwin_premov_object $taggroup

	}

	"Editing" {

	    ;# Get the coords onto the widget
	    set xpos [${gcanvas} canvasx $xpos]
	    set ypos [${gcanvas} canvasy $ypos]

	    ;# Now, figure out what WGOB we want to delete
	    set wgob [${gcanvas} find closest $xpos $ypos]
			

	    ;# Get the taggroup tag from the WGOB
	    set wgobtags [${gcanvas} gettags $wgob]
	    set index [lsearch -glob $wgobtags "taggroup-*"]
	    set taggroup [lindex $wgobtags $index]
	    
	    ;# Get the object name and type from the taggroup
	    regsub {taggroup-([^-]+)-.+} $taggroup "\\1" gm_type
	    regsub {taggroup-[^-]+-(.+)} $taggroup "\\1" gm_name

	    ;# Switch over the type
	    switch $gm_type {

		"Relation" {

		    gui_edit_relation $gm_name

		}

		"Entity" {

		    gui_edit_entity $gm_name

		}
	    }

	}

	"Deleting" {

	    ;# Get the coords onto the widget
	    set xpos [${gcanvas} canvasx $xpos]
	    set ypos [${gcanvas} canvasy $ypos]

	    ;# Now, figure out what WGOB we want to delete
	    set wgob [${gcanvas} find closest $xpos $ypos]
			

	    ;# Get the taggroup tag from the WGOB
	    set wgobtags [${gcanvas} gettags $wgob]
	    set index [lsearch -glob $wgobtags "taggroup-*"]
	    set taggroup [lindex $wgobtags $index]

	    ;# Get the object name and type from the taggroup
	    regsub {taggroup-([^-]+)-.+} $taggroup "\\1" gm_type
	    regsub {taggroup-[^-]+-(.+)} $taggroup "\\1" gm_name

	    ;# Switch over the type
	    switch $gm_type {
		
		"Relation" {
		    ;# Delete the relation from the canvas
		    gm_delete_relation $gm_name

		    ;# Delete the relation from all entities that haves that relation registered
		    foreach e [gob_relation_get_entities $gm_name] {
			
			gob_entity_delete_relation [lindex $e 0] $gm_name

		    }
		    
		    ;# Delete relation's links
		    ${gcanvas} delete "taggroup-Link-$gm_name"

		    ;# Delete the GOB
		    gob_delete_relation $gm_name

		    ;# Delete the relation page from the editionarea
		    gui_edition_area_delete_page $gm_name
		}

		"Entity" {
		    ;# Delete the entity from the canvas
		    gm_delete_entity $gm_name

		    ;# Delete all relations with num_entities equal to two
		    foreach r [gob_entity_get_relations $gm_name] {

			if {[gob_relation_get_num_entities $r] == 2} then {

			    ;# Delete the relation from the canvas
			    gm_delete_relation $r

			    ;# Delete the relation from all entities that haves that relation registered
			    foreach e [gob_relation_get_entities $r] {
			
				gob_entity_delete_relation [lindex $e 0] $r

			    }
		    
			    ;# Delete relation's links
			    ${gcanvas} delete "taggroup-Link-$r"

			    ;# Delete the GOB
			    gob_delete_relation $r

			    ;# Delete the relation page from the editionarea
			    gui_edition_area_delete_page $r
			

			} else {

			    ;# Quit the entity from the relation
			    gob_relation_delete_entity $r $gm_name

			    ;# Update the relation links
			    ;# Note that this relation can not be reflexive
			    gm_relation_update_links $r

			}

		    }

		    ;# Delete the GOB
		    gob_delete_entity $gm_name

		    ;# Delete the entity page from the editionarea
		    gui_edition_area_delete_page $gm_name

		}

	    
	    }
	}
    }

}

;#
;# gui_edition_area_delete_page PNAME
;#
;# Deletes the page named PNAME from the edition area

proc gui_edition_area_delete_page {pname} {

    global gmainframe
    global earea_pages
    global gerwin_editionarea

    set userframe [${gmainframe} getframe]

    ;# Find the page to delete
    set index [lsearch $earea_pages $pname]
    
    if {$index != -1} then {

	set w [${gerwin_editionarea} getframe $pname]
	;#set w [${userframe}.editionarea getframe $pname]


	;# Delete the page from the list
	set earea_pages [lreplace $earea_pages $index $index]

	;# Update the notebook
	${gerwin_editionarea} delete $pname
	;#${userframe}.editionarea delete $pname

	;# Destroy the widgets
	destroy $w
    }

}


;#
;# gui_attribute_is_not_key LISTBOX ENTITY
;#
;# Remove the current LISTBOX selection from the ENTITY key

proc gui_attribute_is_not_key {lb entity} {

    global gm_entity
    global gerwin_relation

    set attribute [$lb curselection]

    if {! [string equal $attribute ""]} then {
	
	set attribute [$lb get $attribute]

	gob_entity_delete_attribute_from_key $entity $attribute
	set xpos $gm_entity($entity,xpos)
	set ypos $gm_entity($entity,ypos)
	gm_delete_entity $entity
	gm_draw_entity $entity $xpos $ypos

	foreach r [gob_entity_get_relations $entity] {

	    if {$gerwin_relation($r,reflexive)} then {
		gm_relation_update_links_reflexive $r
	    } else {
		gm_relation_update_links $r
	    }

	}
    }
}

;#
;# gui_attribute_delete_relation LISTBOX RELATION
;#
;# Delete the LISTBOX selected attribute from RELATION

proc gui_attribute_delete_relation {lb relation} {

    global gm_relation
    global gerwin_relation
    global gerwin_editionarea

    set attribute [$lb curselection]

    if {! [string equal $attribute ""]} then {

	set attribute [$lb get $attribute]
	set vattribute $attribute

	# Support delete or Edit
	set editframe [${gerwin_editionarea} getframe $relation]
	set aframe [${editframe}.attributes getframe]

	${aframe}.nattribute.faux.ftop.label configure -text $vattribute

	set dfr [${aframe}.nattribute.faux.fdown.domframe getframe]
	${dfr}.label configure -text [gob_relation_get_attribute_domain $relation $vattribute]



	gob_relation_delete_attribute $relation $attribute
	set xpos $gm_relation($relation,xpos)
	set ypos $gm_relation($relation,ypos)
	gm_delete_relation $relation
	gm_draw_relation $relation $xpos $ypos
	gui_edition_area_delete_page $relation
	gui_edit_relation $relation

	if {$gerwin_relation($relation,reflexive)} then {
	    gm_relation_update_links_reflexive $relation
	} else {
	    gm_relation_update_links $relation
	}


    }

}


;#
;# gui_attribute_delete_entity LISTBOX ENTITY
;#
;# Delete the LISTBOX selected attribute from ENTITY

proc gui_attribute_delete_entity {lb entity} {

    global gm_entity 
    global gerwin_relation
    global gerwin_editionarea

    set attribute [$lb curselection]



    if {! [string equal $attribute ""]} then {
    
	set vattribute [$lb get $attribute]

	# Support delete or Edit
	set editframe [${gerwin_editionarea} getframe $entity]
	set aframe [${editframe}.attributes getframe]

	${aframe}.nattribute.faux.ftop.label configure -text $vattribute

	set dfr [${aframe}.nattribute.faux.fdown.domframe getframe]
	${dfr}.label configure -text [gob_entity_get_attribute_domain $entity $vattribute]


	set attribute [$lb get $attribute]

	gob_entity_delete_attribute $entity $attribute
	set xpos $gm_entity($entity,xpos)
	set ypos $gm_entity($entity,ypos)
	gm_delete_entity $entity
	gm_draw_entity $entity $xpos $ypos
	gui_edition_area_delete_page $entity
	gui_edit_entity $entity

	foreach r [gob_entity_get_relations $entity] {

	    if {$gerwin_relation($r,reflexive)} then {
		gm_relation_update_links_reflexive $r
	    } else {
		gm_relation_update_links $r
	    }

	}

    }
}


;#
;# gui_attribute_is_key LISTBOX ENTITY
;#
;# Add the attribute selected in LISTBOX as a new key on ENTITY

proc gui_attribute_is_key {lb entity} {

    global gm_entity 
    global gerwin_relation

    set attribute [$lb curselection]

    if {! [string equal $attribute ""]} then {

	set attribute [$lb get $attribute]

	;# We exit if the attribute is already a key
	if {[gob_entity_is_key $entity $attribute]} then {
	    return
	}


	gob_entity_add_attribute_to_key $entity $attribute
	set xpos $gm_entity($entity,xpos)
	set ypos $gm_entity($entity,ypos)
	gm_delete_entity $entity
	gm_draw_entity $entity $xpos $ypos

	foreach r [gob_entity_get_relations $entity] {

	    if {$gerwin_relation($r,reflexive)} then {
		gm_relation_update_links_reflexive $r
	    } else {
		gm_relation_update_links $r
	    }

	}

    }
}


;#
;# gui_edit_relation_add_attribute RELATION
;#
;# Add a new attribute with ANAME and ADOMAIN to RELATION
;#
;# ANAME and ADOMAIN cannot be emtpy.
;# All blanks in ANAME and ADOMAIN are transformed to "-" characters.

proc gui_edit_relation_add_attribute {relation} {

    global gm_relation
    global gerwin_relation

    set aname $gm_relation($relation,nattribute)
    set adomain $gm_relation($relation,ndomain)

    ;# First of all, make sure both ANAME and ADOMAIN are not empty
    if {($aname == "")} then {
	
	gui_notice "Please fill the Name field if you want to add an attribute to the relation."
	return
    }

    if {($adomain == "")} then {

	gui_notice "Please fill the Domain field if you want to add an attribute to the relation."
	return
    }

    ;# Replace all blanks with scores
    set aname [string map {{ } -} $aname]
    set adomain [string map {{ } -} $adomain]

    ;# Add the attribute to the gob
    gob_relation_add_attribute $relation $aname $adomain
    
    set gm_relation($relation,ndomain) {}
    set gm_relation($relation,nattribute) {}

    set xpos $gm_relation($relation,xpos)
    set ypos $gm_relation($relation,ypos)

    gm_delete_relation $relation
    gm_draw_relation $relation $xpos $ypos

    if {$gerwin_relation($relation,reflexive)} then {

	gm_relation_update_links_reflexive $relation
             
    } else {
	
	gm_relation_update_links $relation
        
    }

    gui_edition_area_delete_page $relation
    gui_edit_relation $relation

}



;#
;# gui_edit_entity_add_attribute ENTITY
;#
;# Add a new attribute with ANAME and ADOMAIN to ENTITY
;#
;# ANAME and ADOMAIN cannot be emtpy.
;# All blanks in ANAME and ADOMAIN are transformed to "-" characters.

proc gui_edit_entity_add_attribute {entity} {

    global gm_entity
    global gerwin_relation

    set aname $gm_entity($entity,nattribute)
    set adomain $gm_entity($entity,ndomain)

    ;# First of all, make sure both ANAME and ADOMAIN are not empty
    if {($aname == "")} then {
	
	gui_notice "Please fill the Name field if you want to add an attribute to the entity."
	return
    }

    if {($adomain == "")} then {

	gui_notice "Please fill the Domain field if you want to add an attribute to the entity."
	return
    }

    ;# Replace all blanks with scores
    set aname [string map {{ } -} $aname]
    set adomain [string map {{ } -} $adomain]

    ;# Add the attribute to the gob
    gob_entity_add_attribute $entity $aname $adomain

    set gm_entity($entity,ndomain) {}
    set gm_entity($entity,nattribute) {}

    set xpos $gm_entity($entity,xpos)
    set ypos $gm_entity($entity,ypos)

    gm_delete_entity $entity
    gm_draw_entity $entity $xpos $ypos

    foreach r [gob_entity_get_relations $entity] {

	if {$gerwin_relation($r,reflexive)} then {
	    
	    gm_relation_update_links_reflexive $r
	    
	} else {
	    

	    gm_relation_update_links $r

	}
    }

    gui_edition_area_delete_page $entity
    gui_edit_entity $entity

}




;#
;# gui_edit_relation_change_name RELATION
;#
;# Changes the name of RELATION to gm_relation(RELATION,nname)

proc gui_edit_relation_change_name {relation} {

    global gcanvas
    global gm_relation
    global earea_pages
    global gerwin_relation

    ;# See if the new name is empty
    if {$gm_relation($relation,nname) == ""} then {

	gui_notice "The name of the relation cannot be empty!"
	return
    }

    ;# Change spaces for underscores
    set gm_relation($relation,nname) [string map {{ } _} $gm_relation($relation,nname)]

    if {([gob_entity_exist $gm_relation($relation,nname)] ||
	 [gob_relation_exist $gm_relation($relation,nname)])} then {

	gui_notice "The name of any relation or entity must be unique in all the model!"
	return
    }

    ;# Change the name of the gob
    gob_relation_change_name $relation $gm_relation($relation,nname)
    set nname $gm_relation($relation,nname)
    gui_edition_area_delete_page $relation
    
    gm_relation_change_name $relation $nname
    set gm_relation($relation,nname) {}
    
    gui_edit_relation $nname

    set xpos $gm_relation($nname,xpos)
    set ypos $gm_relation($nname,ypos)

    gm_delete_relation $nname
    ${gcanvas} delete taggroup-Link-$relation
    gm_draw_relation $nname $xpos $ypos

    if {$gerwin_relation($nname,reflexive)} then {

	gm_relation_update_links_reflexive $nname

    } else {

	gm_relation_update_links $nname
    }


}

;#
;# gui_edit_entity_change_name ENTITY
;#
;# Changes the name of ENTITY to gm_entity(ENTITY,nname)


proc gui_edit_entity_change_name {entity} {

    global gm_entity
    global earea_pages
    global gerwin_relation

    ;# See if the new name is empty
    if {$gm_entity($entity,nname) == ""} then {

	gui_notice "The name of the entity cannot be empty!"
	return
    }

    ;# Change spaces for underscores
    set gm_entity($entity,nname) [string map {{ } _} $gm_entity($entity,nname)]

    if {([gob_entity_exist $gm_entity($entity,nname)]) || 
	([gob_relation_exist $gm_entity($entity,nname)])} then {

	gui_notice "The name of any entity or relation must be unique in all the model!"
	return 
    }

    ;# Change the name of the gob
    gob_entity_change_name $entity $gm_entity($entity,nname)

    set nname $gm_entity($entity,nname)
    gui_edition_area_delete_page $entity

    ;# Change the name in all relation edition areas
    foreach r [gob_entity_get_relations $nname] {

	if {[lsearch $earea_pages $r] != -1} then {

	    gui_edition_area_delete_page $r
	    gui_edit_relation $r
	}
    }


    gui_edit_entity $nname

    set xpos $gm_entity($entity,xpos)
    set ypos $gm_entity($entity,ypos)

    
    gm_entity_change_name $entity $nname
    gm_delete_entity $nname
    gm_draw_entity $nname $xpos $ypos

    foreach r [gob_entity_get_relations $nname] {

	;# TODO: change to simply gm_relation_update_links
	if {$gerwin_relation($r,reflexive)} then {
	    gm_relation_update_links_reflexive $r
	} else {
	    gm_relation_update_links $r
	    
	}

    }
}


;#
;# gui_edit_entity ENTITY
;#
;# Edit all aspects of a given entity, in a page on the 
;# editionarea

proc gui_edit_entity {entity} {

    global gmainframe
    global gerwin_entity
    global gm_entity
    global gm_relation
    global gerwin_relation

    global earea_pages 
    global gerwin_editionarea

    set userframe [${gmainframe} getframe]
    
    ;# See if the entity already have a page in the edition area
    set index [lsearch $earea_pages $entity]
    if {$index != -1} then {
	
	;# Make the page visible
	${gerwin_editionarea} raise [lindex $earea_pages $index]
	;#${userframe}.editionarea raise [lindex $earea_pages $index]

	return

    }

    ;# Create a new page for this entity
    lappend earea_pages $entity
    ${gerwin_editionarea} insert end $entity -text $entity
    ;#${userframe}.editionarea insert end $entity -text $entity

    ;# Make the entries into the frame
    set editframe [${gerwin_editionarea} getframe $entity]
    ;#set editframe [${userframe}.editionarea getframe $entity]


    ;#####    PROPERTIES FRAME

    TitleFrame ${editframe}.properties -side center -text "Entity Properties"
    set pframe [${editframe}.properties getframe]

    ;# ENTITY NAME
    frame ${pframe}.nameline

    LabelEntry ${pframe}.nameline.label -label "Name: " -labelwidth 6 \
	-text $entity -textvariable gm_entity($entity,nname) \
	-helptext "Name of the entity"

    button ${pframe}.nameline.button -text "change" -relief groove \
	-command [list gui_edit_entity_change_name $entity]


    frame ${pframe}.typeline
    LabelEntry ${pframe}.typeline.label -label "Type:" -labelwidth 6 \
	-textvariable gm_entity($entity,ntype) \
	-helptext "Type of the entity"
    button ${pframe}.typeline.button -text "change" -relief groove \
	-command "gob_entity_change_type $entity \$gm_entity($entity,ntype); \
                  set ttype \$gm_entity($entity,ntype) ; \
                  gui_edition_area_delete_page $entity ; \
                  gm_entity_change_type $entity \$gm_entity($entity,ntype); \
                  gui_edit_entity $entity ; \
                  set xpos \$gm_entity($entity,xpos); \
                  set ypos \$gm_entity($entity,ypos); \
                  gm_delete_entity $entity; \
                  gm_draw_entity $entity \$xpos \$ypos ; " \
	-state disabled



    ;# pack it
    pack ${pframe}.nameline.label -side left
    pack ${pframe}.nameline.button -side left


    pack ${pframe}.typeline.label -side left
    pack ${pframe}.typeline.button -side left

    pack ${pframe}.nameline -side top
    pack ${pframe}.typeline -side top
    pack ${pframe} -side left -anchor nw

    ;####   ATTRIBUTES FRAME

    TitleFrame ${editframe}.attributes -side center -text "Attributes"
    set aframe [${editframe}.attributes getframe]

    frame ${aframe}.nattribute
    label ${aframe}.nattribute.label -text "New Attribute"
    frame ${aframe}.nattribute.faux
    frame ${aframe}.nattribute.faux.ftop
    frame ${aframe}.nattribute.faux.fdown
    LabelEntry ${aframe}.nattribute.faux.ftop.label -label "Name:" -labelwidth 7 \
	-labelanchor w -textvariable gm_entity($entity,nattribute) \
	-helptext "name for the new attribute"

#     LabelEntry ${aframe}.nattribute.faux.fdown.label -label "Domain:" -labelwidth 7 \
# 	-labelanchor w -textvariable gm_entity($entity,ndomain) \
# 	-helptext "domain for the new attribute"

    LabelFrame ${aframe}.nattribute.faux.fdown.domframe -text "Domain:" \
    -side left

    set dfr [${aframe}.nattribute.faux.fdown.domframe getframe]
    
    ComboBox $dfr.label -editable true \
	-textvariable gm_entity($entity,ndomain) \
	-helptext "domain for the new attribute" \
        -width 18 \
        -values {varchar(8) varchar(16) varchar(32) int bigint text date} 

    pack $dfr.label -side top -anchor e



    Button ${aframe}.nattribute.button -text "Add" -relief groove \
	-command [list gui_edit_entity_add_attribute $entity]


    Separator ${aframe}.sep1 -orient vertical

    frame ${aframe}.attributes

    frame ${aframe}.attributes.frame
    scrollbar ${aframe}.attributes.frame.scrollbar \
	-command [list ${aframe}.attributes.frame.lbox yview]



    set lb [listbox ${aframe}.attributes.frame.lbox \
		-yscrollcommand [list ${aframe}.attributes.frame.scrollbar set]]

    ;# Insert the listbox elements
    foreach a [gob_entity_get_attributes $entity] {

	set aname [lindex $a 0]
	;# Make the embedded window
	$lb insert end $aname

    }
    

    ;# make the bbox
    set bbox [ButtonBox ${aframe}.bbox -spacing 0 -padx 1 -pady 0 \
		 -orient vertical]

    ${bbox} add -text "Is key" -command [list gui_attribute_is_key $lb $entity] -relief groove
    ${bbox} add -text "Is not key" -command [list gui_attribute_is_not_key $lb $entity] -relief groove
    ${bbox} add -text "Del/Edit" -command [list gui_attribute_delete_entity $lb $entity] -relief groove 

    pack ${aframe}.nattribute.label -side top
    pack ${aframe}.nattribute.faux.ftop.label -side top -anchor w
    pack ${aframe}.nattribute.faux.ftop -side top
    pack ${aframe}.nattribute.faux.fdown.domframe -side top -anchor w
    pack ${aframe}.nattribute.faux.fdown -side top 
    pack ${aframe}.nattribute.faux -side left
    pack ${aframe}.nattribute.button -side right


    pack ${aframe}.nattribute -side left

    pack ${aframe}.sep1 -fill y -padx 8 -side left

    pack ${aframe}.attributes.frame.scrollbar -side right -fill y
    pack ${aframe}.attributes.frame.lbox -side left
    pack ${aframe}.attributes.frame

    pack ${aframe}.attributes -side left

    pack ${aframe}.bbox -side right

    button ${editframe}.quit -text "Close page" -relief groove \
	-command "destroy ${editframe} ; \
                  gui_edition_area_delete_page $entity"

    ;# Pack the frames
    pack ${editframe}.properties -side left -fill both -padx 10
    pack ${editframe}.attributes -side left -fill both -padx 10
    pack ${editframe}.quit -side right
    pack ${editframe}


    ;# Make the page visible
    ${gerwin_editionarea} raise $entity

    ;# Scale the notebook
    NoteBook::compute_size ${gerwin_editionarea}
}

;#
;# gui_edit_relation
;#
;# Edit all aspects of a given relation, in a page on the
;# editionarea

proc gui_edit_relation {relation} {

    global gmainframe
    global gerwin_relation
    global gm_relation
    global gcanvas

    global earea_pages
    global gerwin_editionarea

    set userframe [${gmainframe} getframe]
    
    ;# See if the relation already have a page in the edition area
    set index [lsearch $earea_pages $relation]
    if {$index != -1} then {

	;# Make the page visible
	${gerwin_editionarea} raise [lindex $earea_pages $index]
	;#${userframe}.editionarea raise [lindex $earea_pages $index]

	return
    }

    ;# Create a new page for this relation
    lappend earea_pages $relation
    ${gerwin_editionarea} insert end $relation -text $relation
    ;#${userframe}.editionarea insert end $relation -text $relation

    ;# Make the entries into the frame
    set editframe [${gerwin_editionarea} getframe $relation]
    ;#set editframe [${userframe}.editionarea getframe $relation]

    ;##### PROPERTIES FRAME

    TitleFrame ${editframe}.properties -side center -text "Relation Properties"
    set pframe [${editframe}.properties getframe]

    ;# RELATION NAME
    frame ${pframe}.nameline

    LabelEntry ${pframe}.nameline.label -label "Name: " -labelwidth 6 \
	-text $relation -textvariable gm_relation($relation,nname) \
	-helptext "Name of the relation"

    button ${pframe}.nameline.button -text "change" -relief groove \
	-command [list gui_edit_relation_change_name $relation]

    
    ;# pack it
    pack ${pframe}.nameline.label -side left
    pack ${pframe}.nameline.button -side left
    pack ${pframe}.nameline -side top


    ;# RELATION ENTITIES and cardinality (entities 0 and 1 only)
    frame ${pframe}.cards


    if {[gob_relation_get_num_entities $relation] > 2} then {
	;# Show a short label 
	;#label ${pframe}.cards.label -text "All entitities cardinality X/N"
	;#pack ${pframe}.cards.label

    } else {
	;# Binary relation: show cardinality
    
	frame ${pframe}.cards.entity1

	set entities [gob_relation_get_entities $relation]
	set entity1_name [lindex [lindex $entities 0] 0]
	set entity1_mincard [lindex [lindex [lindex $entities 0] 1] 0]
	set entity1_maxcard [lindex [lindex [lindex $entities 0] 1] 1]
	set entity2_name [lindex [lindex $entities 1] 0]
	set entity2_mincard [lindex [lindex [lindex $entities 1] 1] 0]
	set entity2_maxcard [lindex [lindex [lindex $entities 1] 1] 1]

	Label ${pframe}.cards.entity1.ename -text $entity1_name

	;# Set up the variables
	set gerwin_relation($relation,e1mincard) $entity1_mincard
	set gerwin_relation($relation,e1maxcard) $entity1_maxcard

	;# Minimal card
	radiobutton ${pframe}.cards.entity1.rbmin0 -text "0" \
	    -variable gerwin_relation($relation,e1mincard) -value 0 \
	    -command "gob_relation_set_entity_min_card $relation $entity1_name \$gerwin_relation($relation,e1mincard) ; \
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"

	radiobutton ${pframe}.cards.entity1.rbmin1 -text "1" \
	    -variable gerwin_relation($relation,e1mincard) -value 1 \
	    -command "gob_relation_set_entity_min_card $relation $entity1_name \$gerwin_relation($relation,e1mincard) ; \
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"

	;# Separator
	label ${pframe}.cards.entity1.sep -text "/"

	;# Maximal card
	radiobutton ${pframe}.cards.entity1.rbmax1 -text "1" \
	    -variable gerwin_relation($relation,e1maxcard) -value 1 \
	    -command "gob_relation_set_entity_max_card $relation $entity1_name \$gerwin_relation($relation,e1maxcard) ; \
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"


	radiobutton ${pframe}.cards.entity1.rbmaxN -text "N" \
	    -variable gerwin_relation($relation,e1maxcard) -value N \
	    -command "gob_relation_set_entity_max_card $relation $entity1_name \$gerwin_relation($relation,e1maxcard) ; \
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"

	;# Set gui_e1mincard and gui_e1maxcard as appropiate
	set qui_e1mincard [gob_relation_get_entity_min_card $relation $entity1_name]
	set gui_e1maxcard [gob_relation_get_entity_max_card $relation $entity1_name]


	pack ${pframe}.cards.entity1.ename -side top
	pack ${pframe}.cards.entity1.rbmin0 -side left
	pack ${pframe}.cards.entity1.rbmin1 -side left
	pack ${pframe}.cards.entity1.sep -side left
	pack ${pframe}.cards.entity1.rbmax1 -side left
	pack ${pframe}.cards.entity1.rbmaxN -side left

	pack ${pframe}.cards.entity1 -side left

	frame ${pframe}.cards.entity2

	;# Set up the variables
	set gerwin_relation($relation,e2mincard) $entity2_mincard
	set gerwin_relation($relation,e2maxcard) $entity2_maxcard

	Label ${pframe}.cards.entity2.ename -text $entity2_name

	;# Minimal card
	radiobutton ${pframe}.cards.entity2.rbmin0 -text "0" \
	    -variable gerwin_relation($relation,e2mincard) -value 0 \
	    -command "
                  set entities \[gob_relation_get_entities $relation\]
                  set e1 \[lindex \$entities 0\]
                  set e2 \[lindex \$entities 1\]
                  set e2name \[lindex \$e2 0\]
                  set e2cards \[lindex \$e2 1\]
                  set gerwin_relation($relation,entities) \[list \$e1 \[list \$e2name \[list \$gerwin_relation($relation,e2mincard) \[lindex \$e2cards 1\]\]\]\]
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"

	radiobutton ${pframe}.cards.entity2.rbmin1 -text "1" \
	    -variable gerwin_relation($relation,e2mincard) -value 1 \
	    -command "
                  set entities \[gob_relation_get_entities $relation\]
                  set e1 \[lindex \$entities 0\]
                  set e2 \[lindex \$entities 1\]
                  set e2name \[lindex \$e2 0\]
                  set e2cards \[lindex \$e2 1\]
                  set gerwin_relation($relation,entities) \[list \$e1 \[list \$e2name \[list \$gerwin_relation($relation,e2mincard) \[lindex \$e2cards 1\]\]\]\]
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"

	;# Separator
	label ${pframe}.cards.entity2.sep -text "/"

	;# Maximal card
	radiobutton ${pframe}.cards.entity2.rbmax1 -text "1" \
	    -variable gerwin_relation($relation,e2maxcard) -value 1 \
	    -command "
                  set entities \[gob_relation_get_entities $relation\]
                  set e1 \[lindex \$entities 0\]
                  set e2 \[lindex \$entities 1\]
                  set e2name \[lindex \$e2 0\]
                  set e2cards \[lindex \$e2 1\]
                  set gerwin_relation($relation,entities) \[list \$e1 \[list \$e2name \[list \[lindex \$e2cards 0\] \$gerwin_relation($relation,e2maxcard) \]\]\]
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \
                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"


	radiobutton ${pframe}.cards.entity2.rbmaxN -text "N" \
	    -variable gerwin_relation($relation,e2maxcard) -value N \
	    -command "
                  set entities \[gob_relation_get_entities $relation\]
                  set e1 \[lindex \$entities 0\]
                  set e2 \[lindex \$entities 1\]
                  set e2name \[lindex \$e2 0\]
                  set e2cards \[lindex \$e2 1\]
                  set gerwin_relation($relation,entities) \[list \$e1 \[list \$e2name \[list \[lindex \$e2cards 0\] \$gerwin_relation($relation,e2maxcard) \]\]\]
                  set xpos \$gm_relation($relation,xpos); \
                  set ypos \$gm_relation($relation,ypos); \
                  gm_delete_relation $relation ; \
                  gm_draw_relation $relation \$xpos \$ypos ; \

                  if {\$gerwin_relation($relation,reflexive)} then {
                         gm_relation_update_links_reflexive $relation
                  } else {
                         gm_relation_update_links $relation
                  }"


	;# Set gui_e2maxcard as appropiate
	set gui_e2maxcard [gob_relation_get_entity_card $relation $entity2_name]

	pack ${pframe}.cards.entity2.ename -side top
	pack ${pframe}.cards.entity2.rbmin0 -side left
	pack ${pframe}.cards.entity2.rbmin1 -side left
	pack ${pframe}.cards.entity2.sep -side left
	pack ${pframe}.cards.entity2.rbmax1 -side left
	pack ${pframe}.cards.entity2.rbmaxN -side left

	Separator ${pframe}.cards.sep2 -orient vertical
	pack ${pframe}.cards.sep2 -fill y -padx 4 -side left

	pack ${pframe}.cards.entity2 -side right


	pack ${pframe}.cards -side left

	pack ${pframe}.cards -side top
    }

    pack ${pframe} -side left -anchor w

    ;#### ATTRIBUTES FRAME

    TitleFrame ${editframe}.attributes -side center -text "Attributes"
    set aframe [${editframe}.attributes getframe]

    frame ${aframe}.nattribute
    label ${aframe}.nattribute.label -text "New Attribute"
    frame ${aframe}.nattribute.faux
    frame ${aframe}.nattribute.faux.ftop
    frame ${aframe}.nattribute.faux.fdown
    LabelEntry ${aframe}.nattribute.faux.ftop.label -label "Name:" -labelwidth 7 \
	-labelanchor w -textvariable gm_relation($relation,nattribute) \
	-helptext "name for the new attribute"
#     LabelEntry ${aframe}.nattribute.faux.fdown.label -label "Domain:" -labelwidth 7 \
# 	-labelanchor w -textvariable gm_relation($relation,ndomain) \
# 	-helptext "domain for the new attribute"

    LabelFrame ${aframe}.nattribute.faux.fdown.domframe -text "Domain:" \
    -side left

    set dfr [${aframe}.nattribute.faux.fdown.domframe getframe]
    
    ComboBox $dfr.label -editable true \
	-textvariable gm_relation($relation,ndomain) \
	-helptext "domain for the new attribute" \
        -width 18 \
        -values {varchar(8) varchar(16) varchar(32) int bigint text date} 

    pack $dfr.label -side top -anchor e

    Button ${aframe}.nattribute.button -text "Add" -relief groove \
	-command [list gui_edit_relation_add_attribute $relation]


    Separator ${aframe}.sep1 -orient vertical

    frame ${aframe}.attributes

    frame ${aframe}.attributes.frame
    scrollbar ${aframe}.attributes.frame.scrollbar \
	-command [list ${aframe}.attributes.frame.lbox yview]



    set lb [listbox ${aframe}.attributes.frame.lbox \
		-yscrollcommand [list ${aframe}.attributes.frame.scrollbar set]]

    ;# Insert the listbox elements
    foreach a [gob_relation_get_attributes $relation] {

	set aname [lindex $a 0]
	;# Make the embedded window
	$lb insert end $aname

    }
    

    ;# make the bbox
    set bbox [ButtonBox ${aframe}.bbox -spacing 0 -padx 1 -pady 0 \
		 -orient vertical]

    ${bbox} add -text "Del/Edit" -command [list gui_attribute_delete_relation $lb $relation] -relief groove 

    pack ${aframe}.nattribute.label -side top
    pack ${aframe}.nattribute.faux.ftop.label -side top -anchor w
    pack ${aframe}.nattribute.faux.ftop -side top
    pack ${aframe}.nattribute.faux.fdown.domframe -side top -anchor w
    pack ${aframe}.nattribute.faux.fdown -side top 
    pack ${aframe}.nattribute.faux -side left
    pack ${aframe}.nattribute.button -side right


    pack ${aframe}.nattribute -side left

    pack ${aframe}.sep1 -fill y -padx 8 -side left

    pack ${aframe}.attributes.frame.scrollbar -side right -fill y
    pack ${aframe}.attributes.frame.lbox -side left
    pack ${aframe}.attributes.frame

    pack ${aframe}.attributes -side left

    pack ${aframe}.bbox -side right

    button ${editframe}.quit -text "Close page" -relief groove \
	-command "destroy ${editframe} ; \
                  gui_edition_area_delete_page $relation"

    ;# Pack the frames
    pack ${editframe}.properties -side left -fill both -padx 10
    pack ${editframe}.attributes -side left -fill both -padx 10
    pack ${editframe}.quit -side right
    pack ${editframe}


    ;# Make the page visible
    ${gerwin_editionarea} raise $relation
    ;#${userframe}.editionarea raise $relation

    ;# Scale the notebook
    NoteBook::compute_size ${gerwin_editionarea}
}


;#
;# gui_select_file_to_save_sql
;#
;# Select a file to save the generated SQL

proc gui_select_file_to_save_sql {} {

    set filetypes {{"SQL File" {.sql}}}

    set f [tk_getSaveFile -initialdir "." \
	       -filetypes $filetypes -title "Select a SQL file"]

    ;# Switch over the result
    switch $f {

	"" {
	    ;# Do nothing
	    return
	}
	default {
	    ;# Put the .sql suffix if it is not done
	    if {! [string match "*.sql" $f] } then {
		append f ".sql"
	    }

	    return $f

	}

    }
}


;#
;# gui_select_file_to_save
;#
;# Select a file to save the project

proc gui_select_file_to_save {} {

    set filetypes {{"Gerwin Project" {.ger}}}

    set f [tk_getSaveFile -initialdir "." \
	       -filetypes $filetypes -title "Select a project file"]

    ;# Switch over the result
    switch $f {

	"" {
	    ;# Do nothing
	    return
	}
	default {
	    ;# Put the .ger suffix if it is not done
	    if {! [string match "*.ger" $f] } then {
		append f ".ger"
	    }

	    return $f

	}

    }
}

;# 
;# gui_edit_project 
;#
;# Edit the actual Project parameters
;# Another edit widget can be put at the right of that.

proc gui_edit_project {} {

    global gerwin_cproject_name
    global gerwin_cproject_file
    global gerwin_editionarea
    global earea_pages

    lappend earea_pages Project

    ${gerwin_editionarea} insert end Project -text Project
    ${gerwin_editionarea} raise Project

    set widget [${gerwin_editionarea} getframe Project]

    TitleFrame ${widget}.editframe -side center -text "Project Properties"

    set editwidget [${widget}.editframe getframe]
    
    ;# Project name 
    frame ${editwidget}.nameline 
    LabelEntry ${editwidget}.nameline.label -label "Project Name:" -labelwidth 16 \
	-textvariable gerwin_cproject_name \
	-helptext "Name of the current project" -width 50

    ;# Project file
    frame ${editwidget}.fileline
    LabelEntry ${editwidget}.fileline.label -label "Project File:" -labelwidth 16 \
	-textvariable gerwin_cproject_file \
	-helptext "Storage file of the current project" -editable false \
	-width 50

    Button ${editwidget}.fileline.button -text "select file" \
	-helptext "Select a file to store the project in" \
	-command "set tempfile \[gui_select_file_to_save\] ; \

                  if {\$tempfile != {}} then {
                      set gerwin_cproject_file \$tempfile
                      gerwin_save_project_file
                  } " \
	-relief groove


    ;# Project author
    frame ${editwidget}.authorline
    LabelEntry ${editwidget}.authorline.label -label "Project Author:" -labelwidth 16 \
	-textvariable gerwin_cproject_author \
	-helptext "Author of the current project" -width 50
    
    pack ${editwidget}.nameline.label -side left -anchor w
    pack ${editwidget}.nameline -side top -anchor w

    pack ${editwidget}.fileline.label -side left -anchor w
    pack ${editwidget}.fileline.button -side left
    pack ${editwidget}.fileline -side top -anchor w

    pack ${editwidget}.authorline.label -side left -anchor w
    pack ${editwidget}.authorline -side top -anchor w

    pack ${editwidget} -side top -fill both -expand true

    pack ${widget}.editframe -side top ;#-fill both -expand true
    
    ;# Scale the notebook
    NoteBook::compute_size ${gerwin_editionarea}

}
    

;#
;# gui_get_free_oct OBJ TYPE CUAD
;#
;# Get a free oct on CUAD for OBJ-TYPE

proc gui_get_free_oct {obj type cuad} {

    global gerwin_entity

    switch $cuad {

	1 {
	    ;# oct for obj must be o2 or o1
	    if {[gui_oct_is_busy $obj $type 1]} then {
		
		return 2

	    } else {

		return 1
	    }

	}

	2 {

	    ;# oct for obj must be o3 or o4 
	    if {[gui_oct_is_busy $obj $type 3]} then {
		
		return 4

	    } else {

		return 3
	    }
	}

	3 {

	    ;# oct for object 1 must be o5 or o6
	    if {[gui_oct_is_busy $obj $type 5]} then {
		
		return 6

	    } else {

		return 5
	    }

	}


	4 {

	    ;# oct for object 1 must be o7 or o8
	    if {[gui_oct_is_busy $obj $type 8]} then {
		
		return 7

	    } else {
		
		return 8
	    }

	}

    }

}




;#
;# gui_oct_is_busy OBJ TYPE OCT
;#
;# See if OCT is busy for OBJ-TYPE

proc gui_oct_is_busy {obj type oct} {

    global gerwin_entity

    switch $type {


	"Entity" {

	    ;# Get the oct array from the entity
	    set octarray $gerwin_entity($obj,octs)

	    if {[lindex $octarray [expr $oct - 1]] != ""} then {

		;# oct busy
		return 1

	    } else {
		
		return 0

	    }
	}
	    

    }
}

;#
;# gui_get_link_point_from_oct {obj type oct} 
;#

proc gui_get_link_point_from_oct {obj type oct} {

    global gerwin_entity
    global gcanvas


    switch $type {


	"Entity" {

	    ;# Get the bbox of the entity
	    set bbox [${gcanvas} bbox taggroup-$type-$obj]
	    set x1 [lindex $bbox 0]
	    set y1 [lindex $bbox 1]
	    set x2 [lindex $bbox 2]
	    set y2 [lindex $bbox 3]
	    

	    ;# Calculate the link point based on the oct
	    switch $oct {

		1 {
		    return [list $x2 [expr ($y1 + (($y2 - $y1) / 4))]]
		}
		2 {
		    return [list [expr ($x1 + ((($x2 - $x1) / 4) * 3))] $y1]
		}
		3 {
		    return [list [expr ($x1 + (($x2 - $x1) / 4))] $y1]
		}
		4 {
		    return [list $x1 [expr ($y1 + (($y2 - $y1) / 4))]]
		}
		5 {
		    return [list $x1 [expr ($y1 + ((($y2 - $y1) /4) * 3))]]
		}
		6 {
		    return [list [expr ($x1 + (($x2 - $x1) / 4))] $y2]
		}
		7 {
		    return [list [expr ($x1 + ((($x2 - $x1) / 4) * 3))]]
		}
		8 {
		    return [list $x2 [expr ($y1 + ((($y2 - $y1) / 4) * 3))]]
		}

	    }

	}

    }

}


;#
;# gui_add_link_to_oct TYPE1 OBJ1 TYPE2 OBJ2 OCT
;#
;# Add a link to OBJ2 to OBJ1's OCT

proc gui_add_link_to_oct {type1 obj1 type2 obj2 oct} {

    global gerwin_entity
    global gerwin_links

    ;# Add the new link to gerwin_links
    lappend gerwin_links [list $type1 $obj1 $type2 $obj2]
    lappend gerwin_links [list $type2 $obj2 $type1 $obj1]

    switch $type1 {


	Entity {
	    
	    set temp [lindex $gerwin_entity($obj1,octs) [expr $oct - 1]]
	    lappend temp [list $type2 $obj2]
	    set gerwin_entity($obj1,octs) \
		[lreplace $gerwin_entity($obj1,octs) [expr $oct - 1] [expr $oct - 1] $temp]

	}


    }

}

;#
;# gui_redraw_oct TYPE OBJ OCT
;#
;# Redraw all links on the OBJ oct named OCT

proc gui_redraw_oct {type obj oct} {

    global gerwin_entity


    switch $type {

	Entity {
	    
	    set oct_content [lindex $gerwin_entity($obj,octs) [expr $oct - 1]]

	    ;# Get the internal pad
	    set nlinks [llength $oct_content]
	    set ipad 

	}
    }


}

;#
;# gui_link {TYPE1 OBJ1} {TYPE2 OBJ2}
;#
;# Makes and draw a link between OBJ1 and OBJ2


proc gui_link {elem1 elem2} {

    global gerwin_entity
    global gm_entity
    global gcanvas


    set type1 [lindex $elem1 0]
    set type2 [lindex $elem2 0]
    set object1 [lindex $elem1 1]
    set object2 [lindex $elem2 1]

    ;# Get bboxes
    set bbox1 [${gcanvas} bbox "taggroup-$type1-$object1"]
    set bbox2 [${gcanvas} bbox "taggroup-$type2-$object2"]

    ;# Obtain the cuads
    set refpoint1x [lindex $bbox1 2]
    set refpoint1y [lindex $bbox1 3]
    set refpoint2x [lindex $bbox2 2]
    set refpoint2y [lindex $bbox2 3]


    set cuad1 [gui_get_cuad $refpoint1x $refpoint1y \
		  $refpoint2x $refpoint2y]

    switch $cuad1 {

	1 { set cuad2 3 }
	2 { set cuad2 4 }
	3 { set cuad2 1 }
	4 { set cuad2 2 }
    }

    ;# Depending of the cuad, we must find an oct link point for both
    ;# objects.

    set oct1 [gui_get_free_oct $object1 $type1 $cuad1]
    ;# We get oct2 depending of oct1
    switch $oct1 {
	8 { set oct2 3}
	7 { set oct2 4}
	6 { set oct2 1}
	5 { set oct2 2}
	4 { set oct2 7}
	3 { set oct2 8}
	2 { set oct2 5}
	1 { set oct2 6}
    }



    ;# Add the new link to the links of both objects octs
    gui_add_link_to_oct $type1 $obj1 $type2 $obj2 $oct1
    gui_add_link_to_oct $type2 $obj2 $type1 $obj1 $oct2

    ;# Redraw the octs links for both objects
    ;#gui_redraw_oct $type1 $obj1 $oct1
    ;#gui_redraw_oct $type2 $obj2 $oct2

    ;# Get the link points
    set lpoint1 [gui_get_link_point_from_oct $object1 $type1 $oct1]
    set lp1x [lindex $lpoint1 0]
    set lp1y [lindex $lpoint1 1]
    set lpoint2 [gui_get_link_point_from_oct $object2 $type2 $oct2]
    set lp2x [lindex $lpoint2 0]
    set lp2y [lindex $lpoint2 1]

    ;# Get the link's intermediate point
    ;# It depend of the cuad

    switch $cuad1 {
	1 {
	    set intpx $lp2x
	    set intpy $lp1y
	}
	2 {
	    set intpx $lp1x
	    set intpy $lp2y
	}
	3 {
	    set intpx $lp2x
	    set intpy $lp1y
	}
	4 {
	    set intpx $lp2x
	    set intpy $lp1y
	}
    }

    ;# make the link
    ${gcanvas} create line $lp1x $lp1y \
	$intpx $intpy \
	$lp2x $lp2y \
	-tag $type1-$object1-$type2-$object2

}


;#
;# gui_link_delete type1 object1 type2 object2
;#
;# Deletes the link


proc gui_link_delete {type1 obj1 type2 obj2} {

    global gcanvas

    ${gcanvas} delete $type1-$obj1-$type2-$obj2



}
