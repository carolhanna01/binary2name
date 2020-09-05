;# gerwin.tcl -- Gerwin
;#
;# Main file.
;#
;# Copyright (C) 2002-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:20:26 jemarch"

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
;# gerwin_init
;#
;# Initializes gerwin


proc gerwin_init {} {

    ;# Initialize the gui
    gui_init .gerwin

    ;# Initialize the bitmap library
    ;#Bitmap::_init


    ;# Set the initial state: Moving
    ;#gerwin_set_state Editing

}


;#
;# gerwin_set_state STATE
;#
;# Set STATE as the new STATE for gerwin

proc gerwin_set_state {state} {

    global gmainframe
    global gerwin_state
    global gcanvas

    set userframe [${gmainframe} getframe]

    set gerwin_state $state
    gui_change_state $state


    ;# Change the bindings if necessary
    switch $state {


	"Inserting entity" -
	"Inserting label" {

	    ;# Destroy the binding on all the canvas objects
	    ${gcanvas} bind all <Button-1> {}
	    ;# Bind on the entire widget
	    bind ${gcanvas} <Button-1> {gui_mark %x %y}
	}
	
	default {
	    ;# Destroy the binding on the canvas
	    bind ${gcanvas} <Button-1> {}

	    ;# Bind on all the canvas elements
	    gui_launch_bindings
	    
	}
    }
}


;#
;# gerwin_quit
;#
;# Quits Gerwin

proc gerwin_quit {} {

    exit

}


;#
;# gerwin_print_TD
;#
;# Print the gcanvas_td into a postscript file

proc gerwin_print_TD {} {

    global gcanvas_td

    ;# Get the file where put the postscrip

    set filetypes {{"PostScript" {.ps}}}

    set f [tk_getSaveFile -initialdir "." \
	       -filetypes $filetypes -title "Print to a PostScript file"]

    ;# Switch over the result
    switch $f {

	"" {
	    ;# Do nothing
	    return
	}
	default {
	    ;# Put the .ps suffix if it is not done
	    if {! [string match "*.ps" $f] } then {
		append f ".ps"
	    }
	    ;# Ok, save the postscript into f
	    ;# Get the bounding box
	    set bbox [${gcanvas_td} bbox all]
	    set x [lindex $bbox 0]
	    set y [lindex $bbox 1]
	    set height [lindex $bbox 3]
	    set width [lindex $bbox 2]
	    
	    ${gcanvas_td} postscript -file $f -colormode color \
		-pageanchor c -width $width -height $height \
		-x $x -y $y



	}
    }

}


;#
;# gerwin_print_ER
;#
;# Print the gcanvas into a postscript file

proc gerwin_print_ER {} {

    global gcanvas

    ;# Get the file where put the postscrip

    set filetypes {{"PostScript" {.ps}}}

    set f [tk_getSaveFile -initialdir "." \
	       -filetypes $filetypes -title "Print to a PostScript file"]

    ;# Switch over the result
    switch $f {

	"" {
	    ;# Do nothing
	    return
	}
	default {
	    ;# Put the .ps suffix if it is not done
	    if {! [string match "*.ps" $f] } then {
		append f ".ps"
	    }

	    ;# Ok, save the postscript into f
	    ;# Get the bounding box
	    set bbox [${gcanvas} bbox all]
	    set x [lindex $bbox 0]
	    set y [lindex $bbox 1]
	    set height [lindex $bbox 3]
	    set width [lindex $bbox 2]
	    
	    ${gcanvas} postscript -file $f -colormode color \
		-pageanchor c -width $width -height $height \
		-x $x -y $y
	    

	}
    }

}


;#
;# gerwin_save_sql
;# 
;# Saves the current SQL on a file

proc gerwin_save_sql {} {

    global gmainframe
    global gerwin_cproject_sqlfile
    global gtext_sql

    if {($gerwin_cproject_sqlfile == "none")} then {

	MessageDlg ${gmainframe}.notice -title "Saving project" \
	    -message "You must assign a SQL file to the project before saving it!" \
	    -type ok

	return

    }
    
    set res [MessageDlg ${gmainframe}.notice -title "Saving SQL" \
		 -message "Are you sure you want to save the SQL listing into ${gerwin_cproject_sqlfile}?" \
		 -type yesno]

    switch $res {

	0 {
	    ;# Yes, save it
	    set fout [open ${gerwin_cproject_sqlfile} w]
	    puts $fout [${gtext_sql} get 1.0 end]
	    close $fout
	} 
	1 {
	    ;# No
	    return
	}
	    
    }

}



;#
;# gerwin_save_project
;#
;# Project saving dialog

proc gerwin_save_project {} {

    global gmainframe
    global gerwin_cproject_name
    global gerwin_cproject_file
    
    if {($gerwin_cproject_file == "none") || ($gerwin_cproject_file == "")} then {

	MessageDlg ${gmainframe}.notice -title "Saving project" \
	    -message "You must assign a file to the project before saving it!" \
	    -type ok

	return

    }

    set res [MessageDlg ${gmainframe}.notice -title "Saving project" \
		 -message "Are you sure you want to save $gerwin_cproject_name into ${gerwin_cproject_file}?" \
		 -type yesno]

    switch $res {

	0 {
	    ;# Yes, save it
	    gerwin_save_project_file
	} 
	1 {
	    ;# No
	    return
	}
	    
    }

}


;# 
;# gerwin_get_default_project_name
;#

proc gerwin_get_default_project_name {} {

    global gerwin_project_counter 

    return "Project$gerwin_project_counter"

    incr gerwin_project_counter
}

;#
;# gerwin_close_project
;#
;# Closes the current project

proc gerwin_close_project {} {
    
    global gerwin_cproject_name
    global gmainframe

    global gcanvas
    global gcanvas_td

    ;# Do nothing if there is not any project open
    if {${gerwin_cproject_name} == "none"} then {
	return 
    }

    ;# Make sure we want to close the project
    set res [MessageDlg ${gmainframe}.notice -title "Closing the project" \
		 -message "Are you sure you want to close ${gerwin_cproject_name}?" \
		 -type yesno]

    switch $res {

	0 {
	    ;# Yes, close it
	    gerwin_purge_project

	    ;# Disable some buttons
	    gui_no_project_opened

	    ;# Resize the canvas
# 	    ${gcanvas} configure -scrollregion {}
# 	    ${gcanvas_td} configure -scrollregion {}

	}
	1 {
	    ;# No, coward
	    return
	}

    }

}


;#
;# gerwin_open_project
;#
;# Select a project file and then make that project the current one

proc gerwin_open_project {} {

    global gmainframe
    global gerwin_cproject_name

    ;# Warning if there is another project opened
    if {$gerwin_cproject_name != "none"} then {

	set res [MessageDlg ${gmainframe}.notice -title "Open a new project?" \
		     -message "There is another project opened. You must close it and then open a new one." \
		     -type ok]
	return
    }
		     

    set filetypes {{"Gerwin Project" {.ger}}}
    set f [tk_getOpenFile -initialdir "." \
	       -filetypes $filetypes -title "Open a gerwin file"]

    ;# switch over the result
    switch $f {

	"" {
	    ;# Do nothing
	    return
	}
	default {


	    ;# Now we can edit
	    gui_project_opened

	    ;# Load the project from $f
	    gerwin_load_project_file $f

	}

    }


}


;#
;# gerwin_purge_project
;#
;# Purges all the gerwin state

proc gerwin_purge_project {} {

    global gerwin_entities
    global gerwin_relations
    global gerwin_tables
    global gerwin_cproject_name 
    global gerwin_cproject_file
    global gerwin_cproject_sqlfile
    global gerwin_cproject_author
    global gerwin_entity_seq
    global gerwin_output_active_format

    global output_pages


    set gerwin_cproject_name none
    set gerwin_cproject_file none
    set gerwin_cproject_author none

    set gerwin_entity_seq 1


    ;# Purge all relations
    foreach r $gerwin_relations {

	gob_delete_relation $r
	gm_delete_relation $r
	gui_edition_area_delete_page $r


    }

    set gerwin_relations {}

    ;# Purge all entities
    foreach e $gerwin_entities {

	gob_delete_entity $e
	gm_delete_entity $e
	gui_edition_area_delete_page $e

    }

    set gerwin_entities {}

    ;# Purge all tables
    foreach t $gerwin_tables {

	gob_delete_table $t
	gm_delete_table $t
    }

    set gerwin_tables {}


    ;# Purge project variables
    set gerwin_cproject_file none
    set gerwin_cproject_sqlfile none

    ;# Purge the output pages
    foreach op $output_pages {

	gui_destroy_output_page $op

    }

    ;# Destroy the app area Domain page
    ;#gui_app_area_destroy_Domain_page
    ;# Destroy the app area Project page
    gui_app_area_destroy_Project_page

    ;# Destroy the rest of the app areas
    gui_app_area_destroy_Output_page
    gui_app_area_destroy_TD_page
    gui_app_area_destroy_ER_page

    ;# Delete the active format
    set gerwin_output_active_format {}
}


;#
;# gerwin_save_output_file FORMAT
;#
;# Save the FORMAT output to a file

proc gerwin_save_output_file {format} {

    global gmainframe
    global gtext_output

    set filename [tk_getSaveFile -initialdir "." \
		      -title "Saving an output file"]

    ;# Switch over the result
    switch $filename {

	"" {
	    ;# Do nothing
	    return

	} 

	default {

	    ;# Make sure we want to save
	    set res [MessageDlg ${gmainframe}.notice -title "Saving SQL" \
			 -message "Are you sure you want to save this $format output into ${filename}?" \
			 -type yesno]

	    switch $res {

		0 {
		    ;# Yes, save it
		    set fout [open ${filename} w]
		    puts $fout [$gtext_output($format) get 1.0 end]
		    close $fout

		}
		1 {
		    ;# No
		    return 

		}
	    }

	}
    }
}


;#
;# gerwin_new_project
;#
;# Creates a new project, named after the project count

proc gerwin_new_project {} {

    global gerwin_cproject_name
    global gerwin_cproject_file
    global gerwin_cproject_author

    global gcanvas

    ;# Make sure the user really want to get a new project
    if {$gerwin_cproject_name != "none"} then {

	set answer [gui_yes_no_cancel "Another project is already opened.\nAre you sure \
you want to discard the old project and start another one?"]


	switch $answer {

	    0 {
		;# Purge the old project
		gerwin_purge_project
		
	    }

	    1 -
	    2 {

		return 
	    }

	}

    }


    ;# Set the default values for project attributes
    set gerwin_cproject_name [gerwin_get_default_project_name]
    set gerwin_cproject_file none
    set gerwin_cproject_author "Anonymous GNUdist"



    ;# Activate the buttons
    gui_project_opened

    ;# Scale the canvas
    ${gcanvas} configure -scrollregion {0 0 1024 768}

}


;#
;# gerwin_load_project_file FILENAME
;# 
;# Load a project from FILENAME

proc gerwin_load_project_file {filename} {

    global gerwin_cproject_name
    global gerwin_cproject_file
    global gerwin_cproject_author

    global gerwin_entities
    global gerwin_relations
    global gerwin_entity
    global gerwin_relation

    global gm_entity
    global gm_relation

    global gerwin_relation_seq 
    global gerwin_entity_seq

    global gcanvas


    ;# Open the input channel
    ;# We suppose the file exist
    set fin [open $filename r]

    ;##
    ;## Load in project information
    ;##

    gets $fin gerwin_cproject_name
    gets $fin gerwin_cproject_file
    gets $fin gerwin_cproject_author
    gets $fin gerwin_entity_seq
    gets $fin gerwin_relation_seq

    ;##
    ;## Load canvas information
    ;##
    gets $fin sreg
    ${gcanvas} configure -scrollregion $sreg


    ;##
    ;## Load entity information
    ;##

    gets $fin numentities

    for {set x 0} {$x < $numentities} {incr x} {

	gets $fin ename ;# Entity name


	gets $fin xpos ;# Get geometry information
	gets $fin ypos

	gob_create_entity $ename {} {} {} ;# Create the entity

	;# Load in the attributes
	gets $fin numattributes 

	for {set y 0} {$y < $numattributes} {incr y} {

	    gets $fin aline ;# Read the attribute line

	    ;# Add the new attribute to the entity
	    gob_entity_add_attribute $ename [lindex $aline 0] [lindex $aline 1]

	}

	;# Load in the keys
	gets $fin numkeys
	for {set y 0} {$y < $numkeys} {incr y} {

	    gets $fin key ;# Read the key

	    ;# Add the key to the entity
	    gob_entity_add_attribute_to_key $ename $key
	}


	;# Load in the relations
	gets $fin numrelations
	for {set y 0} {$y < $numrelations} {incr y} {

	    gets $fin relation ;# Read the relation

	    ;# Add the relation to the entity
	    gob_entity_add_relation $ename $relation

	}

	;# Ok, draw the entity
	gm_draw_entity $ename $xpos $ypos
    }

    ;##
    ;## Load in the relations
    ;##

    gets $fin numrelations

    for {set x 0} {$x < $numrelations} {incr x} {

	gets $fin rname ;# Relation name
	gets $fin temp_reflexive ;# Reflexive relation?

	gets $fin xpos ;# Geometry information
	gets $fin ypos 

	;# Create the relation
	gob_create_relation $rname {} {}
	set gerwin_relation($rname,reflexive) $temp_reflexive

	;# Load in the attributes
	gets $fin numattributes ;# Number of attributes

	for {set y 0} {$y < $numattributes} {incr y} {

	    gets $fin aline ;# Read the attribute line

	    gob_relation_add_attribute $rname [lindex $aline 0] [lindex $aline 1]
	}

	;# Load in the entities
	gets $fin numentities ;# Number of entities

	for {set y 0} {$y < $numentities} {incr y} {

	    gets $fin ename ;# Entity name
	    gets $fin emincard ;# Entity minimal cardinality
	    gets $fin emaxcard ;# Entity maximal cardinality

	    gob_relation_add_entity $rname $ename $emincard $emaxcard

	}

	;# Ok, draw the relation
	gm_draw_relation $rname $xpos $ypos

	if {$gerwin_relation($rname,reflexive)} then {
	    gm_relation_update_links_reflexive $rname
	} else {
	    gm_relation_update_links $rname
	}

    }

    ;# Set some internal state

    ;#set gerwin_entity_seq [expr $numentities]
    ;#set gerwin_relation_seq [expr $numrelations]

}


;#
;# gerwin_save_project_file
;#
;# Save the current project

proc gerwin_save_project_file {} {

    global gerwin_cproject_name
    global gerwin_cproject_file
    global gerwin_cproject_author

    global gerwin_entities
    global gerwin_relations
    global gerwin_entity
    global gerwin_relation

    global gm_entity
    global gm_relation

    global gerwin_entity_seq
    global gerwin_relation_seq
    
    global gcanvas

    ;# Open the out channel
    ;# We suposse the file exist
    set filename $gerwin_cproject_file
    set fout [open $filename w]


    ;##
    ;## Write out project information
    ;##

    puts $fout $gerwin_cproject_name
    puts $fout $gerwin_cproject_file
    puts $fout $gerwin_cproject_author
    puts $fout $gerwin_entity_seq
    puts $fout $gerwin_relation_seq

    ;##
    ;## Write out canvas size information
    ;##
    set sreg [${gcanvas} cget -scrollregion]
    puts $fout $sreg


    ;## 
    ;## Write out entity information
    ;##

    set nentities [llength $gerwin_entities]

    puts $fout $nentities   ;# Number of entities

    foreach e $gerwin_entities {

	puts $fout $e  ;# Entity name

	;# Geometry attributes
	puts $fout $gm_entity($e,xpos)
	puts $fout $gm_entity($e,ypos)

	set nattributes [llength $gerwin_entity($e,attributes)]
	puts $fout $nattributes ;# Number of attributes

	foreach a $gerwin_entity($e,attributes) {

	    puts $fout $a   ;# Attributes

	}

	set nkeys [llength $gerwin_entity($e,key)]
	puts $fout $nkeys ;# Number of key attributes

	foreach k $gerwin_entity($e,key) {

	    puts $fout $k  ;# Keys

	}

	set nrelations [llength $gerwin_entity($e,relations)]
	puts $fout $nrelations ;# Number of relations

	foreach r $gerwin_entity($e,relations) {

	    puts $fout $r  ;# Relations

	}

    }

    ;# Output relations
    
    set nrelations [llength $gerwin_relations]
    puts $fout $nrelations ;# Number of relations

    foreach r $gerwin_relations {

	puts $fout $r  ;# Relation name
	puts $fout $gerwin_relation($r,reflexive) ;# It is a reflexive relation?

	;# Output geometry information
	puts $fout $gm_relation($r,xpos)
	puts $fout $gm_relation($r,ypos)

	set nattributes [llength [gob_relation_get_attributes $r]]
	puts $fout $nattributes ;# Number of attributes

	foreach a [gob_relation_get_attributes $r] {

	    puts $fout $a

	}

	set nentities [llength [gob_relation_get_entities $r]]
	puts $fout $nentities ;# Number of entities

	foreach e [gob_relation_get_entities $r] {

	    set e [lindex $e 0]
	    puts $fout $e ;# Entity name
	    puts $fout [gob_relation_get_entity_min_card $r $e] ;# Entity min card
	    puts $fout [gob_relation_get_entity_max_card $r $e] ;# Entity max card

	}


    }
    
    close $fout
}



;##### Load external packages

lappend auto-path [file join . widgets]

puts "Loading external megawidget libraries:"
puts ""

puts "     BWidget 1.6"
lappend auto_path [file join . widgets bwidget]
package require BWidget 1.6

puts "     Icons 1.0"
lappend auto_path [file join . widgets icons]
package require icons 1.0    


puts "     Tablelist 2.7"
lappend auto_path [file join . widgets tablelist]
package require Tablelist 2.7

puts ""
;###### Done loading libraries


source src/globals.tcl
source src/gob.tcl
source src/gui.tcl
source src/gm.tcl
source src/gen.tcl
source src/postgresql.tcl
source src/mysql.tcl
source src/sql92.tcl
source src/gerwinml.tcl


;# Ok, start!
gerwin_init

