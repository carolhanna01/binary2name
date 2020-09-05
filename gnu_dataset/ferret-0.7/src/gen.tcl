;# gen.tcl -- Gerwin
;#
;# Generation Routines
;#
;# Copyright (C) 2002-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:19:51 jemarch"

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
;# gen_move_relation_attributes TABLE RELATION NULL
;#
;# Pass all attributes from RELATION  to TABLE, with NULL

proc gen_move_relation_attributes {table relation null} {

    foreach ak [gob_relation_get_attributes $relation] {

	set ak [lindex $ak 0] ;# the attribute name

	;# See if there is already an attribute named $ak on the table
	if {[gob_table_attribute_exist $table $ak]} then {

	    ;# Rename the attribute with #
	    set nname "${relation}%${ak}"

	} else {
	    
	    set nname $ak
	}


	gob_table_attribute_add $table $nname \
	    [gob_relation_get_attribute_domain $relation $ak] $null
    }

}

;#
;# gen_move_key_attributes_as_foreign_keys TABLE ENTITY NULL
;#
;# Pass all attributes from the ENTITY key to TABLE, with NULL

proc gen_move_entity_key_attributes_as_foreign_keys {table entity null} {

    global gen_current_relation

    foreach ak [gob_entity_get_key $entity] {

	;# See if there is already an attribute named $ak on the table
	if {[gob_table_attribute_exist $table $ak]} then {
	    
	    ;# See if the table and the entity have the same name (reflexive relation)
	    if {$table == $entity} then {

		set nname "${entity}%${gen_current_relation}%${ak}"

	    } else {
		
		;# Rename the attribute with #
		set nname "${entity}%${ak}"
	    }


	} else {
	    
	    set nname $ak
	}
	

	gob_table_attribute_add $table $nname \
	    [gob_entity_get_attribute_domain $entity $ak] $null
	gob_table_add_attribute_to_foreign_keys $table $nname $entity
    }

}


;#
;# gen_ER_to_TD
;#
;# Generates the TD in the td page from the ER in the er page


proc gen_ER_to_TD {} {

    global gmainframe

    global gcanvas
    global gcanvas_td
    global gerwin_entities
    global gerwin_relations
    global gerwin_tables
    global gm_entity
    global gm_relation
    global gerwin_table
    global gerwin_relation
    global gerwin_entity
    global gen_current_relation
    global gm_managed
    global gm_flkey

    ;# Scale the TD canvas
    set re_sreg [${gcanvas} cget -scrollregion]
    ${gcanvas_td} configure -scrollregion $re_sreg


    ;# All entities must have a primary key!
    foreach e $gerwin_entities {

	if {[llength [gob_entity_get_key $e]] == 0} then {

	    gui_notice "$e have not a primary key! All entities must have a primary key to create tables"

	    return
	}
    }


    ;# Erase all possible previous tables
    foreach t $gerwin_tables {
	
	gob_delete_table $t

    }
    ;# Erase all possible previous flkeys
    if {[info exist gm_flkey]} then {
	unset gm_flkey
    }

    set gerwin_tables {}
    
    ;# Erase the td canvas
    ${gcanvas_td} delete all


    ;# Examine entities 
    foreach e $gerwin_entities {

	;# Create a new table
	gob_create_table $e {} {} {}
	
	;# The table have the same attributes than the entity
	foreach a [gob_entity_get_attributes $e] {
	    
	    gob_table_attribute_add $e [lindex $a 0] [lindex $a 1] 1

	}
	

	;# Also, both have the same key
	foreach k [gob_entity_get_key $e] {

	    gob_table_add_attribute_to_key $e $k
	    gob_table_set_attribute_null $e $k 0

	}


	set temp($e,xpos) $gm_entity($e,xpos)
	set temp($e,ypos) $gm_entity($e,ypos)
    }


    ;# Examine relations
    foreach r $gerwin_relations {

	set gen_current_relation $r

	set nentities [gob_relation_get_num_entities $r]
	if {$nentities == 2} then {
	    ;# Binary relation
	    
	    if {$gerwin_relation($r,reflexive)} then {
		;# Reflexive relation
		set entity1 [lindex [lindex $gerwin_relation($r,entities) 0] 0]
		set entity2 $entity1

		;# Get the cards
		set cardmax1 [lindex [lindex [lindex $gerwin_relation($r,entities) 0] 1] 1]
		set cardmax2 [lindex [lindex [lindex $gerwin_relation($r,entities) 1] 1] 1]
		set cardmin1 [lindex [lindex [lindex $gerwin_relation($r,entities) 0] 1] 0]
		set cardmin2 [lindex [lindex [lindex $gerwin_relation($r,entities) 1] 1] 0]
	    } else {
		;# Non-reflexive relations

		set entity1 [lindex [lindex $gerwin_relation($r,entities) 0] 0]
		set entity2 [lindex [lindex $gerwin_relation($r,entities) 1] 0]
		set cardmax1 [gob_relation_get_entity_max_card $r $entity1]
		set cardmin1 [gob_relation_get_entity_min_card $r $entity1]
		set cardmax2 [gob_relation_get_entity_max_card $r $entity2]
		set cardmin2 [gob_relation_get_entity_min_card $r $entity2]
	    
	    }

	    switch $cardmax1 {

		1 {
		    switch $cardmax2 {

			1 {
			    ;# /1 -> /1 RELATIONS
			    if {($cardmin1 == 1) && ($cardmin2 == 1)} then {

				;# Move the entity1 key to entity2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 0

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1
			    }
			    if {($cardmin1 == 0) && ($cardmin2 == 1)} then {

				;# Move the entity2 keys to entity1
				gen_move_entity_key_attributes_as_foreign_keys $entity1 $entity2 0

				;# Insert the relation attributes into entity1
				gen_move_relation_attributes $entity1 $r 1
			    }
			    if {($cardmin1 == 1) && ($cardmin2 == 0)} then {

				;# Move the entity1 keys to entity2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 0

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1

			    }
			    if {($cardmin1 == 0) && ($cardmin2 == 0)} then {

				;# Move the entity 1 keys to entity 2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 1

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1
			    }
			}
			N {
			    ;# /1 -> /N relations

			    if {($cardmin1 == 1) && ($cardmin2 == 1)} then {

				;# Move entity 1 keys to entity 2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 0

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1
			    }
			    if {($cardmin1 == 0) && ($cardmin2 == 1)} then {

				;# Move entity 1 keys to entity 2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 0

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1

			    }
			    if {($cardmin1 == 1) && ($cardmin2 == 0)} then {

				;# Move entity 1 keys to entity 2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 1

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1

			    }
			    if {($cardmin1 == 0) && ($cardmin2 == 0)} then {

				;# Move entity 1 keys to entity 2
				gen_move_entity_key_attributes_as_foreign_keys $entity2 $entity1 1

				;# Insert the relation attributes into entity2
				gen_move_relation_attributes $entity2 $r 1
			    }

			}
		    }

		}

		N {
		    
		    switch $cardmax2 {


			1 {
			    ;# /N -> /1 RELATIONS
			    if {($cardmin1 == 1) && ($cardmin2 == 1)} then {

				;# Move entity 2 keys to entity 1
				gen_move_entity_key_attributes_as_foreign_keys $entity1 $entity2 0

				;# Insert the relation attributes into entity1
				gen_move_relation_attributes $entity1 $r 1

			    }
			    if {($cardmin1 == 0) && ($cardmin2 == 1)} then {

				;# Move entity 2 keys to entity 1
				gen_move_entity_key_attributes_as_foreign_keys $entity1 $entity2 0

				;# Insert the relation attributes into entity1
				gen_move_relation_attributes $entity1 $r 1

			    }
			    if {($cardmin1 == 1) && ($cardmin2 == 0)} then {
				
				;# Move entity 2 keys to entity 1
				gen_move_entity_key_attributes_as_foreign_keys $entity1 $entity2 1

				;# Insert the relation attributes into entity1
				gen_move_relation_attributes $entity1 $r 1
				
			    }
			    if {($cardmin1 == 0) && ($cardmin2 == 0)} then {

				;# Move entity 2 keys to entity 1
				gen_move_entity_key_attributes_as_foreign_keys $entity1 $entity2 1

				;# Insert the relation attributes into entity1
				gen_move_relation_attributes $entity1 $r 1

			    }

			}
			
			N { ;# /N -> /N

			    ;# Create a new table
			    gob_create_table $r {} {} {}
			    
			    ;# Add the keys from the related entities
			    foreach ak [gob_entity_get_key $entity1] {

				;# See if a rename is necessary
				if {[gob_table_attribute_exist $r $ak]} then {

				    ;# Rename the attribute with #
				    set nname "${entity1}%${ak}"
				} else {
				    set nname $ak
				}
				

				gob_table_attribute_add $r $nname \
				    [gob_entity_get_attribute_domain $entity1 $ak] 0
				gob_table_add_attribute_to_foreign_keys $r \
				    $nname $entity1

				gob_table_add_attribute_to_key $r $nname

				
			    }
			    foreach ak [gob_entity_get_key $entity2] {
				
				;# See if a rename is necessary
				if {[gob_table_attribute_exist $r $ak]} then {

				    ;# Rename the attribute with #
				    set nname "${entity2}%${ak}"
				} else {
				    set nname $ak
				}


				gob_table_attribute_add $r $nname \
				    [gob_entity_get_attribute_domain $entity2 $ak] 0
				gob_table_add_attribute_to_foreign_keys $r \
				    $nname $entity2

				gob_table_add_attribute_to_key $r $nname

				
			    }
			    
			    ;# Insert the relation attributes into the new table
			    set toattr $r
			    foreach ak [gob_relation_get_attributes $r] {

				gob_table_attribute_add $toattr [lindex $ak 0] [lindex $ak 1] \
					1
			    }

			    ;# Set the geometry for the new table
			    set temp($r,xpos) $gm_relation($r,xpos)
			    set temp($r,ypos) $gm_relation($r,ypos)
			}

		    }

		}

	    }

	} else {

	    ;# Relations with >2 links
	    ;# All cards are N

	    ;# Create a new table
	    gob_create_table $r {} {} {}
	    ;# Set the geometry for the new table
	    set temp($r,xpos) $gm_relation($r,xpos)
	    set temp($r,ypos) $gm_relation($r,ypos)

	    foreach e [gob_relation_get_entities $r] {

		set e [lindex $e 0]

		;# Add the keys to the foreign keys to the new table,
		;# and to the primary key.
		foreach ak [gob_entity_get_key $e] {

		    ;# See if a rename is necessary
		    if {[gob_table_attribute_exist $r $ak]} then {

			;# Rename the attribute with #
			set nname "${e}%${ak}"
		    } else {
			set nname $ak
		    }


		    gob_table_attribute_add $r $nname \
			[gob_entity_get_attribute_domain $e $ak] 0

		    gob_table_add_attribute_to_foreign_keys $r \
			$nname $e

		    gob_table_add_attribute_to_key $r $nname

		}

	    }

	}

    }

    ;# Draw all the tables
    foreach t $gerwin_tables {

	gm_draw_table $t $temp($t,xpos) $temp($t,ypos)

    }

    ;# Finally, update all the links
    foreach t $gerwin_tables {

	set gm_managed {}
	gm_table_update_links $t 
    }

}


;#
;# gen_TD_to_Output FORMAT
;#
;# Generates the Output corresponding to the actual TD contents 
;# (that is, the actual table space)
;#
;# FORMAT  is the Output format.

proc gen_TD_to_Output {format} {

    global output_pages
    global output_pages_widget
    global gtext_output
    global gerwin_output_formats

    ;# Do nothing if FORMAT is empty or is not a valid output format
    if {$format == ""} then {
	return
    }
    if {[lsearch $gerwin_output_formats $format] == -1} then {
	return
    }


    ;# Create or switch to the good output page

    ;# See if FORMAT have alreay a page in the output area
    set index [lsearch $output_pages $format]
    if {$index != -1} then {

	;# Make the page visible
	${output_pages_widget} raise [lindex $output_pages $index]

    } else {
	
	;# Create a new page for the format
	gui_create_output_page $format

    }

    ;# Make the text widget editable
    $gtext_output($format) configure -state normal

    ;# Call to the appropiate generation routine
    omit_clear
    gen_TD_to_format_${format}

    ;# Make the text widget no editable
    $gtext_output($format) configure -state disabled

    ;# Finally, colorize gtext_output($format)
    gen_colorize $format
}


;#
;# gen_colorize FORMAT
;#
;# Colorizes the generated text for FORMAT

proc gen_colorize {format} {

    global gtext_output
    global output_colors

    set gtext $gtext_output($format)
    set colors $output_colors($format)

    ;# Rewind the text widget
    ${gtext} mark set insert 1.0

    ;# Foreach colors 
    foreach color $colors {

	set er [lindex $color 0]
	set col [lindex $color 1]

	;# Find all occurrences of ER on the text widget, and assign it
	;# the tag "col"
	${gtext} mark set traveller 1.0
	while {1} {

	    set sresult [${gtext} search -regexp -nocase -count foundnum -- $er [${gtext} index traveller] end]
	    if {$sresult == ""} then {
		;# No more matches
		break
	    }

	    ${gtext} mark set traveller $sresult 
	    ${gtext} tag add $col [${gtext} index traveller]  "[${gtext} index traveller] + $foundnum chars"
	    ${gtext} mark set traveller "[${gtext} index traveller] + [expr $foundnum + 1] chars"
	}

	;# Colorize the tag
	${gtext} tag configure $col -foreground $col
    }


}


;#
;# omit_line LINE
;#
;# Outputs LINE to the right output text widget

proc omit_line {line} {

    global gerwin_output_active_format
    global gtext_output

    $gtext_output($gerwin_output_active_format) insert end "${line}\n"

}


;#
;# omit_clear
;#
;# Clear the right output text widget

proc omit_clear {} {
    
    global gerwin_output_active_format
    global gtext_output

    $gtext_output($gerwin_output_active_format) delete 1.0 end
}

