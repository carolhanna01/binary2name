;# gm.tcl -- Gerwin
;#
;# Geometry Manager
;#
;# Copyright (C) 2003-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:21:12 jemarch"

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

;##################################
;####### ENTITY ROUTINES ###########
;##################################


;#
;# gm_draw_entity ENTITY XPOS YPOS
;#
;# Draw ENTITY at (XPOS,YPOS) on the canvas

proc gm_draw_entity {entity xpos ypos} {


    global gmainframe
    global gerwin_entity
    global gm_entity
    global gui_concise_view
    global gcanvas
    global gfonts

    ;# Save the original position of the entity
    set origx $xpos
    set origy $ypos


    set userframe [${gmainframe} getframe]
    
    ;# Create all the slaves objects
    set slaves {}

    ;# The box with the name
    lappend slaves [${gcanvas} create text \
			[expr $xpos + 17] [expr $ypos + 2] \
			-anchor nw -text "$entity" -fill blue \
			-tag taggroup-Entity-$entity \
			-font $gfonts(ercanvas_titles)]



    lappend slaves [${gcanvas} create rect \
			$xpos $ypos \
			[expr $xpos + 15] [expr $ypos + 20] \
			-tag taggroup-Entity-$entity -fill blue -outline blue]

    lappend slaves [${gcanvas} create text \
			[expr $xpos + 4] [expr $ypos + 4] \
			-anchor nw -text "E" -fill white \
			-tag taggroup-Entity-$entity \
		       -font $gfonts(ercanvas_minilabels)]


    set line_title [${gcanvas} create line \
			    $xpos [expr $ypos + 20] [expr $xpos + 150] [expr $ypos + 20] \
			-tag taggroup-Entity-$entity -fill blue]

    lappend slaves $line_title
    lappend to_expand $line_title

 

    ;# Draw all the key attributes for the entity
    set were_keys 0
    set i 2
    set ypos [expr $ypos + 5]
    foreach attribute $gerwin_entity($entity,attributes) {




	if {[gob_entity_is_key $entity [lindex $attribute 0]]} then {

	    set were_keys 1

	    ;# Get the coords for the new rect
	    set bxi [expr $xpos]
	    set byi [expr $ypos + (($i * 15) - 15)]
	    set bxf [expr $xpos + 150]
	    set byf [expr $ypos + ($i * 15)]


	    ;# Get the coords for the new text
	    set tx [expr $bxi + 2]
	    set ty [expr $byi + 2]

	    ;# Determine the text contents
	    set ttext "${attribute} [gob_entity_get_attribute_domain $entity $attribute]"

	    lappend slaves [${gcanvas} create text $tx $ty -anchor nw \
				-text $ttext -tag taggroup-Entity-$entity \
			       -font $gfonts(ercanvas_default)]

			
	    incr i

	}

    }

    if {$were_keys} then {
	;# Make the separator line
	set line_key [${gcanvas} create line $xpos $byf $bxf $byf \
			  -tag taggroup-Entity-$entity]

	lappend slaves $line_key
	lappend to_expand $line_key
    }

    ;# Draw all the non-key attributes for the entity
    foreach attribute $gerwin_entity($entity,attributes) {

	if {! [gob_entity_is_key $entity [lindex $attribute 0]] } then {

	    ;# Get the coords for the new rect
	    set bxi [expr $xpos]
	    set byi [expr $ypos + (($i * 15) - 15)]
	    set bxf [expr $xpos + 150]
	    set byf [expr $ypos + ($i * 15)]

	
	    ;# Get the coords for the new text
	    set tx [expr $bxi + 2]
	    set ty [expr $byi + 2]

	    set ttext "${attribute} [gob_entity_get_attribute_domain $entity $attribute]"


	    lappend slaves [${gcanvas} create text $tx $ty -anchor nw \
				-text $ttext -tag taggroup-Entity-$entity \
			       -font $gfonts(ercanvas_default)]

	    incr i

	}

    }


    ;# Create the new wgob
    set gm_entity($entity,grouptag) taggroup-Entity-$entity

    ;# Get the bbox
    set ebbox [${gcanvas} bbox taggroup-Entity-$entity]
    set gm_entity($entity,xpos) [lindex $ebbox 0]
    set gm_entity($entity,ypos) [lindex $ebbox 1]
    set gm_entity($entity,xlong) \
	[expr [lindex $ebbox 2] - $gm_entity($entity,xpos)]
    set gm_entity($entity,ylong) \
	[expr [lindex $ebbox 3] - $gm_entity($entity,ypos)]

    ;# Create the surrounding rect
    lappend slaves [${gcanvas} create rect \
			$gm_entity($entity,xpos) $gm_entity($entity,ypos) \
			[lindex $ebbox 2] [lindex $ebbox 3] \
			-tag $gm_entity($entity,grouptag) -outline blue]

    set ebbox [${gcanvas} bbox $gm_entity($entity,grouptag)]

    set gm_entity($entity,xpos) [lindex $ebbox 0]
    set gm_entity($entity,ypos) [lindex $ebbox 1]
    set gm_entity($entity,xlong) \
	[expr [lindex $ebbox 2] - $gm_entity($entity,xpos)]
    set gm_entity($entity,ylong) \
	[expr [lindex $ebbox 3] - $gm_entity($entity,ypos)]



    ;# Resize the to_expand lines
    foreach l $to_expand {

	set lcoords [${gcanvas} coords $l]
	set lcoords [lreplace $lcoords 2 2 [lindex $ebbox 2]]
	;#set lcoords [lreplace $lcoords 0 0 [lindex $ebbox 0]]
	${gcanvas} coords $l $lcoords

    }

    ;# Resave the positions
    set ebbox [${gcanvas} bbox taggroup-Entity-$entity]
    set gm_entity($entity,xpos) $origx
    set gm_entity($entity,ypos) $origy


    ;# Put the slaves into the wgob
    set gm_entity($entity,slaves) $slaves

    ;# Bind for moving
    ${gcanvas} bind $gm_entity($entity,grouptag) <B1-Motion> {gui_drag %x %y}
}



;#
;# gm_delete_entity ENTITY
;#
;# Deletes ENTITY from the canvas

proc gm_delete_entity {entity} {

    global gmainframe
    global gm_entity
    global gm_entities
    global gcanvas

    set userframe [${gmainframe} getframe]

    ;# Unbind the movement
    ${gcanvas} bind $gm_entity($entity,grouptag) <B1-Motion> {}

    ;# Delete all the slaves
    ${gcanvas} delete $gm_entity($entity,grouptag)

    ;# Free the gm_entity array
    unset gm_entity($entity,xpos)
    unset gm_entity($entity,ypos)
    unset gm_entity($entity,xlong)
    unset gm_entity($entity,ylong)
    unset gm_entity($entity,grouptag)
    unset gm_entity($entity,slaves)

    set index [lsearch $gm_entities $entity]
    set gm_entities [lreplace $gm_entities $index $index]

}


;#
;# gm_entity_change_name ENTITY NEWNAME
;#
;# Set NEWNAME as the new name for ENTITY

proc gm_entity_change_name {entity nname} {

    global gm_entities
    global gm_entity

    ;# Change the name in entities
    set index [lsearch $gm_entities $entity]
    set gm_entities [lreplace $gm_entities $index $index $nname]

    ;# Change the name in entity
    set temp_slaves $gm_entity($entity,slaves)
    set temp_grouptag $gm_entity($entity,grouptag)
    set temp_xpos $gm_entity($entity,xpos)
    set temp_ypos $gm_entity($entity,ypos)
    set temp_xlong $gm_entity($entity,xlong)
    set temp_ylong $gm_entity($entity,ylong)

    unset gm_entity($entity,slaves)
    unset gm_entity($entity,grouptag)
    unset gm_entity($entity,xpos)
    unset gm_entity($entity,ypos)
    unset gm_entity($entity,xlong)
    unset gm_entity($entity,ylong)
    unset gm_entity($entity,nname)

    set gm_entity($nname,slaves) $temp_slaves
    set gm_entity($nname,grouptag) $temp_grouptag
    set gm_entity($nname,xpos) $temp_xpos
    set gm_entity($nname,ypos) $temp_ypos
    set gm_entity($nname,xlong) $temp_xlong
    set gm_entity($nname,ylong) $temp_ylong
    set gm_entity($nname,nname) {}

}




;##################################################
;############## RELATION ROUTINES #################
;##################################################


;#
;# gm_draw_relation RELATION XPOS YPOS
;#
;# Draw RELATION at (XPOS,YPOS) on the canvas

proc gm_draw_relation {relation xpos ypos} {

    global gmainframe
    global gerwin_relation
    global gm_relation
    global gcanvas
    global gfonts

    #fix moving box :)
    set origx $xpos
    set origy $ypos

    set userframe [${gmainframe} getframe]

    ;# Create all the slaves objects
    set slaves {}

    ;# The box with the name
    lappend slaves [${gcanvas} create text \
			[expr $xpos + 17] [expr $ypos + 2] \
			-anchor nw -text "$relation" -fill orange \
			-tag taggroup-Relation-$relation \
		       -font $gfonts(ercanvas_titles)]

    lappend slaves [${gcanvas} create rect \
			$xpos $ypos \
			[expr $xpos + 15] [expr $ypos + 20] -fill orange -outline orange \
			-tag taggroup-Relation-$relation]

    lappend slaves [${gcanvas} create text \
			[expr $xpos + 4] [expr $ypos + 4] \
			-anchor nw -text "R" -fill white \
			-tag taggroup-Relation-$relation \
		       -font $gfonts(ercanvas_minilabels)]

    set line_title [${gcanvas} create line \
			    $xpos [expr $ypos + 20] [expr $xpos + 150] [expr $ypos + 20] \
			-tag taggroup-Relation-$relation -fill orange]

    lappend slaves $line_title
    lappend to_expand $line_title
    

    ;# Draw all the existing attributes for the relation
    set i 2
    set ypos [expr $ypos + 5]
    foreach attribute $gerwin_relation($relation,attributes) {

	;# Get the coords for the new rect
	set bxi [expr $xpos]
	set byi [expr $ypos + (($i * 15) - 15)]
	set bxf [expr $xpos + 150]
	set byf [expr $ypos + ($i * 15)]

	
	;# Get the coords for the new text
	set tx [expr $bxi + 2]
	set ty [expr $byi + 2]

	;# Determine the text contents
	set ttext "${attribute} [gob_relation_get_attribute_domain $relation $attribute]"

	lappend slaves [${gcanvas} create text $tx $ty -anchor nw \
			    -text $ttext -tag taggroup-Relation-$relation \
			   -font $gfonts(ercanvas_default)]
	incr i

    }


    ;# Create the new wgob
    set gm_relation($relation,grouptag) taggroup-Relation-$relation

    ;# Get the bbox
    set ebbox [${gcanvas} bbox taggroup-Relation-$relation]
    set gm_relation($relation,xpos) [lindex $ebbox 0]
    set gm_relation($relation,ypos) [lindex $ebbox 1]
    set gm_relation($relation,xlong) \
	[expr [lindex $ebbox 2] - $gm_relation($relation,xpos)]
    set gm_relation($relation,ylong) \
	[expr [lindex $ebbox 3] - $gm_relation($relation,ypos)]

    ;# Create the surrounding rect
    lappend slaves [${gcanvas} create rect \
			$gm_relation($relation,xpos) $gm_relation($relation,ypos) \
			[lindex $ebbox 2] [lindex $ebbox 3] \
			-tag $gm_relation($relation,grouptag) -outline orange]

    set ebbox [${gcanvas} bbox $gm_relation($relation,grouptag)]
    set gm_relation($relation,xpos) [lindex $ebbox 0]
    set gm_relation($relation,ypos) [lindex $ebbox 1]
    set gm_relation($relation,xlong) \
	[expr [lindex $ebbox 2] - $gm_relation($relation,xpos)]
    set gm_relation($relation,ylong) \
	[expr [lindex $ebbox 3] - $gm_relation($relation,ypos)]


    ;# Resize to_expand lines
    foreach l $to_expand {

	set lcoords [${gcanvas} coords $l]
	set lcoords [lreplace $lcoords 2 2 [lindex $ebbox 2]]
	;#set lcoords [lreplace $lcoords 0 0 [lindex $ebbox 0]]
	${gcanvas} coords $l $lcoords

    }

    ;# Resave the positions
    set ebbox [${gcanvas} bbox taggroup-Relation-$relation]
    set gm_relation($relation,xpos) $origx
    set gm_relation($relation,ypos) $origy

    ;# Put the slaves into the wgob
    set gm_relation($relation,slaves) $slaves

    ;# Bind for moving
    ${gcanvas} bind $gm_relation($relation,grouptag) <B1-Motion> {gui_drag %x %y}
}


;#
;# gm_delete_relation RELATION
;#
;# Deletes RELATION from the canvas

proc gm_delete_relation {relation} {

    global gmainframe
    global gm_relation
    global gm_relations
    global gcanvas

    set userframe [${gmainframe} getframe]


    ;# Unbind the movement
    ${gcanvas} bind $gm_relation($relation,grouptag) <B1-Motion> {}

    ;# Delete all the slaves
    ${gcanvas} delete $gm_relation($relation,grouptag)

    ;# Delete all links from the relation
    ${gcanvas} delete "taggroup-Link-$relation"

    ;# Free the gm_relation array
    unset gm_relation($relation,xpos)
    unset gm_relation($relation,ypos)
    unset gm_relation($relation,xlong)
    unset gm_relation($relation,ylong)
    unset gm_relation($relation,grouptag)
    unset gm_relation($relation,slaves)

    set index [lsearch $gm_relations $relation]
    set gm_relations [lreplace $gm_relations $index $index]


}


;#
;# gm_relation_change_name RELATION NNAME
;#
;# Set NNAME as the new name for RELATION

proc gm_relation_change_name {relation nname} {

    global gm_relations
    global gm_relation

    ;# Change the name in relations
    set index [lsearch $gm_relations $relation]
    set gm_relations [lreplace $gm_relations $index $index $nname]


    ;# Change the name in relation
    set temp_slaves $gm_relation($relation,slaves)
    set temp_grouptag $gm_relation($relation,grouptag)
    set temp_xpos $gm_relation($relation,xpos)
    set temp_ypos $gm_relation($relation,ypos)
    set temp_xlong $gm_relation($relation,xlong)
    set temp_ylong $gm_relation($relation,ylong)

    unset gm_relation($relation,slaves)
    unset gm_relation($relation,grouptag)
    unset gm_relation($relation,xpos)
    unset gm_relation($relation,ypos)
    unset gm_relation($relation,xlong)
    unset gm_relation($relation,ylong)

    set gm_relation($nname,slaves) $temp_slaves
    set gm_relation($nname,grouptag) $temp_grouptag
    set gm_relation($nname,xpos) $temp_xpos
    set gm_relation($nname,ypos) $temp_ypos
    set gm_relation($nname,xlong) $temp_xlong
    set gm_relation($nname,ylong) $temp_ylong

}

;#
;# gm_relation_update_links_reflexive {relation} 
;#
;# Update and redraw all links to the reflexive RELATION

proc gm_relation_update_links_reflexive {relation} {

    global gcanvas
    global gm_relations
    global gm_relation
    global gm_entity

    ;# Delete old links
    ${gcanvas} delete "taggroup-Link-$relation"

    ;# Get the two links
    set entity1 [lindex [gob_relation_get_entities $relation] 0]
    set entity2 [lindex [gob_relation_get_entities $relation] 1]
    
    set entity [lindex $entity1 0]
    set card1 [lindex $entity1 1]
    set card2 [lindex $entity2 1]

    ;# Draw the two links

    ;# Get the bboxes for the entity and the relation
    set bbox_relation [${gcanvas} bbox "taggroup-Relation-$relation"]
    set bbox_entity [${gcanvas} bbox "taggroup-Entity-$entity"]

    ;# Obtain the cuads
    set refpointrx [expr \
			[lindex $bbox_relation 0] + \
			(([lindex $bbox_relation 2] - [lindex $bbox_relation 0]) / 2)]
						 
    set refpointry [expr \
			[lindex $bbox_relation 1] +  \
			(([lindex $bbox_relation 3] - [lindex $bbox_relation 1]) / 2)]

    set refpointex [expr \
			[lindex $bbox_entity 0] + \
			(([lindex $bbox_entity 2] - [lindex $bbox_entity 0]) / 2)]

    set refpointey [expr \
			[lindex $bbox_entity 1] +  \
			(([lindex $bbox_entity 3] - [lindex $bbox_entity 1]) / 2)]


    set cuad_relation_to_entity [gm_get_cuad_reflexive $refpointrx $refpointry \
				     $refpointex $refpointey]



    switch $cuad_relation_to_entity {
	    
	1 { set cuad_entity_to_relation 3 }
	2 { set cuad_entity_to_relation 4 }
	3 { set cuad_entity_to_relation 1 }
	4 { set cuad_entity_to_relation 2 }
    }

    ;# Now, find all the link points
    
    switch $cuad_entity_to_relation {

	1 {

	    set lpr1-x $gm_relation($relation,xpos)
	    set lpr1-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]

	    set lpr1mid-x $refpointex
	    set lpr1mid-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]
	    
	    set lpe1-x $refpointex
	    set lpe1-y $gm_entity($entity,ypos)

	    set lpr2-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2-y [expr ($gm_relation($relation,ypos) + \
				  $gm_relation($relation,ylong))]

	    set lpr2mid-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2mid-y $refpointey

	    set lpe2-x [expr ($gm_entity($entity,xpos) + \
				  $gm_entity($entity,xlong))]
	    set lpe2-y $refpointey
	    
	}

	2 {
	    set lpr1-x [expr ($gm_relation($relation,xpos) + \
				  $gm_relation($relation,xlong))]
	    set lpr1-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]

	    set lpr1mid-x $refpointex
	    set lpr1mid-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]
	    
	    set lpe1-x $refpointex
	    set lpe1-y $gm_entity($entity,ypos)

	    set lpr2-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2-y [expr ($gm_relation($relation,ypos) + \
				  $gm_relation($relation,ylong))]

	    set lpr2mid-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2mid-y $refpointey

	    set lpe2-x $gm_entity($entity,xpos)
	    set lpe2-y $refpointey



	}

	3 {
	    set lpr1-x [expr ($gm_relation($relation,xpos) + \
				  $gm_relation($relation,xlong))]
	    set lpr1-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]

	    set lpr1mid-x $refpointex
	    set lpr1mid-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]
	    
	    set lpe1-x $refpointex
	    set lpe1-y [expr ($gm_entity($entity,ypos) + \
				  $gm_entity($entity,ylong))]

	    set lpr2-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2-y $gm_relation($relation,ypos)


	    set lpr2mid-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2mid-y $refpointey

	    set lpe2-x $gm_entity($entity,xpos)
	    set lpe2-y $refpointey

	}

	4 {

	    set lpr1-x $gm_relation($relation,xpos)
	    set lpr1-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]

	    set lpr1mid-x $refpointex
	    set lpr1mid-y [expr ($gm_relation($relation,ypos) + \
				     ($gm_relation($relation,ylong) / 2))]
	    
	    set lpe1-x $refpointex
	    set lpe1-y [expr ($gm_entity($entity,ypos) + \
				  $gm_entity($entity,ylong))]

	    set lpr2-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2-y $gm_relation($relation,ypos)


	    set lpr2mid-x [expr ($gm_relation($relation,xpos) + \
				  ($gm_relation($relation,xlong) / 2))]
	    set lpr2mid-y $refpointey

	    set lpe2-x [expr ($gm_entity($entity,xpos) + \
				  $gm_entity($entity,xlong))]
	    set lpe2-y $refpointey


	}
    }



    ;# Draw both links into the canvas
    ${gcanvas} create line ${lpr1-x} ${lpr1-y} \
	${lpr1mid-x} ${lpr1mid-y} \
	-tag "taggroup-Link-$relation"
    ${gcanvas} create line ${lpr1mid-x} ${lpr1mid-y} \
	${lpe1-x} ${lpe1-y} \
	-tag "taggroup-Link-$relation"

    ${gcanvas} create line ${lpr2-x} ${lpr2-y} \
	${lpr2mid-x} ${lpr2mid-y} \
	-tag "taggroup-Link-$relation"
    ${gcanvas} create line ${lpr2mid-x} ${lpr2mid-y} \
	${lpe2-x} ${lpe2-y} \
	-tag "taggroup-Link-$relation"

    ;# Draw the cardinality on the middle points
    set card1-x ${lpr1mid-x}
    set card1-y ${lpr1mid-y}
    set card2-x ${lpr2mid-x}
    set card2-y ${lpr2mid-y}

    ${gcanvas} create text \
	${card1-x} ${card1-y} -anchor nw \
	-text "[lindex $card1 0]/[lindex $card1 1]" \
	-tag "taggroup-Link-$relation" -fill red
    ${gcanvas} create text \
	${card2-x} ${card2-y} -anchor nw \
	-text "[lindex $card2 0]/[lindex $card2 1]" \
	-tag "taggroup-Link-$relation" -fill red

    ;# Uff! what a fucking chaos here! ;) [jemarch]
}

;#
;# gm_relation_update_links RELATION
;#
;# Update and redraw all links to RELATIONs entities


proc gm_relation_update_links {relation} {

    global gcanvas
    global gm_relations
    global gm_relation

    ;# Delete old links
    ${gcanvas} delete "taggroup-Link-$relation"

    ;# For each entity
    foreach e [gob_relation_get_entities $relation] {


	;# Get the entity card
	set entity [lindex $e 0]
	set card [lindex $e 1]
	

	;# Draw new links
	gm_draw_link $relation $entity $card
    }

}


;#
;# gm_get_cuad_reflexive BBOX X2 Y2
;#
;# Obtain the cuad relative to (x2,y2) from BBOX1 (the rectangle)

proc gm_get_cuad_reflexive {x1 y1 x2 y2} {

    if {($x2 >= $x1) && ($y2 < $y1)} then {
 	return 1
    }
    if {($x2 >= $x1) && ($y2 >= $y1)} then {
 	return 4
    }
    if {($x2 < $x1) && ($y2 < $y1)} then {
 	return 2
    }
    if {($x2 < $x1) && ($y2 >= $y1)} then {
 	return 3
    }

}

;#
;# gm_get_cuad BBOX X2 Y2
;#
;# Obtain the (irregular) cuad relative to (x2,y2) from BBOX1 (the rectangle)

proc gm_get_cuad {x1 y1 x2 y2} {

#     if {($x2 >= $x1) && ($y2 < $y1)} then {
# 	return 1
#     }
#     if {($x2 >= $x1) && ($y2 >= $y1)} then {
# 	return 4
#     }
#     if {($x2 < $x1) && ($y2 < $y1)} then {
# 	return 2
#     }
#     if {($x2 < $x1) && ($y2 >= $y1)} then {
# 	return 3
#     }
    
    ;# Solve the trivial cases
    if {($x2 == $x1)} then {

	if {$y2 >= $y1} then {
	    return 4
	} 
	if {$y2 < $y1} then {
	    return 2
	}
    }
	


    ;# Calculate the pend of the rect (x1,y1)-(x2,y2)
    set pend [expr ((${y2}.0 - ${y1}.0) / (${x2}.0 - ${x1}.0))]


    ;# Determine the cuad (really the side)
    if {($pend >= 0.5)} then {


	if {$x2 > $x1} then { 

	    return 4 
	}
	if {$x2 < $x1} then {
	    return 2
	}

    }

    if {($pend < -0.5)} then {


	if {$x2 > $x1} then {
	    return 2
	}
	if {$x2 < $x1} then {
	    return 4
	}
    }


    if {$x2 > $x1} then {
	return 1
    }

    if {$x2 < $x1} then {
	return 3
    }

}


;#
;# gm_draw_link RELATION ENTITY CARD
;#
;# Draw a link from RELATION to ENTITY on the canvas, with CARD

proc gm_draw_link {relation entity card} {

    global gm_relation
    global gm_entity
    global gcanvas

    ;# Get the bboxes for the entity and the relation
    set bbox_relation [${gcanvas} bbox "taggroup-Relation-$relation"]
    set bbox_entity [${gcanvas} bbox "taggroup-Entity-$entity"]

    ;# Obtain the cuads
    ;# We use the botton-right points of the rectangles for that.
    ;# Question: Perhaps would be better to use the middle point? [jemarch]
    ;# Answer: yes! done! [jemarch]
    set refpointrx [expr \
			[lindex $bbox_relation 0] + \
			(([lindex $bbox_relation 2] - [lindex $bbox_relation 0]) / 2)]
						 
    set refpointry [expr \
			[lindex $bbox_relation 1] +  \
			(([lindex $bbox_relation 3] - [lindex $bbox_relation 1]) / 2)]

    set refpointex [expr \
			[lindex $bbox_entity 0] + \
			(([lindex $bbox_entity 2] - [lindex $bbox_entity 0]) / 2)]

    set refpointey [expr \
			[lindex $bbox_entity 1] +  \
			(([lindex $bbox_entity 3] - [lindex $bbox_entity 1]) / 2)]

    set cuad_relation_to_entity [gm_get_cuad $refpointrx $refpointry \
				     $refpointex $refpointey]

    switch $cuad_relation_to_entity {

	1 { set cuad_entity_to_relation 3 }
	2 { set cuad_entity_to_relation 4 }
	3 { set cuad_entity_to_relation 1 }
	4 { set cuad_entity_to_relation 2 }
    }


    ;# Now, find the link points
    
    ;# Get the link point of the relation
    switch $cuad_relation_to_entity {

	1 {

	    set lprelation-x [expr ($gm_relation($relation,xpos) + \
					$gm_relation($relation,xlong))]

	    set lprelation-y [expr ($gm_relation($relation,ypos) + \
					($gm_relation($relation,ylong) / 2))]
				

	}

	2 {

	    set lprelation-x [expr ($gm_relation($relation,xpos) + \
					($gm_relation($relation,xlong) / 2))]

	    set lprelation-y $gm_relation($relation,ypos)

	}

	3 {
	    set lprelation-x $gm_relation($relation,xpos)

	    set lprelation-y [expr ($gm_relation($relation,ypos) + \
					($gm_relation($relation,ylong) / 2))]
	}

	4 {
	    set lprelation-x [expr ($gm_relation($relation,xpos) + \
					($gm_relation($relation,xlong) / 2))]
	    set lprelation-y [expr ($gm_relation($relation,ypos) + \
					$gm_relation($relation,ylong))]

	}
    }


    ;# Get the link point of the entity
    switch $cuad_entity_to_relation {

	1 {

	    set lpentity-x [expr ($gm_entity($entity,xpos) + \
					$gm_entity($entity,xlong))]

	    set lpentity-y [expr ($gm_entity($entity,ypos) + \
					($gm_entity($entity,ylong) / 2))]
				

	}

	2 {

	    set lpentity-x [expr ($gm_entity($entity,xpos) + \
					($gm_entity($entity,xlong) / 2))]

	    set lpentity-y $gm_entity($entity,ypos)

	}

	3 {
	    set lpentity-x $gm_entity($entity,xpos)

	    set lpentity-y [expr ($gm_entity($entity,ypos) + \
					($gm_entity($entity,ylong) / 2))]
	}

	4 {
	    set lpentity-x [expr ($gm_entity($entity,xpos) + \
					($gm_entity($entity,xlong) / 2))]
	    set lpentity-y [expr ($gm_entity($entity,ypos) + \
					$gm_entity($entity,ylong))]

	}
    }


    ;# Ok, now we have both link points:
    ;#  
    ;#   (lprelation-x,lprelation-y) and (lpentity-x,lpentity-y)

    ;# Draw the link itself into the canvas
    ${gcanvas} create line ${lprelation-x} ${lprelation-y} \
	${lpentity-x} ${lpentity-y} \
	-tag "taggroup-Link-$relation"


    ;# Draw the cardinality 
    set card-x [expr (${lpentity-x} + ((${lprelation-x} - ${lpentity-x}) / 2))]
    set card-y [expr (${lpentity-y} + ((${lprelation-y} - ${lpentity-y}) / 2))]

    ${gcanvas} create text \
	${card-x} ${card-y} -anchor nw \
	-text \
	"[gob_relation_get_entity_min_card $relation $entity]/[gob_relation_get_entity_max_card $relation $entity]" \
	-tag "taggroup-Link-$relation" -fill red
    

}


;##################################################
;############## FLYING KEYS ROUTINES ##############
;##################################################


;#
;# gm_draw_flkey FROMTABLE TOTABLE KEYLIST XPOS YPOS
;#
;# Draw a flying key

proc gm_draw_flkey {fromtable totable keylist xpos ypos} {

    global gmainframe
    global gerwin_table
    global gm_table


    global gcanvas_td

    set userframe [${gmainframe} getframe]

    ;# Create all slaves objects
    set slaves {}

    ;# The box with the name
    lappend slaves [${gcanvas_td} create text \
			[expr $xpos + 17] [expr $ypos + 2] \
			-anchor nw -text "$keylist" -fill darkgreen \
			-tag taggroup-Flkey-$fromtable-$totable]

    ;# Get the bbox
    set ebbox [${gcanvas_td} bbox taggroup-Flkey-$fromtable-$totable]

    ;# Create the surrounding rect
    lappend slaves [${gcanvas_td} create rect \
			$xpos $ypos \
			[lindex $ebbox 2] [lindex $ebbox 3] \
			-tag taggroup-Flkey-$fromtable-$totable -outline darkgreen]

    ;# Bind for moving
    ${gcanvas_td} bind taggroup-Flkey-$fromtable-$totable <B1-Motion> {gui_drag_td %x %y}

    ;# Bind for marking
    ${gcanvas_td} bind taggroup-Flkey-$fromtable-$totable

}




;##################################################
;############## TABLE ROUTINES  ###################
;##################################################


;#
;# gm_draw_table TABLE XPOS YPOS
;#
;# Draw TABLE at (XPOS,YPOS)  on the TD canvas

proc gm_draw_table {table xpos ypos} {

    global gmainframe
    global gerwin_table
    global gm_table
    
    global gcanvas_td

    set userframe [${gmainframe} getframe]

    ;# Create all slaves objects
    set slaves {}

    ;# The box with the name
    lappend slaves [${gcanvas_td} create text \
			[expr $xpos + 17] [expr $ypos + 2] \
			-anchor nw -text "$table" -fill violet \
			-tag taggroup-Table-$table]

    lappend slaves [${gcanvas_td} create rect \
			$xpos $ypos \
			[expr $xpos + 15] [expr $ypos + 20] \
			-tag taggroup-Table-$table -fill violet -outline violet]

    lappend slaves [${gcanvas_td} create text \
			[expr $xpos + 4] [expr $ypos + 4] \
			-anchor nw -text "T" -fill white \
			-tag taggroup-Table-$table]

    set line_title [${gcanvas_td} create line \
			$xpos [expr $ypos + 20] [expr $xpos + 150] [expr $ypos + 20] \
			-tag taggroup-Table-$table -fill violet]

    lappend slaves $line_title
    lappend to_expand $line_title

    ;# Draw all the key attributes for the table
    set were_keys 0
    set i 2
    set ypos [expr $ypos + 5]
    foreach attribute $gerwin_table($table,attributes) {


	if {[gob_table_is_key $table [lindex $attribute 0]]} then {


	    set were_keys 1

	    ;# Get the coords for the new rect
	    set bxi [expr $xpos]
	    set byi [expr $ypos + (($i * 15) - 15)]
	    set bxf [expr $xpos + 150]
	    set byf [expr $ypos + ($i * 15)]


	    ;# Get the coords for the new text
	    set tx [expr $bxi + 2]
	    set ty [expr $byi + 2]

	    ;# Determine the text contents
	    set ttext "[lindex ${attribute} 0] [gob_table_get_attribute_domain $table [lindex $attribute 0]]"

	    ;# See if we must to insert the NON NULL
	    if {! [gob_table_get_attribute_null $table [lindex $attribute 0]] } then {
		lappend ttext "NOT_NULL"
	    }


	    lappend slaves [${gcanvas_td} create text $tx $ty -anchor nw \
				-text $ttext -tag taggroup-Table-$table]

	    incr i

	}

    }

    if {$were_keys} then {
	;# Make the separator line
	set line_key [${gcanvas_td} create line $xpos $byf $bxf $byf \
			  -tag taggroup-Table-$table]

	lappend slaves $line_key
	lappend to_expand $line_key

    }

    ;# Draw all the non-key attributes for the table
    foreach attribute $gerwin_table($table,attributes) {

	if {! [gob_table_is_key $table [lindex $attribute 0]] } then {


	    ;# Get the coords for the new rect
	    set bxi [expr $xpos]
	    set byi [expr $ypos + (($i * 15) - 15)]
	    set bxf [expr $xpos + 150]
	    set byf [expr $ypos + ($i * 15)]

	
	    ;# Get the coords for the new text
	    set tx [expr $bxi + 2]
	    set ty [expr $byi + 2]

	    set ttext "[lindex ${attribute} 0] [gob_table_get_attribute_domain $table [lindex $attribute 0]]"

	    ;# See if we must to insert the NON NULL
	    if {! [gob_table_get_attribute_null $table [lindex $attribute 0]] } then {
		lappend ttext "NOT_NULL"
	    }

	    lappend slaves [${gcanvas_td} create text $tx $ty -anchor nw \
				-text $ttext -tag taggroup-Table-$table]

	    incr i
	}
    }

    ;# Create the new wgob
    set gm_table($table,grouptag) taggroup-Table-$table

    ;# Get the bbox
    set ebbox [${gcanvas_td} bbox taggroup-Table-$table]
    set gm_table($table,xpos) [lindex $ebbox 0]
    set gm_table($table,ypos) [lindex $ebbox 1]
    set gm_table($table,xlong) \
	[expr [lindex $ebbox 2] - $gm_table($table,xpos)]
    set gm_table($table,ylong) \
	[expr [lindex $ebbox 3] - $gm_table($table,ypos)]

    ;# Create the surrounding rect
    lappend slaves [${gcanvas_td} create rect \
			$gm_table($table,xpos) $gm_table($table,ypos) \
			[lindex $ebbox 2] [lindex $ebbox 3] \
			-tag $gm_table($table,grouptag) -outline violet]

    ;# Another time, get the bbox
    set ebbox [${gcanvas_td} bbox $gm_table($table,grouptag)]
    set gm_table($table,xpos) [lindex $ebbox 0]
    set gm_table($table,ypos) [lindex $ebbox 1]
    set gm_table($table,xlong) \
	[expr [lindex $ebbox 2] - $gm_table($table,xpos)]
    set gm_table($table,ylong) \
	[expr [lindex $ebbox 3] - $gm_table($table,ypos)]
    
    ;# Resize to expand lines
    foreach l $to_expand {

	set lcoords [${gcanvas_td} coords $l]
	set lcoords [lreplace $lcoords 2 2 [lindex $ebbox 2]]
	
	${gcanvas_td} coords $l $lcoords

    }

    ;# Resave the positions
    set ebbox [${gcanvas_td} bbox taggroup-Table-$table]
    set gm_table($table,xpos) [lindex $ebbox 0]
    set gm_table($table,ypos) [lindex $ebbox 1]

    ;# Put the slaves into the wgob
    set gm_table($table,slaves) $slaves

    ;# Bind for moving
    ${gcanvas_td} bind $gm_table($table,grouptag) <B1-Motion> {gui_drag_td %x %y}

    ;# Bind for marking
    ${gcanvas_td} bind $gm_table($table,grouptag) 

}

;#
;# gm_delete_table TABLE
;#
;# Removes TABLE from the TD canvas 

proc gm_delete_table {table} {

    global gmainframe
    global gm_table
    global gm_tables
    global gcanvas_td

    set userframe [${gmainframe} getframe]

    ;# Unbind the movement
    ${gcanvas_td} bind $gm_table($table,grouptag) <B1-Motion> {}

    ;# Delete all the slaves 
    ${gcanvas_td} delete $gm_table($table,grouptag)

    ;# Dree the gm_table array entry for table
    unset gm_table($table,xpos)
    unset gm_table($table,ypos)
    unset gm_table($table,xlong)
    unset gm_table($table,ylong)
    unset gm_table($table,grouptag)
    unset gm_table($table,slaves)

    set index [lsearch $gm_tables $table]
    set gm_tables [lreplace $gm_tables $index $index]

    ;# Delete all the table's links
    ${gcanvas_td} delete "taggroup-Link-$table"
    ;#gm_delete_flkey 
}


;#
;# gm_table_update_links TABLE [XPOS YPOS]
;#
;# Update and redraw all links to related tables
;#

proc gm_table_update_links {table} {

    global gcanvas_td
    global gm_tables
    global gm_table
    global gerwin_table
    global gm_managed
    global gm_flkey

    

    ;# Delete old links
    foreach t [gob_table_get_foreign_keys $table] {
	
	set rtable [lindex $t 1]
	
	;#${gcanvas_td} delete "taggroup-Flkey-$rtable-$table"
	${gcanvas_td} delete "taggroup-Flkey-link-$rtable-$table"

    }

    ;# For each foreign key
    foreach t [gob_table_get_foreign_keys $table] {

	set rtable [lindex $t 1]
	set rattribute [lindex $t 0]

	lappend rattributes($rtable) $rattribute

    }

    ;# For each rtable
    foreach rtable [array names rattributes] {

	;# Draw the new link if the rtable is not the table
	if {$rtable != $table} then {
	    gm_draw_link_td $table $rtable $rattributes($rtable)
	}

    }

    ;# Then, for each referring table
    foreach t $gerwin_table($table,rtables) {

	;# Update the links if table is not rtable
	if {$t != $table} then {

	    ;# Update the managed list
	    if {[lsearch $gm_managed $t] == -1} then {

		lappend gm_managed $table

		gm_table_update_links $t
	    }

	    
	}

    }

    ;# Clear the managed list
    set gm_managed {}

}


;#
;# gob_draw_link_td TABLE RTABLE RATTRIBUTES
;#
;# Draw a link from RTABLE to TABLE with RATTRIBUTES (a list with the labels)

proc gm_draw_link_td {table rtable rattributes} {


    global gm_table
    global gcanvas_td
    global gm_flkey

    ;# Get the bboxes for both tables
    set bbox_table [${gcanvas_td} bbox "taggroup-Table-$table"]
    set bbox_rtable [${gcanvas_td} bbox "taggroup-Table-$rtable"]
    
    ;# Decompose it into single variables
    set t_x [lindex $bbox_table 0]
    set t_y [lindex $bbox_table 1]
    set tr_x [lindex $bbox_rtable 0]
    set tr_y [lindex $bbox_rtable 1]


    ;# Draw the flying key

    ;# Make the label

    set le [llength $rattributes]
    set i 1
    set ltext {}
    foreach lname $rattributes {

	;# Transform any 'object%attribute' to 'attribute'
	if {[regexp \% $lname]} then {
	    regsub {.+%(.+)} $lname "\\1" lname
	}

	if {$i == $le} then {
	    if {! [info exists ${lname}]} then {
		lappend ltext "${lname}"
		set ${lname} lala
	    }
	    
	} else {
	    if {! [info exists ${lname}]} then {
		lappend ltext "${lname},"
	    } 
	}
	incr i
    } 


    ;# Create the new flkey, if it do not exist
    if {! [gm_flkey_exist $rtable $table]} then {
	gm_flkey_create $rtable $table
    }



    ;# Draw the rattribute, according with the new or old status of the flkey
    if {([gm_flkey_get_xpos $rtable $table] == -1) && ([gm_flkey_get_ypos $rtable $table] == -1)} then {


	set {rattr_x} [expr (${t_x} + ((${tr_x} - ${t_x}) / 2))]
	set {rattr_y} [expr (${t_y} + ((${tr_y} - ${t_y}) / 2))]

	set drawit 1

	;# Update the new coordinates
	gm_flkey_set_xpos $rtable $table ${rattr_x}
	gm_flkey_set_ypos $rtable $table ${rattr_y}

    } else {
	;# Put it on $xpos and $ypos
	set {rattr_x} [gm_flkey_get_xpos $rtable $table]
	set {rattr_y} [gm_flkey_get_ypos $rtable $table]

	set drawit 0
    }

     if {$drawit} then {

	 if {[info exist gm_flkey("$table-$rtable",xpos)]} then {

	     ;# Get the bounding box of the inverse flkey
	     set bbox_iflkey [${gcanvas_td} bbox "taggroup-Flkey-$table-$rtable"]

	     set iflk1_x [lindex $bbox_iflkey 0]
	     set iflk1_y [lindex $bbox_iflkey 1]
	     set iflk2_x [lindex $bbox_iflkey 2]
	     set iflk2_y [lindex $bbox_iflkey 3]

	     ;# We determine rattr_x and rattr_y depending of the relative position of the
	     ;# iflkey.
	     ;#
	     ;# Such relative position can be obtained from the relative cuads of the two involving tables. 

	     ;# Obtain the relative quad from RTABLE to TABLE
	     set refpointtx [expr \
				     [lindex $bbox_table 0] + \
				     (([lindex $bbox_table 2] - [lindex $bbox_table 0]) / 2)]
	     
	     set refpointty [expr \
				 [lindex $bbox_table 1] +  \
				 (([lindex $bbox_table 3] - [lindex $bbox_table 1]) / 2)]

	     set refpointrtx [expr \
				  [lindex $bbox_rtable 0] + \
				  (([lindex $bbox_rtable 2] - [lindex $bbox_rtable 0]) / 2)]

	     set refpointrty [expr \
				  [lindex $bbox_rtable 1] +  \
				  (([lindex $bbox_rtable 3] - [lindex $bbox_rtable 1]) / 2)]


	     set cuad_table_to_rtable [gm_get_cuad $refpointtx $refpointty \
					  $refpointrtx $refpointrty]



	     ;# If cuad_table_to_rtable is 1 or 3, then put the new flkey behind the last
	     if {($cuad_table_to_rtable == 1) || ($cuad_table_to_rtable == 3)} then {

		 set rattr_x $iflk1_x
		 set rattr_y [expr $iflk2_y + 20]

	     } else  {
		 ;# If cuad_table_to_rtable is 2 or 4, then put the new flkey at the right of the last
		 
		 set rattr_x [expr $iflk2_x + 20]
		 set rattr_y ${iflk1_y}


	     }

	 }


	 gm_draw_flkey $rtable $table $ltext ${rattr_x} ${rattr_y}
     }


    ;# Obtain the Flkey bounding box
    set bbox_flkey [${gcanvas_td} bbox "taggroup-Flkey-$rtable-$table"]

    ;# Decompose it into simple variables
    set fl_x [lindex $bbox_flkey 0]
    set fl_y [lindex $bbox_flkey 1]


    ;# 
    ;# Get the link points from TABLE to the FLKEY
    ;#
    ;#

    ;# Obtain the cuads 
    set refpointtx [expr \
			[lindex $bbox_table 0] + \
			(([lindex $bbox_table 2] - [lindex $bbox_table 0]) / 2)]
						 
    set refpointty [expr \
			[lindex $bbox_table 1] +  \
			(([lindex $bbox_table 3] - [lindex $bbox_table 1]) / 2)]

    set refpointflx [expr \
			 [lindex $bbox_flkey 0] + \
			 (([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0]) / 2)]

    set refpointfly [expr \
			 [lindex $bbox_flkey 1] +  \
			 (([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]) / 2)]


    set cuad_table_to_flkey [gm_get_cuad $refpointtx $refpointty \
				  $refpointflx $refpointfly]

    switch $cuad_table_to_flkey {

	1 { set cuad_flkey_to_table 3 }
	2 { set cuad_flkey_to_table 4 }
	3 { set cuad_flkey_to_table 1 }
	4 { set cuad_flkey_to_table 2 }
    }

    ;# Get the link point for the table
    switch $cuad_table_to_flkey {

	1 {

	    set lptable-x [expr ($gm_table($table,xpos) + \
					$gm_table($table,xlong))]

	    set lptable-y [expr ($gm_table($table,ypos) + \
					($gm_table($table,ylong) / 2))]
				

	}

	2 {

	    set lptable-x [expr ($gm_table($table,xpos) + \
					($gm_table($table,xlong) / 2))]

	    set lptable-y $gm_table($table,ypos)

	}

	3 {
	    set lptable-x $gm_table($table,xpos)

	    set lptable-y [expr ($gm_table($table,ypos) + \
					($gm_table($table,ylong) / 2))]
	}

	4 {
	    set lptable-x [expr ($gm_table($table,xpos) + \
					($gm_table($table,xlong) / 2))]
	    set lptable-y [expr ($gm_table($table,ypos) + \
					$gm_table($table,ylong))]

	}
    }

    ;# Get the link point for the flkey
    switch $cuad_flkey_to_table {

	1 {

	    set lpflkey-x [expr [lindex $bbox_flkey 0] + \
			       ([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0])]

	    set lpflkey-y [expr [lindex $bbox_flkey 1] + \
			       (([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]) / 2)]
				

	}

	2 {

	    set lpflkey-x [expr [lindex $bbox_flkey 0] + \
			       (([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0]) / 2)]

	    set lpflkey-y [lindex $bbox_flkey 1]

	}

	3 {
	    set lpflkey-x [lindex $bbox_flkey 0]

	    set lpflkey-y [expr ([lindex $bbox_flkey 1] + \
				     (([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]) / 2))]
	}

	4 {
	    set lpflkey-x [expr ([lindex $bbox_flkey 0] + \
				     (([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0]) / 2))]
	    set lpflkey-y [expr ([lindex $bbox_flkey 1] + \
				     ([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]))]

	}
    }


    ;# 
    ;# Get the link points from RTABLE to the FLKEY
    ;#
    ;#

    ;# Obtain the cuads 
    set refpointtx [expr \
			[lindex $bbox_rtable 0] + \
			(([lindex $bbox_rtable 2] - [lindex $bbox_rtable 0]) / 2)]
						 
    set refpointty [expr \
			[lindex $bbox_rtable 1] +  \
			(([lindex $bbox_rtable 3] - [lindex $bbox_rtable 1]) / 2)]

    set refpointflx [expr \
			 [lindex $bbox_flkey 0] + \
			 (([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0]) / 2)]

    set refpointfly [expr \
			 [lindex $bbox_flkey 1] +  \
			 (([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]) / 2)]


    set cuad_rtable_to_flkey [gm_get_cuad $refpointtx $refpointty \
				  $refpointflx $refpointfly]

    switch $cuad_rtable_to_flkey {

	1 { set cuad_flkey_to_rtable 3 }
	2 { set cuad_flkey_to_rtable 4 }
	3 { set cuad_flkey_to_rtable 1 }
	4 { set cuad_flkey_to_rtable 2 }
    }

    ;# Get the link point for the rtable
    switch $cuad_rtable_to_flkey {

	1 {

	    set lprtable-x [expr ($gm_table($rtable,xpos) + \
					$gm_table($rtable,xlong))]

	    set lprtable-y [expr ($gm_table($rtable,ypos) + \
					($gm_table($rtable,ylong) / 2))]
				

	}

	2 {

	    set lprtable-x [expr ($gm_table($rtable,xpos) + \
					($gm_table($rtable,xlong) / 2))]

	    set lprtable-y $gm_table($rtable,ypos)

	}

	3 {
	    set lprtable-x $gm_table($rtable,xpos)

	    set lprtable-y [expr ($gm_table($rtable,ypos) + \
					($gm_table($rtable,ylong) / 2))]
	}

	4 {
	    set lprtable-x [expr ($gm_table($rtable,xpos) + \
					($gm_table($rtable,xlong) / 2))]
	    set lprtable-y [expr ($gm_table($rtable,ypos) + \
					$gm_table($rtable,ylong))]

	}
    }

    ;# Get the link point for the flkey
    switch $cuad_flkey_to_rtable {

	1 {

	    set lpflkey2-x [expr [lindex $bbox_flkey 0] + \
			       ([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0])]

	    set lpflkey2-y [expr [lindex $bbox_flkey 1] + \
			       (([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]) / 2)]
				

	}

	2 {

	    set lpflkey2-x [expr [lindex $bbox_flkey 0] + \
			       (([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0]) / 2)]

	    set lpflkey2-y [lindex $bbox_flkey 1]

	}

	3 {
	    set lpflkey2-x [lindex $bbox_flkey 0]

	    set lpflkey2-y [expr ([lindex $bbox_flkey 1] + \
				     (([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]) / 2))]
	}

	4 {
	    set lpflkey2-x [expr ([lindex $bbox_flkey 0] + \
				     (([lindex $bbox_flkey 2] - [lindex $bbox_flkey 0]) / 2))]
	    set lpflkey2-y [expr ([lindex $bbox_flkey 1] + \
				     ([lindex $bbox_flkey 3] - [lindex $bbox_flkey 1]))]

	}
    }






    ;# Draw the link itself into the canvas
    ${gcanvas_td} create line ${lprtable-x} ${lprtable-y} \
 	${lpflkey2-x} ${lpflkey2-y} \
 	-tag "taggroup-Flkey-link-$rtable-$table"
    
    ${gcanvas_td} create line ${lptable-x} ${lptable-y} \
 	${lpflkey-x} ${lpflkey-y} \
 	-tag "taggroup-Flkey-link-$rtable-$table" -arrow first
    


}


;#
;# gm_flkey_create RTABLE TABLE
;#
;# Creates a new flkey on the system, from RTABLE to TABLE
;#

proc gm_flkey_create {rtable table} {

    global gm_flkey

    ;# See if the flkey already exist on the system
    if {[gm_flkey_exist $rtable $table]} then {
	
	return

    }

    ;# Ok, create the new rtable
    ;# On (-1, -1), by default
    set gm_flkey("$rtable-$table",xpos) -1
    set gm_flkey("$rtable-$table",ypos) -1

    
    ;# Done.
}


;#
;# gm_flkey_exist RTABLE TABLE
;#
;# Return 1 if RTABLE-TABLE exist on the system
;# Return 0 else.

proc gm_flkey_exist {rtable table} {

    global gm_flkey

    return [info exist gm_flkey("$rtable-$table",xpos)]
}

;#
;# gm_flkey_get_xpos RTABLE TABLE
;#
;# Return the x position for RTABLE-TABLE

proc gm_flkey_get_xpos {rtable table} {

    global gm_flkey

    ;# FIXME: no checking at all here!
    return $gm_flkey("$rtable-$table",xpos)


}

;#
;# gm_flkey_get_ypos RTABLE TABLE
;#
;# Return the y position for RTABLE-TABLE

proc gm_flkey_get_ypos {rtable table} {

    global gm_flkey

    ;# FIXME: no checking at all here!
    return $gm_flkey("$rtable-$table",ypos)


}


;#
;# gm_flkey_set_xpos RTABLE TABLE XPOS
;#
;# Set a new position x for RTABLE-TABLE

proc gm_flkey_set_xpos {rtable table xpos} {

    global gm_flkey

    ;# FIXME: no checking at all here!
    set gm_flkey("$rtable-$table",xpos) $xpos

}

;#
;# gm_flkey_set_ypos RTABLE TABLE YPOS
;#
;# Set a new position y for RTABLE-TABLE

proc gm_flkey_set_ypos {rtable table ypos} {

    global gm_flkey

    ;# FIXME: no checking at all here!
    set gm_flkey("$rtable-$table",ypos) $ypos

}


;#
;# gm_delete_flkey TABLE
;#
;# Deletes all flkeys on which TABLE appears.

proc gm_delete_flkey {table} {

    FIXME "implement gm_delete_flkey"

}