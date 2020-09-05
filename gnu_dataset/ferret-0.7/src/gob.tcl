;# gob.tcl -- Gerwin
;#
;# Gerwin-objects implementation
;#
;# Copyright (C) 2003-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:21:28 jemarch"

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

;####################################
;########## GOB LABEL ###############
;####################################


;# 
;# gob_create_label TEXT FONT
;#
;# Creates a new label containing TEXT with FONT

proc gob_create_label {text font} {

    global gerwin_labels
    global gerwin_label
    global gerwin_label_seq

    ;# Get a new name for the label
    set nname [gob_label_get_new_name]

    lappend gerwin_labels $nname
    set gerwin_label($nname,text) $text
    set gerwin_label($nname,font) $font

}

;#
;# gob_destroy_label LABEL
;#
;# Destroy LABEL

proc gob_destroy_label {label} {

    global gerwin_labels
    global gerwin_label

    ;# Manage gerwin_labels
    set index [lsearch $gerwin_labels]
    set gerwin_labels [lreplace $gerwin_labels $index $index]

    ;# Manage gerwin_label
    unset gerwin_label($label,text)
    unset gerwin_label($label,font)

}


;#
;# gob_label_get_new_name
;#
;# Return a new unique name for a label

proc gob_label_get_new_name {} {

    global gerwin_label_seq

    set name label$gerwin_label_seq

    incr gerwin_label_seq

    return $name

}


;####################################
;##########  GOB ENTITY ##############
;####################################

;#
;# gob_create_entity NAME ATTRIBUTES KEY RELATIONS
;#
;# Creates a new entity into the system

proc gob_create_entity {name attributes key relations} {

    global gerwin_entities
    global gerwin_entity

    ;# See if already exist an entity named NAME
    if {[gob_entity_exist $name]} then {
	gui_notice "Entity $name already exist."
	return
    }

    ;# OK, create the new entity
    lappend gerwin_entities $name
    
    set gerwin_entity($name,attributes) $attributes
    set gerwin_entity($name,key) $key
    set gerwin_entity($name,relations) $relations


}

;#
;# gob_entity_change_relation_name ENTITY RELATION NNAME
;#
;# RELATION is now called NNAME...

proc gob_entity_change_relation_name {entity relation nname} {

    global gerwin_entity

    set relations $gerwin_entity($entity,relations)
    set index [lsearch $relations $relation]
    set relations [lreplace $relations $index $index $nname]
    
    set gerwin_entity($entity,relations) $relations

}

;#
;# gob_entity_add_relation ENTITY RELATION
;#
;# Add RELATION to ENTITYs relations set

proc gob_entity_add_relation {entity relation} {

    global gerwin_entity

    lappend gerwin_entity($entity,relations) $relation

}

;#
;# gob_entity_delete_relation ENTITY RELATION
;#
;# Deletes RELATION from ENTITYs relation set

proc gob_entity_delete_relation {entity relation} {

    global gerwin_entity

    set relations $gerwin_entity($entity,relations)
    set index [lsearch $relations $relation]
    set relations [lreplace $relations $index $index]

    set gerwin_entity($entity,relations) $relations

}


;#
;# gob_entity_get_relations ENTITY
;#
;# Return a list with all relations of ENTITY

proc gob_entity_get_relations {entity} {

    global gerwin_entity

    return $gerwin_entity($entity,relations)

}


;#
;# gob_delete_entity ENTITY
;#
;# Destroys the GOB ENTITY object

proc gob_delete_entity {entity} {

    global gerwin_entities
    global gerwin_entity

    unset gerwin_entity($entity,attributes)
    unset gerwin_entity($entity,key)

    set index [lsearch $gerwin_entities $entity]
    set gerwin_entities [lreplace $gerwin_entities $index $index]

}

;#
;# gob_entity_add_attribute NAME ATTRIBUTE DOMAIN
;#
;# Add ATTRIBUTE to the attributes set for NAME

proc gob_entity_add_attribute {name attribute domain} {

    global gerwin_entity

    ;# See if attribute already exist into the entity
    if {[gob_entity_attribute_exist $name $attribute]} then {
	gui_notice "Attribute $attribute already exist in $name"
	return 
    }

    ;# OK, add the attribute
    set temp $gerwin_entity($name,attributes)
    lappend temp [list $attribute $domain]
    set gerwin_entity($name,attributes) $temp

}


;#
;# gob_entity_set_attribute_domain ENTITY ATTRIBUTE DOMAIN
;#
;# Set DOMAIN as the new domain for ATTRIBUTE, in ENTITY

proc gob_entity_set_attribute_domain {entity attribute domain} {

    global gerwin_entity

    set attributes $gerwin_entity($entity,attributes)

    ;# Replace the old entry with the newer one
    set index [lsearch -glob $attributes "$attribute *"]
    set attributes [lreplace $attributes $index $index [list $attribute $domain]]

    set gerwin_entity($entity,attributes) $attributes

}

;# 
;# gob_entity_exist NAME
;#
;# Return 1 if NAME is an entity.
;# Return 0 else.

proc gob_entity_exist {name} {

    global gerwin_entities

    if {[lsearch $gerwin_entities $name] == -1} then {
	return 0
    } else {
	return 1
    }
}


;#
;# gob_entity_attribute_exist ENTITY ATTRIBUTE
;#
;# Return 1 if ATTRIBUTE exist in ENTITY
;# Return 0 else.

proc gob_entity_attribute_exist {entity attribute} {

    global gerwin_entity

    set attributes $gerwin_entity($entity,attributes)

    if {[lsearch -glob $attributes "$attribute *"] == -1} then {
	return 0
    } else {
	return 1
    }

}


;#
;# gob_entity_get_attributes ENTITY
;#
;# Return a list with all ENTITY's attributes names

proc gob_entity_get_attributes {entity} {
    
    global gerwin_entity

    return $gerwin_entity($entity,attributes)

}


;#
;# gob_entity_get_attribute_domain ENTITY ATTRIBUTE
;#
;# Return the domain of ATTRIBUTE, from ENTITY

proc gob_entity_get_attribute_domain {entity attribute} {

    global gerwin_entity

    set attributes $gerwin_entity($entity,attributes)

    set index [lsearch -glob $attributes "$attribute *"]

    return [lindex [lindex $attributes $index] 1]
}


;#
;# gob_entity_add_attribute_to_key ENTITY ATTRIBUTE
;#
;# Add ATTRIBUTE to be part of the key of ENTITY

proc gob_entity_add_attribute_to_key {entity attribute} {
    
    global gerwin_entity

    ;# Get the actual key
    set akey $gerwin_entity($entity,key)
    
    ;# Add the new attribute
    lappend akey $attribute

    ;# Set the new key
    set gerwin_entity($entity,key) $akey

}


;# 
;# gob_entity_delete_attribute_from_key ENTITY ATTRIBUTE
;#
;# Delete ATTRIBUTE from ENTITY's key

proc gob_entity_delete_attribute_from_key {entity attribute} {

    global gerwin_entity

    ;# Get the actual key
    set akey $gerwin_entity($entity,key)

    ;# Find the attribute to delete
    set index [lsearch $akey $attribute]

    ;# Remove the attribute
    set akey [lreplace $akey $index $index]

    ;# Set the new key
    set gerwin_entity($entity,key) $akey

}


;#
;# gob_entity_get_key ENTITY
;#
;# Return the ENTITY's key

proc gob_entity_get_key {entity} {

    global gerwin_entity

    return $gerwin_entity($entity,key)

}


;#
;# gob_entity_is_key ENTITY ATTRIBUTE
;#
;# Return 1 if ATTRIBUTE is in ENTITY's key
;# Return 0 else

proc gob_entity_is_key {entity attribute} {

    global gerwin_entity

    if {[lsearch $gerwin_entity($entity,key) $attribute] == -1} then {
	return 0
    } else {
	return 1
    }

}


;#
;# gob_entity_get_num_attributes ENTITY
;#
;# Return the number of attributes of ENTITY

proc gob_entity_get_num_attributes {entity} {

    global gerwin_entity 

    return [llength $gerwin_entity($entity,attributes)]

}

;#
;# gob_entity_delete_attribute ENTITY ATTRIBUTE
;#
;# Deletes ATTRIBUTE from ENTITY
;# Of course, if ATTRIBUTE is in the key, then it is also removed from the key

proc gob_entity_delete_attribute {entity attribute} {

    global gerwin_entity

    set attributes $gerwin_entity($entity,attributes)
    
    set index [lsearch -glob $attributes "$attribute *"]

    set attributes [lreplace $attributes $index $index]

    set gerwin_entity($entity,attributes) $attributes

    ;# Process the key
    if {[gob_entity_is_key $entity $attribute]} then {
	gob_entity_delete_attribute_from_key $entity $attribute
    }

}


;#
;# gob_entity_change_name ENTITY NEWNAME
;#
;# Set NEWNAME as the new name for ENTITY

proc gob_entity_change_name {entity nname} {

    global gerwin_entities
    global gerwin_entity
    global gerwin_relation

    ;# Change the name in entities
    set index [lsearch $gerwin_entities $entity]
    set gerwin_entities [lreplace $gerwin_entities $index $index $nname]

    ;# Change the name in entity
    set temp_attributes $gerwin_entity($entity,attributes)
    set temp_key $gerwin_entity($entity,key)
    set temp_relations $gerwin_entity($entity,relations)

    unset gerwin_entity($entity,attributes)
    unset gerwin_entity($entity,key)
    unset gerwin_entity($entity,relations)

    set gerwin_entity($nname,attributes) $temp_attributes
    set gerwin_entity($nname,key) $temp_key
    set gerwin_entity($nname,relations) $temp_relations

    
    ;# Change from $entity to $nname in all the relations
    foreach r [gob_entity_get_relations $nname] {

	if {$gerwin_relation($r,reflexive)} then {
	    ;# Reflexive relation
	    gob_relation_change_entity_name_reflexive $r $nname
	} else {

	    gob_relation_change_entity_name $r $entity $nname
	}

    }
}


;####################################
;##########  GOB RELATION ############
;####################################

;#
;# gob_create_relation NAME ENTITIES-CARDS ATTRIBUTES
;#
;# Creates a new relation into the system

proc gob_create_relation {name entities attributes} {

    global gerwin_relations
    global gerwin_relation

    ;# See if already exist a relation named NAME
    if {[gob_relation_exist $name]} then {
	gui_notice "Relation $name already exist."
	return
    }

    ;# Ok, create the new relation
    lappend gerwin_relations $name
    
    set gerwin_relation($name,entities) $entities
    set gerwin_relation($name,attributes) $attributes
    set gerwin_relation($name,reflexive) 0 ;# Not by default

}


;#
;# gob_relation_exist NAME
;#
;# Return 1 if NAME is a relation
;# Return 0 else.

proc gob_relation_exist {name} {

    global gerwin_relations
    
    if {[lsearch $gerwin_relations $name] == -1} then {
	return 0
    } else {
	return 1
    }

}


;# 
;# gob_relation_add_attribute NAME ATTRIBUTE DOMAIN
;#
;# Add ATTRIBUTE to the attributes set for NAME

proc gob_relation_add_attribute {name attribute domain} {

    global gerwin_relation

    ;# See if attribute already exist into the relation
    if {[gob_relation_attribute_exist $name $attribute]} then {
	gui_notice "Attribute $attribute already exist in $name"
	return
    }

    ;# Ok, add the attribute
    set temp $gerwin_relation($name,attributes)
    lappend temp [list $attribute $domain]
    set gerwin_relation($name,attributes) $temp

}

;#
;# gob_relation_attribute_exist NAME ATTRIBUTE
;#
;# Return 1 if ATTRIBUTE is part of NAME's attributes set
;# Return 0 else

proc gob_relation_attribute_exist {name attribute} {

    global gerwin_relation

    set attributes $gerwin_relation($name,attributes)

    if {[lsearch -glob $attributes "$attribute *"] == -1} then {
	return 0
    } else {
	return 1
    }

}


;#
;# gob_relation_set_attribute_domain RELATION ATTRIBUTE DOMAIN
;#
;# Set DOMAIN as the new domain for ATTRIBUTE, in RELATION

proc gob_relation_set_attribute_domain {relation attribute domain} {

    global gerwin_relation

    set attributes $gerwin_relation($relation,attributes)

    ;# Replace the old entry with the newer one
    set index [lsearch -glob $attributes "$attribute *"]
    set attributes [lreplace $attributes $index $index [list $attribute $domain]]

    set gerwin_relation($relation,attributes) $attributes
}



;#
;# gob_relation_get_attributes RELATION
;#
;# Return a list with all attributes of RELATION

proc gob_relation_get_attributes {relation} {

    global gerwin_relation


    return $gerwin_relation($relation,attributes)


}


;#
;# gob_relation_get_attribute_domain RELATION ATTRIBUTE
;#
;# Return the domain of ATTRIBUTE, in RELATION

proc gob_relation_get_attribute_domain {relation attribute} {

    global gerwin_relation

    set attributes $gerwin_relation($relation,attributes)

    set index [lsearch -glob $attributes "$attribute *"]

    return [lindex [lindex $attributes $index] 1]

}


;#
;# gob_relation_get_num_attributes RELATION
;#
;# Return the number of attributes RELATION have

proc gob_relation_get_num_attributes {relation} {

    global gerwin_relation

    return [llength $gerwin_relation($relation,attributes)]


}

;#
;# gob_relation_delete_attribute RELATION ATTRIBUTE
;#
;# Delete ATTRIBUTE from RELATION's attributes set

proc gob_relation_delete_attribute {relation attribute} {

    global gerwin_relation

    set attributes $gerwin_relation($relation,attributes)

    set index [lsearch -glob $attributes "$attribute *"]
    set attributes [lreplace $attributes $index $index]

    set gerwin_relation($relation,attributes) $attributes

}


;#
;#  gob_relation_add_entity RELATION ENTITY MINCARD MAXCARD
;#
;# Add ENTITY with MINCARD/MAXCARD to RELATION's entities set

proc gob_relation_add_entity {relation entity mincard maxcard} {

    global gerwin_relation

    ;# See if that entity already exist in entities
#     if {[gob_relation_entity_exist $relation $entity]} then {
# 	gui_notice "$entity already exist in $relation"
# 	return
#     }

    ;# Ok, add the entity
    set entities $gerwin_relation($relation,entities)
    lappend entities [list $entity [list $mincard $maxcard]]
    set gerwin_relation($relation,entities) $entities

}


;#
;# gob_relation_entity_exist RELATION ENTITY
;#
;# Return 1 if ENTITY exist into RELATION
;# Return 0 else

proc gob_relation_entity_exist {relation entity} {

    global gerwin_relation

    set entities $gerwin_relation($relation,entities)

    if {[lsearch -glob $entities "$entity *"] == -1} then {
	return 0
    } else {
	return 1
    }

}


;#
;# gob_relation_set_entity_card RELATION ENTITY CARD
;#
;# Set CARD as the new card for ENTITY, in RELATION

proc gob_relation_set_entity_card {relation entity card} {

    global gerwin_relation

    set entities $gerwin_relation($relation,entities)

    set index [lsearch -glob $entities "$entity *"]
    set entities [lreplace $entities $index $index \
		      [list $entity $card]]

    set gerwin_relation($relation,entities) $entities

}


;#
;# gob_relation_get_entities RELATION
;#
;# Return a list with all entities in RELATION

proc gob_relation_get_entities {relation} {

    global gerwin_relation

    return $gerwin_relation($relation,entities)

}


;#
;# gob_relation_get_entity_card RELATION ENTITY
;#
;# Return the card of ENTITY, in RELATION

proc gob_relation_get_entity_card {relation entity} {

    global gerwin_relation

    set entities $gerwin_relation($relation,entities)

    set index [lsearch -glob $entities "$entity *"]

    return [lindex [lindex $entities $index] 1]

}


;#
;# gob_relation_get_entity_min_card RELATION ENTITY
;#
;# Return the minimal cardinality for ENTITY

proc gob_relation_get_entity_min_card {relation entity} {


    return [lindex [gob_relation_get_entity_card $relation $entity] 0]


}


;#
;# gob_relation_get_entity_max_card RELATION ENTITY
;#
;# Return the maximal cardinality for ENTITY

proc gob_relation_get_entity_max_card {relation entity} {

    return [lindex [gob_relation_get_entity_card $relation $entity] 1]

}



;#
;# gob_relation_set_entity_min_card RELATION ENTITY CARD
;#
;# Set CARD as the new minimal card for ENTITY

proc gob_relation_set_entity_min_card {relation entity mincard} {


    set card [gob_relation_get_entity_card $relation $entity]

    set card [lreplace $card 0 0 $mincard]

    gob_relation_set_entity_card $relation $entity $card

}


;#
;# gob_relation_set_entity_max_card RELATION ENTITY CARD
;#
;# Set CARD as the new maximal card for ENTITY

proc gob_relation_set_entity_max_card {relation entity maxcard} {


    set card [gob_relation_get_entity_card $relation $entity]
    set card [lreplace $card 1 1 $maxcard]

    gob_relation_set_entity_card $relation $entity $card

}



;# 
;# gob_relation_get_num_entities RELATION
;#
;# Return the number of entities from RELATION

proc gob_relation_get_num_entities {relation} {

    global gerwin_relation

    return [llength $gerwin_relation($relation,entities)]

}

;#
;# gob_relation_delete_entity RELATION ENTITY
;#
;# Delete ENTITY from RELATIONs entity set

proc gob_relation_delete_entity {relation entity} {

    global gerwin_relation

    set entities $gerwin_relation($relation,entities)

    set index [lsearch -glob $entities "$entity *"]
    set entities [lreplace $entities $index $index]

    set gerwin_relation($relation,entities) $entities

}


;#
;# gob_delete_relation RELATION
;#
;# Deletes RELATION from the system

proc gob_delete_relation {relation} {

    global gerwin_relations
    global gerwin_relation

    ;# Free gerwin_relations
    set index [lsearch $gerwin_relations $relation]
    set gerwin_relations [lreplace $gerwin_relations $index $index]

    ;# Free gerwin_relation
    unset gerwin_relation($relation,attributes)
    unset gerwin_relation($relation,entities)
    unset gerwin_relation($relation,reflexive)

}


;#
;# gob_relation_change_entity_name_reflexive RELATION NNAME
;#
;# NNAME is the new entity name for the reflexive relation RELATION...


proc gob_relation_change_entity_name_reflexive {relation nname} {

    global gerwin_relation
    
    set entities $gerwin_relation($relation,entities)
    
    set gerwin_relation($relation,entities) \
	[list [list $nname [lindex [lindex $entities 0] 1]] \
	     [list $nname [lindex [lindex $entities 1] 1]]]


}

;#
;# gob_relation_change_entity_name RELATION ENTITY NNAME
;#
;# ENTITY now is called NNAME...

proc gob_relation_change_entity_name {relation entity nname} {

    global gerwin_relation


    set entities $gerwin_relation($relation,entities)
    set index [lsearch -glob $entities "$entity *"]
    set card [lindex [lindex $entities $index] 1]
    set entities [lreplace $entities $index $index [list $nname $card]]

    set gerwin_relation($relation,entities) $entities
}

;#
;# gob_relation_change_name RELATION NNAME
;#
;# Changes the name of RELATION to NNAME

proc gob_relation_change_name {relation nname} {

    global gerwin_relation

    ;# Backup properties
    set attributes $gerwin_relation($relation,attributes)
    set entities $gerwin_relation($relation,entities)
    set reflexive $gerwin_relation($relation,reflexive)

    ;# Delete the relation
    gob_delete_relation $relation

    ;# Create the new relation
    gob_create_relation $nname $entities $attributes
    set gerwin_relation($nname,reflexive) $reflexive

    ;# Update the relation name on all entities affected by that relation
    foreach e [gob_relation_get_entities $nname] {

	gob_entity_change_relation_name [lindex $e 0] $relation $nname

    }

}

;####################################
;########## GOB TABLE  ###############
;####################################



;#
;# gob_create_table TABLE ATTRIBUTES PKEY FKEYS
;#
;# Creates a new table named TABLE into the system

proc gob_create_table {name attributes pkey fkeys} {

    global gerwin_tables
    global gerwin_table

    ;# See if already exist a table named NAME
    if {[gob_table_exist $name]} then {

	gui_notice "Table $name already exist. Something horrible happens."
	return
    }

    ;# Ok, create the new table
    lappend gerwin_tables $name

    set gerwin_table($name,attributes) $attributes
    set gerwin_table($name,key) $pkey
    set gerwin_table($name,fkeys) $fkeys
    set gerwin_table($name,rtables) {}

}


;#
;# gob_delete_table TABLE
;#
;# Deletes TABLE from the system

proc gob_delete_table {table} {

    global gerwin_tables
    global gerwin_table
    
    ;# Remove the table from gerwin_tables
    set index [lsearch $gerwin_tables $table]
    set gerwin_tables [lreplace $gerwin_tables $index $index]

    ;# Remove the table fields from gerwin_table
    unset gerwin_table($table,attributes)
    unset gerwin_table($table,key)
    unset gerwin_table($table,fkeys)

}


;#
;# gob_table_exist TABLE
;#
;# Return 1 if TABLE exist in the system
;# Return 0 else

proc gob_table_exist {table} {

    global gerwin_tables

    set result [lsearch $gerwin_tables $table]

    if {$result != -1} then {
	return 1
    } else {
	return 0
    }
}


;#
;# gob_table_attribute_exist TABLE ATTRIBUTE
;#
;# Return 1 if there is already an attribute named ATTRIBUTE on TABLE
;# Return 0 else

proc gob_table_attribute_exist {table attribute} {

    global gerwin_table

    set attributes $gerwin_table($table,attributes)
    set index [lsearch -glob $attributes "$attribute *"]

    if {$index == -1} then {
	return 0
    } else {
	return 1
    }

}

;#
;# gob_table_set_attribute_null TABLE ATTRIBUTE NULL
;#
;# Set the null property for ATTRIBUTE to NULL, on TABLE

proc gob_table_set_attribute_null {table attribute null} {

    global gerwin_table

    set attributes $gerwin_table($table,attributes)
    set index [lsearch -glob $attributes "$attribute *"]
    set sattribute [lindex $attributes $index]

    set domain [lindex $sattribute 1]
    set attributes [lreplace $attributes $index $index [list $attribute $domain $null]]

    set gerwin_table($table,attributes) $attributes

}



;#
;# gob_table_attribute_add TABLE ATTRIBUTE DOMAIN NULL
;# 
;# Add ATTRIBUTE to TABLE, with DOMAIN

proc gob_table_attribute_add {table attribute domain null} {

    global gerwin_table
    
    set attributes $gerwin_table($table,attributes)
    lappend attributes [list $attribute $domain $null]

    set gerwin_table($table,attributes) $attributes
}


;#
;# gob_table_get_attribute_null TABLE ATTRIBUTE
;#
;# Return 1 if ATTRIBUTE is anullable
;# Return 0 else

proc gob_table_get_attribute_null {table attribute} {

    global gerwin_table

    set attributes $gerwin_table($table,attributes)
    set index [lsearch -glob $attributes "$attribute *"]
    set a [lindex $attributes $index]

    return [lindex $a 2]



}

;#
;# gob_table_attribute_delete TABLE ATTRIBUTE
;#
;# Delete ATTRIBUTE from TABLE's attribute set

proc gob_table_attribute_delete {table attribute} {

    global gerwin_table

    set attributes $gerwin_table($table,attributes)

    set index [lsearch -glob $attributes "$attribute *"]
    set attributes [lreplace $attributes $index $index]

    set gerwin_table($table,attributes) $attributes
}



;#
;# gob_table_get_attributes TABLE
;#
;# Return a list with all attributes names from TABLE

proc gob_table_get_attributes {table} {

    global gerwin_table


    set result {}

    foreach a $gerwin_table($table,attributes) {

	lappend result [lindex $a 0]

    }

    return $result
}


;# 
;# gob_table_get_attribute_domain TABLE ATTRIBUTE
;#
;# Return the domain of ATTRIBUTE from TABLE

proc gob_table_get_attribute_domain {table attribute} {

    global gerwin_table


    set attributes $gerwin_table($table,attributes)

    set index [lsearch -glob $attributes "$attribute *"]

    return [lindex [lindex $attributes $index] 1]

}


;#
;# gob_table_set_attribute_domain TABLE ATTRIBUTE DOMAIN
;#
;# Set DOMAIN as the new ATTRIBUTE domain, in TABLE

proc gob_table_set_attribute_domain {table attribute domain} {

    global gerwin_table

    set attributes $gerwin_table($table,attributes)

    set index [lsearch -glob $attributes "$attribute *"]
    set attributes [lreplace $attributes $index $index [list $attribute $domain]]

    set gerwin_table($table,attributes) $attributes
}


;#
;# gob_table_get_nattributes TABLE
;#
;# Return the number of attributes that TABLE have

proc gob_table_get_nattributes {table} {

    global gerwin_table

    return [llength $gerwin_table($table,attributes)]

}


;#
;# gob_table_add_attribute_to_key TABLE ATTRIBUTE
;#
;# Add ATTRIBUTE to the primary key of TABLE

proc gob_table_add_attribute_to_key {table attribute} {

    global gerwin_table


    set key $gerwin_table($table,key)
    lappend key $attribute

    set gerwin_table($table,key) $key
}


;#
;# gob_table_remove_attribute_from_key TABLE ATTRIBUTE
;#
;# Removes ATTRIBUTE from TABLE's primary key

proc gob_table_remove_attribute_from_key {table attribute} {

    global gerwin_table

    
    set key $gerwin_table($table,key)
    set index [lsearch $key $attribute]
    set key [lreplace $key $index $index]
    

    set gerwin_table($table,key)
}


;#
;# gob_table_get_key TABLE
;#
;# Return the primary key of TABLE

proc gob_table_get_key {table} {
    
    global gerwin_table

    return $gerwin_table($table,key)

}

;#
;# gob_table_is_foreign_key TABLE ATTRIBUTE
;#
;# Return 1 if ATTRIBUTE is a foreign key on TABLE

proc gob_table_is_foreign_key {table attribute} {

    global gerwin_table

    set fkeys $gerwin_table($table,fkeys)
    set index [lsearch -glob $fkeys "$attribute *"]
    
    if {$index == -1} then {
	return 0
    } else {
	return 1
    }

}


;#
;# gob_table_get_foreign_key_rtable TABLE ATTRIBUTE
;#
;# Return the rtable of the foreign key ATTRIBUTE on TABLE

proc gob_table_get_foreign_key_rtable {table attribute} {

    global gerwin_table

    set fkeys $gerwin_table($table,fkeys)
    set index [lsearch -glob $fkeys "$attribute *"]

    set fkey [lindex $fkeys $index]

    return [lindex $fkey 1]

}



;#
;# gob_table_get_foreign_keys TABLE
;#
;# Return the foreign keys of TABLE

proc gob_table_get_foreign_keys {table} {

    global gerwin_table

    return $gerwin_table($table,fkeys)

}


;#
;# gob_table_add_attribute_to_foreign_keys TABLE ATTRIBUTE RTABLE
;#
;# Add ATTRIBUTE as a foreign key, referring RTABLE

proc gob_table_add_attribute_to_foreign_keys {table attribute rtable} {

    global gerwin_table

    set fkeys $gerwin_table($table,fkeys)
    lappend fkeys [list $attribute $rtable]
    
    set gerwin_table($table,fkeys) $fkeys

    ;# Now, update the rtables field to rtable
    lappend gerwin_table($rtable,rtables) $table
}


;#
;# gob_table_remove_attribute_from_foreign_keys TABLE ATTRIBUTE
;#
;# Remove ATTRIBUTE as a foreign key of TABLE

proc gob_table_remove_attribute_from_foreign_keys {table attribute} {

    global gerwin_table

    set fkeys $gerwin_table($table,fkeys)
    set index [lsearch -glob $fkeys "$attribute *"]

    ;# Update the rtables field in the attribute's rtable
    set rtable [lindex [lindex $fkeys $index] 1]
    set index2 [lsearch $gerwin_table($rtable,rtables) $table]
    set gerwin_table($rtable,rtables) \
	[lreplace $gerwin_table($rtable,rtables) $index $index]

    set fkeys [lreplace $fkeys $index $index]

    set gerwin_table($table,fkeys) $fkeys

}


;#
;# gob_table_set_foreign_key_rtable TABLE ATTRIBUTE RTABLE
;#
;# Set RTABLE as the ATTRIBUTE referred table in TABLE

proc gob_table_set_foreign_key_rtable {table attribute rtable} {

    global gerwin_table

    set fkeys $gerwin_table($table,fkeys)
    set index [lsearch -glob $fkeys "$attribute *"]
    set fkeys [lreplace $fkeys $index $index [list $attribute $rtable]]

    set gerwin_table($table,fkeys) $fkeys
}


;#
;# gob_table_is_key TABLE ATTRIBUTE
;#
;# Return 1 if ATTRIBUTE is part of TABLE primary key
;# Return 0 else

proc gob_table_is_key {table attribute} {

    global gerwin_table

    set key $gerwin_table($table,key)
    set index [lsearch $key $attribute]

    if {$index == -1} then {
	return 0
    } else {
	return 1
    }

}


;####################################
;########## GOB DOMAIN  #############
;####################################


;#
;# gob_create_domain DOMAIN
;#
;# Creates a new domain called DOMAIN on the project
;#
;# Initially, all maps are UNDEFINED

proc gob_create_domain {dom} {

    global gerwin_output_formats
    global gerwin_domains
    global gerwin_domain

    ;# Update gerwin_domains
    lappend gerwin_domains $dom

    ;# Update gerwin_domain
    foreach of $gerwin_output_formats {

	set gerwin_domain($dom,$of) UNDEFINED

    }
}


;#
;# gob_domain_create_predefined_domains 
;#
;# Creates into the system all the predefined domains rom gerwin_predefined_domains

proc gob_domain_create_predefined_domains {} {

    global gerwin_output_formats
    global gerwin_predefined_domains
    global gerwin_domains
    global gerwin_output_format_types


    foreach pd $gerwin_predefined_domains {

	;# Add the domain to gerwin_domains
	lappend gerwin_domains $pd

	;# Create the maps from the several output formats
	foreach of $gerwin_output_formats {

	    set format_types $gerwin_output_format_types($of)

	    ;# Locate the correct entry for $pd
	    set index [lsearch -glob $format_types ".* $pd"]
	    set ftype [lindex [lindex $format_types $index] 0]

	    ;# Add the map
	    gob_domain_map_type $pd $of $ftype

	}

    }

}



;#
;# gob_domain_map_type DOMAIN FORMAT TYPE
;#
;# Map TYPE as the new type for DOMAIN to FORMAT

proc gob_domain_map_type {domain format type} {

    global gerwin_domain


    set gerwin_domain($domain,$format) $type

}



;#
;# gob_domain_delete DOMAIN
;#
;# Deletes DOMAIN from the project, only if it is not a predefined domain

proc gob_domain_delete {dom} {

    global gerwin_domains
    global gerwin_domain
    global gerwin_output_formats

    ;# We cannot delete a predefined domain
    if {[gob_domain_is_predefined $dom]} then {

	return
    }

    ;# Free gerwin_domains
    set index [lsearch $gerwin_domains $dom]
    set gerwin_domains [lreplace $gerwin_domains $index $index]

    ;# Free gerwin_domain
    foreach of $gerwin_output_format {
	unset gerwin_domain($dom,$of)
    }

}