;# globals.tcl -- Gerwin
;#
;# Global variables.
;#
;# Copyright (C) 2002-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:23:58 jemarch"

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

set gerwin_version 0.7

;###################
;# Current project data # 
;###################


set gerwin_cproject_name none ;# current project name
set gerwin_cproject_file none ;# current project .ger file
set gerwin_cproject_sqlfile none ;# current .sql file
set gerwin_cproject_author none ;# current project author

set gerwin_project_counter 0

;################
;# GOB Entity data #
;################

set gerwin_entities {} ;# List of the names of all entities from the current project

;# gerwin_entity(entity_name, attributes)   ;# Per-entity information
;# gerwin_entity(entity_name, key)

;#################
;# GOB Relation data #
;#################

set gerwin_relations {} ; ;# List of the names of all relations from the current project

;# gerwin_relation(relation_name,entities) -> {{e1 c1} {e2 c2} ... {eN cN}}
;# gerwin_relation(relation_name,attributes) -> {{a1 d1} {a2 d2} ... {aN dN}}

;################
;# GOB Label data  #
;###############

set gerwin_labels {} ;# List of the name of labels in the system

set gerwin_label_seq 0

;# gerwin_label(label_name,text)
;# gerwin_label(label_name,font)


;#################
;# GOB Domain data #
;#################

set gerwin_predefined_domains {}
set gerwin_domains {} ;# Active domains on the system

;# set gerwin_domain($domain,$output_format) -> type


;###############
;# GOB Table data #
;###############

set gerwin_tables {}

;# set gerwin_table(table,


;##################
;# WGOB Table data ##
;#################

set gm_tables {}

set gm_managed {} ;# Already painted relations



;# set gm_table(table,

;##################
;# WGOB Entity data #
;##################


set gm_entities {}  ;# List of the names of all entities printed in the canvas


;# gm_entity(entity_name, attributes)   ;# Per-entity information
;# gm_entity(entity_name, key)


set gm_relations {} ;# List of the names of all relations printed in the canvas

;# gm_relation(relation_name,attr)

set gerwin_entity_seq 1 ;# Sequence to make anonymous entities
set gerwin_relation_seq 1 ;# Sequence to make anonymous relations

set gerwin_changing_entity_name {} ;# Temporary
set gerwin_changing_attribute {}
set gerwin_changing_domain {}


set gerwin_obj_moving {}
set gerwin_obj_type_moving {}


;##########
;# GUI data #
;##########

;# gfonts() Gerwin fonts


set gmainframe {}            ;# Gerwin mainframe widget

set apparea {}               ;# Application area notebook
set apptree {}               ;# Application tree

set gcanvas {}    ;# Canvas for the ER diagram
set gcanvas_td {} ;# Canvas for the TD diagram
;# set gtext_output(format)  ;# Text widget for the several Output generated text

set gui_button_bbox_project {}
set gui_button_bbox_elements {}
set gui_button_bbox_toos {}
set gui_button_bbox_gen_TD {}
set gui_button_bbox_gen_output {}
set gui_button_bbox_gen_GerwinML {}
set gui_button_bbox_ssql {}
set gui_button_bbox_print_ER {}
set gui_button_bbox_print_TD {}

set earea_pages {"Project"}  ;# Edition area pages
set output_pages {} ;# Output pages

set output_pages_widget {} ;# Widget with the output pages

set gui_font {} ;# Global font

set fontselwidget {} ;# Widget for the font selection

set gerwin_editionarea {} ;# Widget with the notebook for the edition area

set gui_entity1 {} ;# First entity in a relation creation
set gui_entity2 {} ;# Second entity in a relation creation

set gui_concise_view 0 ;# Concise view mean not to show the attributes of the objects

set gen_current_relation {}

;#############
;# Gerwin data  #
;#############

set gerwin_output_formats {} ;# Supported Output formats
set gerwin_output_active_format {} ;# currently active Output format

;# set gerwin_output_format_types($format) -> {{t1 pd1} {t2 pd2} ... {tN pdN}}


set gerwin_state Editing ;# Actual status of the GUI
                         ;# Inserting entity
                         ;# Inserting relation
                         ;# Inserting label
                         ;# Moving (default)
                         ;# Editing
                         ;# Deleting