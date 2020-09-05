;# gerwinml.tcl -- Gerwin
;#
;# GerwinML tables generation routine
;#
;# Copyright (C) 2002-2008 Jose E. Marchesi
;#
;# Time-stamp: "08/11/16 19:20:07 jemarch"

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

;# Register the format
lappend gerwin_output_formats GerwinML

;# Set the color scheme for this output format
set output_colors(GerwinML) {

    {{<!--.*} red}
    {{.*-->} red}

    {{<[a-zA-Z_]+} blue}
    {{</[a-zA-Z_]+} blue}
    {{>} blue}
    {{/>} blue}
    {{=} blue}
    {{\".*?\"} orange}

}

;# Set the type scheme for this output format

set gerwin_output_format_types(GerwinML) {

    {{DYNAMIC_STRING(%d)}  {DYNAMIC_STRING(%d)}}
    {{FIXED_STRING(%d)} {FIXED_STRING(%d)}}
    {INTEGER INTEGER}
    {FLOAT FLOAT}
    {NATURAL INTEGER}
    {CHARACTER CHARACTER}
}



;# The generation routine

proc gen_TD_to_format_GerwinML {} {

    global gerwin_cproject_name
    global gerwin_cproject_author
    global gerwin_cproject_file
    global gerwin_output_active_format
    global gerwin_tables
    global gerwin_table

    ;# Ok, generate the GerwinML
    omit_line "<gerwinml>"
    omit_line ""
    omit_line "  <project name=\"$gerwin_cproject_name\" file=\"$gerwin_cproject_file\" author=\"$gerwin_cproject_author\">"
    omit_line ""
    omit_line "    <tables>"
    ;# Iterate on the tables
    foreach t $gerwin_tables {

	set table_name $t

	;# Some blank space before each table
	omit_line ""
	omit_line ""

	omit_line "      <table name=\"$t\">"
	omit_line ""

	;# Emit the table attributes
	omit_line "        <table_attributes>"      
	omit_line ""

	set nattributes [gob_table_get_nattributes $t]
	set i 0
	foreach a [gob_table_get_attributes $t] {
	    incr i

	    ;# See if the name contain any %
	    ;# If it is the case, then this is a repeated foreign key
	    ;# and the name in gob_table_get_foreign_keys is without the #!

	    if {[regexp \% $a]} then {

		;# Convert the # to an underscore
		set nname [string map {% _} $a]

	    } else {
		set nname $a
	    }

	    ;# Write the attribute name and the attribute domain
	    append line  "          <table_attribute name=\"$nname\" domain=\"[gob_table_get_attribute_domain $t $a]\"/>"

	    ;# Ok, omit the line
	    omit_line $line
	    set line {}
	}

	omit_line ""
	omit_line "        </table_attributes>"      


	;# Now, emit the table restrictions
	omit_line ""

	;# Begin of table restrictions
	omit_line ""
	omit_line "        <table_restrictions>"

	;# Begin of primary key
	omit_line ""
	omit_line "          <primary_key>"

	;# Begin of primary_key_attributes
	omit_line "            <primary_key_attributes>"

	;# Generate primary key attributes
	omit_line ""
	foreach a [gob_table_get_key $t] {
	    ;# Obtain the original name
	    ;#regsub {.+%([^%]+)} $a "\\1" a
	    set a [string map "% _" $a]
	    omit_line "              <primary_key_attribute name=\"$a\"/>"
	}

	omit_line ""
	omit_line "            </primary_key_attributes>"
	;# End of primary_key_attributes

	omit_line "          </primary_key>"
	;# End of primary key

	;# Begin of not_nulleable
	omit_line ""
	omit_line "          <not_nulleable>"

	;# Begin of not_nulleable_attributes
	omit_line "            <not_nulleable_attributes>"

	;# Generate not-nulleable attributes
	omit_line ""
	foreach a [gob_table_get_attributes $t] {
	    if {![gob_table_get_attribute_null $t $a]} then {
		;# Normalize the name
		;#regsub {.+%([^%]+)} $a "\\1" a
		set a [string map "% _" $a]
		
		omit_line "              <not_nulleable_attribute name=\"$a\"/>"
	    }
	}


	omit_line ""
	omit_line "            </not_nulleable_attributes>"
	;# End of not _nulleable_attributes

	omit_line ""
	omit_line "          </not_nulleable>"
	;# End of not_nulleable

	;# Begin of foreign_keys
	omit_line ""
	omit_line "          <foreign_keys>"
	omit_line ""

	;# Generate foreign_key elements
	set rtables {}
	foreach fkd [gob_table_get_foreign_keys $t] {

	    set rtable [lindex $fkd 1]

	    if {[lsearch $rtables $rtable] == -1} then {
		lappend rtables $rtable
	    }
	}
	;# Ok, now we have all the rtables into $rtables
	
	foreach rtable $rtables {

	    ;# Begin of foreign_key
	    omit_line "            <foreign_key reference_table=\"$rtable\">"
	    omit_line ""

	    ;# Begin of foreign_key_attributes
	    omit_line "              <foreign_key_attributes>"
	    omit_line ""

	    ;# Emit the foreign keys for this rtable
	    foreach a [gob_table_get_attributes $t] {

		;# Get the remote name for the attribute
		regsub {.+%([^%]+)} $a "\\1" nname
		set nname [string map "% _" $nname]


		if {[gob_table_is_foreign_key $t $a]} then {


		    ;# Get the foreign key for the attribute
		    set art [gob_table_get_foreign_key_rtable $t $a]

		    if {$art == $rtable} then {

			set a [string map "% _" $a]
			;# Begin foreign_key_attribute
			omit_line "                <foreign_key_attribute name=\"$a\" rattribute=\"$nname\"/>"

		    }
		}
	    }


	    omit_line ""
	    omit_line "              </foreign_key_attributes>"
	    ;# End of foreign_key_attributes

	    omit_line ""
	    omit_line "            </foreign_key>"
	    ;# End of foreign_key
	    omit_line ""

	}


	omit_line ""
	omit_line "          </foreign_keys>"
	;# End of foreign_keys

	omit_line ""
	omit_line "        </table_restrictions>"
	;# End of table restrictions

	omit_line ""
	omit_line "      </table>"
	omit_line ""
	
    }

    omit_line "    </tables>"
    omit_line ""
    omit_line "  </project>"
    omit_line ""
    omit_line "</gerwinml>"
}
	