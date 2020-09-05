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

# This file contains various functions that are not directly window
# related.  Mainly for dumping state, configuration and graphs.
# and for determining what's abstract, concrete and module

proc dumpState { } {
    # Dumps information about classes that may be used
    # by other code to process the sather code.
    # Iterates overa all classes and dumps the "classInfo" that
    # is provided by the browser
    # Resulting file is of the form:
    #  set gModToClasses(./../Misc/Misc.module) { I_INTERVAL MATRIX }
    #  set gClassDef(I_INTERVAL)   {  ./../Misc/i_interval.sa 5 { 
    #	   { size:INT ./../Misc/i_interval.sa 15 nnnrnnn } 
    #	   { union(I_INTERVAL):I_INTERVAL ./../Misc/i_interval.sa 88 nnnnnnn } 
    #  etc.
    #    } 
    # } 
    #   set gAllClasses { { nodename filename linenumber modulename } 
    #        etc. }

    global gModToClasses

    set res ""
    set gAllClasses [tkkit_cb allClasses]
    set dialog [tk_dialog .dialog "Dumping documentation" \
		    "Writing docs to gen_bs_info_raw_dump.tcl"\
		    "" 0 "Confirm" "Cancel"]
    if {$dialog != 0} {
	return
    }
    set sorted [lsort -increasing -ascii $gAllClasses]
    # First dump an outer file of class names
    set mainfl [open "gen_bs_info_raw_dump.tcl" w]
    puts ${mainfl} "${res} set gAllClasses ${gAllClasses} \n"

    set unsortedModules [array names gModToClasses]
    set sortedModules [lsort -increasing -ascii $unsortedModules ]
    foreach module $sortedModules {
	set res  "${res} set gModToClasses($module) { $gModToClasses($module) }\n"
	set classes [lsort -increasing -ascii $gModToClasses($module)]
	foreach cl ${classes} {
	    puts "Processing ${cl}..."
	    inform "Processing ${cl}..."
	    update
	    set classNm ${cl}
	    set cdef [tkkit_cb getClassInfo ${classNm}]
	    set res "${res} set gClassDef($classNm) { $cdef }\n"
	}
	# End of a module
	regsub -all {[\$]} $res "\\\$" newRes
	puts $mainfl "$newRes"
    }
    inform "Done with dumping classes ..."
    update
    close $mainfl
}
