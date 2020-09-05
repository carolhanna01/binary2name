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

# Various prompter dialog boxes used by browser

set promptResult(ok) 0
set promptResult(fileName) ""
proc promptWriteFileFromUser { suggestion } {
    # Prompt the user for a file to write to
    global promptResult
    set promptResult(fileName) ${suggestion}
    set f [toplevel .prompt -borderwidth 10]
    label $f.msg -text "File name:"
    entry $f.entry -textvariable promptResult(fileName) -bd 2 -relief sunken
    set b [frame $f.buttons -bd 10]
    pack $f.msg $f.entry $f.buttons -side top -fill x
    wm minsize $f 100 100
    bind $f.entry <Return> {set promptResult(ok) 1}
    bind $f.entry <Control-C> {set promptResult(ok) 0}
    button $b.ok -text OK -command {set promptResult(ok) 1}
    button $b.cancel -text Cancel -command {set promptResult(ok) 0}
    pack $b.ok -side right
    pack $b.cancel -side left
    focus $f.entry
    grab $f
    tkwait variable promptResult(ok)
    grab release $f
    destroy $f
    if {$promptResult(ok)} {
	set fn $promptResult(fileName)
        if { [file exists ${fn}] } {
	    set dialog [tk_dialog .dialog "File exists!" \
		    "File ${fn} exists!"\
		    "" 0 "Overwrite?" "Cancel"]
	    if {$dialog != 0} {
		# i.e. cancel was hit
		return "0 junk"
	    }
	} 
    }
    return "$promptResult(ok) $promptResult(fileName)"
	
}

proc promptReadFileFromUser { suggestion } {
    # Prompt the user for a file to read
    global promptResult
    set promptResult(fileName) ${suggestion}
    set f [toplevel .prompt -borderwidth 10]
    label $f.msg -text "File name:"
    entry $f.entry -textvariable promptResult(fileName) -bd 2 -relief sunken
    set b [frame $f.buttons -bd 10]
    pack $f.msg $f.entry $f.buttons -side top -fill x
    wm minsize $f 100 100
    bind $f.entry <Return> {set promptResult(ok) 1}
    bind $f.entry <Control-C> {set promptResult(ok) 0}
    button $b.ok -text OK -command {set promptResult(ok) 1}
    button $b.cancel -text Cancel -command {set promptResult(ok) 0}
    pack $b.ok -side right
    pack $b.cancel -side left
    focus $f.entry
    grab $f
    tkwait variable promptResult(ok)
    grab release $f
    destroy $f
    if {$promptResult(ok)} {
	set fn $promptResult(fileName)
        if { [file exists ${fn}] } {
	    return "$promptResult(ok) $promptResult(fileName)"
	} else {
	    promptInformBox "Open file for read" \
		    "File ${fileName} does not exist"	
	    return "0 junk"
	}
    } else {
	promptInformBox "Open file for read" "No file specified"
	return "0 junk"
    }
	
}


# result and then quit the routine if no confirmation
# Return FALSE if the query is confirmed
proc promptGetNonConfirmation { title query } {
    set dialog [tk_dialog .dialog ${title} ${query} "" 0 "Confirm" "Cancel"]
    if {$dialog != 0} {
	return true
    } else {
	return false
    }
}

proc promptInformBox { title query } {
    set dialog [tk_dialog .dialog ${title} ${query} "" 0 "OK" ]
}

