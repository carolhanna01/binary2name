#
# TkGnats PR folders module
#

#
# -- globals
#
set Tkprfolder(site_print_dir)   "$TkGnats(SiteServerDir)/print"
set Tkprfolder(site_query_dir)   "$TkGnats(SiteServerDir)/query"
set Tkprfolder(site_sort_dir)    "$TkGnats(SiteServerDir)/sort"
set Tkprfolder(user_query_dir)   "$TkGnats(UserServerDir)/query"
set Tkprfolder(user_sort_dir)    "$TkGnats(UserServerDir)/sort"
set Tkprfolder(build_query_menu) build_query_menu
set Tkprfolder(build_sort_menu)  build_sort_menu
set Tkprfolder(foldernameregexp) {^[a-zA-Z0-9]+[a-zA-Z0-9_.]*}

proc tkprfolder_button_cmd {cmd top lbox type} {
    global Tkprfolder Query
    if {"$cmd" == "Close"} {
	destroy $top
	return
    }
    if {"$cmd" == "New"} {
	tkprfolder_new $top $type
	return
    }
    # get the selected item (if there is one)
    set x  [$lbox curselection]
    if {"$x" == ""} {
	Msg "No selection available in the folder list box!"
	return
    }
    set idx [lindex $x 0]
    set folder [$lbox get $idx]
    if {"$cmd" == "Rename"} {
        if {[catch {entryDialog "Enter new name for $folder" Cancel "" 0 $top} newname]} {
            return ""
        }
	regexp $Tkprfolder(foldernameregexp) $newname match
	if {$newname != $match} {
	    Msg "Folder names must be composed only of letters, numbers, underscores and periods."
	    return
	}
        if {[file exists $Tkprfolder(user_${type}_dir)/$newname]} {
            bell
            if {[tk_dialog .tkprfolder_delete "Confirm_Rename" "$newname already exists" \
                    "warning" -1 "Rename" "Cancel"] != 0} {
                return
            }
        }
	file rename -force \
		$Tkprfolder(user_${type}_dir)/$folder $Tkprfolder(user_${type}_dir)/$newname
	tkprfolder_resync $top $type $newname
        $Tkprfolder(build_${type}_menu)
	return
    }
    if {"$cmd" == "Delete"} {
#####	if {"$folder" == "Backup--Folder"} {
#####	    Msg "You are not allowed to delete $folder."
#####	    return
#####	}
#####	file rename $Tkprfolder(user_${type}_dir)/$folder $Tkprfolder(user_${type}_dir)/Backup--Folder
        bell
        if {[tk_dialog .tkprfolder_delete "Confirm_Delete" "Delete $folder?" "warning" -1 \
                "Delete" "Cancel"] == 0} {
            file delete $Tkprfolder(user_${type}_dir)/$folder
            tkprfolder_resync $top $type
            $Tkprfolder(build_${type}_menu)
        }
	return
    }
    if {"$cmd" == "Edit"} {
#####	if {"$folder" == "Backup--Folder"} {
#####	    Msg "You are not allowed to edit $folder."
#####	    return
#####	}
#####	file copy $Tkprfolder(user_${type}_dir)/$folder $Tkprfolder(user_${type}_dir)/Backup--Folder
	tkprfolder_edit $folder $top $type
	return
    }
    Msg "tkprfolder_button_cmd:\n" "do not understand the '$cmd' operation"
}

proc tkprfolder_edit_Cancel {top txt fname flisttop type} {
    destroy $top
}
proc tkprfolder_edit_Save {top txt fname flisttop type} {
    global Tkprfolder
    file_put_text $Tkprfolder(user_${type}_dir)/$fname [$txt get 1.0 end] 
    destroy $top
    tkprfolder_resync $flisttop $type $fname
    $Tkprfolder(build_${type}_menu)
}

proc tkprfolder_resync {w type {activate ""}} {
    global Tkprfolder 
    #####    if {[winfo exists $flisttop]} {
    #####	tkprfolder_dialog $flisttop
    #####    }
    # first get the list of folders for this person
    set folder_list {}
    set ltemp ""
    catch {[set ltemp [lsort [glob $Tkprfolder(user_${type}_dir)/*]]]}
    foreach file $ltemp {
        lappend folder_list [file tail $file]
    }
    # decide which element from the new list to activate
    if {$activate == ""} {
        set active [$w.list index active]
    } {
        set active [lsearch -exact $folder_list $activate]
    }
    $w.list delete 0 end
    eval $w.list insert end $folder_list
    $w.list activate $active
    $w.list selection set active
}

proc tkprfolder_edit {fname flisttop type} {
    global TkGnats Tkprfolder 
    set f .tkprfolder_edit_file
    if {[winfo exists $f]} {
	Msg "You can only edit one folder at a time."
	return
    }
    toplevel $f
    frame $f.buttons
    foreach x {Save Cancel} {
	button $f.buttons._$x -text $x \
	    -command "tkprfolder_edit_$x $f $f.text $fname $flisttop $type"
	pack $f.buttons._$x -side left -padx 4
    }
    scrollbar $f.sb -command "$f.text yview" -relief sunken
    text $f.text \
	-font $TkGnats(textfont) \
	-yscrollcommand "$f.sb set" \
	-height 20 -width 90 -relief sunken -padx 4 -insertwidth 1 \
	-insertofftime 400 -borderwidth 2 -background $TkGnats(EditFieldBackground)
    set_focus_style $f.text
    bind $f.text <Control-g> {
	set s [prid_from_selection]
	if {"$s" != ""} {
	    %W insert 1.0 "$s\n"
	}
    }
    bind $f.text <3> "clipboard_post $f.text %X %Y"

    pack $f.buttons -side bottom
    pack $f.sb      -side left  -fill y
    pack $f.text    -side right -fill both -expand true
    if {[file exists $Tkprfolder(user_${type}_dir)/$fname]} {
	$f.text insert 1.0 [file_get_text $Tkprfolder(user_${type}_dir)/$fname]
    }
    wm title $f "Tkprfolder: $fname"
}


proc tkprfolder_new {flisttop type} {
    global Tkprfolder
    if {[catch {entryDialog "Enter name of folder file" Cancel "" 0 $flisttop} fname]} {
        return ""
    }
    regexp $Tkprfolder(foldernameregexp) $fname match
    if {$fname != $match} {
	Msg "Folder names must be composed only of letters, numbers, underscores and periods."
	return
    }
    if {[file exists $Tkprfolder(user_${type}_dir)/$fname]} {
        bell
        if {[tk_dialog .tkprfolder_delete "Confirm_New" "$fname already exists" \
                "warning" -1 "Edit" "Cancel"] != 0} {
            return
        }
    }
    tkprfolder_edit $fname $flisttop $type
}

proc XXXXXtkprfolder_cmd {cmd w y} {
    set idx [$w nearest $y]
    set fname [$w get $idx]
    query_cmd [split [file_get_text $fname] " \n\t"]
}

proc tkprfolder_dialog {w type title} {
    global Tkprfolder TkGnats env
    if {[winfo exists $w]} {
	$w.list delete 0 end
    } {
	toplevel $w
        wm title      $w "TkGnats - $title"
        wm iconbitmap $w  @$TkGnats(lib)/tkgnats.xbm
        wm iconname   $w "$TkGnats(LogName)'s tkquerypr $title"
        
	message   $w.msg -anchor center -text $title -aspect 10000
	scrollbar $w.sb -borderwidth 2 -relief sunken -command "$w.list yview"
	listbox   $w.list -yscroll "$w.sb set" -setgrid 1 -relief sunken -borderwidth 2 \
                -width 24 -height 8 -exportselection false
	frame     $w.buttons
	foreach x {Close Edit Delete Rename New} {
	    button $w.buttons._$x -text $x -width 6 \
		-command "tkprfolder_button_cmd $x $w $w.list $type"
	    pack $w.buttons._$x -side top -padx 5 -pady 5
	}
	pack $w.msg     -side top   -fill x
	pack $w.buttons -side right
	pack $w.sb      -side left  -fill y
	pack $w.list    -side right -fill both -expand true
    }
    tkprfolder_resync $w $type
}
