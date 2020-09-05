#
# -- Sort Callbacks
#
proc clear_cmd {} {
    global sortDialog
    make_lists .sort.fields $sortDialog(fieldnames) {}
}

proc defaults_cmd {} {
    global sortDialog
    make_lists .sort.fields $sortDialog(fieldnames) $sortDialog(sortdefault)
}

proc reset_cmd {} {
    global sortDialog
    make_lists .sort.fields $sortDialog(fieldnames) $sortDialog(sortfields)
}

proc cancel_cmd {} {
    global sortDialog
    set sortDialog(done) 1
}

proc apply_cmd {lbname} {
    global sortDialog Query
    if {[$lbname size] == 0} {
        Msg "Please select some sort fields."
        return
    }
    busy_cursor set
    #set sortDialog(sortfields) [$lbname get 0 end]
    set sortDialog(sortfields) ""
    foreach fname [$lbname get 0 end] {
        lappend sortDialog(sortfields) [get_field_alias_reverse $fname]
    }
    set Query(user_sort_flds) $sortDialog(sortfields)
    perform_sort_cmd
    busy_cursor clear
}

#
# -- Sort Procs
#

proc fld_cmd {srcw y destw} {
    set idx [$srcw nearest $y]
    set ln [$srcw get $idx]
    if {"$ln" != ""} {
	$srcw delete $idx
	$destw insert end $ln
    }
}
    
proc make_lists {parent choices selected} {
    global sortDialog
    
    if {![winfo exists $parent.l]} {
        frame $parent.l
        frame $parent.r
        pack  $parent.l -side left  -padx 10 -pady 6
        pack  $parent.r -side right -padx 10 -pady 6
        
        set lbl(l) "Field Choices"
        set lbl(r) "Sorted as"
        foreach side {l r} {
            set p $parent.$side
            message   $p.msg -anchor center -text $lbl($side): -aspect 10000
            scrollbar $p.sb  -command "$p.list yview" -borderwidth 2 -relief sunken
            listbox   $p.list -yscroll "$p.sb set" -setgrid 1 \
                    -relief sunken -borderwidth 2 \
                    -width  [expr 2 + [get_max_strlen $sortDialog(fieldnames)]] \
                    -height [llength $choices]
            pack $p.msg  -side top   -fill x
            pack $p.list -side right -fill both -expand true
#####       pack $p.sb   -side left  -fill y
        }
        bind $parent.l.list <B1-ButtonRelease> "fld_cmd %W %y $parent.r.list"
        bind $parent.r.list <B1-ButtonRelease> "fld_cmd %W %y $parent.l.list
        sort_listbox $parent.l.list"
    } {
        clear_lists .sort.fields
    }

    foreach fname $selected {
        #$parent.r.list insert end $fname
        $parent.r.list insert end [get_field_alias $fname]
    }
    foreach fname $choices {
        if {[lsearch $selected $fname] < 0} {
            #$parent.l.list insert end $fname
            $parent.l.list insert end [get_field_alias $fname]
        }
    }
    sort_listbox $parent.l.list
}

proc clear_lists {parent} {
    $parent.l.list delete 0 end
    $parent.r.list delete 0 end
}

#
# -- Sort Widgets
#

proc sort_Dialog {names flgs fields default file} {
    global sortDialog TkGnats env

    set sortDialog(fieldnames)  $names
    set sortDialog(fieldflgs)   $flgs
    set sortDialog(sortfields)  $fields
    set sortDialog(sortdefault) $default
    set sortDialog(sortfile)    $file
    set sortDialog(msg_strings) {}
    set sortDialog(done)        0
    
    ### -- root frame
    catch {destroy  .sort}
    set w [toplevel .sort]
    
    ### -- msg area
    message $w.msg -aspect 500 -justify center \
            -text "Click on an item to move it\nfrom one box to another"
    pack    $w.msg -side top -padx 4 -pady 4
    
    ### -- fields
    frame $w.fields

    reset_cmd
    
    pack $w.fields -side left -padx 2 -pady 4
    
    ### -- buttons
    frame   $w.buttons
    button  $w.buttons.clear    -text Clear     -width 8 -command clear_cmd
    button  $w.buttons.reset    -text Reset     -width 8 -command reset_cmd
    button  $w.buttons.defaults -text Defaults  -width 8 -command defaults_cmd
    button  $w.buttons.cancel   -text Close     -width 8 -command cancel_cmd
    button  $w.buttons.apply    -text Apply     -width 8 -command "apply_cmd $w.fields.r.list"
    button  $w.buttons.save     -text "Save..." -width 8 -command folder_save_sort_cmd
    
    message $w.buttons.filler  -text " "       -width 8 -anchor center -pady 0
    
    pack    $w.buttons.filler $w.buttons.cancel   $w.buttons.apply $w.buttons.clear \
            $w.buttons.reset  $w.buttons.defaults $w.buttons.save -side top -padx 10 -pady 5
    pack    $w.buttons

    wm title      $w "TkGnats - New Query Sort"
    wm iconbitmap $w  @$TkGnats(lib)/tkgnats.xbm
    wm iconname   $w "$TkGnats(LogName)'s tkquerypr sort"
    
    tkwait variable sortDialog(done)
    destroy $w
#####    return $sortDialog(sortfields)
}
