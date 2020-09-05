proc tkquerypr_usage {{str ""}} {
    bell
    wm withdraw .
    tk_dialog .tkgnatsError "TkGnats Error" \
            "${str}usage: tkquerypr -server 'ServerInfo'" "error" 0 "OK"
    exit
}

proc tkquerypr_process_args {} {
    global TkGnats argc argv env

    set TkGnats(CurrentProgram) tkquerypr
    
    if {$argc != 0} {
	if {$argc%2 != 0} {
	    tkquerypr_usage
	}
	for {set x 0} {$x<$argc} {incr x 2} {
	    set opt [lindex $argv $x]
	    set val [lindex $argv [expr $x+1]]
	    switch -exact -- $opt -server {
		set TkGnats(ServerInfo) $val
            } default {
		tkquerypr_usage "Illegal option pair:\n'$opt $val'\n\n"
	    }
	}
    }

    if {![info exists TkGnats(ServerInfo)]} {
        tkquerypr_usage "No -server argument given.\n\n"
    }
    
    foreach var {TKGNATSLIB TKGNATSINI} {
        if {[info exists env($var)]} {
            set TkGnats($var) $env($var)
        }
    }
    
    set TkGnats(lib) $TkGnats(TKGNATSLIB)

    if {[info exists TkGnats(TKGNATSINI)]} {
        if {[file readable $TkGnats(TKGNATSINI)]} {
            source $TkGnats(TKGNATSINI)
        } {
            bell
            wm withdraw .
            tk_dialog .tkgnatsError "TkGnats Error" \
                    "TkGnats INI file '$TkGnats(TKGNATSINI)' not readable" "error" 0 "OK"
            exit
        }
    }

    if {[file exists $TkGnats(lib)/tkgnatsini]} {
        source       $TkGnats(lib)/tkgnatsini
    }
}

tkquerypr_process_args
   
foreach f { tkpr_library.tcl tkprprint.tcl tkprfolder.tcl tkprsort.tcl tkprhelp.tcl tkprfont.tcl tkprdatesel.tcl } {
    source $TkGnats(lib)/$f
}

if {[get_gnats_config] == "-1"} {
    exit
}

set Query(query_mode) sql2
#TTD remove me after testing
#set Query(query_mode) sql2

set Query(sort_flds) {
    Number	  Category	Synopsis	Confidential
    Severity	  Priority	Responsible	State   	Class
    Submitter-Id  Arrival-Date	Originator	Release
    Last-Modified Closed-Date
}

set Query(sort_flgs) {
    "n"		""		""  		""
    "n"		"n"		""  		"n"	"n" 
    ""		""		""		""
    ""          ""
}

set Query(default_sort_flds) {Category State Priority Severity Number}
set Query(user_sort_flds)    {}

set Query(default_sort_file) "$TkGnats(UserServerDir)/default-sort"
set Query(default_view_file) "$TkGnats(UserServerDir)/default-view"

# Query listbox headings
set list_flds_list [list  \
        Number        Id       \
        Submitter-Id  Sub-Id   \
        Originator    Originator  \
        Responsible   Responsible \
        Category      Category  \
        Class         Class     \
        Confidential  Conf      \
        State         State     \
        Priority      Priority  \
        Severity      Severity  \
        Release       Release   \
        Arrival-Date  Arr-Date  \
        Last-Modified Last-Mod  \
        Closed-Date   Clos-Date \
        Synopsis      Synopsis  \
        ]

array set list_flds_headings $list_flds_list

#TTD: for custom states and classes, we need to dynamically size the field-width
array set list_flds_formats  [list \
        Number        "%6d"    \
        Submitter-Id  " %-8s"  \
        Originator    " %-10s" \
        Responsible   " %-11s" \
        Category      " %-16s" \
        Class         " %-5s"  \
        Confidential  " %-4s"  \
        State         " %-9s"  \
        Priority      " %-8s"  \
        Severity      " %-12s" \
        Release       " %-28s" \
        Arrival-Date  " %-10s" \
        Last-Modified " %-10s" \
        Closed-Date   " %-10s" \
        Synopsis      " %s"    \
]

array set list_flds_defaults  [list \
        Number        " Number"     \
        Submitter-Id  ""  \
        Originator    ""  \
        Responsible   "Responsible" \
        Category      "Category"    \
        Class         "Class"       \
        Confidential  ""  \
        State         "State"       \
        Priority      "Priority"    \
        Severity      "Severity"    \
        Release       ""  \
        Arrival-Date  ""  \
        Last-Modified ""  \
        Closed-Date   ""  \
        Synopsis      "Synopsis"    \
]

if {$TkGnats(ReleaseBased)} {
    set Query(sort_flds) [concat $Query(sort_flds) Keywords $TkGnats(Quarter) Date-Required]
    set Query(sort_flgs) [concat $Query(sort_flgs) "" "" ""]
    set list_flds_list   [concat [lrange $list_flds_list 0 [expr [llength $list_flds_list] - 3]] \
            Keywords Keywords \
            $TkGnats(Quarter) $TkGnats(Quarter) \
            Date-Required     Date-Req \
            Synopsis          Synopsis \
            ]
    array set list_flds_headings $list_flds_list
    array set list_flds_formats [list  \
            Keywords          " %-9s"  \
            $TkGnats(Quarter) " %-9s"  \
            Date-Required     " %-10s" \
            ]
    array set list_flds_defaults  [list \
            Keywords          ""  \
            $TkGnats(Quarter) ""  \
            Date-Required     ""  \
            ]
}

set Query(orig_wid) [expr [string trim $list_flds_formats(Originator) " %-s"] - 1]
if {[info exists TkGnats(QueryOriginatorFormat)] && \
        $TkGnats(QueryOriginatorFormat) == "short"} {
    set Query(orig_short) 1
} {
    set Query(orig_short) 0
}
    
set Query(done_msg) ""

#
# numeric --> textual mappings for some query-pr --sql2 fields
#
####set Mappings(State)    {open analyzed suspended feedback closed}
set Mappings(State)    $TkGnats(StatesList)
set Mappings(Priority) {high medium low}
set Mappings(Severity) {critical serious non-critical}
####set Mappings(Class)    {sw-bug doc-bug support change-request mistaken duplicate}
set Mappings(Class)    $TkGnats(ClassesList)

# List of entry widgets for traverse key binding
set Query(tlist) {}

#
# ---- Procedures
#

proc disable_listbox_menus {} {
    global Query
    .menu.print   configure -state disabled
    .menu.sel     configure -state disabled
    # current selection for maintaining selection across queries, when possible
}
  
proc enable_listbox_menus {} {
    .menu.print   configure -state normal
    .menu.sel     configure -state normal
}
   
proc headingMsg {s} {
    .menu.msg configure -text $s
    update
}

proc get_default_sort_criteria {} {
    global Query
    set Query(user_sort_flds) $Query(default_sort_flds)
    catch {source $Query(default_sort_file)}
}

proc get_default_view_fields {} {
    global Query list_flds_selected list_flds_defaults list_flds_list
    # set default view fields
    for {set i 0} {$i < [llength $list_flds_list]} {incr i 2} {
        set x [lindex $list_flds_list $i]
        set list_flds_selected($x) $list_flds_defaults($x)
    }
    if {$Query(default_view_file) != ""} {
	if {[file exists $Query(default_view_file)]} {
            source $Query(default_view_file)
	}
    }
}

proc prids_from_selection {} {
    set s ""
    catch {set s [selection get STRING]}
    set lines [split $s]
    set s {}
    foreach line $lines {
	set num [lindex [string trim $line "\t\n !@#\$%^&*()_-+=|\\{}\[\]:;'~`<>,.?\""] 0]
	if {[regexp {(.*/|)[0-9]+$} $num]} {
	    lappend s $num
	}
    }
    return $s
}

proc query_from_selection {} {
    global TkGnats Query
    if {[TkGnats_UpdateCheck]} {
	return
    }
    set prid_list [prids_from_selection]
    if {"$prid_list" == ""} {
        bell
	Msg "No PR id available in selection!"
	return;
    }
    clear_query_cmd
    set Query(default__query) [list proc default__query {flds} {return 1}]
    
    if {$TkGnats(GNATS_ACCESS_METHOD) == "batch"} {
        set Query(query_pr_opts) "$prid_list"
    } {
        set Query(query_pr_opts) ""
	set sep ""
	foreach prid $prid_list {
	    append Query(query_pr_opts) [concat $sep "Number==$prid_list"]
	    set sep "|"
	}
	append Query(query_pr_opts) " "
    }
    set Query(by_id_only) 1
    set Query(done_msg) "query from selection for Id $prid_list"
    if {[catch {perform_query_cmd prid $prid_list} errs]} {
	Msg "Error querying with selection\n<<<$prid_list>>>\n$errs"
    }
}

proc category_listbox {parent} {
    global TkGnats Query Category Category_regexp

    if {![winfo exists $parent.tit]} {
        set wid [expr 2 + [get_max_strlen $TkGnats(CategoryList)]]

        set alias [get_field_alias Category]
        button  $parent.tit -anchor center -text "${alias}:" \
                -command "helpMsg $alias" -relief flat -padx 0 -pady 0 -borderwidth 0
        pack    $parent.tit -side top -fill x
        
        frame   $parent.cat
        pack    $parent.cat -side top -fill x
        
        frame   $parent.cat.l
        message $parent.cat.l.msg -anchor center -relief sunken -borderwidth 2 \
                -text "Available" -aspect 10000 -padx 0
        pack    $parent.cat.l -side left  -padx 0
        
        frame   $parent.cat.r
        message $parent.cat.r.msg -anchor center -relief sunken \
                -text "Selected" -aspect 10000 -padx 0
        pack    $parent.cat.r -side right -padx 0
        
        foreach side {l r} {
            set p $parent.cat.$side
            scrollbar $p.sb -command "$p.list yview" -borderwidth 2 \
                    -relief sunken
            listbox $p.list -yscroll "$p.sb set" -setgrid 1 \
                    -relief sunken -borderwidth 2 -width $wid -height 6
            pack $p.msg  -side top   -fill x
            pack $p.sb   -side left  -fill y
            pack $p.list -side right -fill both -expand true
        }
        
        bind $parent.cat.l.list <B1-ButtonRelease> \
                "category_add_cmd %W %y $parent.cat.r.list"
        
        bind $parent.cat.r.list <B1-ButtonRelease> \
                "category_delete_cmd %W %y $parent.cat.l.list"
        
        frame $parent.reg
        pack  $parent.reg -side top -fill x
        
        lappend Query(tlist) [singletext $parent.reg "RegExp" 10 "" 7]
        set Category_regexp $parent.reg
    } {
        # Just clear out the existing widgets
        $parent.cat.l.list delete 0 end 
        $parent.cat.r.list delete 0 end 
        unset Category
    }
    
    # Just a place holder so that Category is defined as an array
    set Category(All) ""
    
    eval $parent.cat.l.list insert end $TkGnats(CategoryList)
}

proc submitter-id_listbox {parent} {
    global TkGnats Query Submitter-Id Submitter-Id_regexp
    
    if {![winfo exists $parent.tit]} {
        set wid [expr 2 + [get_max_strlen $TkGnats(SubmitterList)]]
        
        set alias [get_field_alias Submitter-Id]
        button  $parent.tit -anchor center -text "${alias}:" \
                -command "helpMsg $alias" -relief flat -padx 0 -pady 0 -borderwidth 0
        pack    $parent.tit -side top -fill x
        
        frame   $parent.sub 
        pack    $parent.sub -side top -fill x
        
        frame   $parent.sub.l
        message $parent.sub.l.msg -anchor center -relief sunken \
                -text "Available" -aspect 10000
        pack    $parent.sub.l -side left
        
        frame   $parent.sub.r
        message $parent.sub.r.msg -anchor center  -relief sunken \
                -text "Selected" -aspect 10000
        pack    $parent.sub.r -side right
        
        foreach side {l r} {
            set p $parent.sub.$side
            scrollbar $p.sb -command "$p.list yview" -borderwidth 2 \
                    -relief sunken
            listbox $p.list -yscroll "$p.sb set" -setgrid 1 \
                    -relief sunken -borderwidth 2 -width $wid -height 6
            pack $p.msg  -side top   -fill x
            pack $p.sb   -side left  -fill y
            pack $p.list -side right -fill both -expand true
        }
        
        bind $parent.sub.l.list <B1-ButtonRelease> \
                "submitter-id_add_cmd %W %y $parent.sub.r.list"
        
        bind $parent.sub.r.list <B1-ButtonRelease> \
                "submitter-id_delete_cmd %W %y $parent.sub.l.list"
        
        frame $parent.reg
        pack  $parent.reg -side top -fill x
        
        lappend Query(tlist) [singletext $parent.reg "RegExp" 10 "" 7]
        set Submitter-Id_regexp $parent.reg
    } {
        # Just clear out the existing widgets
        $parent.sub.l.list delete 0 end 
        $parent.sub.r.list delete 0 end 
        unset Submitter-Id
    }
    
    # Just a place holder so that Submitter-Id is defined as an array
    set Submitter-Id(All) ""
    
    eval $parent.sub.l.list insert end $TkGnats(SubmitterList)
}

proc responsible_listbox {parent} {
    global TkGnats Query Responsible Responsible_regexp

    if {![winfo exists $parent.tit]} {
        set wid [expr 2 + [get_max_strlen $TkGnats(ResponsibleList)]]
        
        set alias [get_field_alias Responsible]
        button  $parent.tit -anchor center -text "${alias}:" \
                -command "helpMsg $alias" -relief flat -padx 0 -pady 0 -borderwidth 0
        pack    $parent.tit -side top -fill x
        
        frame   $parent.res 
        pack    $parent.res -side top -fill x
        
        frame   $parent.res.l
        message $parent.res.l.msg -anchor center -relief sunken \
                -text "Available" -aspect 10000
        pack    $parent.res.l -side left
        
        frame   $parent.res.r
        message $parent.res.r.msg -anchor center  -relief sunken \
                -text "Selected" -aspect 10000
        pack    $parent.res.r -side right
        
        foreach side {l r} {
            set p $parent.res.$side
            scrollbar $p.sb -command "$p.list yview" -borderwidth 2 \
                    -relief sunken
            listbox $p.list -yscroll "$p.sb set" -setgrid 1 \
                    -relief sunken -borderwidth 2 -width $wid -height 6
            pack $p.msg  -side top   -fill x
            pack $p.sb   -side left  -fill y
            pack $p.list -side right -fill both -expand true
        }
        
        bind $parent.res.l.list <B1-ButtonRelease> \
                "responsible_add_cmd %W %y $parent.res.r.list"
        
        bind $parent.res.r.list <B1-ButtonRelease> \
                "responsible_delete_cmd %W %y $parent.res.l.list"
        
        frame $parent.reg
        pack  $parent.reg -side top -fill x
        
        lappend Query(tlist) [singletext $parent.reg "RegExp" 10 "" 7]
        set Responsible_regexp $parent.reg
    } {
        # Just clear out the existing widgets
        $parent.res.l.list delete 0 end 
        $parent.res.r.list delete 0 end 
        unset Responsible
    }
    
    # Just a place holder so that Responsible is defined as an array
    set Responsible(All) ""
    
    eval $parent.res.l.list insert end $TkGnats(ResponsibleList)
}
    
proc list_item_switch_cmd {srcw y destw} {
    set idx [$srcw nearest $y]
    set ln [$srcw get $idx]
    if {"$ln" != ""} {
	$srcw delete $idx
	$destw insert end $ln
    }
    return $ln
}

proc category_add_cmd {srcw y destw} {
    global Category
    set val [list_item_switch_cmd $srcw $y $destw]
    set Category($val) $val
}

proc category_delete_cmd {srcw y destw} {
    global Category
    set val [list_item_switch_cmd $srcw $y $destw]
    if {"$val" != ""} {
	unset Category($val)
    }
    sort_listbox $destw
}

proc submitter-id_add_cmd {srcw y destw} {
    global Submitter-Id
    set val [list_item_switch_cmd $srcw $y $destw]
    set Submitter-Id($val) $val
}

proc submitter-id_delete_cmd {srcw y destw} {
    global Submitter-Id
    set val [list_item_switch_cmd $srcw $y $destw]
    if {"$val" != ""} {
	unset Submitter-Id($val)
    }
    sort_listbox $destw
}

proc responsible_add_cmd {srcw y destw} {
    global Responsible
    set val [list_item_switch_cmd $srcw $y $destw]
    set Responsible($val) $val
}

proc responsible_delete_cmd {srcw y destw} {
    global Responsible
    set val [list_item_switch_cmd $srcw $y $destw]
    if {"$val" != ""} {
	unset Responsible($val)
    }
    sort_listbox $destw
}

proc set_query_view_heading {} {
    global  list_flds_selected list_flds_formats list_flds_format list_flds_list \
            list_flds_headings list_flds_heading list_flds_widths
    set list_flds_heading "    Id"
    set list_flds_widths  [string length $list_flds_heading]
    for {set i 2} {$i < [llength $list_flds_list]} {incr i 2} {
        set x [lindex $list_flds_list $i]
        if {$list_flds_selected($x) != ""} {
            #append  list_flds_heading [format $list_flds_formats($x) $list_flds_headings($x)]
            append  list_flds_heading [format $list_flds_formats($x) [get_field_alias $list_flds_headings($x)]]
            lappend list_flds_widths  [string trim $list_flds_formats($x) "%- s"]
        }
    }
}

proc save_query_view_fields {sfout} {
    global list_flds_selected
    puts  $sfout "array set list_flds_selected \{[array get list_flds_selected]\}"
    puts  $sfout "set_query_view_fields"
}

proc update_query_view_fields {} {
    global Query list_flds_selected list_flds_heading
    set_query_view_fields
    query_fill_listbox
}

proc set_query_view_fields {} {
    global Query list_flds_selected list_flds_heading
    set_query_view_heading
    set Query(lbheading) $list_flds_heading
}

proc query_listbox {p} {
    global TkGnats Query Category list_flds_heading tcl_platform
    set lboxwidth  100
    set Query(maxlen) $lboxwidth
    #set Query(lbheading) $list_flds_heading
    frame $p.query 
    pack  $p.query -side top -fill both -expand true
    set_query_view_heading
    button  $p.temp
    set bg [$p.temp cget -background]
    destroy $p.temp
    frame   $p.query.top
    entry   $p.query.entry -font $TkGnats(textfont) -textvar Query(lbheading) -background $bg \
            -borderwidth 2 -highlightthickness 0 -state disabled -relief flat -cursor left_ptr
    bind $p.query.entry <1> "break"
    bind $p.query.entry <B1-Motion> "break"
    bind $p.query.entry <2> "break"
    bind $p.query.entry <B2-Motion> "break"
    listbox $p.query.list  -font $TkGnats(textfont) -relief sunken -setgrid 1 \
            -xscroll "query_listbox_xscroll $p.query.sbx $p.query.list" \
            -yscroll "$p.query.sby set"  -takefocus 1 \
            -width ${lboxwidth} -height 12 -exportselection false \
            -borderwidth 2 -highlightthickness 2
    scrollbar $p.query.sby -command "$p.query.list yview" \
            -borderwidth 2 -highlightthickness 2 -orient vertical -takefocus 0
    # Create padding based on the y scrollbar width and border
    frame $p.query.bottom
    scrollbar $p.query.sbx -command "$p.query.list xview" \
            -borderwidth 2 -highlightthickness 2 -orient horizontal -takefocus 0

    if {$tcl_platform(platform) == "unix"} {
        set pad2 [expr 2 * ([$p.query.sby cget -bd] + [$p.query.sby cget -highlightthickness])]
    } {
        set pad2 0
    }
    
    set pad [expr [$p.query.sby cget -width] + $pad2]

    frame $p.query.padt -width $pad -height $pad
    frame $p.query.padb -width $pad -height $pad

    pack  $p.query.padt   -in $p.query.top -side left
    pack  $p.query.entry  -in $p.query.top -side left -fill x -anchor w -expand true \
	    -padx [$p.query.list cget -highlightthickness]
    pack  $p.query.top    -side top    -fill x 
    
    pack  $p.query.padb   -in $p.query.bottom -side left
    pack  $p.query.sbx    -in $p.query.bottom -side bottom -fill x
    pack  $p.query.bottom -side bottom -fill x 

    pack  $p.query.sby    -side left   -fill y 
    pack  $p.query.list   -side right  -fill both -expand true

    # I think this is a bug in the listbox widget. Prior and Next are missing the selection set.
    bind Listbox <Prior> {
        %W yview scroll -1 pages
        %W activate @0,0
        %W selection clear 0 end
        %W selection set active
    }
    bind Listbox <Next> {
        %W yview scroll 1 pages
        %W activate @0,0
        %W selection clear 0 end
        %W selection set active
    }
    bind Listbox <ButtonRelease-1> {
        tkCancelRepeat
        %W activate @%x,%y
        %W selection clear 0 end
        %W selection set active
    }
    
    if {[info exists TkGnats(QueryDefaultAction)]  && \
            $TkGnats(QueryDefaultAction) == "edit" && $TkGnats(edit_authorized)} {
        bind  $p.query.list <Double-Button-1>  "selection_Edit_cmd $p.query.list"
        bind  $p.query.list <Return>           "selection_Edit_cmd $p.query.list"
    } {
        set  TkGnats(QueryDefaultAction) view
        bind  $p.query.list <Double-Button-1>  "selection_View_Formatted_cmd %W"
        bind  $p.query.list <Return>           "selection_View_Formatted_cmd %W"
    }
    bind  $p.query.list <Control-l>        "%W xview 0"
    bind  $p.query.list <KeyRelease-Left>  "%W xview 0"
    bind  $p.query.list <KeyRelease-Right> "%W xview [expr $lboxwidth/2]"
    bind  $p.query.list <Control-r>        "%W xview [expr $lboxwidth/2]"
    bind  $p.query.list <3> {
        %W activate @%x,%y
        %W selection clear 0 end
        %W selection set active
        tk_popup .menu.sel.m %X %Y
    }

    set_focus_style $p.query.list
    if {$TkGnats(edit_authorized)} {
        bind  $p.query.list e "selection_Edit_cmd   $p.query.list"
        bind  $p.query.list E "selection_Edit_cmd   $p.query.list"
    }
    if {$TkGnats(delete_authorized)} {
        bind  $p.query.list d "selection_Delete_cmd $p.query.list"
        bind  $p.query.list D "selection_Delete_cmd $p.query.list"
    }
    bind  $p.query.list v "selection_View_Formatted_cmd %W"
    bind  $p.query.list V "selection_View_Formatted_cmd %W"
    bind  $p.query.list r "selection_View_Raw_cmd %W"
    bind  $p.query.list R "selection_View_Raw_cmd %W"
    bind  $p.query.list h "selection_Remove_cmd %W"
    bind  $p.query.list H "selection_Remove_cmd %W"
    bind  $p.query.list m "selection_Email_cmd %W"
    bind  $p.query.list M "selection_Email_cmd %W"

    return $p.query.list
}

proc query_listbox_xscroll {args} {
    global Query list_flds_heading
    eval [lindex $args 0] set [lrange $args 2 end]
    set  first [expr int([lindex [[lindex $args 1] xview] 0] * $Query(maxlen) + .5)]
    set Query(lbheading) [string range $list_flds_heading $first end]
}

#
# ---- Callbacks
#

proc folder_view_query_cmd {} {
    tkprfolder_dialog .tkprqueryfolder query "Saved Query Commands"
}

proc folder_save_query_cmd {} {
    save_query_cmd save
    build_query_menu
}

proc folder_view_sort_cmd {} {
    tkprfolder_dialog .tkprsortfolder sort "Saved Sort Commands"
}

proc folder_save_sort_cmd {{fname ""}} {
    global Query Tkprfolder
    if {$fname == ""} {
        while {"$fname" == ""} {
            ###set fname [file tail [get_save_file_name Sort]]
            set fname [get_save_file_name Sort]
            if {"$fname" == ""} {
                headingMsg "Save cancelled"
                return
            }
	    regexp $Tkprfolder(foldernameregexp) $fname match
	    if {$fname != $match} {
		Msg "Folder names must be composed only of letters, numbers, underscores and periods."
		return
	    }
            if {[file exists $Tkprfolder(user_sort_dir)/$fname]} {
                bell
                if {[tk_dialog .tkprfolder_delete "Confirm_Save" "$fname already exists" \
                        "warning" -1 "Overwrite" "Cancel"] != 0} {
                    set fname ""
                }
            }
        }
        set fname $Tkprfolder(user_sort_dir)/$fname
    }
    save_sort_fields $fname $Query(user_sort_flds)
    build_sort_menu
}

proc pridfromsummaryline {ln} {
    if {[scan $ln "%d" prid] < 1} {
        return 0
    } {
        return $prid
    }
}

proc selln {w} {
    set x  [$w curselection]
    if {[llength $x] == 0} {
	return ""
    } {
	return [$w get [lindex $x 0]]
    }
}

proc selection_Remove_cmd {w} {
    global Query
    if {[$w curselection] != ""} {
        set idx [$w curselection]
        $w delete $idx
        $w selection set active
        set Query(PrList) [lreplace $Query(PrList) $idx $idx]
    }
    if {[$w size] == 0} {
        disable_listbox_menus
    }
}

proc selection_Delete_cmd {w} {
    set ln [selln $w]
    if {"$ln" != ""} {
	set prid [pridfromsummaryline $ln]
        busy_cursor set
	headingMsg "Please Wait..."
        set result [delete_pr $prid]
        if {[string first deleted $result] >= 0} {
            # Successful deletion
            $w delete [$w curselection]
            $w selection set active
            headingMsg "$result"
        } {
            if {"$result" != ""} {
                tk_dialog .tkquerypr_delete "TkQuery Delete" $result "info" -1 "OK"
            }
            headingMsg "Delete failed."
        }
        busy_cursor clear
    }
    if {[$w size] == 0} {
        disable_listbox_menus
    }
}

proc selection_Email_cmd {w} {
    set ln [selln $w]
    if {"$ln" != ""} {
        busy_cursor set
	headingMsg "Please Wait..."
	set prid [pridfromsummaryline $ln]
        if {[set prtext  [get_pr_full_text $prid]] == "-1"} {
            set time 1
        } {
            set time 2250
            parsepr_txt $prtext flds
            email_originator [ftrim $flds(X-GNATS-Notify)] [ftrim $flds(>Responsible)] \
                    [ftrim $flds(Reply-To)] [ftrim $flds(>Category)]/$prid [ftrim $flds(>Synopsis)]
        }
	after $time {headingMsg " " ; busy_cursor clear}
    }
}

proc selection_Edit_cmd {w} {
    global TkGnats
    set ln [selln $w]
    if {"$ln" != ""} {
	headingMsg "Please Wait..."
        busy_cursor set
	set prid [pridfromsummaryline $ln]
	TkGnats_exec $TkGnats(WISHPATH) $TkGnats(lib)/tkeditpr.tcl -prid $prid \
                -server $TkGnats(ServerInfo) \
                -classes $TkGnats(ClassesFile) -states $TkGnats(StatesFile) \
                -categories $TkGnats(CategoryList) -submitters $TkGnats(SubmitterList) \
                -responsible $TkGnats(ResponsibleFile) &
	schedule_reap
	after 2250 {headingMsg " " ; busy_cursor clear}
    }
}

proc selection_View_Formatted_cmd {w} {
    global TkGnats
    set ln [selln $w]
    if {"$ln" != ""} {
	headingMsg "Please Wait..."
        busy_cursor set
	set prid [pridfromsummaryline $ln]
	TkGnats_exec $TkGnats(WISHPATH) $TkGnats(lib)/tkviewpr.tcl -prid $prid \
                -server $TkGnats(ServerInfo) \
                -classes $TkGnats(ClassesFile) -states $TkGnats(StatesFile) \
                -categories $TkGnats(CategoryList) -submitters $TkGnats(SubmitterList) \
                -responsible $TkGnats(ResponsibleFile) &
	schedule_reap
	after 2250 {headingMsg " " ; busy_cursor clear}
    }
}

proc selection_View_Raw_cmd {w} {
    global TkGnats
    set ln [selln $w]
    if {"$ln" != ""} {
        busy_cursor set
	headingMsg "Please Wait..."
	set prid [pridfromsummaryline $ln]
	TkGnats_exec $TkGnats(WISHPATH) $TkGnats(lib)/tkviewpr.tcl -prid $prid -raw 1 \
                -server $TkGnats(ServerInfo) \
                -classes $TkGnats(ClassesFile) -states $TkGnats(StatesFile) \
                -categories $TkGnats(CategoryList) -submitters $TkGnats(SubmitterList) \
                -responsible $TkGnats(ResponsibleFile) &
	schedule_reap
	after 2250 {headingMsg " " ; busy_cursor clear}
    }
}

proc build_query_header {} {
    global TkGnats Query
    set Query(query_pr_opts) ""
    set Query(by_id_only)    0
    set     Query(default__query) "proc default__query \{f\} \{\n"
    append  Query(default__query) "\tupvar 1 \$f flds\n"
    append  Query(default__query) "\tif \{\n"
}

proc build_query_trailer {} {
    global Query
    append Query(default__query) "\t\t1==1 \} \{\n"
    append Query(default__query) "\t\treturn 1\n\t\}\n"
    append Query(default__query) "\treturn 0\n"
    append Query(default__query) \}
}

proc build_date_query_qualifier {tag d when} {
    global TkGnats Query 
    set mode $TkGnats(GNATS_ACCESS_METHOD)
    set times(after)  "00:00"
    set times(before) "23:59"
    set vars(Arrival-Date.before)  arrived-before
    set vars(Arrival-Date.after)   arrived-after
    set vars(Last-Modified.before) modified-before
    set vars(Last-Modified.after)  modified-after
    set vars(Closed-Date.before)   closed-before
    set vars(Closed-Date.after)    closed-after
    set vars(Date-Required.before) required-before
    set vars(Date-Required.after)  required-after
    set var $vars($tag.$when)
    set last [lindex $d end]
    set date $d
    if {![regexp {[0-9][0-9]:[0-9][0-9]} $last]} {
        append date " $times($when)"
    }
    if {$mode == "socket"} {
	# XXX GNATS 4
        #set str "$gnatsd_commands($tag.$when) $date"
	if { $when == "after" } {
	    set str "$tag > \"$date\""
	} {
	    set str "$tag < \"$date\""
	}
    } {
        set str "--$var=$date"
    }
    lappend Query(query_pr_opts) "$str"
}
    
proc build_regex_query_qualifier {subclauseop type tag lst regexp} {
    global TkGnats Query 

    dputs "build_regex_query_qualifier: $subclauseop $type $tag $lst $regexp"

    if {$tag == "Submitter-Id"} {
        set tag2 Submitter
    } {
        set tag2 $tag
    }
    set mode $TkGnats(GNATS_ACCESS_METHOD)
    # set AND or OR subclaus operator
    switch -exact -- $subclauseop -and { 
	set subclauseop & 
    } -or {
	set subclauseop |
    }
    set subclausestr   ""
    set subclauseopstr ""
    foreach data $lst {
	set data [string trim $data " \n\t"]
	if {"$data" == ""} {
	    continue
	}
	# first clause: put in leading option stuff
	if {"$subclausestr" == ""} {
            if {$mode == "socket"} {
                set subclausestr ""
            } {
                set subclausestr [format "--%s=" [string tolower $tag2]]
            }
	}
	switch -exact -- $type -exact {
	    append subclausestr "$subclauseopstr $tag = \"$data\""
	} -glob {
	    append subclausestr "$subclauseopstr$data"
	}
	set subclauseopstr $subclauseop
    }
    if {"$subclausestr" == ""} {
	# no clauses were written
        if {"$regexp" != "" } {
            if {$mode == "socket"} {
                set subclausestr "$tag = $regexp"
            } {
                set subclausestr [format "--%s=" [string tolower $tag2]]$regexp
            }
        }
    } {
        if {"$regexp" != "" } {
            append subclausestr "|$regexp"
        }
    }
    if {$subclausestr != ""} {
        lappend Query(query_pr_opts) "$subclausestr"
    }
}

# This isn't currently used: query-pr can handle all fields now, as of beta 3.102.
proc build_dumb_query_qualifier {subclauseop type tag lst} {
    # puts "build dumb! subclauseop=$subclauseop type=$type tag=$tag lst=$lst"
    # set AND or OR subclaus operator
    switch -exact -- $subclauseop -and { 
	set subclauseop && 
    } -or {
	set subclauseop ||
    }
    set subclausestr ""

    foreach data $lst {
	set data [string trim $data " \n\t"]
	if {"$data" == ""} {
	    continue
	}
	# first clause , put in the leading parens
	if {"$subclausestr" == ""} {
	    append Query(default__query) "\t\t"
	    append Query(default__query) "( "
	}
	switch -exact -- $type -exact {
	    append Query(default__query) \
	 "$subclausestr ( \$flds($tag) == \"$data\" ) " nonewline
	} -glob {
	    append Query(default__query) \
		    "$subclausestr ( \[info exists flds($tag)\] && \
		    \[regexp -nocase -- \{$data\} \$flds($tag) \] ) " nonewline
	}
	set subclausestr "\\\n\t\t\t$subclauseop"
    }

    ##### "$subclausestr ( \[string match \{$data\} \$flds($tag) \] ) "

    if {"$subclausestr" == ""} {
	# no clauses were written so just return
	return
    }

    append Query(default__query) ") && \\"
}

proc get_last_modified {arr_date last_mod} {
    if {$last_mod != ""} {
        set date $last_mod
    } {
        set date $arr_date
    }
    return [clock scan [convert_date_format $date]]
}

proc build_mtime_query_qualifier {mtime} {
    global Query

    set mtime [expr [clock seconds] - $mtime * 24 * 60 * 60]

    append Query(default__query) "\t\t( \[get_last_modified \$flds(Arrival-Date) \$flds(Last-Modified)\] <= $mtime ) && \\\n"
}
 
proc XXXbuild_mtime_query_qualifier {mtime} {
    global TkGnats Query

    set mtime [expr [clock seconds] - $mtime * 24 * 60 * 60]

    append Query(default__query) "\t\t( \[file exists $TkGnats(GNATS_ROOT)/\$flds(Category)/\$flds(Number)\] && \\\n"

    append Query(default__query) "\t\t  \[file mtime  $TkGnats(GNATS_ROOT)/\$flds(Category)/\$flds(Number)\] <= $mtime ) && \\\n"

}

proc save_query_listbox { sfout s } {
    switch $s {
        Category {
            set ew .eboxs.clb
        }
        Submitter-Id {
            set ew .eboxs.slb
        }
        Responsible {
            set ew .eboxs.rlb
        }
    }

    if {![winfo exists $ew]} {
        return
    }
    
    puts $sfout "load_query_listbox $s \{[textget RegExp $ew.reg]\}"
}

proc load_query_listbox { s {re {}}} {
    global TkGnats $s
    switch $s {
        Category {
            set lb .eboxs.clb.cat
            set ew .eboxs.clb
            set list Category
        }
        # Submitter is for compatibility with old saved queries prior to tkgnats-3.0.12
        Submitter -
        Submitter-Id {
            set lb .eboxs.slb.sub
            set ew .eboxs.slb
            set list Submitter
        }
        Responsible {
            set lb .eboxs.rlb.res
            set ew .eboxs.rlb
            set list Responsible
        }
    }

    if {![winfo exists $lb]} {
        return
    }
    
    $lb.l.list delete 0 end
    $lb.r.list delete 0 end

    foreach a $TkGnats(${list}List) {
	if {[lsearch [array names $s] $a] < 0} {
	    $lb.l.list insert end $a
	} {
	    $lb.r.list insert end $a
	}
    }

    if {$re != ""} {
	textset RegExp $re $ew.reg
    }
}

proc load_query_entry { s re {when {}}} {
    global Query
    if {![info exists Query(stextparent,$s)]} {
        return
    }
    if {$when == ""} {
        textset $s $re $Query(stextparent,$s)
    } {
        textset $s $re $Query(stextparent,$s) $when
    }
}

proc get_save_file_name {title} {
    if {[catch {entryDialog "Enter name of file to save $title into:\n\n(Hint: underscores in name are replaced\nwith blanks for nice looking menu names)" Cancel "" 0 .} origp]} {
        return ""
    }
    set p [string trim $origp " \t\n!;'<>?*%$#"]
    if {"$p" == ""} {
        Msg "'$origp' is not a legal filename."
    }
    return $p
}

proc save_query_cmd {mode} {
    global TkGnats Query Tkprfolder lbpath

    if {$mode == "save"} {
        set p ""
        while {"$p" == ""} {
            ###set p [file tail [get_save_file_name Query]]
            set p [get_save_file_name Query]
            if {"$p" == ""} {
                headingMsg "Save cancelled"
                return
            }
	    regexp $Tkprfolder(foldernameregexp) $p match
	    if {$p != $match} {
		Msg "Folder names must be composed only of letters, numbers, underscores and periods."
		return
	    }
            set sfname $Tkprfolder(user_query_dir)/$p
            if {[file exists $sfname]} {
                bell
                if {[tk_dialog .tkprfolder_delete "Confirm_Save" "$p already exists" \
                        "warning" -1 "Overwrite" "Cancel"] != 0} {
                    set p ""
                }
            }
        }
    }

    set number_field [textget Number $Query(stextparent,Number)]
    if {"$number_field" != ""} {
        clear_query_cmd noclearheadingmsg
        regsub -all "," $number_field " " prid_list
        textset Number $prid_list $Query(stextparent,Number)
        if {$mode != "save"} {
            set Query(default__query) [list proc default__query {flds} {return 1}]
	    set Query(query_pr_opts) ""
	    set sep ""
	    foreach prid $prid_list {
		append Query(query_pr_opts) [concat $sep "Number==$prid_list"]
		set sep "|"
	    }
	    append Query(query_pr_opts) " "
            set Query(by_id_only) 1
            query_cmd $prid_list
            return 1
        }
    }
                
    if {$mode == "save"} {
        set   sfout [open $sfname w]
        puts $sfout "clear_query_cmd"
        puts $sfout "set Query(user_sort_flds) \{$Query(user_sort_flds)\}"
        save_query_view_fields $sfout
    } {
        build_query_header
    }

    # for array globals
    
    foreach f {State Priority Confidential Category Severity Class Responsible Submitter-Id} {
        if {[check_suppressed_field $f]} {
            continue
        }
	global $f
        if {$mode == "save"} {
            foreach a [array names $f] {
                if {"[set [set f]($a)]" != ""} {
                    puts $sfout "set [format "%s(%s)" $f $a] [set [set f]($a)]"
                }
            }
        } {
            set l {}
            foreach a [array names $f] {
                lappend l [set [set f]($a)]
            }
            global ${f}_regexp
            set regexp ""
            if {[info exists ${f}_regexp]} {
                set regexp [textget RegExp [set [set f]_regexp]]
            }
            build_regex_query_qualifier -or -exact $f $l $regexp
        }
    }

    # save query for listboxes
    
    if {$mode == "save"} {
        save_query_listbox $sfout Category
        save_query_listbox $sfout Submitter-Id
        save_query_listbox $sfout Responsible
    }

    # the text field regexp values
    
    foreach f "Synopsis Days-Idle Originator Text-Fields Release Keywords $TkGnats(Quarter)" {
        if {[check_suppressed_field $f]} {
            continue
        }
        if {$mode == "save"} {
            if {[info exists Query(stextparent,$f)] && "[textget $f $Query(stextparent,$f)]" != ""} {
                puts $sfout "load_query_entry $f \{[textget $f $Query(stextparent,$f)]\}"
            }
        } {
            if {[info exists Query(stextparent,$f)] && "[textget $f $Query(stextparent,$f)]" != ""} {
                if {$f == "Text-Fields"} {
                    set tag Multitext
                } {
                    set tag $f
                }
                # This next bit is because the form of the following switch statement doesn't
                # substitute variables.
                if {$tag == $TkGnats(Quarter)} {
                    set tmptag Quarter
                } {
                    set tmptag $tag
                }
                switch -exact -- $tmptag {
                    Originator -
                    Keywords -
                    Quarter -
                    Synopsis -
                    Multitext -
                    Release {
                        build_regex_query_qualifier -or -glob $tag {} \
                                [textget $f $Query(stextparent,$f)]
                    }
                    Days-Idle {
                        build_mtime_query_qualifier [textget $f $Query(stextparent,$f)]
                    }
                    default {
                        Msg "Illegal query text field '$f'."
                    }
                }
            }
        }
    }

    # the date field values
    
    foreach f {Arrival-Date Last-Modified Closed-Date Date-Required} {
        if {[check_suppressed_field $f]} {
            continue
        }
        if {$mode == "save"} {
            if {[info exists Query(stextparent,$f)] && "[textget $f $Query(stextparent,$f) after]" != ""} {
                puts $sfout "load_query_entry $f \{[textget $f $Query(stextparent,$f) after]\} after"
            }
            if {[info exists Query(stextparent,$f)] && "[textget $f $Query(stextparent,$f) before]" != ""} {
                puts $sfout "load_query_entry $f \{[textget $f $Query(stextparent,$f) before]\} before"
            }
        } {
            if {[info exists Query(stextparent,$f)] && "[textget $f $Query(stextparent,$f) after]" != ""} {
                build_date_query_qualifier $f [textget $f $Query(stextparent,$f) after] after
            }
            if {[info exists Query(stextparent,$f)] && "[textget $f $Query(stextparent,$f) before]" != ""} {
                build_date_query_qualifier $f [textget $f $Query(stextparent,$f) before] before
            }
        }
    }

    if {$mode == "save"} {
        close $sfout
        headingMsg "Query saved as $sfname"
    } {
        build_query_trailer
        #puts "$Query(default__query)"  
    }  
}

proc clear_query_cmd {{headingmsgflag ""}} {
    global TkGnats Query Tkprfolder lbpath

    if {[string compare $headingmsgflag ""] == 0} {
        headingMsg ""
    }

    # for array globals
    foreach f {State Priority Confidential Category Severity Class Responsible Submitter-Id} {
	global $f
        foreach a [array names $f] {
            set [set f]($a) {}
        }
    }

    # Query listboxes
    foreach {efield eframe} {Category clb Submitter-Id slb Responsible rlb} {
        if {![check_suppressed_field $efield]} {
            [string tolower $efield]_listbox .eboxs.$eframe
            textset RegExp ""    .eboxs.$eframe.reg
        }
    }

    foreach {f t} [list Synopsis text Days-Idle text Originator text Text-Fields text Release text Number text Arrival-Date after Arrival-Date before Last-Modified after Last-Modified before Closed-Date after Closed-Date before Keywords text $TkGnats(Quarter) text Date-Required after Date-Required before] {
        if {[info  exists  Query(stextparent,$f)]} {
            textset $f "" $Query(stextparent,$f) $t
        }
    }
        
    # the query listbox
    set prid [pridfromsummaryline [selln $lbpath]]
    if {$prid > 0} {
        set Query(curr_prid) $prid
    }
    $lbpath delete 0 end
    disable_listbox_menus
}

proc query_fill_listbox {} {
    global TkGnats Query Mappings \
            lbpath list_flds_formats list_flds_format list_flds_list list_flds_selected

    if {![info exists Query(PrList)]} {
        return
    }

    #puts [join $Query(PrList) \n]
    
    array set Class_vals \
            [list sw-bug sw doc-bug doc change-request chg support sup mistaken mis duplicate dup]
    
    foreach fld "Class State" {
        if {[info exists TkGnats(${fld}Abbreviations)]} {
            array set ${fld}_vals $TkGnats(${fld}Abbreviations)
        }
    }

    set prid [pridfromsummaryline [selln $lbpath]]
    if {$prid > 0} {
        set Query(curr_prid) $prid
    }
    $lbpath delete 0 end; # clear current list
    set c 0
    set Query(maxlen) 0
    foreach ln $Query(PrList) {
	incr c
	if {"$ln" == ""} {
            Msg "tkquerypr: Line $c empty in query output.\n" \
                    "Have the GNATS administrator check the index file for bogus entries."
	    continue
	}
        
	#
	# XXX TBD BUG XXX there is a problemo here if the synopsis
	# has a '|' character in it..
	#
	set l    [split $ln "|"]
	set llen [llength $l]
        set len  19
        if {$TkGnats(ReleaseBased)} {
            incr len 3
        }
	if {$llen != $len} {
            Msg "tkquerypr: Line $ln has $llen fields. It should have $len fields.\n" \
                    "Have the GNATS administrator check the index file for bogus entries. (Especially for |'s in any of the text fields)"
	    continue
	}

        if {$Query(query_mode) == "sql2"} {
            set flds(Number)        [lindex $l  0]
            set flds(Category)      [lindex $l  1]
            set flds(Synopsis)      [lindex $l  2]
            set flds(Confidential)  [lindex $l  3]
            set flds(Severity)      [lindex $l  4]
            set flds(Priority)      [lindex $l  5]
            set flds(Responsible)   [lindex $l  6]
            set flds(State)         [lindex $l  7]
            set flds(Class)         [lindex $l  8]
            set flds(Submitter-Id)  [lindex $l  9]
            set flds(Arrival-Date)  [lindex $l 10]
            set flds(Originator)    [lindex $l 11]
            set flds(Release)       [lindex $l 12]
            set flds(Last-Modified) [lindex $l 13]
            set flds(Closed-Date)   [lindex $l 14]
            if {$TkGnats(ReleaseBased)} {
                set flds($TkGnats(Quarter)) [lindex $l 15]
                set flds(Keywords)          [lindex $l 16]
                set flds(Date-Required)     [lindex $l 17]
            }
        } {
            set flds(Number)        [string trimright [lindex $l  0] " "]
            set flds(Category)      [string trimright [lindex $l  1] " "]
            set flds(Synopsis)      [string trimright [lindex $l  2] " "]
            set flds(Confidential)  [string trimright [lindex $l  3] " "]
            set flds(Severity)      [string trimright [lindex $l  4] " "]
            set flds(Priority)      [string trimright [lindex $l  5] " "]
            set flds(Responsible)   [string trimright [lindex $l  6] " "]
            set flds(State)         [string trimright [lindex $l  7] " "]
            set flds(Class)         [string trimright [lindex $l  8] " "]
            set flds(Submitter-Id)  [string trimright [lindex $l  9] " "]
            set flds(Arrival-Date)  [string trimright [lindex $l 10] " "]
            set flds(Originator)    [string trimright [lindex $l 11] " "]
            set flds(Release)       [string trimright [lindex $l 12] " "]
            set flds(Last-Modified) [string trimright [lindex $l 13] " "]
            set flds(Closed-Date)   [string trimright [lindex $l 14] " "]
            if {$TkGnats(ReleaseBased)} {
                set flds($TkGnats(Quarter)) [string trimright [lindex $l 15] " "]
                set flds(Keywords)          [string trimright [lindex $l 16] " "]
                set flds(Date-Required)     [string trimright [lindex $l 17] " "]
            }
        }
        
	# re-map the numeric fields into text
	foreach f {State Priority Severity Class} {
	    set flds($f) [lindex $Mappings($f) [expr "$flds($f) - 1"]]
	}

        #####   "%5d %-11s %-16s %-5s %-9s %-8s %-12s %s"
        
        # list_flds_format doesn't seem to be used anymore?
        set list_flds_format ""
	case $flds(Category) "_*" {
            continue
	} default {
	    if {[default__query flds]} {
                set ln ""
                for {set i 0} {$i < [llength $list_flds_list]} {incr i 2} {
                    set x [lindex $list_flds_list $i]
                    if {$list_flds_selected($x) != ""} {
                        if {$x == "Originator"} {
                            set orig [extract_full_name_from_address $flds($x)]
                            if {[string first "@" $orig] > 0} {
                                set orig [lindex [split $flds($x) "@"] 0]
                            }
                            set orig [string trim $orig]
                            if {$Query(orig_short)} {
                                # The following line reverts to old-style single name
                                set orig [lindex [split $orig " "] 0]
                            }
                            append ln [format $list_flds_formats($x) \
                                    [string range $orig 0 $Query(orig_wid)]]
                        } elseif {($x == "Arrival-Date") || ($x == "Last-Modified") || \
                                ($x == "Closed-Date") || ($x == "Date-Required")} {
                            append ln [format $list_flds_formats($x) \
                                    [lindex [split $flds($x) "@ "] 0]]
                        } elseif {[info exists ${x}_vals]} {
                            if {[info exists [subst ${x}_vals($flds($x))]]} {
                                set val [subst $${x}_vals($flds($x))]
                            } {
                                set val $flds($x)
                            }
                            append ln [format $list_flds_formats($x) $val]
                        } {
                            append ln [format $list_flds_formats($x) $flds($x)]
                        }
                        append list_flds_format $list_flds_formats($x)
                        #puts "$x width: [string trim $list_flds_formats($x) "%- sd"]"
                    }
                }
		$lbpath insert end $ln
                set maxlen [string length $ln]
                if {$Query(maxlen)  < $maxlen} {
                    set Query(maxlen) $maxlen
                }
	    }
	}
    }

    if {[$lbpath size] == 0 } {
        bell
        disable_listbox_menus
    } {
        enable_listbox_menus
        set lidx 0
        if {$Query(curr_prid) > 0} {
            set lidx [lsearch -regexp [$lbpath get 0 end] "^ *$Query(curr_prid) "]
            if {$lidx < 0} {
                set lidx 0
            }
        }
        $lbpath activate $lidx
        $lbpath selection set $lidx
        $lbpath see $lidx
    }
}

proc perform_sort_cmd {} {
    global Query
    sort_cmd
    query_fill_listbox
}

proc sort_cmd {} {
    global TkGnats Query
    set_query_sorting_cmd
    if {![info exists Query(PrList)]} {
        return
    }
    sort_cmd_$TkGnats(QuerySortMethod)
    #dputs "sort $TkGnats(QuerySortMethod)=[time {sort_cmd_$TkGnats(QuerySortMethod)}]"
}

proc sort_cmd_external {} {
    global Query
    #puts "sort_cmd_external sort=$Query(sort_keys)"
    set prlist [join $Query(PrList) \n]
    if {[catch {open "|sort $Query(sort_keys) << [list $prlist]" r} fin]} {
        Msg "Error executing sort cmd: |sort $Query(sort_keys)\n" "$fin"
        return
    }
    set    Query(PrList) [split [string trim [read $fin] \n] \n]
    close  $fin
}

proc sort_cmd_internal {} {
    global Query
    set keys  $Query(sort_keys)
    set nkeys [llength $keys]
    set sprlist {}
    foreach l $Query(PrList) {
        set le  [split $l |]
        set sle {}
        foreach key $keys {
            if {$key == 0} {
                lappend sle [format "%8s" [string trimright [lindex $le $key]]]
            } {
                lappend sle [string toupper [lindex $le $key]]
            }
        }
        lappend sprlist [join [concat $sle $le] |]
    }
    set sprlist [lsort $sprlist]
    set Query(PrList) {}
    foreach l $sprlist {
        lappend Query(PrList) [join [lrange [split $l |] $nkeys end] |]
    }
}

proc query_cmd_batch {} {
    global TkGnats Query

    if {$Query(query_mode) == "sqlf"} {
        set mode "--sql"
    } {
        set mode "--$Query(query_mode)"
    }

    set prdata ""
    if {$Query(by_id_only)} {
        set stat 1
        foreach prid [split $Query(query_pr_opts)] {
            if {[regexp "^\[0-9\]+$" $prid]} {
                if {![catch {eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) $mode $prid} data]} {
                    set stat 0
                    lappend prdata $data
                }
            }
        }
        set Query(PrList) $prdata
    } {
        set stat [catch {eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) $mode $Query(query_pr_opts)} Query(PrList)]
        set Query(PrList) [split $Query(PrList) \n]
    }
    if {$stat != 0} {
        set Query(PrList) ""
    }
    if {$mode == ""} {
        #puts "prlist=$Query(PrList)"
        set newlist {}
        set len [llength $Query(PrList)]
        for {set i 0} {$i < $len} {incr i 15} {
            lappend newlist [join [lrange $Query(PrList) $i [expr $i + 13]] \n]
        }
        set Query(PrList) $newlist
        #puts "prlist=$Query(PrList)"
    }
    
}

proc query_cmd_socket {} {
    global TkGnats Query

    if {[set s [open_socket_gnatsd]] == "-1"} {
        return
    }
    
    dputs "Query_pr_opts $Query(query_pr_opts)"
    set clist { "RSET" "QFMT sql2" }

    foreach condition $Query(query_pr_opts) {
	lappend clist [concat EXPR $condition]
	dputs "clist now $clist"
    }

    set doit  ""

    foreach cmd $clist {
	dputs "query sending $cmd"
        gnatsd_send $s $cmd
        set rep [get_socket_reply $s]
        if {![string match 210* [lindex $rep 0]]} {
            Msg "GNATSD error on cmd: $cmd\n" "[join $rep \n]"
            close_socket_gnatsd
            return
        }
    }

    set prdata ""

       gnatsd_send $s "QUER"
        
        set rep [get_socket_reply $s]
        if {[string match 440* [lindex $rep 0]]} {
            close_socket_gnatsd
	    return
        } elseif {![string match 30* [lindex $rep 0]]} {
            Msg "GNATSD error on cmd: $doit\n" "[join $rep \n]"
            close_socket_gnatsd
            return
        } {
            set prdata [concat $prdata [get_gnatsd_reply_dot_ended $s]]
        }

    set Query(PrList) $prdata

    catch {close_socket_gnatsd}
}

proc query_cmd {{prid ""}} {
    global TkGnats Query
    
    if {"$prid" == ""} {
	# Build a query from the widget specifiers
	if {[save_query_cmd query] == 1} {
            return
        }
    }
    eval $Query(default__query)

    set Query(PrList) {}
    
    #puts "query_cmd_$TkGnats(GNATS_ACCESS_METHOD) Query(query_pr_opts)=$Query(query_pr_opts)"
    #puts "query_cmd_$TkGnats(GNATS_ACCESS_METHOD) Query(default__query)=$Query(default__query)"
    
    query_cmd_$TkGnats(GNATS_ACCESS_METHOD)
    #dputs "query cmd $TkGnats(GNATS_ACCESS_METHOD)=[time {query_cmd_$TkGnats(GNATS_ACCESS_METHOD)}]"
}

proc perform_query_cmd {{query _list_} {prid {}}} {
    global TkGnats Query lbpath list_flds_heading
    if {[TkGnats_UpdateCheck]} {
	return
    }
    busy_cursor set
    if {[string compare $Query(done_msg) ""] == 0} {
        set Query(done_msg) Query
    }
    headingMsg "Performing $Query(done_msg)..."

    switch $query {
        _list_ {
            query_cmd
        }
        prid {
            query_cmd $prid
        }
    }

    sort_cmd
    query_fill_listbox
    
    headingMsg "Done $Query(done_msg):   [$lbpath size] matches found"
    set Query(done_msg) ""
    busy_cursor clear
    return
}

proc perform_print_cmd {{print _list_}} {
    global TkGnats Query Print lbpath list_flds_heading list_flds_widths
        
    set Print(fields) $list_flds_heading
    set Print(widths) $list_flds_widths
    
    set Query(done_msg) "$print print"
    headingMsg "Performing $Query(done_msg)..."
    set done_msg "Done $Query(done_msg)"
    
    if {[print_dialog] != 0} {
        headingMsg "Cancelled $Query(done_msg)"
        set done_msg ""
        return
    }
    
    busy_cursor set

    if {![info exists TkGnats(MsMacroSet)]} {
	set TkGnats(MsMacroSet) "-ms"
    }
    
    switch $Print(Device) {
        previewer {
            set previewfile /tmp/tkq.ps.$TkGnats(LogName)
            if {[string compare $Print(Format) "troff"] == 0} {
                set fout [open $previewfile w]
            } {
                if {[string compare $Print(Format) "ascii"] == 0 || \
                        [string compare $Print(Format) "latin1"] == 0} {
                    set fout [open "|groff -t -T$Print(Format) $TkGnats(MsMacroSet) -P-b > $previewfile" w]
                } {
                    set fout [open "|groff -t -T$Print(Format) $TkGnats(MsMacroSet)      > $previewfile" w]
                }
            }
        }
        printer {
            if {[string compare $Print(Format) "troff"] == 0} {
                set fout [open "| $Print(PrintSpooler,$Print(Format))" w]
            } {
                set fout [open "|groff -t -T$Print(Format) $TkGnats(MsMacroSet) | $Print(PrintSpooler,$Print(Format))" "w"]
            }
        }
        file {
            if {[string compare $Print(Format) "troff"] == 0} {
                set fout [open $Print(savefile) w]
            } {
                if {[string compare $Print(Format) "ascii"] == 0 || \
                        [string compare $Print(Format) "latin1"] == 0} {
                    set fout [open "|groff -t -T$Print(Format) $TkGnats(MsMacroSet) -P-b > $Print(savefile)" w]
                } {
                    set fout [open "|groff -t -T$Print(Format) $TkGnats(MsMacroSet)      > $Print(savefile)" w]
                }
            }
        }
    }

    set Print(fout) $fout

    # If TkGnats(GNATS_ACCESS_METHOD) != "socket" then this does nothing
    if {[open_socket_gnatsd 1] == "-1"} {
        busy_cursor clear
        return
    }
    
    print_listbox $lbpath $print
   
    # If TkGnats(GNATS_ACCESS_METHOD) != "socket" then this does nothing
    close_socket_gnatsd 1

    catch {close $fout}
    if {[string compare $Print(Device) "previewer"] == 0} {
	exec sh -c \
	 "[format $Print(Previewer,$Print(Format)) $previewfile]\;rm -f $previewfile"  &
	schedule_reap
    }
    
    headingMsg "$done_msg"
    set Query(done_msg) ""
    busy_cursor clear
    return
}

proc dump_listbox_data {} {
    global Query
    headingMsg "Saving Listbox Dump..."
    busy_cursor set
    set filename  [tk_getSaveFile]
    if {$filename == ""} {
        headingMsg "Saving Listbox Dump...Cancelled"
    } {
        file_put_text $filename [join $Query(PrList) \n]
        headingMsg "Saving Listbox Dump...done (written to $filename)"
    }
    busy_cursor clear
}

proc set_query_sorting_cmd {} {
    global Query
    set Query(sort_keys) \
            [build_sort_cmd $Query(sort_flds) $Query(sort_flgs) $Query(user_sort_flds)]
}

proc set_query_sort_fields {} {
    global Query
    sort_Dialog $Query(sort_flds) $Query(sort_flgs) $Query(user_sort_flds) \
            $Query(default_sort_flds) $Query(default_sort_file)
}

proc save_default_view {} {
    global Query list_flds_selected
    # save default-view file
    set    sfout [open $Query(default_view_file) w]
    puts  $sfout "array set list_flds_selected \{[array get list_flds_selected]\}"
    close $sfout
    Notice "Default fields saved."
}

proc save_default_sort {} {
    global Query
    save_sort_fields $Query(default_sort_file)  $Query(user_sort_flds)
    Notice "Default sort saved."
}

proc exit_cmd {} {
    global Query list_flds_selected
    Exit 0
}

#
# ---- Process args
#

get_default_sort_criteria
get_default_view_fields

# The following is for compatability with old saved queries
set categories   $TkGnats(CategoryList)
set submitters   $TkGnats(SubmitterList)
set responsibles $TkGnats(ResponsibleList)

#
# ---- Build widgets
#

frame   .mframe     -borderwidth 1 -relief raised
pack    .mframe     -side top -fill x

menubutton .mframe.file -text "File" -menu .mframe.file.m -underline 0
menu       .mframe.file.m
.mframe.file.m add command -label "New Problem Report..." \
        -command {TkGnats_exec $TkGnats(WISHPATH) $TkGnats(lib)/tksendpr.tcl \
        -server $TkGnats(ServerInfo) \
        -classes $TkGnats(ClassesFile) -states $TkGnats(StatesFile) \
        -categories $TkGnats(CategoryList) -submitters $TkGnats(SubmitterList) \
        -responsible $TkGnats(ResponsibleFile) &; schedule_reap}
.mframe.file.m add separator
.mframe.file.m add command -label "Exit" \
        -command exit_cmd

menubutton .mframe.edit -text "Edit" -menu .mframe.edit.m -underline 0
menu       .mframe.edit.m
.mframe.edit.m     configure -disabledforeground [.mframe.edit.m cget -foreground]
.mframe.edit.m add command -label "Use right mouse button for Cut/Copy/Paste" -state disabled
.mframe.edit.m add separator
.mframe.edit.m add command -label "Fonts..." -command "edit_fonts"

pack .mframe.file .mframe.edit -side left

menubutton .mframe.help -text "Help" -menu .mframe.help.m -underline 0
menu       .mframe.help.m
.mframe.help.m add command -label "Overview" \
        -command "helpMsg Query_Overview"
.mframe.help.m add separator
.mframe.help.m add command -label "Cut, Copy, Paste Operations" \
        -command "helpMsg Cut_Copy_Paste"
.mframe.help.m add separator
.mframe.help.m add command -label "Field Definitions" \
        -command "helpMsg Field_Definitions"
.mframe.help.m add command -label "Regular Expressions" \
        -command "helpMsg Query_Regular_Expressions"
.mframe.help.m add separator
.mframe.help.m add command -label "Check Buttons (Class, etc)" \
        -command "helpMsg Query_Check_Buttons"
.mframe.help.m add command -label "Listbox Selectors (Category, etc)" \
        -command "helpMsg Query_Listbox_Selectors"
.mframe.help.m add command -label "Entry Fields (Number, etc)" \
        -command "helpMsg Query_Entry_Fields"
.mframe.help.m add command -label "Menubar (Do Query, etc)" \
        -command "helpMsg Query_Menubar"
.mframe.help.m add command -label "Query Results Listbox" \
        -command "helpMsg Query_Results_Listbox"
.mframe.help.m add separator
.mframe.help.m add command -label "View Configuration Variables" \
        -command "helpMsg TkGnats_Variables"
.mframe.help.m add separator
.mframe.help.m add command -label "Changes" \
        -command "helpMsg Changes"
.mframe.help.m add command -label "About" \
        -command "helpMsg TkGnats_About"
pack .mframe.help -side right

frame .eflds
set panelnum 0
radiobar_frame .eflds .eflds.lb
foreach {efield elist} [list Class $TkGnats(ClassesList) State $TkGnats(StatesList) Priority {low medium high} Severity {non-critical serious critical} Confidential {no yes}] {
    if {![check_suppressed_field $efield]} {
        checkbar .eflds.lb [string tolower $efield] $efield $elist All $panelnum
	incr panelnum
    }
}
pack .eflds.lb -side left -pady 0
pack .eflds -side top  -padx  0 -fill x -anchor w

#puts stderr [concat "query boxes: " [grid slaves .eflds.lb]]


frame .eboxs    
set   nboxes 1
array set xpad {1 0 2 20 3 0}
foreach {efield eframe epath} {Category clb cbpath Submitter-Id slb sbpath Responsible rlb rbpath} {
    if {![check_suppressed_field $efield]} {
        frame .eboxs.$eframe -relief groove -borderwidth 2
        set   $epath [[string tolower $efield]_listbox .eboxs.$eframe]
        pack .eboxs.$eframe -side left -padx $xpad($nboxes)
        incr nboxes
    }
}
if {$nboxes > 1} {
    pack .eboxs     -side top  -pady  4 -fill x -anchor w
}

set tlist [list Number Release Synopsis Originator Text-Fields Days-Idle]
if {$TkGnats(ReleaseBased)} {
    lappend tlist Keywords $TkGnats(Quarter)
}
set textlist ""
set ntlist   0
foreach f $tlist {
    if {![check_suppressed_field $f]} {
        lappend textlist $f
        incr ntlist
    }
}

set dlist [list Arrival-Date Last-Modified Closed-Date]
if {$TkGnats(ReleaseBased)} {
    lappend dlist Date-Required
}
set datelist ""
set ndlist   0
foreach f $dlist {
    if {![check_suppressed_field $f]} {
        lappend datelist $f
        incr ndlist
    }
}

frame .stext   -relief flat
frame .stext.l -relief flat
pack  .stext   -side top  -fill x    -expand no  -anchor w
pack  .stext.l -side left -fill x    -expand yes -anchor nw
if {$ntlist <= $ndlist} {
    set frames  .stext.l
    set nframes 1
} {
    set frames  [list .stext.l .stext.c]
    set nframes 2
    frame .stext.c -relief flat
    pack  .stext.c -side left -fill x    -expand yes -anchor nw
}
if {$ndlist > 0} {
    frame .stext.r -relief flat
    pack  .stext.r     -side left -fill none -expand no  -anchor nw
}

set n 0
foreach f $textlist {
    set Query(stextparent,$f) [lindex $frames [expr $n % $nframes]]
    lappend Query(tlist) [singletext $Query(stextparent,$f) $f 20 "" 12]
    incr n
}
foreach f $datelist {
    set Query(stextparent,$f) .stext.r
    eval lappend Query(tlist) [daterange $Query(stextparent,$f) $f 16 "" "" 14]
}

set Query(curr_prid) 0

# Do this before adding the listbox to Query(tlist); we don't want Return bound there.
foreach w $Query(tlist) {
    bind $w <Return> "perform_query_cmd"
}

#
# Menus
#

frame .menu -relief raised -borderwidth 1
pack  .menu -side top -fill x -pady 4

frame .qlb
pack  .qlb -expand true -fill both
set   lbpath [query_listbox .qlb]
lappend Query(tlist) $lbpath

set_text_traversal $Query(tlist)

#
# Query Menu
#

proc get_merge_save_dirs {sitedir serverdir} {
    set queries {}
    set files {}
    set paths {}
    foreach x "[glob -nocomplain $sitedir/*]" {
        lappend paths $x
        lappend files [file tail $x]
    }
    foreach x "[glob -nocomplain $serverdir/*]" {
        set  i [lsearch $files [file tail $x]]
        if {$i < 0} {
            lappend paths $x
            lappend files [file tail $x]
        } {
            set paths [lreplace $paths $i $i $x]
        }
    }
    set spaths {}
    foreach x [lsort $files] {
        set i [lsearch -regexp $paths $x\$]
        lappend spaths [lindex $paths $i]
    }
    return $spaths
}

proc build_query_menu {} {
    global TkGnats Query Tkprfolder
    if {[winfo exists .menu.query.m]} {
        destroy .menu.query.m
    } {
        menubutton .menu.query -text "Query" -menu .menu.query.m -underline 0
    }
    menu .menu.query.m
    .menu.query.m  configure  -disabledforeground [.menu.query.m cget -foreground]
    .menu.query.m add command -label "Do Query"        -command perform_query_cmd
    .menu.query.m add command -label "Query Selection" -command query_from_selection
    .menu.query.m add command -label "Clear Widgets"   -command clear_query_cmd

    .menu.query.m add separator
    .menu.query.m add cascade -label "Save Current"    -menu   .menu.query.m.save
    .menu.query.m add command -label "Manage Saved..." -command folder_view_query_cmd
    menu .menu.query.m.save -tearoff no
    .menu.query.m.save add command -label "To Saved Queries Menu..." \
            -command folder_save_query_cmd

    .menu.query.m add separator
    .menu.query.m add cascade -label "Query For" -menu .menu.query.m.query
    menu .menu.query.m.query -tearoff no
#   foreach x "[lsort [glob -nocomplain $Tkprfolder(site_query_dir)/*]]"
    foreach x [get_merge_save_dirs $TkGnats(lib)/query $Tkprfolder(site_query_dir)] {
        regsub -all "_" [file tail $x] " " name
        .menu.query.m.query add command -label " - $name" -command "source $x
        set Query(done_msg) \"query for $name\"
        perform_query_cmd"
    }
    .menu.query.m add separator
    .menu.query.m add command -label "Saved Queries:" -state disabled
    set n 0
#    foreach x [get_merge_save_dirs $TkGnats(UserDir)/query $Tkprfolder(user_query_dir)]
    foreach x "[lsort [glob -nocomplain $Tkprfolder(user_query_dir)/*]]" {
        incr n
        regsub -all "_" [file tail $x] " " name
        .menu.query.m add command -label "$n. $name" -command "source $x
        set Query(done_msg) \"query for $name\"
        perform_query_cmd"
    }
}
build_query_menu

#
# Sort Menu
#

proc build_sort_menu {} {
    global TkGnats Tkprfolder
    if {[winfo exists .menu.sort.m]} {
        destroy .menu.sort.m
    } {
        menubutton .menu.sort -text "Sort" -menu .menu.sort.m -underline 0
    }
    menu .menu.sort.m
    .menu.sort.m  configure  -disabledforeground [.menu.sort.m cget -foreground]
    .menu.sort.m add command -label "New..."          -command set_query_sort_fields

    .menu.sort.m add separator
    .menu.sort.m add cascade -label "Save Current"    -menu   .menu.sort.m.save
    .menu.sort.m add command -label "Manage Saved..." -command folder_view_sort_cmd
    menu .menu.sort.m.save -tearoff no
    .menu.sort.m.save add command -label "To Saved Sorts Menu..." -command folder_save_sort_cmd
    .menu.sort.m.save add command -label "As Startup Default"     -command save_default_sort

    .menu.sort.m add separator
    .menu.sort.m add cascade -label "Sort By" -menu .menu.sort.m.sort
    menu .menu.sort.m.sort -tearoff no
#   foreach x "[lsort [glob -nocomplain $Tkprfolder(site_sort_dir)/*]]"
    foreach x [get_merge_save_dirs $TkGnats(lib)/sort $Tkprfolder(site_sort_dir)] {
        regsub -all "_" [file tail $x] " " name
        .menu.sort.m.sort add command -label " - $name" -command "source $x
        set Query(done_msg) \"sort by $name\"
        perform_sort_cmd"
    }
    .menu.sort.m add separator
    .menu.sort.m add command -label "Saved Sorts:" -state disabled
    set n 0
#   foreach x [get_merge_save_dirs $TkGnats(UserDir)/sort $Tkprfolder(user_sort_dir)]
    foreach x "[lsort [glob -nocomplain $Tkprfolder(user_sort_dir)/*]]" {
        incr n
        regsub -all "_" [file tail $x] " " name
        .menu.sort.m add command -label "$n. $name" -command "source $x
        perform_sort_cmd"
    }
}
build_sort_menu

#
# Fields Menu
#

menubutton .menu.view -text "Fields" -menu .menu.view.m -underline 0
menu       .menu.view.m
.menu.view.m add cascade -label "Save Current" -menu .menu.view.m.save
menu .menu.view.m.save -tearoff no
.menu.view.m.save add command -label "As Startup Default" -command save_default_view
.menu.view.m add separator
set list_flds_selected(Number) Number
for {set i 2} {$i < [llength $list_flds_list]} {incr i 2} {
    set x [lindex $list_flds_list $i]
    #if {![check_suppressed_field $x]} {
    #    .menu.view.m add checkbutton -label $x -command "update_query_view_fields" \
    #            -offvalue "" -onvalue $x -variable [format "list_flds_selected(%s)" $x]
    #}
    if {![check_suppressed_field $x]} {
        .menu.view.m add checkbutton -label [get_field_alias $x] \
                -command "update_query_view_fields" \
                -offvalue "" -onvalue $x -variable [format "list_flds_selected(%s)" $x]
    }
}

#
# Print Menu
#

set Print(savefile)    ""
set Print(Device)      printer
set Print(Format)      ps
set Print(Select)      all

foreach format {ascii dvi latin1 ps troff} {
    if {[info exists TkGnats(${format}Previewer)] && \
            [string compare $TkGnats(${format}Previewer) ""] != 0} {
        set Print(Previewer,$format)    $TkGnats(${format}Previewer)
    }
    if {[info exists TkGnats(${format}PrintSpooler)] && \
            [string compare $TkGnats(${format}PrintSpooler) ""] != 0} {
        set Print(PrintSpooler,$format) $TkGnats(${format}PrintSpooler)
    }
}

menubutton .menu.print -text "Print" -menu .menu.print.m -underline 0
menu       .menu.print.m
.menu.print.m  configure  -disabledforeground [.menu.print.m cget -foreground]

#####.menu.print.m add command -label "  Report Formats:" -state disabled
.menu.print.m add command -label "Summary..." \
        -command "perform_print_cmd Summary"
.menu.print.m add command -label "Medium..." \
        -command "perform_print_cmd Medium"
.menu.print.m add command -label "Full..." \
        -command "perform_print_cmd Full"
.menu.print.m add command -label "Raw Data..." \
        -command "perform_print_cmd Raw_Data"

.menu.print.m add separator

.menu.print.m add command -label "Dump Listbox Data..." \
        -command dump_listbox_data

.menu.print.m add separator
 
# Add any site print commands
set n 0
#foreach x "[lsort [glob -nocomplain $Tkprfolder(site_print_dir)/*]]"
foreach x [get_merge_save_dirs $TkGnats(lib)/print $Tkprfolder(site_print_dir)] {
    incr n
    set  cmd [file tail $x]
    regsub -all "_" $cmd " " name
    .menu.print.m add command -label "$n. $name..." -command "source $x
    perform_print_cmd $cmd"
}

#
# Actions Menu
#

menubutton .menu.sel -text "Actions" -menu .menu.sel.m -underline 0
menu       .menu.sel.m

.menu.sel.m add command -label "Edit..."           -command "selection_Edit_cmd $lbpath" -accel e
if {! $TkGnats(edit_authorized)} {
    .menu.sel.m entryconfigure "Edit..." -state disabled
}
.menu.sel.m add command -label "View..."           -command "selection_View_Formatted_cmd $lbpath" -accel v
.menu.sel.m add command -label "View Raw Data..."  -command "selection_View_Raw_cmd       $lbpath" -accel r

.menu.sel.m add separator

if {$TkGnats(delete_authorized)} {
    .menu.sel.m add command -label "Delete PERMANENTLY" -command "selection_Delete_cmd $lbpath" -accel d
    .menu.sel.m add separator
}

.menu.sel.m add command -label "Hide From List"    -command "selection_Remove_cmd $lbpath" -accel h

.menu.sel.m add separator

.menu.sel.m add command -label "Send Email..."     -command "selection_Email_cmd  $lbpath" -accel m

button .menu.doquery -text "Do Query" -command perform_query_cmd
button .menu.clear   -text "Clear"    -command clear_query_cmd

pack   .menu.doquery .menu.clear -side left

foreach x {
    query sort view print sel
} {
    pack .menu.$x -side left
}

disable_listbox_menus

message .menu.msg -aspect 10000 -relief sunken -bd 1
pack    .menu.msg -side left -fill x -expand 1

###wm title      . "TkGnats - Query Problem Reports"
wm title      . "TkGnats - [lindex $TkGnats(ServerInfo) 0]"
wm iconbitmap . @$TkGnats(lib)/tkquerypr.xbm
###wm iconname   . "$TkGnats(LogName)'s tkquerypr"
wm iconname   . "[lindex $TkGnats(ServerInfo) 0]"
