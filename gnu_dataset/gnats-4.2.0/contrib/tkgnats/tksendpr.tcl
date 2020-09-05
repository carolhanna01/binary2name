proc tksendpr_usage {{str ""}} {
    bell
    wm withdraw .
    tk_dialog .tkgnatsError "TkGnats Error" \
            "${str}usage: tksendpr -server 'ServerInfo' \[-categories 'list'\] \
             \[-classes 'list'\] \[-states 'list'\] \
            \[-db 'database'\] \[-submitters 'list'\] \[-responsible 'list'\]" "error" 0 "OK"
    exit
}

proc tksendpr_process_args {} {
    global TkGnats argc argv env

    set TkGnats(CurrentProgram) tksendpr
    
    if {$argc != 0} {
	if {$argc%2 != 0} {
	    tksendpr_usage
	}
    }
    set addressval ""
    for {set x 0} {$x<$argc} {incr x 2} {
	set opt [lindex $argv $x]
	set val [lindex $argv [expr $x+1]]
	switch -exact -- $opt -server {
            set TkGnats(ServerInfo) $val
        } -categories {
	    set TkGnats(CategoryList)  $val
	} -submitters {
	    set TkGnats(SubmitterList) $val
        } -responsible {
            set TkGnats(ResponsibleFile) $val
        } -classes {
            set TkGnats(ClassesFile) $val
        } -states {
            set TkGnats(StatesFile)  $val
	} default {
	    tksendpr_usage "Illegal option pair:\n'$opt $val'\n\n"
	}
    }

    if {![info exists TkGnats(ServerInfo)]} {
        tksendpr_usage "No -server argument given.\n\n"
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

tksendpr_process_args

foreach f { tkpr_library.tcl tkprhelp.tcl tkprfont.tcl tkprdatesel.tcl } {
    source $TkGnats(lib)/$f
}

if {[get_gnats_config] == "-1"} {
    exit
}

# Remove "pending" from the Category list
set idx [lsearch -exact $TkGnats(CategoryList) "pending"]
set TkGnats(CategoryList) [lreplace $TkGnats(CategoryList) $idx $idx]

proc headingMsg {a} {
    .action.msg configure -text $a
    update idletasks
}

# Assemble the problem report in gnats format from the fields in
# Before assembly some of the widgets are checked for validity.

proc send_report {} {
    global TkGnats Tksendpr frm current_multi_text Class State Priority Severity Confidential Category Submitter-Id Responsible
    
    flush_singletext $Tksendpr(singletextflds)
    flush_multitext
    
    # check all the fields first...

    # Check for blank mandatory fields.
    foreach f [concat $Tksendpr(radioflds) $Tksendpr(listboxflds) $Tksendpr(singletextflds) $Tksendpr(multitextflds)] {
        if {[check_suppressed_field $f]} {
            continue
        }
	if {![check_mandatory_field $f]} {
	    continue
	}
	if {[info exists frm($f)]} {
	    # singletext and multitext
	    set val $frm($f)
	} {
	    # radiobutton and listbox fields
	    set val [set $f]
	}
        set val [string trim $val " \n\t\f!:~-#_?"]
        if {$val == ""} {
            Msg "You must supply a value for '[get_field_alias $f]'."
            return -1
        }
    }

    # Check listbox fields
    foreach {field list} {Category Category Submitter-Id Submitter Responsible Responsible} {
	if {![check_suppressed_field $field]} {
	    if {[set $field] != "" && [lsearch -exact $TkGnats(${list}List) [set $field]] < 0} {
		Msg "You have specified an invalid [get_field_alias $field]: [set $field]"
		return -1
	    }
	}
    }

    foreach f $Tksendpr(singletextflds) {
        if {[string first "|" $frm($f)] >= 0} {
            Msg " '|' is an illegal character for the '[get_field_alias $f]' field!"
            return -1
        }
    }

    if {$TkGnats(ReleaseBased) && ![check_suppressed_field Date-Required]} {
        if {[check_date_invalid $frm(Date-Required) "Invalid [get_field_alias Date-Required]:"]} {
            return -1
        }
        set frm(Date-Required) [normalize_date $frm(Date-Required)]
    }
        
    # ok, now send off the report

    set    mailtxt ""
    append mailtxt "From: $TkGnats(EmailAddr)\n"
    append mailtxt "Reply-To: $TkGnats(EmailAddr)\n"
    append mailtxt "To: $TkGnats(GNATS_ADDR)\n"
    append mailtxt "Subject: [string trim $frm(Synopsis)]\n"
    append mailtxt "X-send-pr-version: $Tksendpr(version)\n"
    append mailtxt "X-GNATS-Notify: [add_email_domainname [string trim $frm(X-GNATS-Notify)]]\n"
    append mailtxt "\n"
    #### End of standard mail headers
    
    foreach f [concat $Tksendpr(radioflds) $Tksendpr(listboxflds)] {
        if {![check_suppressed_field $f]} {
	    append mailtxt ">${f}:	[set $f]\n"
	} elseif {[info exists TkGnats(CreateDefault$f)]} {
	    append mailtxt ">${f}:	$TkGnats(CreateDefault$f)\n"
        }
    }
    
    foreach f $Tksendpr(singletextflds) {
        if {$f != "X-GNATS-Notify"} {
            if {![check_suppressed_field $f]} {
                append mailtxt ">${f}: $frm($f)\n"
            } elseif {[info exists TkGnats(CreateDefault$f)]} {
                append mailtxt ">${f}: $TkGnats(CreateDefault$f)\n"
            }
        }
    }
    
    foreach f $Tksendpr(multitextflds) {
        if {![check_suppressed_field $f]} {
	    append mailtxt "${f}:\n$frm($f)\n"
	} elseif {[info exists TkGnats(CreateDefault[string trim $f >])]} {
	    append mailtxt "${f}:\n$TkGnats(CreateDefault[string trim $f >])\n"
        }
    }

    #
    # mail the assembled problem report file to the gnats system
    #

    headingMsg "Sending Report to $TkGnats(GNATS_ADDR)..."
    if {[TkGnats_sendmail $TkGnats(GNATS_ADDR) $mailtxt] == "-1"} {
        headingMsg "Sending Report to $TkGnats(GNATS_ADDR)...***ERROR***"
        return -1
    } {
        headingMsg "Sending Report to $TkGnats(GNATS_ADDR)...Done"
    }

    #
    # clear out some fields
    #

    set flist {Synopsis}
    if {$TkGnats(ReleaseBased)} {
	lappend flist Keywords $TkGnats(Quarter) Date-Required
    }
    foreach f $flist {
	textset $f ""
    }
    # Some folks may not want these reset
    #foreach t {Environment Description How-To-Repeat Fix Organization}
    foreach f {Description How-To-Repeat Fix} {
	set frm(>$f) ""
        if {">$f" == $current_multi_text} {
            .multiline.text delete 1.0 end
        }
    }
    switch_txt $TkGnats(first_multitext) $Tksendpr(multitextflds)
    return 0
}

proc send_report_and_exit {} {
    if {[send_report] == 0} {
	Exit 0
    }
}

# bail out completely
proc cancel_report {} {
    Exit 0
}

proc snd_category_listbox {p category_list} {
    global TkGnats Tksendpr Category
    set Category ""

    set alias [get_field_alias Category]
    frame  $p.cat -relief groove  -borderwidth 2
    pack   $p.cat -side top -expand true -fill both
    button $p.cat.lab -anchor w -text "${alias}: " -width 14 -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0
    frame  $p.cat.msg -relief flat
    set wid [expr 2 + [get_max_strlen $category_list]]
    set ew  [entry  $p.cat.msg.ent -width $wid -insertwidth 1 -insertofftime 400 \
            -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground) -highlightthickness 2 \
            -textvariable Category]
    lappend Tksendpr(tlist) $ew
    trace variable Category w set_snd_category_ew
    set_focus_style $ew
    bind $ew <BackSpace> { tkEntrySetCursor %W [%W index insert] }
    bind $ew <Control-h> { tkEntrySetCursor %W [%W index insert] }
    set height [llength $category_list]
    if {$height > 6 } {
        set height 6
    }
    scrollbar $p.cat.msg.sb -command "$p.cat.msg.list yview" -borderwidth 2 -relief sunken
    listbox   $p.cat.msg.list -yscroll "$p.cat.msg.sb set" -setgrid 1 -relief sunken -borderwidth 2 \
            -width $wid -height $height -exportselection false
    eval   $p.cat.msg.list insert end $category_list
    pack   $p.cat.msg.ent  -side top   -fill x
    pack   $p.cat.msg.sb   -side right -fill y
    pack   $p.cat.msg.list -side right -fill both -expand true
    pack   $p.cat.lab      -side left  -anchor nw -ipady 4
    pack   $p.cat.msg      -side left  -anchor nw -fill y
    bind   $p.cat.msg.list <B1-ButtonRelease> "set_snd_category $p.cat.msg.ent %W %y"
    return $p.cat.msg.list
}

proc snd_submitter-id_listbox {p submitters_list} {
    global TkGnats Tksendpr Submitter-Id
    set Submitter-Id ""

    set alias [get_field_alias Submitter-Id]
    frame  $p.sub -relief groove  -borderwidth 2
    pack   $p.sub -side top -expand true -fill both
    button $p.sub.lab -anchor w -text "${alias}: " -width 14 -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0
    frame  $p.sub.msg -relief flat
    set wid [expr 2 + [get_max_strlen $submitters_list]]
    set ew  [entry  $p.sub.msg.ent -width $wid -insertwidth 1 -insertofftime 400 \
            -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground) -highlightthickness 2 \
            -textvariable Submitter-Id]
    lappend Tksendpr(tlist) $ew
    trace variable Submitter-Id w set_snd_submitter-id_ew
    set_focus_style $ew
    bind $ew <BackSpace> { tkEntrySetCursor %W [%W index insert] }
    bind $ew <Control-h> { tkEntrySetCursor %W [%W index insert] }
    set height [llength $submitters_list]
    if {$height > 6 } {
        set height 6
    }
    scrollbar $p.sub.msg.sb -command "$p.sub.msg.list yview" -borderwidth 2 -relief sunken
    listbox   $p.sub.msg.list -yscroll "$p.sub.msg.sb set" -setgrid 1 -relief sunken -borderwidth 2 \
            -width $wid -height $height -exportselection false
    eval   $p.sub.msg.list insert end $submitters_list
    pack   $p.sub.msg.ent  -side top   -fill x
    pack   $p.sub.msg.sb   -side right -fill y
    pack   $p.sub.msg.list -side right -fill both -expand true
    pack   $p.sub.lab      -side left  -anchor nw -ipady 4
    pack   $p.sub.msg      -side left  -anchor nw -fill y
    bind   $p.sub.msg.list <B1-ButtonRelease> "set_snd_submitter-id $p.sub.msg.ent %W %y"
    return $p.sub.msg.list
}
  
proc snd_responsible_listbox {p responsibles_list} {
    global TkGnats Tksendpr Responsible
    set Responsible ""

    set alias [get_field_alias Responsible]
    frame  $p.res -relief groove  -borderwidth 2
    pack   $p.res -side top -expand true -fill both
    button $p.res.lab -anchor w -text "${alias}: " -width 14 -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0
    frame  $p.res.msg -relief flat
    set wid [expr 2 + [get_max_strlen $responsibles_list]]
    set ew  [entry  $p.res.msg.ent -width $wid -insertwidth 1 -insertofftime 400 \
            -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground) -highlightthickness 2 \
            -textvariable Responsible]
    lappend Tksendpr(tlist) $ew
    trace variable Responsible w set_snd_responsible_ew
    set_focus_style $ew
    bind $ew <BackSpace> { tkEntrySetCursor %W [%W index insert] }
    bind $ew <Control-h> { tkEntrySetCursor %W [%W index insert] }
    set height [llength $responsibles_list]
    if {$height > 6 } {
        set height 6
    }
    scrollbar $p.res.msg.sb -command "$p.res.msg.list yview" -borderwidth 2 -relief sunken
    listbox   $p.res.msg.list -yscroll "$p.res.msg.sb set" -setgrid 1 -relief sunken -borderwidth 2 \
            -width $wid -height $height -exportselection false
    eval   $p.res.msg.list insert end $responsibles_list
    pack   $p.res.msg.ent  -side top   -fill x
    pack   $p.res.msg.sb   -side right -fill y
    pack   $p.res.msg.list -side right -fill both -expand true
    pack   $p.res.lab      -side left  -anchor nw -ipady 4
    pack   $p.res.msg      -side left  -anchor nw -fill y
    bind   $p.res.msg.list <B1-ButtonRelease> "set_snd_responsible $p.res.msg.ent %W %y"
    return $p.res.msg.list
}

proc set_snd_category_ew {a b c} {
    upvar #0 $a f
    global TkGnats
    quickfill_entry_from_listbox f .eboxs.clb.cat.msg.ent .eboxs.clb.cat.msg.list $TkGnats(CategoryList)
}
 
proc set_snd_submitter-id_ew {a b c} {
    upvar #0 $a f
    global TkGnats
    quickfill_entry_from_listbox f .eboxs.slb.sub.msg.ent .eboxs.slb.sub.msg.list $TkGnats(SubmitterList)
}

proc set_snd_responsible_ew {a b c} {
    upvar #0 $a f
    global TkGnats
    quickfill_entry_from_listbox f .eboxs.rlb.res.msg.ent .eboxs.rlb.res.msg.list $TkGnats(ResponsibleList)
}

proc set_snd_category {ent w y} {
    global Category
    trace vdelete Category w set_snd_category_ew
    $ent delete 0 end
    set idx [$w nearest $y]
    set Category [$w get $idx]
    trace variable Category w set_snd_category_ew
}

proc set_snd_submitter-id {ent w y} {
    global Submitter-Id
    trace vdelete Submitter-Id w set_snd_submitter-id_ew
    $ent delete 0 end
    set idx [$w nearest $y]
    set Submitter-Id [$w get $idx]
    trace variable Submitter-Id w set_snd_submitter-id_ew
}

proc set_snd_responsible {ent w y} {
    global Responsible
    trace vdelete Responsible w set_snd_responsible_ew
    $ent delete 0 end
    set idx [$w nearest $y]
    set Responsible [$w get $idx]
    trace variable Responsible w set_snd_responsible_ew
}

proc env_fld_text {} {
    global TkGnats tcl_platform
    set txt ""

    if {[info exists TkGnats(ENVIRONMENT)]} {
        set txt $TkGnats(ENVIRONMENT)
    } {
        if {[catch {exec uname -a} result]} {
            set result "$tcl_platform(os) [info hostname] $tcl_platform(osVersion) $tcl_platform(machine)"
        }
        append txt "System: $result\n"
        
        if {![catch {exec arch} result]} {
            append txt "Architecture: $result\n"
        }
        
        if {![catch {exec xdpyinfo | fgrep endor} result]} {
            append txt "X:\n----\n$result\n"
        }
    }

    return $txt
}

proc org_fld_text {} {
    global TkGnats tcl_platform
    return $TkGnats(ORGANIZATION)
}

#################################

if {"$TkGnats(LogName)" == "root"} {
    Msg "You cannot send problem reports as root.\n" "Please use your own login."
    Exit 1
}

# Set Create Defaults
foreach d [list [eval list Class $TkGnats(ClassesList)] [eval list State $TkGnats(StatesList)] \
        {Priority medium} {Severity serious} {Confidential no}] {
    if {![info exists TkGnats(CreateDefault[lindex $d 0])]} {
        set TkGnats(CreateDefault[lindex $d 0]) [lindex $d 1]
    }
}

# List of entry widgets for traverse key binding
set Tksendpr(tlist) {}

set Tksendpr(radioflds) {
    State
    Confidential
    Severity
    Priority
    Class
}

set Tksendpr(listboxflds) {
    Category
    Submitter-Id
    Responsible
}

set Tksendpr(singletextflds) {
    Originator
    X-GNATS-Notify
    Release
    Synopsis
}

if {$TkGnats(ReleaseBased)} {
    lappend Tksendpr(singletextflds) Keywords $TkGnats(Quarter) Date-Required
}
    
set Tksendpr(multitextflds) {
    >Description
    >How-To-Repeat
    >Environment
    >Organization
    >Fix
    >Audit-Trail
    >Unformatted
    >Release-Note
}

#########

if {[regexp "tkgnats-\[^ \]*" $TkGnats(tkgnats_version) ver]} {
    set Tksendpr(version) $ver
} {
    set Tksendpr(version) "Unknown TkGnats version"
}

frame   .mframe -borderwidth 1 -relief raised
pack    .mframe -side top -fill x

menubutton .mframe.file -text "File" -menu .mframe.file.m -underline 0
menu       .mframe.file.m
.mframe.file.m add command -label "Send"          -command send_report
.mframe.file.m add command -label "Send and Exit" -command send_report_and_exit
.mframe.file.m add separator
.mframe.file.m add command -label "Cancel"        -command cancel_report

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
        -command "helpMsg Create_Overview"
.mframe.help.m add separator
.mframe.help.m add command -label "Cut, Copy, Paste Operations" \
        -command "helpMsg Cut_Copy_Paste"
.mframe.help.m add separator
.mframe.help.m add command -label "Field Definitions" \
        -command "helpMsg Field_Definitions"
.mframe.help.m add separator
.mframe.help.m add command -label "Radio Buttons (Class, etc)" \
        -command "helpMsg Create_Radio_Buttons"
.mframe.help.m add command -label "Listbox Selectors (Category, etc)" \
        -command "helpMsg Create_Listbox_Selectors"
.mframe.help.m add command -label "Entry Fields (Originator, etc)" \
        -command "helpMsg Create_Entry_Fields"
.mframe.help.m add command -label "Text Fields (Description, etc)" \
        -command "helpMsg Create_Text_Fields"
.mframe.help.m add separator
.mframe.help.m add command -label "View Configuration Variables" \
        -command "helpMsg TkGnats_Variables"
.mframe.help.m add separator
.mframe.help.m add command -label "Changes" \
        -command "helpMsg Changes"
.mframe.help.m add command -label "About" \
        -command "helpMsg TkGnats_About"
pack .mframe.help -side right

frame   .action -borderwidth 1 -relief raised
pack    .action -side top -fill x -anchor w
#button  .action.cancel      -borderwidth 1 -text "Cancel"        -command cancel_report
button  .action.send        -borderwidth 1 -text "Send"          -command send_report
button  .action.sendandexit -borderwidth 1 -text "Send and Exit" -command send_report_and_exit
#pack    .action.send .action.sendandexit .action.cancel -side left -padx 0
pack    .action.send .action.sendandexit -side left -padx 0

message .action.msg -aspect 10000 -relief sunken -bd 1 -text ""
pack    .action.msg -side left -fill x -expand 1

frame .eflds
radiobar_frame .eflds .eflds.lb
set panelnum 0
foreach {efield elist} [list Class $TkGnats(ClassesList) State $TkGnats(StatesList) Priority {low medium high} Severity {non-critical serious critical} Confidential {no yes}] {
    if {![check_suppressed_field $efield]} {
        radiobar     .eflds.lb [string tolower $efield] $efield $elist None "" "" 0 $panelnum
        radiobar_set .eflds.lb [string tolower $efield] $TkGnats(CreateDefault$efield)
	incr panelnum
    } {
        set $efield [lindex $elist 0]
    }
}
pack .eflds.lb -side left
pack .eflds    -side top  -fill x -anchor w

frame .eboxs
set   nboxes 1
array set xpad {1 0 2 20 3 0}
foreach {efield eframe list} {Category clb Category Submitter-Id slb Submitter Responsible rlb Responsible} {
    if {![check_suppressed_field $efield]} {
        frame .eboxs.$eframe
        snd_[string tolower $efield]_listbox .eboxs.$eframe $TkGnats(${list}List)
        pack  .eboxs.$eframe -side left -anchor nw -pady 2 -fill y -padx $xpad($nboxes)
        incr nboxes
    } {
        set $efield ""
    }
}
if {$nboxes > 1} {
    pack .eboxs -side top -fill x -anchor w -pady 2
}

set Submitter-Id $TkGnats(SUBMITTER)

foreach f $Tksendpr(listboxflds) {
    if {[info exists TkGnats(CreateDefault$f)]} {
        set $f $TkGnats(CreateDefault$f)
    }
}

foreach f $Tksendpr(singletextflds) {
    if {[check_suppressed_field $f]} {
        continue
    }
    if {[info exists TkGnats(CreateDefault$f)]} {
        set     val $TkGnats(CreateDefault$f)
    } {
        if {$f == "Originator"} {
            set val $TkGnats(FullName)
        } {
            set val ""
        }
    }
    lappend Tksendpr(tlist) [singletext . $f 60 $val 14]
}

foreach f $Tksendpr(multitextflds) {
    set frm($f) ""
}
set frm(>Environment)  [env_fld_text]
set frm(>Organization) [org_fld_text]
foreach f $Tksendpr(multitextflds) {
    set field [string trimleft $f >]
    if {[info exists TkGnats(CreateDefault$field)]} {
        set frm($f) $TkGnats(CreateDefault$field)
    }
}

set current_multi_text ""

lappend Tksendpr(tlist) [make_txt_mb $Tksendpr(multitextflds)]
switch_txt $TkGnats(first_multitext) $Tksendpr(multitextflds)

set_text_traversal $Tksendpr(tlist)

#frame  .action -borderwidth 3
#button .action.cancel      -borderwidth 2 -text "Cancel"        -command cancel_report
#button .action.send        -borderwidth 2 -text "Send"          -command send_report
#button .action.sendandexit -borderwidth 2 -text "Send and Exit" -command send_report_and_exit
#
#pack .action.send .action.sendandexit .action.cancel -side left -padx 10
#
#pack .action -side top

wm title      . "TkGnats - [lindex $TkGnats(ServerInfo) 0] - $TkGnats(GNATS_ADDR)"
wm iconbitmap . @$TkGnats(lib)/tksendpr.xbm
wm iconname   . "$TkGnats(LogName)'s tksendpr"
