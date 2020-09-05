proc tkviewpr_usage {{str ""}} {
    bell
    wm withdraw .
    tk_dialog .tkgnatsError "TkGnats Error" \
            "${str}usage: tkviewpr -prid nnnn -server 'ServerInfo' \[-raw 1\] \
            \[-classes 'list'\] \[-states 'list'\] \
            \[-categories 'list'\] \[-submitters 'list'\] \[-responsible 'list'\]" "error" 0 "OK"
    exit
}

proc tkviewpr_process_args {} {
    global TkGnats argc argv env prid rawflag

    set TkGnats(CurrentProgram) tkviewpr
    
    if {$argc != 0} {
	if {$argc%2 != 0} {
	    tkviewpr_usage
	}
	for {set x 0} {$x<$argc} {incr x 2} {
	    set opt [lindex $argv $x]
	    set val [lindex $argv [expr $x+1]]
	    switch -exact -- $opt -server {
		set TkGnats(ServerInfo) $val
            } -categories {
		set TkGnats(CategoryList) $val
            } -submitters {
                set TkGnats(SubmitterList) $val
            } -responsible {
                set TkGnats(ResponsibleFile) $val
            } -classes {
                set TkGnats(ClassesFile) $val
            } -states {
                set TkGnats(StatesFile)  $val
            } -prid {
                set prid $val
            } -raw {
                set rawflag $val
	    } default {
		tkviewpr_usage "Illegal option pair:\n'$opt $val'\n\n"
	    }
	}
    }

    if {![info exists TkGnats(ServerInfo)]} {
        tkviewpr_usage "No -server argument given.\n\n"
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

proc tkviewpr_raw {prid prtext} {
    global TkGnats env
    
    wm minsize . 100 100
    
#    set f [frame .top]
#    pack  .top -expand true -fill both
#    set f .top
    
    view_menu_bar $prid
    
    .mframe.help.m add command -label "Cut, Copy, Paste Operations" \
            -command "helpMsg Cut_Copy_Paste"
    .mframe.help.m add separator
    .mframe.help.m add command -label "View Configuration Variables" \
        -command "helpMsg TkGnats_Variables"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Changes" \
            -command "helpMsg Changes"
    .mframe.help.m add command -label "About" \
            -command "helpMsg TkGnats_About"
    
    view_action_bar $prid

    scrollbar .sb -command ".text yview" -relief sunken
    pack .sb -side left -fill y
    
    text .text \
            -font $TkGnats(textfont) \
            -yscrollcommand ".sb set" \
            -height 30 -width 80 -relief sunken -padx 4 -insertwidth 1 \
            -insertofftime 400 -borderwidth 2 -background [.action.close cget -background]
    pack .text -side right -expand true -fill both
    set_focus_style .text

    .text insert 1.0 $prtext
    .text configure -state disabled
    bind .text <3> "clipboard_post .text %X %Y"
    
    wm title . "TkGnats - View Problem Report: $prid"
    wm iconbitmap . @$TkGnats(lib)/tkeditpr.xbm
    wm iconname   . "$TkGnats(LogName)'s tkviewpr $prid"
}

# bail out completely
proc close_report {} {
    #Exit 0
    destroy .
}

proc fillfrm {} {
    global Tkviewpr flds frm
    ### pre-set editable PR values to values currently in the PR

    # now the multi line textual flds
    foreach tag $Tkviewpr(multitextflds) {
	if {[info exists  flds($tag)]} {
	    set flds($tag) [string trim $flds($tag) "\n"]
	    set frm($tag) $flds($tag)
	} {
	    set frm($tag) "\n"
	    set flds($tag) "\n"
	}
    }
}

proc headingMsg {a} {
    .action.msg configure -text $a
    update idletasks
}

proc editSelection_cmd {prid} {
    global TkGnats
    busy_cursor set
    update idletasks
    headingMsg "Please Wait..."
    TkGnats_exec $TkGnats(WISHPATH) $TkGnats(lib)/tkeditpr.tcl -prid $prid \
            -server $TkGnats(ServerInfo) \
            -classes $TkGnats(ClassesFile) -states $TkGnats(StatesFile) \
            -categories $TkGnats(CategoryList) -submitters $TkGnats(SubmitterList) \
            -responsible $TkGnats(ResponsibleFile) &
    schedule_reap
    after 4000
    busy_cursor clear
    close_report
}
    
proc view_menu_bar {prid} {
    global TkGnats flds
    frame      .mframe -borderwidth 1 -relief raised
    pack       .mframe -side top -fill x

    menubutton .mframe.file -text "File" -menu .mframe.file.m -underline 0
    menu       .mframe.file.m
    .mframe.file.m add command -label "Edit Report..." -command "editSelection_cmd $prid"
    if {! $TkGnats(edit_authorized)} {
        .mframe.file.m entryconfigure "Edit Report..." -state disabled
    }
    .mframe.file.m add command -label "Send Email..."  -command \
            "email_originator [list [ftrim $flds(X-GNATS-Notify)]] \
            [list [ftrim $flds(>Responsible)]] [list [ftrim $flds(Reply-To)]] \
            [ftrim $flds(>Category)]/$prid [list [ftrim $flds(>Synopsis)]]"
    .mframe.file.m add separator
    .mframe.file.m add command -label "Close"          -command close_report
    
    menubutton .mframe.edit -text "Edit" -menu .mframe.edit.m -underline 0
    menu       .mframe.edit.m
    .mframe.edit.m     configure -disabledforeground [.mframe.edit.m cget -foreground]
    .mframe.edit.m add command -label "Use right mouse button for Cut/Copy/Paste" -state disabled
    .mframe.edit.m add separator
    .mframe.edit.m add command -label "Fonts..." -command "edit_fonts"
    
    pack .mframe.file .mframe.edit -side left
    
    menubutton .mframe.help -text "Help" -menu .mframe.help.m -underline 0
    menu       .mframe.help.m
    pack       .mframe.help -side right
}

proc view_action_bar {prid} {
    global TkGnats flds
    frame  .action -borderwidth 1 -relief raised
    pack   .action -side top -fill x -anchor w
    button .action.close -borderwidth 1 -text "Close"   -command close_report
    button .action.edit  -borderwidth 1 -text "Edit..." -command "editSelection_cmd $prid"
    button .action.email -borderwidth 1 -text "Send Email..." \
            -command "email_originator [list [ftrim $flds(X-GNATS-Notify)]] \
            [list [ftrim $flds(>Responsible)]] [list [ftrim $flds(Reply-To)]] \
            [ftrim $flds(>Category)]/$prid [list [ftrim $flds(>Synopsis)]]"
####       [lrange [split [ftrim $flds(>Originator)]] 0 0]
    
    pack .action.close .action.edit .action.email -side left -padx 0
    if {! $TkGnats(edit_authorized)} {
        .action.edit configure -state disabled
    }
    
    message .action.msg -aspect 10000 -relief sunken -bd 1 -text ""
    pack    .action.msg -side left -fill x -expand 1
}

proc tkviewpr_formatted {prid} {
    global TkGnats Tkviewpr flds env current_multi_text

    set Tkviewpr(shortflds) {
	>Cost
	>Release
	>Responsible
    }

    set Tkviewpr(radioflds) {
        >State
        >Confidential
        >Severity
        >Priority
        >Class
    }
    
    set Tkviewpr(multitextflds) {
        >Description
        >How-To-Repeat
        >Environment
	>IPsec-look
        >Fix
        >Audit-Trail
        >Unformatted
        >Release-Note
        >Organization
    }
    
    set current_multi_text ""
    
    # Get the maximum width of the value fields for the listboxes and radio (enumerated) fields
    set Tkviewpr(value_width) 0
    foreach tag [concat >Category >Submitter-Id >Responsible $Tkviewpr(radioflds)] {
        set f [ftrim $flds($tag)]
        if {[string length $f] > $Tkviewpr(value_width)} {
            set Tkviewpr(value_width) [string length $f]
        }
    }
    incr Tkviewpr(value_width)

    # Get the maximum width of the value fields for the date fields
    set Tkviewpr(datevalue_width) 0
    set flist {>Last-Modified >Arrival-Date >Closed-Date}
    foreach tag $flist {
        set f [ftrim $flds($tag)]
        if {[string length $f] > $Tkviewpr(datevalue_width)} {
            set Tkviewpr(datevalue_width) [string length $f]
        }
    }
    incr Tkviewpr(datevalue_width)

    # Get the maximum width of the value fields for the readonly_singletext fields
    set Tkviewpr(singletextvalue_width) 0
    set flist {>Originator X-GNATS-Notify >Release >Synopsis}
    if {$TkGnats(ReleaseBased)} {
        lappend flist >Keywords >$TkGnats(Quarter) >Date-Required
    }
    foreach tag $flist {
        set f [ftrim $flds($tag)]
        if {[string length $f] > $Tkviewpr(singletextvalue_width)} {
            set Tkviewpr(singletextvalue_width) [string length $f]
        }
    }
    incr Tkviewpr(singletextvalue_width)

    fillfrm

    view_menu_bar $prid
    
    .mframe.help.m add command -label "Overview" \
            -command "helpMsg View_Overview"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Cut, Copy, Paste Operations" \
            -command "helpMsg Cut_Copy_Paste"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Field Definitions" \
            -command "helpMsg Field_Definitions"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Text Fields (Description, etc)" \
            -command "helpMsg View_Text_Fields"
    .mframe.help.m add separator
    .mframe.help.m add command -label "View Configuration Variables" \
        -command "helpMsg TkGnats_Variables"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Changes" \
            -command "helpMsg Changes"
    .mframe.help.m add command -label "About" \
            -command "helpMsg TkGnats_About"
    
    view_action_bar $prid

    set flist [list Arrival-Date Last-Modified Closed-Date Class State Priority Severity Confidential Category Submitter-Id Responsible Originator X-GNATS-Notify Release Synopsis IPsec-barf-location Cost]
    if {$TkGnats(ReleaseBased)} {
        lappend flist  Keywords $TkGnats(Quarter) Date-Required
    }
    
    foreach f $flist {
        if {[check_suppressed_field $f] == 1} {
            continue
        }
	#set tmptag [get_field_alias $f]
        switch $f {
            Class -
            State -
            Priority -
            Severity -
            Confidential -
            Category -
            Submitter-Id -
            Responsible {
                readonly_singletext $f [ftrim $flds(>$f)] 14 $Tkviewpr(value_width)
            }
            Arrival-Date -
            Last-Modified -
            Closed-Date {
                readonly_singletext $f [ftrim $flds(>$f)] 14 $Tkviewpr(datevalue_width)
            }
            X-GNATS-Notify {
                readonly_singletext $f [ftrim $flds($f)]  14 $Tkviewpr(singletextvalue_width)
            }
            default {
                readonly_singletext $f [ftrim $flds(>$f)] 14 $Tkviewpr(singletextvalue_width)
            }
        }
    }
    
    make_txt_mb $Tkviewpr(multitextflds)
    destroy .mb.insert
    .multiline.text configure -height 16
    bind multiline.text <Enter> ""

    wm title . "TkGnats - [lindex $TkGnats(ServerInfo) 0] - View Problem Report: [ftrim $flds(>Category)]/[ftrim $flds(>Number)]"
    wm iconbitmap . @$TkGnats(lib)/tkeditpr.xbm
    wm iconname   . "$TkGnats(LogName)'s tkviewpr [ftrim $flds(>Number)]"
    switch_txt $TkGnats(first_multitext) $Tkviewpr(multitextflds)
}

set prid    ""
set rawflag ""

tkviewpr_process_args

foreach f { tkpr_library.tcl tkprhelp.tcl tkprfont.tcl } {
    source $TkGnats(lib)/$f
}

# If TkGnats(GNATS_ACCESS_METHOD) != "socket" then this does nothing
if {[open_socket_gnatsd 1] == "-1"} {
    exit
}

if {[get_gnats_config] == "-1"} {
    exit
}

#TTD remove me after testing
#set TkGnats(ReleaseBased) 1

if {"$prid" == ""} {
    wm withdraw .
    Msg "Missing prid argument!"
    close_socket_gnatsd 1
    Exit -1
}

# load the report fields

set prtxt [get_pr_full_text $prid]
parsepr_txt  $prtxt flds
load_field_defaults flds

if {"$rawflag" != ""} {
    tkviewpr_raw $prid $prtxt
} {
    tkviewpr_formatted $prid
}

#tkwait window .

close_socket_gnatsd 1

