proc putlines {fout lst} {
    foreach ln $lst {
	puts $fout $ln
##        puts "$ln"
    }
}

proc putfld {fout f tag {prefix ""}} {
    upvar 1 $f flds
##    set data [string trimright [string trim $flds($tag) " \t"] "\n"]
    set data [ftrim $flds($tag)]
    if {"$prefix" == ""} {
	set prefix [string trimright [string trimleft $tag >] :]
    }
    puts $fout ".ti +.1i"
    puts $fout "${prefix}:%" nonewline
    puts $fout "T{\n.nf"
    puts $fout $data
    puts $fout ".fi\nT}"
}

proc putfldvalbold {fout f tag} {
    upvar 1 $f flds
##    set data [string trimright [string trim $flds($tag) " \t"] "\n"]
    set data [ftrim $flds($tag)]
    puts $fout "\\fB$data\\fR" nonewline
}

proc putfldval1 {fout f tag} {
    upvar 1 $f flds
##    set data [string trimright [string trim $flds($tag) " \t"] "\n"]
    set data [ftrim $flds($tag)]
    puts $fout "$data" nonewline
}

proc putfldval2 {fout f tag} {
    upvar 1 $f flds
##    set data [string trimright [string trim $flds($tag) " \t"] "\n"]
    set data [ftrim $flds($tag)]
    puts $fout "T{\n.na"
    puts $fout $data
    puts $fout ".ad\nT}"
}

proc endtbl {fout} {
    putlines $fout { {.TE} }
}

proc starttbl {fout} {
    putlines $fout { {.sp} {.sp} {.TS} {box, expand, tab(%);} }
}

proc heading {fout s} {
    putlines $fout [list $s _]
}

proc freeform_text {fout f tag {prefix ""}} {
    upvar 1 $f flds
    if {![info exists flds($tag)]} {
	return
    }
    set data [string trimright [string trim $flds($tag) " \t"] "\n"]

    if {[string compare "$data" ""] == 0} {
        return
    }
    
    if {"$prefix" == ""} {
	set prefix [string trimright [string trimleft $tag >] :]
    }
    starttbl $fout
    putlines $fout [list {Cbp12w(7.75i).} $prefix]
    endtbl $fout

    putlines $fout { {.sp} {.nf} }
    puts $fout $data
    putlines $fout { {.fi} }
}

proc freeform_text2 {fout f tag} {
    upvar 1 $f flds
    if {![info exists flds($tag)]} {
	return
    }
    set data [string trimright [string trim $flds($tag) " \t"] "\n"]

    if {[string compare "$data" ""] == 0} {
        return
    }

    puts $fout "T{\n.nf"
    puts $fout $data
    puts $fout ".fi\nT}"
}

proc Summary {prid ln} {
    global Print
    
    headingMsg "Doing $prid..."

    set fout    $Print(fout)
    set fields  $Print(fields)
    set widths  $Print(widths)
    set nfields [llength $fields]

    if {$Print(first_time) == 1} {
        # title section
        set dat "[clock format [clock seconds] -format "%a %b %e %H:%M %Y"]"
        putlines $fout { {.po 0.375i} {.ll 7.75i} }
        putlines $fout { {.TS} {expand, tab(%);} }
        putlines $fout [list {Cbp12w(7.75i).} "Problem Report Summary   --   $dat" {.TE}]
        puts     $fout ".sp"
        
        # format section
        putlines $fout {
            {.ds CF "                \\n(yr / \\n(mo / \\n(dy}
            {.TS H}
            {box;}
        }
        ##	{.sp 1}
        set headspec "Cb"
        set dataspec "N"
        set heading [lindex  $fields 0]
        for {set i 1} {$i < $nfields} {incr i} {
            append headspec "|Lb"
            append dataspec "|L"
            append heading "\t[lindex $fields $i]"
        }
        if {[lsearch -exact $fields "Synopsis"] >= 0} {
            set wid [expr 7.625 - [expr 0.75 * [expr $nfields - 1]]]
            if {$wid < 1.0} {
                set wid 1.0
            }
            append headspec "w(${wid}i)"
        }
        putlines $fout [list "$headspec" "$dataspec."]
        
        # table heading
        putlines $fout [list "$heading" {=} {.TH}]
    }

    set end -2
    for {set i 0} {$i < [expr $nfields - 1]} {incr i} {
        set beg [expr $end + 2]
        set end [expr $end + 1 + [lindex $widths $i]]
        if {$i == 0} {
            puts $fout "\\fB[string range $ln $beg $end]\\fR\t" nonewline
        } {
            puts $fout "[string trim [string range $ln $beg $end]]\t" nonewline
        }
    }
    set beg [expr $end + 2]
    puts $fout "T{\n.na\n[string trim [string range $ln $beg end]]\n.ad\nT}"
    puts $fout "_"
    
    if {$Print(last_time) == 1} {
        # trailer
        puts $fout ".TE"
    }
    return 0
}

proc Medium {prid ln} {
    global Print TkGnats
    
##    workingMsg
    headingMsg "Doing $prid..."

    if {$Print(first_time) == 1} {
        # title section
        set dat "[clock format [clock seconds] -format "%a %b %e %H:%M %Y"]"
        putlines $Print(fout) { {.po 0.375i} {.ll 7.75i} }
        putlines $Print(fout) { {.TS} {box, expand, tab(%);} }
        putlines $Print(fout) [list {Cbp12w(7.75i).} "Problem Report Summary   --   $dat" {.TE}]
        puts     $Print(fout) ".sp 2"
        # format section
        putlines $Print(fout) { {.ds CF "                \\n(yr / \\n(mo / \\n(dy} }
    }
        
    puts     $Print(fout) ".nf"
    puts     $Print(fout) [get_pr_medium_text $prid]
    puts     $Print(fout) "============================================================\n"
    puts     $Print(fout) ".fi"
    return 0
}

proc Full {prid ln} {
    global Print TkGnats
    
##    workingMsg
    headingMsg "Doing $prid..."

    set fout $Print(fout)
    print_parsepr $prid flds
    
    if {$Print(first_time) == 1} {
        # format section
        putlines $fout [list {.po 0.375i} {.ll 7.75i}]
        putlines $fout { {.ds CF "                \\n(yr / \\n(mo / \\n(dy} }
    }
    
    ###################################

    putlines $fout { {.TS} {doublebox, expand, tab(%);} }
    putlines $fout [list {Cbp12w(7.75i).} \
            "Problem Report  --  [string trimright [string trim $flds(>Category) " \t"] "\n"]/\
            [string trimright [string trim $flds(>Number) " \t"] "\n"]             \
            [clock format [clock seconds] -format "%a %b %e %H:%M %Y"]" {.TE}]

    ###################################

    starttbl $fout
    putlines $fout {
	{Cbp12 s}
	{Lbp10w(1.1i) Lw(6.65i).}
    }
    heading  $fout Identification
    putfld   $fout flds {>Synopsis}
    putfld   $fout flds {>Release}
    putfld   $fout flds {>Confidential}
    putlines $fout {.sp}

    putfld   $fout flds {>Arrival-Date}
    putfld   $fout flds {>Last-Modified}
    putfld   $fout flds {>Closed-Date}
    putfld   $fout flds {>Responsible}
    
    putlines $fout {.sp}
    putfld   $fout flds {>Class}
    putfld   $fout flds {>State}
    putfld   $fout flds {>Priority}
    putfld   $fout flds {>Severity}

    if {$TkGnats(ReleaseBased)} {
        putlines $fout {.sp}
        putfld   $fout flds ">$TkGnats(Quarter)"
        putfld   $fout flds {>Keywords}
        putfld   $fout flds {>Date-Required}
    }
    
    putlines $fout {.sp}
    putfld   $fout flds {>Originator}
    putfld   $fout flds {>Submitter-Id}
    putfld   $fout flds {>Organization}
    putfld   $fout flds {>Environment}
    endtbl   $fout

    ###################################

    freeform_text $fout flds {>Description}
    freeform_text $fout flds {>How-To-Repeat}
    freeform_text $fout flds {>Audit-Trail}
    freeform_text $fout flds {>Fix}
    freeform_text $fout flds {>Release-Note}
    freeform_text $fout flds {>Unformatted}

    ###################################

    if {$Print(last_time) != 1} {
        puts $fout ".sp 2"
    }

    return 0
}

proc Raw_Data {prid ln} {
    global Print
    
##    workingMsg
    headingMsg "Doing $prid..."

    if {$Print(first_time) == 1} {
        # title section
        set dat "[clock format [clock seconds] -format "%a %b %e %H:%M %Y"]"
        putlines $Print(fout) { {.po 0.375i} {.ll 7.75i} }
        putlines $Print(fout) { {.TS} {box, expand, tab(%);} }
        putlines $Print(fout) [list {Cbp12w(7.75i).} "Problem Report Summary   --   $dat" {.TE}]
        puts     $Print(fout) ".sp 2"
        # format section
        putlines $Print(fout) { {.ds CF "                \\n(yr / \\n(mo / \\n(dy} }
    }

    puts     $Print(fout) ".nf"
    puts     $Print(fout) [get_pr_full_text $prid]
    puts     $Print(fout) "============================================================\n"
    puts     $Print(fout) ".fi"
    return 0
}

proc print_listbox {lbname procname} {
    global Print
    if {[string compare $Print(Select) "all"] == 0} {
        set Print(first_time) 1
        set Print(last_time)  0
        set sz [$lbname size]
        set Print(num_ids) $sz
        for {set x 0} {$x < $sz} {incr x 1} {
            if {$x == [expr $sz - 1]} {
                set Print(last_time) 1
            }
            set ln   [$lbname get $x]
            set prid [pridfromsummaryline $ln]
            if {[$procname $prid $ln] != 0} {
                return
            }
            set Print(first_time) 0
        }
    } {
        set Print(first_time) 1
        set Print(last_time)  1
        set Print(num_ids)    1
        set ln   [selln $lbname]
        set prid [pridfromsummaryline $ln]
        $procname $prid $ln
    }
}

proc print_parsepr {prid varname} {
    upvar 1 $varname flds
    set prtext  [get_pr_full_text $prid]
    parsepr_txt $prtext flds
}

proc print_parsepr_medium {prid varname} {
    upvar 1 $varname flds
    set prtext  [get_pr_medium_text $prid]
    parsepr_txt $prtext flds
}

proc print_dialog_SetDevice {a b c} {
    global Print TkGnats

    if {"$Print(Device)" == "file"} {
        .print.d.f     configure -foreground [.print.d.file cget -foreground]
        .print.d.e     configure -state normal -background $TkGnats(EditFieldBackground)
    } {
        .print.d.f     configure -foreground [.print.d.file cget -disabledforeground]
        .print.d.e     configure -state disabled -background [.print.d.file cget -background]
    }
}

proc print_dialog {} {
    global Print TkGnats env

    set w [toplevel .print]
    wm title      $w {TkGnats - Print Configuration}
    wm iconbitmap $w  @$TkGnats(lib)/tkgnats.xbm
    wm iconname   $w "$TkGnats(LogName)'s TkGnats Print Config"
    
    ####
    
    frame  $w.d -borderwidth 5
    pack   $w.d -anchor w

    label  $w.d.l -text "Device:" -anchor w -width 7
    pack   $w.d.l -side left -anchor w
    
    foreach b {printer previewer file} {
        radiobutton $w.d.$b -text $b -relief flat -variable Print(Device) -value $b
        pack $w.d.$b -side left
    }
    
    label  $w.d.f -text "    Filename:" -anchor w
    pack   $w.d.f -side left
    
    entry  $w.d.e -width 32 -textvariable Print(savefile) -relief sunken -highlightthickness 2
    pack   $w.d.e -side left

    ####
    
    frame  $w.t -borderwidth 5
    pack   $w.t -anchor w

    label  $w.t.l -text "Format:" -anchor w -width 7
    pack   $w.t.l -side left -anchor w
    
    foreach b {ps ascii dvi latin1 troff} {
        radiobutton $w.t.$b -text $b -relief flat -variable Print(Format) -value $b
        pack $w.t.$b -side left
    }
    
    ####
    
    frame  $w.s -borderwidth 5
    pack   $w.s -anchor w

    label  $w.s.l -text "Select:" -anchor w -width 7
    pack   $w.s.l -side left -anchor w
    
    radiobutton $w.s.all -text "All Ids in Listbox"     -relief flat -variable Print(Select) \
            -value all
    radiobutton $w.s.sel -text "Current Selection Only" -relief flat -variable Print(Select) \
            -value select
    pack   $w.s.all $w.s.sel -side left
    
    ####
    
    frame  $w.a -borderwidth 5
    pack   $w.a
    
    button $w.a.ok -text OK     -command "set Print(setup_done) 0"
    pack   $w.a.ok -side left  -padx 20
    
    button $w.a.no -text Cancel -command "set Print(setup_done) 1"
    pack   $w.a.no -side right -padx 20
    
    ####
    
    print_dialog_SetDevice a b c
    trace variable Print(Device) w print_dialog_SetDevice
        
    set err 1
    while {"$err" != 0} {
        grab    $w
        tkwait  variable Print(setup_done)
        grab    release $w

        if {$Print(setup_done) == 1} {
            break
        }
        
        set err 0
        case $Print(Device) previewer {
            if {[info exists Print(Previewer,$Print(Format))] == 0} {
                set err 1
            }
        } printer {
            if {[info exists Print(PrintSpooler,$Print(Format))] == 0} {
                set err 1
            }
        }
        if {$err == 1} {
            Msg "Print Configuration Error!\n" \
                    "Sorry, there is no $Print(Device) configured for $Print(Format)."
        }
    }

    destroy $w
    return  $Print(setup_done)
}
