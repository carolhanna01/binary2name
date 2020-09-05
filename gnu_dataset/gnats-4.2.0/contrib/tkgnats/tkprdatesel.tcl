proc {datesel} {{date {}} {title {Select Date}} {font fixed}} {
    global dsel
    set    dsel(return) ""
    catch  {destroy .dsel}

    #TTD remove this if the bug is ever fixed
    datesel_CheckTclClockBug
    
    set t  $date
    # convert yy-mm-dd format coming in
    if {[regexp {^[0-9][0-9]?[0-9]?[0-9]?-[0-9][0-9]?-[0-9][0-9]?([ \t]?[0-9][0-9]?:[0-9][0-9]?)?$} $t]} {
        set t [datesel_ConvertDateFormat $t]
    }
    if {$date == "" || [catch {clock scan $t}] } {
        set dsel(time) [clock seconds]
    } {
        set dsel(time) [clock scan $t]
    }
    set dsel(startdate) [clock format $dsel(time) -format "%Y-%m"]
    
    datesel_Create  .dsel $title $dsel(time) $font
    grab    .dsel
    tkwait  variable dsel(return)
    grab    release .dsel
    destroy .dsel
    return $dsel(return)
}

proc {datesel_ChangeMonth} {opt} {
    global dsel
    # Tcl bug! "next month" and "next year" adds 2 months or years,
    # so we have to go back 1 then forward 2.
    switch $opt {
        m {
            set dsel(time) [clock scan "last month" -base $dsel(time)]
        }
        h {
            if {$dsel(clockbug)} {
                set dsel(time) [clock scan "last month" -base $dsel(time)]
            }
            set dsel(time) [clock scan "next month" -base $dsel(time)]
        }
        y {
            set dsel(time) [clock scan "last year" -base $dsel(time)]
        }
        r {
            if {$dsel(clockbug)} {
                set dsel(time) [clock scan "last year" -base $dsel(time)]
            }
            set dsel(time) [clock scan "next year" -base $dsel(time)]
        }
    }
    datesel_SetMonth $dsel(time)
}

proc datesel_CheckTclClockBug {} {
    global dsel
    #TTD remove this if the bug is ever fixed
    set t  [clock scan "01/01/1999"]
    set mm [clock format $t -format "%m"]
    if {[clock format [clock scan "next month" -base $t] -format "%m"] == [expr $mm + 1]} {
        set dsel(clockbug) 0
    } {
        set dsel(clockbug) 1
    }
}

proc {datesel_ConvertDateFormat} {date} {
    set dt  [split $date]
    set d   [split [lindex $dt 0] -]
    return "[lindex $d 1]/[lindex $d 2]/[lindex $d 0] [lindex $dt 1]"
}

proc {datesel_Return} {dd} {
    global dsel
    if {$dd == ""} {
        set dsel(return) ""
    } {
        set dsel(return) "$dsel(YY)-$dsel(mm)-[format "%02d" $dd]"
    }
}

proc {datesel_SetMonth} {t} {
    global dsel
    set dsel(title) [clock format $t -format "%B - %Y"]
    set dsel(mm)    [clock format $t -format "%m"]
    set dsel(YY)    [clock format $t -format "%Y"]
    set fdm         [clock format [clock scan "$dsel(mm)/01/$dsel(YY)"] -format "%w"]
    # get the first day of next month
    set nm $dsel(time)
    if {$dsel(clockbug)} {
        # Tcl bug! "next month" and "next year" adds 2 months or years,
        # so we have to go back 1 then forward 2.
        set nm [clock scan "last month" -base $nm]
    }
    set nm [clock scan "next month" -base $nm]
    set mm [clock format $nm -format "%m"]
    set YY [clock format $nm -format "%Y"]
    set nm [clock scan "$mm/01/$YY"]
    # get last day of this month
    set ld [clock scan "yesterday"  -base $nm]
    # get number of days this month
    set nd [string trimleft [clock format $ld -format "%d"] 0]
    for {set d 1} {$d <= $fdm} {incr d} {
        $dsel(datesel_$d) config -text ""    -relief flat -state disabled
    }
    for {set date 1} {$date <= $nd} {incr date} {
        $dsel(datesel_$d) config -text $date -relief flat -state normal \
                -command "datesel_Return $date"
        incr d
    }
    for {set d $d} {$d < 42} {incr d} {
        $dsel(datesel_$d) config -text ""    -relief flat -state disabled
    }
    if {$dsel(startdate) == [clock format $dsel(time) -format "%Y-%m"]} {
        set but [expr $fdm + [string trimleft [clock format $dsel(time) -format "%d"] 0]]
        $dsel(datesel_$but) config -relief sunken
    }
}

proc datesel_Create {base title time font} {
    global dsel TkGnats

    set master    .
    toplevel      $base -class Toplevel
    wm  title     $base $title
    wm  transient $base $master

    set xpos [expr [winfo rootx $master]+[winfo width  $master]/2]
    set ypos [expr [winfo rooty $master]+[winfo height $master]/2]
    wm geometry $base +${xpos}+${ypos}

    frame  $base.cal         -height 75 -width 125 
    frame  $base.cal.head    -height 75 -width 125 -borderwidth 1 -relief raised
    frame  $base.cal.head.t1 -height 75 -width 125 -borderwidth 2
    button $base.cal.head.t1.y -command {datesel_ChangeMonth y} -font $font \
        -highlightthickness 0  -padx 8 -pady 0 -relief groove -text << -width 1 
    button $base.cal.head.t1.r -command {datesel_ChangeMonth r} -font $font \
        -highlightthickness 0  -padx 8 -pady 0 -relief groove -text >> -width 1 
    button $base.cal.head.t1.m -command {datesel_ChangeMonth m} -font $font \
        -highlightthickness 0  -padx 8 -pady 0 -relief groove -text <  -width 1 
    button $base.cal.head.t1.h -command {datesel_ChangeMonth h} -font $font \
        -highlightthickness 0  -padx 8 -pady 0 -relief groove -text >  -width 1 
    label  $base.cal.head.t1.t -width 16 \
        -background gray85 -borderwidth 1 -font $font -textvariable dsel(title) 

    pack $base.cal           -anchor center -expand 1 -fill both -side top 
    pack $base.cal.head      -anchor center -expand 1 -fill both -side top   -padx 1
    pack $base.cal.head.t1   -anchor center -expand 1 -fill both -side top 
    pack $base.cal.head.t1.y -anchor center -expand 0 -fill none -side left 
    pack $base.cal.head.t1.r -anchor center -expand 0 -fill none -side right 
    pack $base.cal.head.t1.m -anchor center -expand 0 -fill none -side left  -padx 2
    pack $base.cal.head.t1.t -anchor center -expand 1 -fill x    -side left 
    pack $base.cal.head.t1.h -anchor center -expand 0 -fill none -side right -padx 2

    frame $base.cal.head.t2  -borderwidth 2 -height 75 -width 125
    pack  $base.cal.head.t2  -anchor center -expand 1 -fill both -side top 
    set d 0
    foreach day "Su Mo Tu We Th Fr Sa" {
        incr d
        label $base.cal.head.t2.$d -background gray85 -borderwidth 1 \
                -font $font -text $day -width 1
        pack  $base.cal.head.t2.$d -anchor center -expand 1 -fill x -side left 
    }

    set d 0
    foreach week "w1 w2 w3 w4 w5 w6" {
        frame $base.cal.$week -height 75 -width 125
        pack  $base.cal.$week -anchor center -expand 1 -fill both -padx 3 -side top 
        for {set day 1} {$day <= 7} {incr day} {
            incr d
            set   dsel(datesel_$d) [button $base.cal.$week.$day \
                    -borderwidth 1 -font $font -highlightthickness 0 -padx 1 \
                    -pady 1 -relief flat -width 1]
            pack $dsel(datesel_$d) -anchor center -expand 1 -fill both -side left 
        }
    }
    $dsel(datesel_42) config -text Exit -command {datesel_Return ""} -relief raised

    datesel_SetMonth $time
}
