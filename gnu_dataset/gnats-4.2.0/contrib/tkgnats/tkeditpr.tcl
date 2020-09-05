proc tkeditpr_usage {{str ""}} {
    bell
    wm withdraw .
    tk_dialog .tkgnatsError "TkGnats Error" \
            "${str}usage: tkeditpr -prid nnnn -server 'ServerInfo' \[-categories 'list'\] \
            \[-classes 'list'\] \[-states 'list'\] \
            \[-submitters 'list'\] \[-responsible 'list'\]" "error" 0 "OK"
    exit
}

proc tkeditpr_process_args {} {
    global TkGnats Tkeditpr argc argv env

    set TkGnats(CurrentProgram) tkeditpr
    
    if {$argc != 0} {
        if {$argc%2 != 0} {
            tkeditpr_usage
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
                set Tkeditpr(prid) $val
            } default {
                tkeditpr_usage "Illegal option pair:\n'$opt $val'\n\n"
            }
        }
    }

    if {![info exists TkGnats(ServerInfo)]} {
        tkeditpr_usage "No -server argument given.\n\n"
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

proc headingMsg {a {flash 1}} {
    .action.msg configure -text $a
    update idletasks
    if {$flash} {
        foreach rep {1 2 3 4 5} {
            foreach r {raised sunken flat} {
                .action.msg configure -relief $r;
                update idletasks
                after 50
            }
        }
    }
}

proc merge_into_list {lname new_value {omit_value {}}} {
    upvar 1 $lname l
    set omit_addr [lindex [extract_email_address $omit_value] 0]
    # set omit_name [lindex [extract_email_address $omit_value] 1]
    set adds  [split $new_value ,]
    set nadds [llength $adds]
    for {set i 0} {$i < $nadds} {incr i} {
        set add      [string trim [lindex $adds $i]]
        set new_addr [lindex [extract_email_address $add] 0]
        # set new_name [lindex [extract_email_address $add] 1]
        if {"$new_addr" != "$omit_addr"} {
            if {[lsearch $l $new_addr] < 0} {
                lappend l $new_addr
            }
        }
    }
}

proc file_report {} {
    global TkGnats Tkeditpr flds
    set stat [real_file_report]
    if {$stat == 1} {
        headingMsg "" 0
        return
    }
    if {$stat == -1} {
        #headingMsg "Error filing report!"
        headingMsg "" 0
        return
    }
    bind . <Destroy> ""
    unlock_pr $Tkeditpr(prid)
    exit
}

proc real_file_report_batch {rep} {
    global TkGnats Tkeditpr
    upvar 1 $rep errs
    
    if {[catch {set fout [eval open \"|$TkGnats(pr-edit) $TkGnats(UseridPassword)\" w]} errs]} {
        Msg "Error executing \"$TkGnats(pr-edit)\" to update PRID $Tkeditpr(prid):\n" "$errs"
        return -1
    }

    write_pr $fout

    return [catch {close $fout} errs]
}

proc real_file_report_socket {rep} {
    global TkGnats Tkeditpr
    upvar 1 $rep errs

    set errs ""
    
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return -2
    }

    gnatsd_send $s "EDIT $Tkeditpr(prid)"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        set errs "GNATSD error sending EDIT command for PRID $Tkeditpr(prid):\n[join $rep \n]"
        return -1
    }

    write_pr $s
    #write_pr stdout
    puts     $s "."

    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        set errs "GNATSD error updating PRID $Tkeditpr(prid): [join $rep \n]"
        return -1
    }

    return 0
}

proc real_file_report {} {
    global TkGnats Tkeditpr flds frm errorCode whyText
    flush_singletext $Tkeditpr(singletextflds)
    flush_singletext $Tkeditpr(shorttextflds) .eboxs.shortones
    flush_multitext 

    #
    # do some local field checking..
    #
    
    headingMsg "Checking fields..." 0

    # Check mandatory fields for non-blank input
    foreach f [concat $Tkeditpr(radioflds) $Tkeditpr(listboxflds) $Tkeditpr(singletextflds) $Tkeditpr(multitextflds) $Tkeditpr(shorttextflds)] {
        if {[check_suppressed_field $f]} {
            continue
        }
	if {![check_mandatory_field $f]} {
	    continue
	}
        set  val [string trim $frm($f) " \n\t\f!:~-#_?"]
        if {$val == ""} {
            Msg "You must supply a value for '[get_field_alias $f]'."
            return -1
        }
    }

    # Check listbox fields
    foreach {field list} {Category Category Submitter-Id Submitter Responsible Responsible} {
        if {$frm(>$field) != "" && [lsearch -exact $TkGnats(${list}List) $frm(>$field)] < 0} {
            Msg "You have specified an invalid [get_field_alias $field]: $frm(>$field)"
            return -1
        }
    }

    # Check Date-Required field...
    if {$TkGnats(ReleaseBased)} {
        if {[check_date_invalid $frm(>Date-Required) "Invalid [get_field_alias Date-Required]:"]} {
            return -1
        }
        set frm(>Date-Required) [normalize_date $frm(>Date-Required)]
    }
    
    set requireAudit  0
    set requireEmail  0
    set requireReason 0
    
    set    Audit_text "Changed-When:\t[clock format [clock seconds]]\n"
    append Audit_text "Changed-By:\t$TkGnats(LogName) \"$TkGnats(FullName)\" <[lindex [extract_email_address $TkGnats(EmailAddr)] 0]>\n"
    set    Email_text $Audit_text

    #
    # now see if any fields changed that trigger notifiers or audit records
    #
    
    set mail_list ""
    set changedfields ""

    set responsible_addr     [get_responsible_addr $frm(>Responsible)]
    set old_responsible_addr [get_responsible_addr $flds(>Responsible)]
 
    foreach t [concat $Tkeditpr(listboxflds) $Tkeditpr(radioflds)] {
	#dputs "Comparing frm($t) and flds($t): $frm($t) vs $flds($t)"
        if {$frm($t) != $flds($t)} {
            foreach require {Audit Email Reason} {
                if {[check_audit_trail_opts $require $t]} {
                    set  require$require 1
		    append changedfields "$t "
		    dputs "Field changed: $changedfields"
                    if {$require != "Reason"} {
                        append ${require}_text "$t-Changed-[get_field_alias $t]-From-To:\t$flds($t)->$frm($t)\n"
                    }
                }
            }
        }
    }

    foreach f [concat $Tkeditpr(singletextflds) $Tkeditpr(shorttextflds)] {
	# Check that singletext text fields do not have a | char. (not allowed in gnats index)
        if {[string first "|" $frm($f)] >= 0} {
            Msg " '|' is an illegal character for the '[get_field_alias $f]' field!"
            return -1
        }
	dputs "Comparing frm($f) and flds($f): $frm($f) vs $flds($f)"
        if {$frm($f) != $flds($f)} {
            foreach require {Audit Email Reason} {
                if {[check_audit_trail_opts $require $f]} {
                    set require$require 1
		    append changedfields "$f "
		    dputs "Field changed: $changedfields"
                    if {$require != "Reason"} {
                        set tmptag [get_field_alias $f]
                        append ${require}_text "$f-Changed-$tmptag-From:\t$flds($f)\n"
                        append ${require}_text "$f-Changed-$tmptag-To:  \t$frm($f)\n"
                    }
                }
            }
        }
    }

    foreach t $Tkeditpr(multitextflds) {
	#dputs "Comparing frm($t) and flds($t): $frm($t) vs $flds($t)"
        if {[string trim $frm($t)] != [string trim $flds($t)]} {
            foreach require {Audit Email Reason} {
                if {[check_audit_trail_opts $require $t]} {
                    set  require$require 1
		    append changedfields "$t "
		    dputs "Field changed: $changedfields"
                    if {$require != "Reason"} {
                        append ${require}_text "$t-Changed-[get_field_alias $t]\n"
                    }
                }
            }
        }
    }

    headingMsg "" 0

    if {$requireReason} {
        if {[catch {textEntryDialog "Enter reason for changes" "Return to Edit" $whyText 0 .} whyText]} {
            return 1
        }

	foreach f $changedfields {
	    #dputs "Field changed $f"
	    append frm($f-Changed-Why) "\n[string trim $whyText]\n"
	    append Email_text  "$f-Changed-Why:\t[string trim $whyText]\n"
	}
    }

    if {$requireAudit} {
        set dash "------------------------------------------------------------------\n"
        append frm(>Audit-Trail) "\n$dash$Audit_text$dash"
    }
    
    headingMsg "Filing report..." 0

    set done 0
    while {!$done} {
        set stat [real_file_report_$TkGnats(GNATS_ACCESS_METHOD) errs]
        #puts "stat=$stat errs=$errs"
        if {$stat == 0} {
            set done 1
        } {
            # If the failure is opening gnatsd socket, stat= -2
            if {$stat != -2} {
                # The first message is returned by gnatsd directly;
                # the second by the batch [n]pr-edit.
                if {[string first "currently locked" $errs] >= 0 || [string first "lock file exists" $errs] >= 0} {
                    set errs "The GNATS database is presently locked.\nPlease try again in a moment."
                }
            }
            set msg "Error filing $frm(>Category)/$Tkeditpr(prid):\n\n$errs"
            
            bell
            set rep [tk_dialog .tkquerypr_delete "Error Filing PR Changes" $msg \
                    "warning" 0 "Try Again" "Return to Edit"]
            if {$rep == "1"} {
                return -1
            }
        }
    }

    headingMsg "Done" 0
    
    #
    # Did any notifiable changes take place ??
    #
    
    if {$requireEmail} {
        # Send to the responsible person, original sender and the X-GNATS-Notify list
        merge_into_list mail_list $responsible_addr    $TkGnats(EmailAddr)
        merge_into_list mail_list $flds(Reply-To)      $TkGnats(EmailAddr)
	if {[info exists $frm(X-GNATS-Notify)]} {
	    merge_into_list mail_list $frm(X-GNATS-Notify) $TkGnats(EmailAddr)
	}
        if {$frm(>Responsible) != $flds(>Responsible)} {
            # Mail to the old responsible person too
            merge_into_list mail_list $old_responsible_addr $TkGnats(EmailAddr)
        }
        ##puts "maillist=$mail_list"
        
        if {"$mail_list" != ""} {
            headingMsg "Changes saved. Sending mail..." 0
            set addrs [add_email_domainname [join $mail_list ", "]]
            ##puts "maillist=$addrs"
            set mailtxt ""
            append mailtxt "From: $TkGnats(EmailAddr)\n"
            append mailtxt "Reply-To: $TkGnats(EmailAddr)\n"
            append mailtxt "To: $addrs\n"
            append mailtxt "Subject: Re: $frm(>Category)/$Tkeditpr(prid): Changed information\n\n"
            #TTD: add database name here!
            foreach f {Synopsis Priority Severity} {
                if {[check_suppressed_field $f]} {
                    continue
                }
                set alias [get_field_alias $f]
                append mailtxt "[get_field_alias $f]: $frm(>$f)\n"
            }
            #append mailtxt "Synopsis: $frm(>Synopsis)\n\n"
            #append mailtxt "Priority: $frm(>Priority)\n"
            #append mailtxt "Severity: $frm(>Severity)\n"
            append mailtxt \n$Email_text

            if {[TkGnats_sendmail $addrs $mailtxt] == "-1"} {
                Msg "Error sending mail notification.\n\nPR changes were saved."
            }
        }
    }
    set whyText ""
    return 0
}

proc TTDescape_dots_gnatsd {txt} {
    set txttmp [split $txt \n]
    set len    [llength $txttmp]
    for {set l 0} {$l < $len} {incr l} {
        set line [lindex $txttmp $l]
        # TTD: This is what RFC821 uses for SMTP, but we can't use this since gnatsd
        # doesn't remove the extra dot and you'd get another dot every time the PR is edited.
        # When gnatsd is fixed this proc goes away and we just need escape_dots.
        #if {[string match .* $line]}
        if {$line == "."} {
            #puts "escaping this line: $line"
            set txttmp [lreplace $txttmp $l $l .$line]
        }
    }
    set newlen [llength $txttmp]
    if {$len != $newlen} {
        Msg "Internal programming error escaping .'s in message body.\n" \
                "Lines in=$len; lines out=$newlen"
        return $txt
    }
    return [join $txttmp \n]
}

proc write_multitextfld {fout flds tag} {
    global TkGnats
    upvar 1 $flds f
    #set txt [string trimleft "$tag: \n[string trim $f($tag) "\n"]" "\n"]
    set txt [string trim "$tag: \n[string trim $f($tag) "\n"]" "\n"]
    if {$TkGnats(GNATS_ACCESS_METHOD) == "batch"} {
        puts $fout $txt
    } {
        puts $fout [escape_dots $txt]
    }
}

proc write_pr {fout} {
    global Tkeditpr flds frm

    foreach tag [array names frm] {
        set still_left($tag) $tag
    }

    #
    # for each parsed field from the PR form...
    #
    #puts "tags:$Tkeditpr(parsed_flds)"

    foreach tag $Tkeditpr(parsed_flds) {
        case $tag {_prefix_} {
            #
            #   The mail header, stored under the _prefix_ tag, is written out
            #   unadulterated, except for the edited X-GNATS-Notify field.
            #
	    #puts "prefix before:$flds($tag)"
            set lines [split $flds($tag) \n]
            set idx   [lsearch -regexp $lines "^X-GNATS-Notify:"]
	    
	    if {[info exists frm(X-GNATS-Notify)]} {
                set repto "X-GNATS-Notify: [lindex [split $frm(X-GNATS-Notify) ,] 0]"
		foreach addr [lrange [split $frm(X-GNATS-Notify) ,] 1 end] {
		    append repto ", [string trim $addr]"
		}
		set lines [lreplace $lines $idx [expr $idx + $flds(_prefix_len_X-GNATS-Notify) - 1] $repto\n]
	    }
            #set frm($tag) "[string trim [join $lines \n]]\n"
            set frm($tag) [join $lines \n]
	    #puts "prefix after:$flds($tag)"
            puts -nonewline $fout $frm($tag)
        } {X-GNATS-Notify} {
            #
            # Taken care of above with the mail header
            #
	    if {[info exists still_left($tag)]} {
		unset still_left($tag)
	    }
        } {>Unformatted} {
            #
            # Taken care of later in the function...
            #
            unset still_left($tag)
        } {>*} {
            # When writing out the fields
            #   first check for data present in the form (the frm bag)
            #   If not present use data read from the PR file (the flds bag)
            #
            if {[info exists frm($tag)]} {
                set data $frm($tag)
                unset still_left($tag)
            } {
                set data $flds($tag)
            }

            #
            # Write out fields
            #
            #   Multi line fields are newline trimmed to a single leading
            #   and trailing newline
            #
            #   Single line text fields are whitespace trimmed to a leading
            #   tab and a trailing newline
            #
            case $tag $Tkeditpr(singletextflds) {
                #puts $fout "$tag:\t[string trim [textget $tag] "\t\n "]"
                puts $fout "$tag:\t[string trim $frm($tag) "\t\n "]"
	    } $Tkeditpr(shorttextflds) {
                puts $fout "$tag:\t[string trim $frm($tag) "\t\n "]"
            } [concat >Category >Responsible >Submitter-Id $Tkeditpr(radioflds)] {
                puts $fout "$tag:\t$frm($tag)"
            } $Tkeditpr(multitextflds) {
                write_multitextfld $fout frm $tag
            } default {
                puts -nonewline $fout "$tag:$data"
            }
        }
    }

    #
    # now write any fields in the form that were not in the parsed report
    #
    foreach tag [array names still_left] {
        write_multitextfld $fout frm $tag
    }

    #
    # Finally, write the >Unformatted field
    #   (BUG: >Unformatted should not really be stripped)
    #
    write_multitextfld $fout frm ">Unformatted"
}

proc cancel_report {} {
    global Tkeditpr
    bind . <Destroy> ""
    unlock_pr $Tkeditpr(prid)
    exit
}

proc get_tkeditpr_listbox_height {} {
    global TkGnats Tkeditpr
    set ch [llength $TkGnats(CategoryList)]
    set sh [llength $TkGnats(SubmitterList)]
    set rh [llength $TkGnats(ResponsibleList)]
    set h $ch
    if {$h  < $sh } {
        set h $sh
    }
    if {$h  < $rh } {
        set h $rh
    }
    if {$h > 6 } {
        set h 6
    }
    set Tkeditpr(listbox_height) $h
}
    
proc edit_category_listbox {p {pat *}} {
    global TkGnats Tkeditpr flds frm
    set wid [expr 2 + [get_max_strlen $TkGnats(CategoryList)]]
    set alias [get_field_alias Category]
    frame  $p.cat -relief flat
    pack   $p.cat -side top -anchor w
    button $p.cat.lab -text "${alias}: " -width 14 -anchor w -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0 -highlightthickness 1
    frame  $p.cat.msg -relief flat
    label  $p.cat.msg.val -text "[ftrim $flds(>Category)]" -relief groove -anchor w \
            -width $wid -background $TkGnats(ReadOnlyBackground)
    pack $p.cat.msg.val  -side top   -fill both -expand true -anchor center
    pack $p.cat.lab $p.cat.msg -side left -anchor n
    if {[check_suppressed_field Category] == 2} {
        return ""
    }
    set ew [entry $p.cat.msg.ent -width $wid -insertwidth 1 -insertofftime 400 \
            -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground) -highlightthickness 2 \
            -textvariable frm(>Category)]
    lappend Tkeditpr(tlist) $ew
    set_focus_style $ew
    bind $ew <BackSpace> { tkEntrySetCursor %W [%W index insert] }
    bind $ew <Control-h> { tkEntrySetCursor %W [%W index insert] }
    scrollbar $p.cat.msg.sb -command "$p.cat.msg.list yview" -borderwidth 2 -relief sunken
    listbox $p.cat.msg.list -yscroll "$p.cat.msg.sb set" -setgrid 1 -relief sunken -borderwidth 2 \
            -width $wid -height $Tkeditpr(listbox_height) -exportselection false
    pack $p.cat.msg.ent  -side top   -fill both -expand true -anchor w
    pack $p.cat.msg.list -side left  -fill both -expand true
    pack $p.cat.msg.sb   -side right -fill y
    eval $p.cat.msg.list insert end $TkGnats(CategoryList)
    trace variable frm(>Category) w set_edit_category_ew
    bind   $p.cat.msg.list <B1-ButtonRelease> "set_edit_category $p.cat.msg %W %y"
    return $p.cat.msg.list
}

proc edit_responsible_listbox {p {pat *}} {
    global TkGnats Tkeditpr flds frm
    set wid [expr 2 + [get_max_strlen $TkGnats(ResponsibleList)]]
    set alias [get_field_alias Responsible]
    frame  $p.res -relief flat
    pack   $p.res -side top -anchor w
    button $p.res.lab -text "${alias}: " -width 14 -anchor w -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0 -highlightthickness 1
    frame  $p.res.msg -relief flat
    label  $p.res.msg.val -text "[ftrim $flds(>Responsible)]" -relief groove -anchor w \
            -width $wid -background $TkGnats(ReadOnlyBackground)
    pack $p.res.msg.val  -side top   -fill both -expand true -anchor center
    pack $p.res.lab $p.res.msg -side left -anchor n
    if {[check_suppressed_field Responsible] == 2} {
        return ""
    }
    set ew [entry $p.res.msg.ent -width $wid -insertwidth 1 -insertofftime 400 \
            -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground) -highlightthickness 2 \
            -textvariable frm(>Responsible)]
    lappend Tkeditpr(tlist) $ew
    set_focus_style $ew
    bind $ew <BackSpace> { tkEntrySetCursor %W [%W index insert] }
    bind $ew <Control-h> { tkEntrySetCursor %W [%W index insert] }
    scrollbar $p.res.msg.sb -command "$p.res.msg.list yview" -borderwidth 2 -relief sunken
    set height [llength $TkGnats(ResponsibleList)]
    if {$height > 6 } {
        set height 6
    }
    listbox $p.res.msg.list -yscroll "$p.res.msg.sb set" -setgrid 1 -relief sunken \
            -borderwidth 2 -width $wid -height $Tkeditpr(listbox_height) -exportselection false
    pack $p.res.msg.ent  -side top   -fill both -expand true -anchor w
    pack $p.res.msg.list -side left  -fill both -expand true
    pack $p.res.msg.sb   -side right -fill y
    eval $p.res.msg.list insert end $TkGnats(ResponsibleList)
    trace variable frm(>Responsible) w set_edit_responsible_ew
    bind   $p.res.msg.list <B1-ButtonRelease> "set_edit_responsible $p.res.msg %W %y"
    return $p.res.msg.list
}

proc edit_submitter-id_listbox {p {pat *}} {
    global TkGnats Tkeditpr flds frm
    set wid [expr 2 + [get_max_strlen $TkGnats(SubmitterList)]]
    set alias [get_field_alias Submitter-Id]
    frame  $p.sub -relief flat
    pack   $p.sub -side top -anchor w
    button $p.sub.lab -text "${alias}: " -width 14 -anchor w -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0 -highlightthickness 1
    frame  $p.sub.msg -relief flat
    label  $p.sub.msg.val -text "[ftrim $flds(>Submitter-Id)]" -relief groove -anchor w \
            -width $wid -background $TkGnats(ReadOnlyBackground)
    pack $p.sub.msg.val  -side top   -fill both -expand true -anchor center
    pack $p.sub.lab $p.sub.msg -side left -anchor n
    if {[check_suppressed_field Submitter-Id] == 2} {
        return ""
    }
    set ew [entry $p.sub.msg.ent -width $wid -insertwidth 1 -insertofftime 400 \
            -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground) \
            -highlightthickness 2 -textvariable frm(>Submitter-Id)]
    lappend Tkeditpr(tlist) $ew
    set_focus_style $ew
    bind $ew <BackSpace> { tkEntrySetCursor %W [%W index insert] }
    bind $ew <Control-h> { tkEntrySetCursor %W [%W index insert] }
    scrollbar $p.sub.msg.sb -command "$p.sub.msg.list yview" -borderwidth 2 -relief sunken
    set height [llength $TkGnats(SubmitterList)]
    if {$height > 6 } {
        set height 6
    }
    listbox $p.sub.msg.list -yscroll "$p.sub.msg.sb set" -setgrid 1 -relief sunken \
            -borderwidth 2 -width $wid -height $Tkeditpr(listbox_height) -exportselection false
    pack $p.sub.msg.ent  -side top   -fill both -expand true -anchor w
    pack $p.sub.msg.list -side left  -fill both -expand true
    pack $p.sub.msg.sb   -side right -fill y
    eval $p.sub.msg.list insert end $TkGnats(SubmitterList)
    trace variable frm(>Submitter-Id) w set_edit_submitter-id_ew
    bind   $p.sub.msg.list <B1-ButtonRelease> "set_edit_submitter-id $p.sub.msg %W %y"
    return $p.sub.msg.list
}

proc set_edit_category_ew {a b c} {
    upvar #0 $a f
    global TkGnats
    quickfill_entry_from_listbox f($b) .eboxs.clb.cat.msg.ent .eboxs.clb.cat.msg.list \
            $TkGnats(CategoryList)
}

proc set_edit_submitter-id_ew {a b c} {
    upvar #0 $a f
    global TkGnats
    quickfill_entry_from_listbox f($b) .eboxs.slb.sub.msg.ent .eboxs.slb.sub.msg.list \
            $TkGnats(SubmitterList)
}

proc set_edit_responsible_ew {a b c} {
    upvar #0 $a f
    global TkGnats
    quickfill_entry_from_listbox f($b) .eboxs.rlb.res.msg.ent .eboxs.rlb.res.msg.list \
            $TkGnats(ResponsibleList)
}

proc set_edit_category {msg w y} {
    global frm
    trace vdelete  frm(>Category) w set_edit_category_ew
    $msg.ent delete 0 end
    set idx [$w nearest $y]
    set frm(>Category) [$w get $idx]
    trace variable frm(>Category) w set_edit_category_ew
}

proc set_edit_responsible {msg w y} {
    global frm
    trace vdelete  frm(>Responsible) w set_edit_responsible_ew
    $msg.ent delete 0 end
    set idx [$w nearest $y]
    set frm(>Responsible) [$w get $idx]
    trace variable frm(>Responsible) w set_edit_responsible_ew
}

proc set_edit_submitter-id {msg w y} {
    global frm
    trace vdelete  frm(>Submitter-Id) w set_edit_submitter-id_ew
    $msg.ent delete 0 end
    set idx [$w nearest $y]
    set frm(>Submitter-Id) [$w get $idx]
    trace variable frm(>Submitter-Id) w set_edit_submitter-id_ew
}

proc fillfrm {} {
    global TkGnats Tkeditpr flds frm

    ### re-set editable PR values to values currently in the PR

    # clear listbox entry widgets
    trace vdelete  frm(>Category)     w set_edit_category_ew
    trace vdelete  frm(>Submitter-Id) w set_edit_submitter-id_ew
    trace vdelete  frm(>Responsible)  w set_edit_responsible_ew
    catch {.eboxs.clb.cat.msg.ent delete 0 end}
    catch {.eboxs.slb.sub.msg.ent delete 0 end}
    catch {.eboxs.rlb.res.msg.ent delete 0 end}
    trace variable frm(>Category)     w set_edit_category_ew
    trace variable frm(>Submitter-Id) w set_edit_submitter-id_ew
    trace variable frm(>Responsible)  w set_edit_responsible_ew

    # listboxes and radio (enumerated) fields
    foreach tag [concat $Tkeditpr(listboxflds) $Tkeditpr(radioflds)] {
	if {[info exists flds($tag)]} {
	    set flds($tag) [string trim $flds($tag) "\t\n "]
	} {
	    set flds($tag) ""
	}
        #unset frm($tag)
        catch {set frm($tag) $flds($tag)}
    }

    # now the 1 line textual flds
    foreach tag $Tkeditpr(singletextflds) {
	if {[info exists flds($tag)]} {
	    set flds($tag) [string trim $flds($tag) "\t\n "]
	} {
	    set flds($tag) ""
	}
        textset $tag $flds($tag)
    }

    # now the 1 line textual flds
    foreach tag $Tkeditpr(shorttextflds) {
	if {[info exists flds($tag)]} {
	    set flds($tag) [string trim $flds($tag) "\t\n "]
	} {
	    set flds($tag) ""
	}
	#puts stderr "Setting $tag to $flds($tag)"
	textset $tag $flds($tag) .eboxs.shortones
    }

    # now the multi line textual flds
    foreach tag $Tkeditpr(multitextflds) {
        if {[info exists flds($tag)]} {
            set flds($tag) [string trim $flds($tag) "\t\n "]
            set frm($tag) $flds($tag)\n
        } {
            set frm($tag)  "\n"
            set flds($tag) "\n"
        }
    }
    switch_txt $TkGnats(first_multitext) $Tkeditpr(multitextflds)
}

proc reset_report {} {
    global Tkeditpr flds current_multi_text
    # load the current text widget with the original text
    .multiline.text delete 1.0 end
    .multiline.text insert 1.0 $flds($current_multi_text)
    # reset everything
    set current $current_multi_text
    fillfrm
    switch_txt $current $Tkeditpr(multitextflds)
}

proc edit_email_originator {} {
    global Tkeditpr flds frm
    flush_singletext $Tkeditpr(singletextflds)
    flush_singletext $Tkeditpr(shorttextflds)

    set gn ""
    if {[info exists $frm(X-GNATS-Notify)]} {
	set gn [ftrim $frm(X-GNATS-Notify)]
    }
    email_originator $gn \
            [ftrim $frm(>Responsible)] [ftrim $flds(Reply-To)] \
            [ftrim $frm(>Category)]/$Tkeditpr(prid) [ftrim $frm(>Synopsis)]
}

proc edit_cleanup {} {
    global Tkeditpr
    bind . <Destroy> ""
    unlock_pr $Tkeditpr(prid)
    exit
}

proc edit_window {} {
    global TkGnats Tkeditpr flds frm env current_multi_text

    set prid $Tkeditpr(prid)
    
    set prtxt [lock_pr $prid]
    # if {$prtxt == ""} {
    #     exit 1
    # }

    # Unlock the PR if the user nukes the window
    bind . <Destroy> edit_cleanup
    
    get_tkeditpr_listbox_height

    set Tkeditpr(radioflds) {
        >State
        >Confidential
        >Severity
        >Priority
        >Class
    }
    
    set Tkeditpr(listboxflds) {
        >Category
        >Submitter-Id
        >Responsible
    }

    set Tkeditpr(shorttextflds) {
	>Release
	>Cost
	>XrefPR
    }
    
    set Tkeditpr(singletextflds) {
        >Originator
        >Synopsis
	>IPsec-barf-location
    }

    if {$TkGnats(ReleaseBased)} {
        lappend Tkeditpr(singletextflds) >Keywords >$TkGnats(Quarter) >Date-Required
    }
    
    set Tkeditpr(multitextflds) {
        >Description
        >How-To-Repeat
        >Environment
	>IPsec-look
        >Fix
        >Audit-Trail
        >Organization
        >Unformatted
        >Release-Note
    }
    
    # List of entry widgets for traverse key binding
    set Tkeditpr(tlist) {}
    
    set current_multi_text ""
    
    # load a bunch of defaults into flds.
    set Tkeditpr(parsed_flds) [parsepr_txt $prtxt flds]
    set missing_list [load_field_defaults flds]

    frame   .mframe     -borderwidth 1 -relief raised
    pack    .mframe     -side top -fill x

    menubutton .mframe.file -text "File" -menu .mframe.file.m -underline 0
    menu       .mframe.file.m
    .mframe.file.m add command -label "Save Changes"              -command file_report
    .mframe.file.m add command -label "Reset to Starting Values"  -command reset_report
#    .mframe.file.m add command -label "Send Email..."  -command \
#            "email_originator [list [ftrim $flds(X-GNATS-Notify)]] \
#            [list [ftrim $flds(>Responsible)]] [list [ftrim $flds(Reply-To)]] \
#            [ftrim $flds(>Category)]/$Tkeditpr(prid) [list [ftrim $flds(>Synopsis)]]"
    .mframe.file.m add command -label "Send Email..."  -command edit_email_originator
    .mframe.file.m add separator
    .mframe.file.m add command -label "Cancel"                    -command cancel_report
    
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
            -command "helpMsg Edit_Overview"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Cut, Copy, Paste Operations" \
            -command "helpMsg Cut_Copy_Paste"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Field Definitions" \
            -command "helpMsg Field_Definitions"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Radio Buttons (Class, etc)" \
            -command "helpMsg Edit_Radio_Buttons"
    .mframe.help.m add command -label "Listbox Selectors (Category, etc)" \
            -command "helpMsg Edit_Listbox_Selectors"
    .mframe.help.m add command -label "Entry Fields (Originator, etc)" \
            -command "helpMsg Edit_Entry_Fields"
    .mframe.help.m add command -label "Text Fields (Description, etc)" \
            -command "helpMsg Edit_Text_Fields"
    .mframe.help.m add separator
    .mframe.help.m add command -label "View Configuration Variables" \
        -command "helpMsg TkGnats_Variables"
    .mframe.help.m add separator
    .mframe.help.m add command -label "Changes" \
            -command "helpMsg Changes"
    .mframe.help.m add command -label "About" \
            -command "helpMsg TkGnats_About"
    pack       .mframe.help -side right
    
    frame  .action -borderwidth 1 -relief raised
    pack   .action -side top -fill x -anchor w
    button .action.send   -borderwidth 1 -text "Save Changes"              -command file_report
    #button .action.cancel -borderwidth 1 -text "Cancel"                    -command cancel_report
    button .action.reset  -borderwidth 1 -text "Reset to Starting Values"  -command reset_report
    button .action.email  -borderwidth 1 -text "Send Email..."             -command edit_email_originator
#    button .action.email  -borderwidth 1 -text "Send Email..."             -command \
#            "email_originator [list [ftrim $flds(X-GNATS-Notify)]] \
#            [list [ftrim $flds(>Responsible)]] [list [ftrim $flds(Reply-To)]] \
#            [ftrim $flds(>Category)]/$Tkeditpr(prid) [list [ftrim $flds(>Synopsis)]]"
    
    #pack .action.send .action.reset .action.email .action.cancel -side left -padx 0
    pack .action.send .action.reset .action.email -side left -padx 0
    
    message .action.msg -aspect 10000  -relief sunken -bd 1 -text ""
    pack    .action.msg -side left -fill x -expand 1

    # Get the maximum width of the value fields for the bagged_radiobar
    set Tkeditpr(value_width) 0
    foreach tag $Tkeditpr(radioflds) {
        set f [ftrim $flds($tag)]
        if {[string length $f] > $Tkeditpr(value_width)} {
            set Tkeditpr(value_width) [string length $f]
        }
    }
    
    # Get the maximum width of the value fields for the readonly_singletext fields
    set Tkeditpr(singletextvalue_width) 0
    set flist {>Last-Modified >Arrival-Date >Closed-Date}
    foreach tag $flist {
        set f [ftrim $flds($tag)]
        if {[string length $f] > $Tkeditpr(singletextvalue_width)} {
            set Tkeditpr(singletextvalue_width) [string length $f]
        }
    }
    
    foreach efield [list Arrival-Date Last-Modified Closed-Date] {
        if {[check_suppressed_field $efield] != 1} {
            # We don't want to support editing these for now.
            readonly_singletext $efield [ftrim $flds(>$efield)] 14 $Tkeditpr(singletextvalue_width)
        }
    }

    frame .eflds
    radiobar_frame .eflds .eflds.lb
    set panelnum 0
    foreach {efield elist} [list Class $TkGnats(ClassesList) State $TkGnats(StatesList) Priority {low medium high} Severity {non-critical serious critical} Confidential {no yes}] {
        if {[check_suppressed_field $efield] != 1} {
            bagged_radiobar .eflds.lb [string tolower $efield] $efield \
                    $elist None frm $Tkeditpr(value_width) $panelnum
	    incr panelnum
        }
    }
    pack .eflds.lb -side left -pady 0
    pack .eflds    -side top  -pady 0 -fill x -anchor w

    #puts stderr [concat "This is fun: " [grid slaves .eflds.lb]]

    
    frame .eboxs
    set   nboxes 1
    array set xpad {1 0 2 20 3 0}
    foreach {efield eframe} {Category clb Submitter-Id slb Responsible rlb} {
        if {[check_suppressed_field $efield] != 1} {
            frame .eboxs.$eframe -relief groove -borderwidth 2
            edit_[string tolower $efield]_listbox .eboxs.$eframe *
            pack  .eboxs.$eframe -side left -anchor nw -pady 2 -padx $xpad($nboxes) -fill y
            incr nboxes
        }
    }

    frame .eboxs.shortones -relief groove -borderwidth 2
    foreach f $Tkeditpr(shorttextflds) {
        if {[check_suppressed_field $f] == 1} {
            continue
        }
	lappend Tkeditpr(tlist) [singletext .eboxs.shortones $f 20 "" 14]
    }
    pack .eboxs.shortones -side left 

    if {$nboxes > 1} {
        pack .eboxs -side top  -anchor w -pady 2 -padx  0 -fill x
    }

    foreach f $Tkeditpr(singletextflds) {
        if {[check_suppressed_field $f] == 1} {
            continue
        }
	lappend Tkeditpr(tlist) [singletext . $f 80 "" 14]
    }
    
    lappend Tkeditpr(tlist) [make_txt_mb $Tkeditpr(multitextflds)]
    
    set_text_traversal $Tkeditpr(tlist)
    
    wm title      . "TkGnats - [lindex $TkGnats(ServerInfo) 0] - Edit Problem Report: [ftrim $flds(>Category)]/[ftrim $flds(>Number)]"
    wm iconbitmap . @$TkGnats(lib)/tkeditpr.xbm
    wm iconname   . "$TkGnats(LogName)'s tkeditpr [ftrim $flds(>Number)]"
    fillfrm

    tkwait visibility .
    
    if {"$missing_list" != ""} {
        Msg "The following fields were missing from the report but will be added when you Save Changes:\n" [join $missing_list \n]
        set Tkeditpr(parsed_flds) [concat $Tkeditpr(parsed_flds) $missing_list]
    }
}

##################################################################

set whyText      ""
set Tkeditpr(prid) ""

tkeditpr_process_args

foreach f { tkpr_library.tcl tkprhelp.tcl tkprfont.tcl tkprdatesel.tcl } {
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

set  numerrs 0

if {$Tkeditpr(prid) == ""} {
    wm withdraw .
    Msg "No problem report id supplied!\n" "Usage:  tkeditpr -prid=nnnn"
    incr numerrs
}

if {"$TkGnats(LogName)" == "root"} {
    wm withdraw .
    Msg "You cannot edit problem reports as root.\n" "Please use your own login."
    incr numerrs
}

if {$numerrs > 0} {
    close_socket_gnatsd 1
    exit 1
}

edit_window

# If TkGnats(GNATS_ACCESS_METHOD) != "socket" then this does nothing
close_socket_gnatsd 1
