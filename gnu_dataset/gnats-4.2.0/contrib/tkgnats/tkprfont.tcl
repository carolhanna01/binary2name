proc TkFont_Select {{top .tkfont} {fonts {DialogFont}} {font {}}} {
    global TkFont TkGnats

    if {[info tclversion] < 8.0} {
        bell
        tk_dialog .fontError "TkFont Select Error" \
                "TkFont Select requires Tcl/Tk version 8.0 or newer." "error" 0 "OK"
        return {}
    }
    
    set  TkFont(top) $top
    toplevel $top -class TkFont

    set TkFont(select) [string tolower [lindex $fonts 0]]

    if {$font != ""} {
        option add *TkFont*font $font 100
    }

    set   f [frame $top.mframe -borderwidth 1 -relief raised]
    pack $f -side top -fill x

    menubutton $f.file -text "File" -menu $f.file.m -underline 0
    menu       $f.file.m
    $f.file.m add command -label "Apply"  -command {set TkFont(ok) 2}
    $f.file.m add command -label "Reset"  -command {set TkFont(ok) 4}
    $f.file.m add command -label "Save"   -command {set TkFont(ok) 3}
    $f.file.m add separator
    $f.file.m add command -label "Close"  -command {set TkFont(ok) 1}
    
    menubutton $f.edit -text "Edit" -menu $f.edit.m -underline 0 -state disabled
    menu       $f.edit.m
    
    pack $f.file $f.edit -side left
    
    menubutton $f.help -text "Help" -menu $f.help.m -underline 0 -state disabled
    menu       $f.help.m
    pack       $f.help -side right
#   $f.help.m add separator
#   $f.help.m add command -label "Changes" \
#           -command "helpMsg Changes"
#   $f.help.m add command -label "About" \
#           -command "helpMsg TkGnats_About"

    set     f [frame $top.buttons -relief raised -borderwidth 1]
    button $f.close  -text Close  -bd 1 -command {set TkFont(ok) 1}
    button $f.apply  -text Apply  -bd 1 -command {set TkFont(ok) 2}
    button $f.reset  -text Reset  -bd 1 -command {set TkFont(ok) 4}
    button $f.save   -text Save   -bd 1 -command {set TkFont(ok) 3}
    pack   $f.close $f.apply $f.reset $f.save -padx 0 -pady 0 -side left -anchor w
    pack   $f -side top -pady 0 -anchor nw -fill x

    TkFontInit

    # Font Selection, Type, Format and Size

    set    sf [frame $top.sf -relief groove -borderwidth 2]
    pack  $sf -side top -anchor nw -fill x -pady 12 -padx 12
    label $sf.label -text "Apply To:" -width 10 -anchor e
    pack  $sf.label -side left
    foreach B $fonts {
        set b [string tolower $B]
	radiobutton $sf.$b -text $B -command TkFontSetSelect -value $b -variable TkFont(select)
	pack $sf.$b -side left -padx 6 -pady 2
        set TkFont($b) $B
        set TkFont(orig$b) [font actual $b]
    }

    set    tf [frame $top.tf]
    pack  $tf -side top -anchor nw -pady 4
    label $tf.label -text "Font Type:" -width 12 -anchor e
    pack  $tf.label -side left
    foreach b {all fixed proportional} {
	radiobutton $tf.$b -text $b -command TkFontLoad -value $b -variable TkFont(type)
	pack $tf.$b -side left -padx 6
    }

    set    ff [frame $top.ff]
    pack  $ff -side top -anchor nw -pady 0
    label $ff.label -text "Font Format:" -width 12 -anchor e
    pack  $ff.label -side left
    checkbutton $ff.bold    -text Bold       -variable TkFont(-weight) \
            -onvalue bold   -offvalue normal -command TkFontUpdate
    checkbutton $ff.italic  -text Italic     -variable TkFont(-slant)  \
            -onvalue italic -offvalue roman  -command TkFontUpdate
    checkbutton $ff.underline  -text underline  -variable TkFont(-underline)  \
            -command TkFontUpdate
    checkbutton $ff.overstrike -text overstrike -variable TkFont(-overstrike) \
            -command TkFontUpdate
    pack  $ff.bold $ff.italic $ff.underline $ff.overstrike -side left -padx 6

    # Add an entry to enter a specific size.
    
    set     f [frame $top.size]
    pack   $f       -side top -fill x -anchor nw -pady 4
    label  $f.msg   -text "Size:" -width 12 -anchor e
    entry  $f.entry -textvariable TkFont(-size) -width 4 -background $TkGnats(EditFieldBackground)
    bind   $f.entry <Return> {TkFontUpdate ; TkFontGetSizes quick}
    pack   $f.msg   -side left -anchor w
    pack   $f.entry -side left -fill none -anchor w -padx 6 -pady 4
    button $f.stop  -text Stop -bd 1 -command {set TkFont(stopflag) 1}
    label  $f.prog  -text "" -background $TkGnats(ReadOnlyBackground) -textvariable TkFont(progress_txt)
    set    TkFont(stop)     $f.stop
    set    TkFont(progress) $f.prog 

    # Listboxes for the font families and the available sizes for the current font
    
    set lf  [frame $top.lboxes]
    set TkFont(slb) [TkFontScrolledListbox $lf.slb "Sizes:"    \
            -width  4 -height 12 -exportselection 0 -setgrid 1]
    set TkFont(flb) [TkFontScrolledListbox $lf.flb "Families:" \
            -width 25 -height 12 -exportselection 0 -setgrid 1]
    pack $lf     -side top  -fill both -pady  4 -expand 1 -padx 6
    pack $lf.slb -side left -fill both -padx  6 -expand 0 -anchor w
    pack $lf.flb -side left -fill both -padx  6 -expand 1 -anchor w
    bind $TkFont(slb) <ButtonRelease-1> \
            {set TkFont(-size)   [$TkFont(slb) get [$TkFont(slb) curselection]] ; TkFontUpdate}
    bind $TkFont(flb) <ButtonRelease-1> \
            {set TkFont(-family) [$TkFont(flb) get [$TkFont(flb) curselection]] ; TkFontUpdate ; TkFontGetSizes}
    
    # This label displays the current font
    
    label $top.font -textvar TkFont(name) -bd 5

    # A message displays a string in the font.

    message $top.msg -aspect 1000 \
            -borderwidth 10 -font tkfont \
            -text  "ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789\n!@#$%^&*()_+-=[]{};:\"'` ~,.<>/?\\|"

    pack $top.font $top.msg  -side top

    set allfonts [lsort [font families]]
    set TkFont(type)  all
    set TkFont(all)  $allfonts
    TkFontLoad quick
    
    TkFontSetSelect
    
    TkFontGetSizes
}

proc TkFontSetFamily {} {
    global TkFont
    set f [list $TkFont(-family) $TkFont(-size) $TkFont(-weight) $TkFont(-slant)]
    foreach opt {underline overstrike} {
        if {$TkFont(-$opt)} {
            lappend f $opt
        }
    }
    return $f
}

proc edit_fonts {} {
    global TkGnats TkFont

    set top .tkfont
    set fonts {DialogFont TextFont HelpFont}
    TkFont_Select $top $fonts $TkGnats(TkDialogFont)
    
    set TkFont(ok) 1

    while {$TkFont(ok) != 0} {
        tkwait variable TkFont(ok)
        switch $TkFont(ok) 1 {
            # Close
            set TkFont(ok) 0
        } 2 {
            # Apply
            eval {font configure $TkGnats($TkFont(select))} [array get TkFont -*]
            set TkGnats($TkFont($TkFont(select))) [TkFontSetFamily]
            #puts "$TkFont($TkFont(select))=$TkGnats($TkFont($TkFont(select)))"
        } 3 {
            # Save
            set txt ""
            foreach f $fonts {
                append txt "set TkGnats($f) \{$TkGnats($f)\}\n"
            }
            file_put_text $TkGnats(UserDir)/fonts $txt
            tk_dialog .fontSave "TkFonts Saved" \
                    "Fonts have been saved in $TkGnats(UserDir)/fonts." "info" 0 "OK"
        } 4 {
            # Reset
            eval font configure $TkGnats($TkFont(select)) $TkFont(orig$TkFont(select))
            TkFontSetSelect
            TkFontGetSizes
            set TkGnats($TkFont($TkFont(select))) [TkFontSetFamily]
        }
    }

    destroy $top
}

proc TkFontHideStop {} {
    global TkFont
    set  TkFont(progress_txt) ""
    pack forget $TkFont(stop) $TkFont(progress)
    $TkFont(top) config -cursor ""
}

proc TkFontShowStop {} {
    global TkFont
    set   TkFont(stopflag) 0
    pack $TkFont(stop) $TkFont(progress) -side left -padx 6
    $TkFont(top) config -cursor watch
}

proc TkFontLoad {{quick {}}} {
    global TkFont
    if {$TkFont(type) != "all" && $quick == {} && ![info exists TkFont(fixed)]} {
        TkFontShowStop
	set TkFont(fixed)        {}
	set TkFont(proportional) {}
	set num [llength $TkFont(all)]
	set n   1
	foreach f $TkFont(all) {
	    set TkFont(progress_txt) "Scanning $n of $num fonts..."
	    incr n
	    update
            if {$TkFont(stopflag)} {
                unset TkFont(fixed)
                unset TkFont(proportional)
                break
            }
	    if {[font metrics [list $f] -fixed]} {
		lappend TkFont(fixed) $f
	    } else {
		lappend TkFont(proportional) $f
	    }
	}
        TkFontHideStop
    }
    if {![info exists TkFont(fixed)]} {
        set TkFont(type)  all
    }
    catch {set TkFont(prev) [$TkFont(flb) get [$TkFont(flb) curselection]]}
    $TkFont(flb) delete 0 end
    foreach f $TkFont($TkFont(type)) {
        $TkFont(flb) insert end $f
    }
    if {[info exists TkFont(prev)]} {
	TkFontListboxSelect flb $TkFont(prev)
    }
}
    
proc TkFontGetSizes {{quick {}}} {
    global TkFont
    $TkFont(slb) delete 0 end
    if {[info exists TkFont($TkFont(-family),sizes)]} {
        foreach s $TkFont($TkFont(-family),sizes) {
            $TkFont(slb) insert end $s
        }
    } {
	if {$quick != {}} {
            return
        }
        TkFontShowStop
        for {set s 2} {$s < 25} {incr s} {
	    set TkFont(progress_txt) "Scanning $s of 25 sizes..."
	    update
            if {$TkFont(stopflag)} {
                break
            }
            if {[font actual [list $TkFont(-family) $s] -size] == "$s"} {
                $TkFont(slb) insert end $s
                lappend TkFont($TkFont(-family),sizes) $s
            }
        }
        TkFontHideStop
    }
    TkFontListboxSelect slb $TkFont(-size)
}

proc TkFontSetSelect {} {
    global TkFont
    array set TkFont [font actual $TkFont(select)]
    TkFontUpdate
    set TkFont(type) all
    TkFontLoad quick
    TkFontListboxSelect flb $TkFont(-family)
    TkFontListboxSelect slb $TkFont(-size)
}
    
proc TkFontUpdate { } {
    global TkFont
    
    # The elements of font that have a leading - are
    # used directly in the font configuration command.
    
    eval {font configure tkfont} [array get TkFont -*]
    TkFontSet
}

proc TkFontInit {} {
    global TkFont TkGnats
    catch {font delete tkfont}
    font create tkfont -family $TkGnats($TkFont(select))
}

proc TkFontSet {} {
    global TkFont
    
    # Save the actual font parameters after any font substitutions.
    # "TkFont(name)" is the font configuration information
    # with a line break before "-slant" so it looks nicer.
    # The code is ordered to only execute "font actual" once.
    
    set fn [font actual tkfont]
    array set TkFont $fn
    append fn \n[lrange [font metrics tkfont] 0 5]
    
    regsub -- "-slant" $fn "\n-slant" TkFont(name)
}

proc TkFontListboxSelect {lb element} {
    global TkFont
    $TkFont($lb) selection clear 0 end
    set l [$TkFont($lb) get 0 end]
    if {[set n [lsearch $l $element]] >= 0} {
        $TkFont($lb) selection set $n
        $TkFont($lb) see $n
    }
}
    
proc TkFontScrolledListbox { f title args } {
    frame $f -relief groove -borderwidth 2
    listbox $f.list \
	    -xscrollcommand [list $f.xscroll set] \
	    -yscrollcommand [list $f.yscroll set]
    eval {$f.list configure} $args
    scrollbar $f.xscroll -orient horizontal -command [list $f.list xview]
    scrollbar $f.yscroll -orient vertical   -command [list $f.list yview]
    if {$title != {}} {
        label $f.label -text $title -anchor nw
        grid  $f.label -sticky nw
    }
    grid $f.list $f.yscroll -sticky news
    grid $f.xscroll -sticky news
    grid rowconfigure    $f 1 -weight 1
    grid columnconfigure $f 0 -weight 1
    return $f.list
}

#wm withdraw .
#puts [TkFont_Select]
#exit
