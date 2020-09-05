# ------------------------------------------------------------------------------
#  messagedlg.tcl
#  This file is part of Unifix BWidget Toolkit
# ------------------------------------------------------------------------------
#  Index of commands:
#     - MessageDlg::create
# ------------------------------------------------------------------------------

# BWidget ToolKit
# Copyright (c) 1998-1999 UNIFIX. 
# Copyright (c) 2001-2002 ActiveState Corp. 

# The following terms apply to all files associated with the software
# unless explicitly disclaimed in individual files.

# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.

# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.

# GOVERNMENT USE: If you are acquiring this software on behalf of the
# U.S. government, the Government shall have only "Restricted Rights"
# in the software and related documentation as defined in the Federal 
# Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
# are acquiring the software on behalf of the Department of Defense, the
# software shall be classified as "Commercial Computer Software" and the
# Government shall have only "Restricted Rights" as defined in Clause
# 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
# authors grant the U.S. Government and others acting in its behalf
# permission to use and distribute the software in accordance with the
# terms specified in this license. 

namespace eval MessageDlg {
    Dialog::use

    Widget::tkinclude MessageDlg message .frame.msg \
	    remove [list -cursor -highlightthickness		\
		-highlightbackground -highlightcolor		\
		-relief -borderwidth -takefocus -textvariable	\
		] \
	    rename [list -text -message]			\
	    initialize [list -aspect 800 -anchor c -justify center]

    Widget::bwinclude MessageDlg Dialog :cmd \
	    remove [list -modal -image -bitmap -side -anchor -separator \
		-homogeneous -padx -pady -spacing]

    Widget::declare MessageDlg {
        {-icon       Enum   info 0 {none error info question warning}}
        {-type       Enum   user 0 {abortretryignore ok okcancel \
		retrycancel yesno yesnocancel user}}
        {-buttons    String ""   0}
    }

    Widget::addmap MessageDlg "" tkMBox {
	-parent {} -message {} -default {} -title {}
    }

    proc ::MessageDlg { path args } { return [eval MessageDlg::create $path $args] }
    proc use { } {}
}


# ------------------------------------------------------------------------------
#  Command MessageDlg::create
# ------------------------------------------------------------------------------
proc MessageDlg::create { path args } {
    global tcl_platform

    array set maps [list MessageDlg {} :cmd {} .frame.msg {} tkMBox {}]
    array set maps [Widget::parseArgs MessageDlg $args]
    Widget::initFromODB MessageDlg "$path#Message" $maps(MessageDlg)

    array set dialogArgs $maps(:cmd)

    set type  [Widget::cget "$path#Message" -type]
    set icon  [Widget::cget "$path#Message" -icon]

    set defb  -1
    set canb  -1
    switch -- $type {
        abortretryignore {set lbut {abort retry ignore}}
        ok               {set lbut {ok}; set defb 0 }
        okcancel         {set lbut {ok cancel}; set defb 0; set canb 1}
        retrycancel      {set lbut {retry cancel}; set defb 0; set canb 1}
        yesno            {set lbut {yes no}; set defb 0; set canb 1}
        yesnocancel      {set lbut {yes no cancel}; set defb 0; set canb 2}
        user             {set lbut [Widget::cget "$path#Message" -buttons]}
    }

    # If the user didn't specify a default button, use our type-specific
    # default, adding its flag/value to the "user" settings and to the tkMBox
    # settings
    if { ![info exists dialogArgs(-default)] } {
	lappend maps(:cmd) -default $defb
	lappend maps(tkMBox) -default $defb
    }
    if { ![info exists dialogArgs(-cancel)] } {
        lappend maps(:cmd) -cancel $canb
    }

    # Same with title as with default
    if { ![info exists dialogArgs(-title)] } {
        set frame [frame $path -class MessageDlg]
        set title [option get $frame "${icon}Title" MessageDlg]
        destroy $frame
        if { $title == "" } {
            set title "Message"
        }
	lappend maps(:cmd) -title $title
	lappend maps(tkMBox) -title $title
    }

    # Create the "user" type dialog
    if { $type == "user" } {
        if { $icon != "none" } {
            set image [Bitmap::get $icon]
        } else {
            set image ""
        }
        eval Dialog::create $path $maps(:cmd) -image $image -modal local \
		-side bottom -anchor c
        foreach but $lbut {
            Dialog::add $path -text $but -name $but
        }
        set frame [Dialog::getframe $path]

        eval message $frame.msg $maps(.frame.msg) \
		-relief flat -borderwidth 0 -highlightthickness 0 \
		-textvariable {{}}
        pack  $frame.msg -side left -padx 3m -pady 1m -fill x -expand yes

        set res [Dialog::draw $path]
	destroy $path
    } else {
	# Do some translation of args into tk_messageBox syntax, then create
	# the tk_messageBox
	array set tkMBoxArgs $maps(tkMBox)
	set tkMBoxArgs(-default) [lindex $lbut $tkMBoxArgs(-default)]
	if { ![string equal $icon "none"] } {
	    set tkMBoxArgs(-icon) $icon
	}
	if { [info exists tkMBoxArgs(-parent)] } {
	    if { ![winfo exists $tkMBoxArgs(-parent)] } {
		unset tkMBoxArgs(-parent)
	    }
	}
	set tkMBoxArgs(-type) $type
	set res [eval tk_messageBox [array get tkMBoxArgs]]
	set res [lsearch $lbut $res]
    }
    Widget::destroy "$path#Message"
    return $res
}
