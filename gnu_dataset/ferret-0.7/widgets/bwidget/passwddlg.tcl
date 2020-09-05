# -----------------------------------------------------------------------------
#  passwddlg.tcl
#  This file is part of Unifix BWidget Toolkit
#   by Stephane Lavirotte (Stephane.Lavirotte@sophia.inria.fr)
#  $Id: passwddlg.tcl,v 1.1.1.1 2003/10/09 09:31:55 jemarch Exp $
# -----------------------------------------------------------------------------
#  Index of commands:
#     - PasswdDlg::create
#     - PasswdDlg::configure
#     - PasswdDlg::cget
#     - PasswdDlg::_verifonlogin
#     - PasswdDlg::_verifonpasswd
#     - PasswdDlg::_max
#------------------------------------------------------------------------------

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


namespace eval PasswdDlg {
    Dialog::use
    LabelEntry::use

    Widget::bwinclude PasswdDlg Dialog :cmd \
	    remove     {-image -bitmap -side -default -cancel -separator} \
	    initialize {-modal local -anchor c}
    
    Widget::bwinclude PasswdDlg LabelEntry .frame.lablog \
	    remove [list -command -justify -name -show -side	        \
		-state -takefocus -width -xscrollcommand -padx -pady	\
		-dragenabled -dragendcmd -dragevent -draginitcmd	\
		-dragtype -dropenabled -dropcmd -dropovercmd -droptypes	\
		] \
	    prefix [list login -editable -helptext -helpvar -label      \
		-text -textvariable -underline				\
		] \
	    initialize [list -relief sunken -borderwidth 2		\
		-labelanchor w -width 15 -loginlabel "Login"		\
		]
    
    Widget::bwinclude PasswdDlg LabelEntry .frame.labpass		\
	    remove [list -command -width -show -side -takefocus		\
		-xscrollcommand -dragenabled -dragendcmd -dragevent	\
		-draginitcmd -dragtype -dropenabled -dropcmd		\
		-dropovercmd -droptypes -justify -padx -pady -name	\
		] \
	    prefix [list passwd -editable -helptext -helpvar -label	\
		-state -text -textvariable -underline			\
		] \
	    initialize [list -relief sunken -borderwidth 2		\
		-labelanchor w -width 15 -passwdlabel "Password"	\
		]
    
    Widget::declare PasswdDlg {
        {-type        Enum       ok           0 {ok okcancel}}
        {-labelwidth  TkResource -1           0 {label -width}}
        {-command     String     ""           0}
    }

    proc ::PasswdDlg { path args } { return [eval PasswdDlg::create $path $args] }
    proc use {} {}
}


# -----------------------------------------------------------------------------
#  Command PasswdDlg::create
# -----------------------------------------------------------------------------
proc PasswdDlg::create { path args } {

    array set maps [list PasswdDlg {} :cmd {} .frame.lablog {} \
	    .frame.labpass {}]
    array set maps [Widget::parseArgs PasswdDlg $args]

    Widget::initFromODB PasswdDlg "$path#PasswdDlg" $maps(PasswdDlg)

    # Extract the PasswdDlg megawidget options (those that don't map to a
    # subwidget)
    set type      [Widget::cget "$path#PasswdDlg" -type]
    set cmd       [Widget::cget "$path#PasswdDlg" -command]

    set defb -1
    set canb -1
    switch -- $type {
        ok        { set lbut {ok}; set defb 0 }
        okcancel  { set lbut {ok cancel} ; set defb 0; set canb 1 }
    }

    eval Dialog::create $path $maps(:cmd) -class PasswdDlg \
        -image [Bitmap::get passwd] -side bottom -default $defb -cancel $canb
    foreach but $lbut {
        if { $but == "ok" && $cmd != "" } {
            Dialog::add $path -text $but -name $but -command $cmd
        } else {
            Dialog::add $path -text $but -name $but
        }
    }

    set frame [Dialog::getframe $path]
#    bind $path  <Return>  ""
    bind $frame <Destroy> "Widget::destroy $path#PasswdDlg"

    set lablog [eval LabelEntry::create $frame.lablog $maps(.frame.lablog) \
	    -name login -dragenabled 0 -dropenabled 0 \
	    -command \"PasswdDlg::_verifonpasswd $path $frame.labpass\"]

    set labpass [eval LabelEntry::create $frame.labpass $maps(.frame.labpass) \
	    -name password -show "*" -dragenabled 0 -dropenabled 0 \
	    -command \"PasswdDlg::_verifonlogin $path $frame.lablog\"]

    # compute label width -- TODO: this should probably not override the
    # cmdline arg
    set loglabel  [$lablog cget -label]
    set passlabel [$labpass cget -label]
    set labwidth  [_max [string length $loglabel] [string length $passlabel]]
    incr labwidth 1
    $lablog  configure -labelwidth $labwidth
    $labpass configure -labelwidth $labwidth

    proc ::$path { cmd args } "return \[eval PasswdDlg::\$cmd $path \$args\]"

    pack  $frame.lablog $frame.labpass -fill x -expand 1

    # added by bach@mwgdna.com
    #  give focus to loginlabel unless the state is disabled
    if {[$lablog cget -editable]} {
	focus $frame.lablog.e
    } else {
	focus $frame.labpass.e
    }
    set res [Dialog::draw $path]

    if { $res == 0 } {
        set res [list [$lablog.e cget -text] [$labpass.e cget -text]]
    } else {
        set res [list]
    }
    Widget::destroy "$path#PasswdDlg"
    destroy $path

    return $res
}

# -----------------------------------------------------------------------------
#  Command PasswdDlg::configure
# -----------------------------------------------------------------------------

proc PasswdDlg::configure { path args } {
    set res [Widget::configure "$path#PasswdDlg" $args]
}

# -----------------------------------------------------------------------------
#  Command PasswdDlg::cget
# -----------------------------------------------------------------------------

proc PasswdDlg::cget { path option } {
    return [Widget::cget "$path#PasswdDlg" $option]
}


# -----------------------------------------------------------------------------
#  Command PasswdDlg::_verifonlogin
# -----------------------------------------------------------------------------
proc PasswdDlg::_verifonlogin { path labpass } {
    if { [$labpass.e cget -text] == "" } {
        focus $labpass
    } else {
        Dialog::setfocus $path default
    }
}

# -----------------------------------------------------------------------------
#  Command PasswdDlg::_verifonpasswd
# -----------------------------------------------------------------------------
proc PasswdDlg::_verifonpasswd { path lablog } {
    if { [$lablog.e cget -text] == "" } {
        focus $lablog
    } else {
        Dialog::setfocus $path default
    }
}

# -----------------------------------------------------------------------------
#  Command PasswdDlg::_max
# -----------------------------------------------------------------------------
proc PasswdDlg::_max { val1 val2 } { 
    return [expr {($val1 > $val2) ? ($val1) : ($val2)}] 
}
