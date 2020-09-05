# ----------------------------------------------------------------------------
#  progressdlg.tcl
#  This file is part of Unifix BWidget Toolkit
# ----------------------------------------------------------------------------
#  Index of commands:
#     - ProgressDlg::create
# ----------------------------------------------------------------------------

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


namespace eval ProgressDlg {
    Dialog::use
    ProgressBar::use

    Widget::bwinclude ProgressDlg Dialog :cmd \
        remove {
            -modal -image -bitmap -side -anchor -cancel -default
            -homogeneous -padx -pady -spacing
        }

    Widget::bwinclude ProgressDlg ProgressBar .frame.pb \
        remove {-orient -width -height}

    Widget::declare ProgressDlg {
        {-width        TkResource 25 0 label}
        {-height       TkResource 2  0 label}
        {-textvariable TkResource "" 0 label}
        {-font         TkResource "" 0 label}
        {-stop         String "" 0}
        {-command      String "" 0}
    }

    Widget::addmap ProgressDlg :cmd .frame.msg \
        {-width {} -height {} -textvariable {} -font {} -background {}}

    proc ::ProgressDlg { path args } { return [eval ProgressDlg::create $path $args] }
    proc use {} {}
}


# ----------------------------------------------------------------------------
#  Command ProgressDlg::create
# ----------------------------------------------------------------------------
proc ProgressDlg::create { path args } {
    array set maps [list ProgressDlg {} :cmd {} .frame.msg {} .frame.pb {}]
    array set maps [Widget::parseArgs ProgressDlg $args]
    
    eval Dialog::create $path $maps(:cmd) -image [Bitmap::get hourglass] \
	    -modal none -side bottom -anchor c -class ProgressDlg

    Widget::initFromODB ProgressDlg "$path#ProgressDlg" $maps(ProgressDlg)

    wm protocol $path WM_DELETE_WINDOW {;}

    set frame [Dialog::getframe $path]
    bind $frame <Destroy> "Widget::destroy $path#ProgressDlg"
    $frame configure -cursor watch

    eval label $frame.msg $maps(.frame.msg) -relief flat -borderwidth 0 \
	    -highlightthickness 0 -anchor w -justify left
    pack $frame.msg -side top -pady 3m -anchor nw -fill x -expand yes

    eval ProgressBar::create $frame.pb $maps(.frame.pb) -width 100
    pack $frame.pb -side bottom -anchor w -fill x -expand yes

    set stop [Widget::cget "$path#ProgressDlg" -stop]
    set cmd  [Widget::cget "$path#ProgressDlg" -command]
    if { $stop != "" && $cmd != "" } {
        Dialog::add $path -text $stop -name $stop -command $cmd
    }
    Dialog::draw $path
    BWidget::grab local $path

    proc ::$path { cmd args } "return \[eval ProgressDlg::\$cmd $path \$args\]"

    return $path
}


# ----------------------------------------------------------------------------
#  Command ProgressDlg::configure
# ----------------------------------------------------------------------------
proc ProgressDlg::configure { path args } {
    return [Widget::configure "$path#ProgressDlg" $args]
}


# ----------------------------------------------------------------------------
#  Command ProgressDlg::cget
# ----------------------------------------------------------------------------
proc ProgressDlg::cget { path option } {
    return [Widget::cget "$path#ProgressDlg" $option]
}
