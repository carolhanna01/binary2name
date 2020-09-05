# ------------------------------------------------------------------------------
#  labelentry.tcl
#  This file is part of Unifix BWidget Toolkit
#  $Id: labelentry.tcl,v 1.1.1.1 2003/10/09 09:31:55 jemarch Exp $
# ------------------------------------------------------------------------------
#  Index of commands:
#     - LabelEntry::create
#     - LabelEntry::configure
#     - LabelEntry::cget
#     - LabelEntry::bind
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


namespace eval LabelEntry {
    Entry::use
    LabelFrame::use

    Widget::bwinclude LabelEntry LabelFrame .labf \
        remove {-relief -borderwidth -focus} \
        rename {-text -label} \
        prefix {label -justify -width -anchor -height -font -textvariable}

    Widget::bwinclude LabelEntry Entry .e \
        remove {-fg -bg} \
        rename {-foreground -entryfg -background -entrybg}

    Widget::addmap LabelEntry "" :cmd {-background {}}

    Widget::syncoptions LabelEntry Entry .e {-text {}}
    Widget::syncoptions LabelEntry LabelFrame .labf {-label -text -underline {}}

    ::bind BwLabelEntry <FocusIn> {focus %W.labf}
    ::bind BwLabelEntry <Destroy> {Widget::destroy %W; rename %W {}}

    proc ::LabelEntry { path args } { return [eval LabelEntry::create $path $args] }
    proc use { } {}
}


# ------------------------------------------------------------------------------
#  Command LabelEntry::create
# ------------------------------------------------------------------------------
proc LabelEntry::create { path args } {
    array set maps [list LabelEntry {} :cmd {} .labf {} .e {}]
    array set maps [Widget::parseArgs LabelEntry $args]

    eval frame $path $maps(:cmd) -class LabelEntry \
	    -relief flat -bd 0 -highlightthickness 0 -takefocus 0
    Widget::initFromODB LabelEntry $path $maps(LabelEntry)
	
    set labf  [eval LabelFrame::create $path.labf $maps(.labf) \
                   -relief flat -borderwidth 0 -focus $path.e]
    set subf  [LabelFrame::getframe $labf]
    set entry [eval Entry::create $path.e $maps(.e)]

    pack $entry -in $subf -fill both -expand yes
    pack $labf  -fill both -expand yes

    bindtags $path [list $path BwLabelEntry [winfo toplevel $path] all]

    rename $path ::$path:cmd
    proc ::$path { cmd args } "return \[LabelEntry::_path_command $path \$cmd \$args\]"

    return $path
}


# ------------------------------------------------------------------------------
#  Command LabelEntry::configure
# ------------------------------------------------------------------------------
proc LabelEntry::configure { path args } {
    return [Widget::configure $path $args]
}


# ------------------------------------------------------------------------------
#  Command LabelEntry::cget
# ------------------------------------------------------------------------------
proc LabelEntry::cget { path option } {
    return [Widget::cget $path $option]
}


# ------------------------------------------------------------------------------
#  Command LabelEntry::bind
# ------------------------------------------------------------------------------
proc LabelEntry::bind { path args } {
    return [eval ::bind $path.e $args]
}


#------------------------------------------------------------------------------
#  Command LabelEntry::_path_command
#------------------------------------------------------------------------------
proc LabelEntry::_path_command { path cmd larg } {
    if { ![string compare $cmd "configure"] ||
         ![string compare $cmd "cget"] ||
         ![string compare $cmd "bind"] } {
        return [eval LabelEntry::$cmd $path $larg]
    } else {
        return [eval $path.e:cmd $cmd $larg]
    }
}
