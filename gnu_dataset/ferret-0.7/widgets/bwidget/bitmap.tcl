# ------------------------------------------------------------------------------
#  bitmap.tcl
#  This file is part of Unifix BWidget Toolkit
#  $Id: bitmap.tcl,v 1.1.1.1 2003/10/09 09:31:54 jemarch Exp $
# ------------------------------------------------------------------------------
#  Index of commands:
#     - Bitmap::get
#     - Bitmap::_init
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


namespace eval Bitmap {
    variable path
    variable _bmp
    variable _types {
        photo  .gif
        photo  .ppm
        bitmap .xbm
        photo  .xpm
    }

    proc use {} {}
}


# ------------------------------------------------------------------------------
#  Command Bitmap::get
# ------------------------------------------------------------------------------
proc Bitmap::get { name } {
    variable path
    variable _bmp
    variable _types

    if {[info exists _bmp($name)]} {
        return $_bmp($name)
    }

    # --- Nom de fichier avec extension ------------------------------------------------------
    set ext [file extension $name]
    if { $ext != "" } {
        if { ![info exists _bmp($ext)] } {
            error "$ext not supported"
        }

        if { [file exists $name] } {
            if {![string compare $ext ".xpm"]} {
                set _bmp($name) [xpm-to-image $name]
                return $_bmp($name)
            }
            if {![catch {set _bmp($name) [image create $_bmp($ext) -file $name]}]} {
                return $_bmp($name)
            }
        }
    }

    foreach dir $path {
        foreach {type ext} $_types {
            if { [file exists [file join $dir $name$ext]] } {
                if {![string compare $ext ".xpm"]} {
                    set _bmp($name) [xpm-to-image [file join $dir $name$ext]]
                    return $_bmp($name)
                } else {
                    if {![catch {set _bmp($name) [image create $type -file [file join $dir $name$ext]]}]} {
                        return $_bmp($name)
                    }
                }
            }
        }
    }

    return -code error "$name not found"
}


# ------------------------------------------------------------------------------
#  Command Bitmap::_init
# ------------------------------------------------------------------------------
proc Bitmap::_init { } {
    global   env
    variable path
    variable _bmp
    variable _types

    set path [list "." [file join $::BWIDGET::LIBRARY images]]
    set supp [image types]
    foreach {type ext} $_types {
        if { [lsearch $supp $type] != -1} {
            set _bmp($ext) $type
        }
    }
}


Bitmap::_init
