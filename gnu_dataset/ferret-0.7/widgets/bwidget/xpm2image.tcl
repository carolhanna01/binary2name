# ------------------------------------------------------------------------------
#  xpm2image.tcl
#  Slightly modified xpm-to-image command
#  $Id: xpm2image.tcl,v 1.1.1.1 2003/10/09 09:31:56 jemarch Exp $
# ------------------------------------------------------------------------------
#
#  Copyright 1996 by Roger E. Critchlow Jr., San Francisco, California
#  All rights reserved, fair use permitted, caveat emptor.
#  rec@elf.org
# 
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

proc xpm-to-image { file } {
    set f [open $file]
    set string [read $f]
    close $f

    #
    # parse the strings in the xpm data
    #
    set xpm {}
    foreach line [split $string "\n"] {
        if {[regexp {^"([^\"]*)"} $line all meat]} {
            if {[string first XPMEXT $meat] == 0} {
                break
            }
            lappend xpm $meat
        }
    }
    #
    # extract the sizes in the xpm data
    #
    set sizes  [lindex $xpm 0]
    set nsizes [llength $sizes]
    if { $nsizes == 4 || $nsizes == 6 || $nsizes == 7 } {
        set data(width)   [lindex $sizes 0]
        set data(height)  [lindex $sizes 1]
        set data(ncolors) [lindex $sizes 2]
        set data(chars_per_pixel) [lindex $sizes 3]
        set data(x_hotspot) 0
        set data(y_hotspot) 0
        if {[llength $sizes] >= 6} {
            set data(x_hotspot) [lindex $sizes 4]
            set data(y_hotspot) [lindex $sizes 5]
        }
    } else {
	    error "size line {$sizes} in $file did not compute"
    }

    #
    # extract the color definitions in the xpm data
    #
    foreach line [lrange $xpm 1 $data(ncolors)] {
        set colors [split $line \t]
        set cname  [lindex $colors 0]
        lappend data(cnames) $cname
        if { [string length $cname] != $data(chars_per_pixel) } {
            error "color definition {$line} in file $file has a bad size color name"
        }
        foreach record [lrange $colors 1 end] {
            set key [lindex $record 0]
            set color [string tolower [join [lrange $record 1 end] { }]]
            set data(color-$key-$cname) $color
            if { ![string compare $color "none"] } {
                set data(transparent) $cname
            }
        }
        foreach key {c g g4 m} {
            if {[info exists data(color-$key-$cname)]} {
                set color $data(color-$key-$cname)
                set data(color-$cname) $color
                set data(cname-$color) $cname
                lappend data(colors) $color
                break
            }
        }
        if { ![info exists data(color-$cname)] } {
            error "color definition {$line} in $file failed to define a color"
        }
    }

    #
    # extract the image data in the xpm data
    #
    set image [image create photo -width $data(width) -height $data(height)]
    set y 0
    foreach line [lrange $xpm [expr {1+$data(ncolors)}] [expr {1+$data(ncolors)+$data(height)}]] {
        set x 0
        set pixels {}
        while { [string length $line] > 0 } {
            set pixel [string range $line 0 [expr {$data(chars_per_pixel)-1}]]
            set c $data(color-$pixel)
            if { ![string compare $c none] } {
                if { [string length $pixels] } {
                    $image put [list $pixels] -to [expr {$x-[llength $pixels]}] $y
                    set pixels {}
                }
            } else {
                lappend pixels $c
            }
            set line [string range $line $data(chars_per_pixel) end]
            incr x
        }
        if { [llength $pixels] } {
            $image put [list $pixels] -to [expr {$x-[llength $pixels]}] $y
        }
        incr y
    }

    #
    # return the image
    #
    return $image
}

