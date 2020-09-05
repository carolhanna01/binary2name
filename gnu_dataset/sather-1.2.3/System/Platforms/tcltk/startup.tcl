#------------------------------>  Tcl - script  <-----------------------------#
#- Copyright (C) 199x by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

# Startup file for using Tcl/Tk from Sather. This is a version of the
# standard tcl/tk initialization. To avoid having to search for these
# initialization files which seem to vary widely from one system to
# another, both in name and locations, I've copied the files 
# to tk-system-init.tcl and tcl-system-init.tcl
#
# I've copied these system level files here. From what I can tell, this
# should do no harm, but you may want to use ones customized to your site
# for some reason. In that case, please see the last two lines of this file.

set gCurrentlyLoading ""
set satherHomeEnvVars [array names env SATHER_HOME]

if { ${satherHomeEnvVars} != "" } {
    set sHome $env(SATHER_HOME)
} else { 
#    puts "ERROR::The environment variable SATHER_HOME was not set!"
    set sHome "/usr/lib/sather"
}

proc sourceSystemFile { gname} {
    global gCurrentlyLoading
    global sHome
    set gCurrentlyLoading ${gname}
    if ![file exists ${gCurrentlyLoading} ] {
	puts "ERROR:The tcl/tk startup file: ${gCurrentlyLoading} was not found"
	puts "Try editing SATHER_HOME/System/Platforms/tcltk/startup.tcl and try again"
    }
    set res [catch { uplevel #0 {source "${gCurrentlyLoading}" } } srcErr]
    if { ${res} != 0 } { 
	puts "ERROR: SATHER_HOME/System/Platforms/tcltk/startup.tcl can't load ${gname}"
	puts ${srcErr} 
	puts "Error occured in starting up the Gui. Please see:"
	puts "System/Platforms/tcltk/startup.tcl"
	puts "Error reading ${gname}" "Source failed: ${srcErr}"

    }
}

set tcl_library "${sHome}/System/Platforms/tcltk/init"
set tk_library "${sHome}/System/Platforms/tcltk/init"
set sather_tk_library "${sHome}/System/Platforms/tcltk/init"

sourceSystemFile "${sHome}/System/Platforms/tcltk/init/tcl-system-init.tcl"
sourceSystemFile "${sHome}/System/Platforms/tcltk/init/tk-system-init.tcl"

# Specify an icon for the root window. Some window managers want this
wm iconmask . "@${sHome}/System/Platforms/tcltk/sathermask.xbm"
wm iconbitmap . "@${sHome}/System/Platforms/tcltk/sather.xbm"


########################################################################
#               The (old) standard way to do things
# You can comment the previous two sourceSystemFile lines and uncomment
# the next two to use your own system versions of these files
########################################################################
# setenv tk_library /usr/local/src/lib/tk/library
#sourceSystemFile "${tk_library}/tk.tcl"
#sourceSystemFile "/usr/local/lib/tcl/init.tcl"

