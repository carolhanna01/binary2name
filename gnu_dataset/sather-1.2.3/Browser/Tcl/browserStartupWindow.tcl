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

# The initial window that is thrown up while the front end is running
# Does not update the GIF properly. But you would not want to burden
# your machine with trivialities while it parses, now would you.  

# Turn on sather level debugging, if needed 
#tkkit_cb debugOn

global gText

proc startupWindow { } {
    global gIsColor gText
    set sttext .text
    set stimage  .image
    
    set gText $sttext
    set home [tkkit_cb getHome]
    set camp_img [image create photo -format gif -file \
	    "${home}/Browser/Tcl/campanile.gif"]
    text ${sttext} -font "10x20" -background black -foreground white
    ${sttext} insert 0.0 "Starting up the browser." 
    ${sttext} insert  end "\nRunning the front end..." 

    label ${stimage} -image ${camp_img} -text "The Sather Browser"
    pack ${stimage} -side left
    pack ${sttext}  -side left

    wm minsize . 1 1
    wm geometry . 500x350
    wm title . "Sather Browser"
    # tk3.6    set colormodel [tk colormodel .]
    # tk3.6     if {${colormodel} == "monochrome"} {}
   set colorDepth [winfo depth . ]
   if {${colorDepth} == "1"} {
	set gIsColor 0
	${sttext} insert end "\nAssuming monochrome"
    } else {
	set gIsColor 1
	${sttext} insert end "\nAssuming color"
    }

    update
    #Uncomment to have processing stop here..."
    # spin 

}

proc spin { } {
    puts "Spinning"
    while { 1 } {
    }
}

proc outMessage { msg  } {
    # Write message to cavnas
    global gText
    ${gText} insert  end "\n${msg}"
    update

}

startupWindow