########### instlib.tcl
# Library procedures used by install programs
#
# This file is part of SAUCE, a very picky anti-spam receiver-SMTP.
# SAUCE is Copyright (C) 1997-2001 Ian Jackson
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
#
# $Id: instlib.tcl,v 1.4 2001/03/21 23:53:14 ian Exp $

proc string_quote {v allow0} {
    regsub -all {[[]\"\$\\]} $v {\\\1} v
    if {[regexp {[^-._,:/0-9A-Za-z]} $v] || !$allow0 && ![string length $v]} {
	set v "\"$v\""
    }
    return $v
}
