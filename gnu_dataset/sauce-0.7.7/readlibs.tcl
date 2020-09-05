########### readlibs.tcl
# This file, when sourced, reads the library code files
# specified in sauce_libraries.
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
# $Id: readlibs.tcl,v 1.6 2001/03/21 23:53:14 ian Exp $

set @@@share_dir@@@ .
set @@@tcl_lib_ext?@@@ .tcl

foreach x $sauce_libraries {
    source ${@@@share_dir@@@}/$x${@@@tcl_lib_ext?@@@}
}
