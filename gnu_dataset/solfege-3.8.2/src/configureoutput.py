# GNU Solfege - ear training for GNOME
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006  Tom Cato Amundsen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin ST, Fifth Floor, Boston, MA  02110-1301  USA

MAJOR_VERSION = "3"
MINOR_VERSION = "8"
PATCH_LEVEL = "2"
VERSION_STRING = '%s.%s.%s' % (MAJOR_VERSION, MINOR_VERSION, PATCH_LEVEL)
HAVE_LINUX_AWE_VOICE_H = "yes" == "yes"
ENABLE_TUNER = "no" == "yes"
