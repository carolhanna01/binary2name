# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007  Tom Cato Amundsen
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

SHORT_WAIT = 700
NORMAL_WAIT = 2000

QSTATUS_NO = 0
QSTATUS_NEW = 1
QSTATUS_WRONG = 2
QSTATUS_SOLVED = 3
QSTATUS_GIVE_UP = 4
QSTATUS_VOICING_SOLVED = 5
QSTATUS_VOICING_WRONG = 6
QSTATUS_TYPE_WRONG = 7
QSTATUS_TYPE_SOLVED = 8

int_interval = (_("unison"), _("minor second"), _("major second"),
                 _("minor third"), _("major third"), _("perfect fourth"),
                 _("diminished fifth"), _("perfect fifth"),
                 _("minor sixth"), _("major sixth"),
                 _("minor seventh"), _("major seventh"),
                 _("perfect octave"),
                 _("minor ninth"), _("major ninth"),
                 _("minor decim"), _("major decim"))

short_interval_name = (_i("interval|u"),
                       _i("interval|m2"), _i("interval|M2"),
                       _i("interval|m3"), _i("interval|M3"),
                       _i("interval|4"), _i("interval|d5"),
                       _i("interval|5"),
                       _i("interval|m6"), _i("interval|M6"),
                       _i("interval|m7"), _i("interval|M7"),
                       _i("interval|8"),
                       _i("interval|m9"), _i("interval|M9"),
                       _i("interval|m10"), _i("interval|M10"))

