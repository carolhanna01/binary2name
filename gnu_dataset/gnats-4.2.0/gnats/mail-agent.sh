#!/bin/sh
# Program to send mail for GNATS.
# Copyright (C) 1999,2007 Free Software Foundation, Inc.
# Contributed by Bob Manson (manson@juniper.net)
#
# This file is part of GNU GNATS.
#
# GNU GNATS is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU GNATS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU GNATS; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

# This is a rather pointless script, but at least it isolates the number
# of programs that actually invoke the mail agent to 1, making it easy
# to change.

MAIL_AGENT="@DEFAULT_MAIL_AGENT@"

exec $MAIL_AGENT $@ > /dev/null
