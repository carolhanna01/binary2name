#!/bin/sh
# Delete a category from GNATS.
# Copyright (C) 1993,94,95,2007 Free Software Foundation, Inc.
# Contributed by Brendan Kehoe (brendan@cygnus.com).
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

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

prog=rmcat
if [ $# -eq 0 ]; then
  echo "usage: $prog category [category...]"
  exit 1
fi

GNATS_DB_DIR="`query-pr --print-directory-for-database`"

if [ ! -d "$GNATS_DB_DIR" ]
then
    echo "No directory $GNATS_DB_DIR"
    exit 1
fi

for i in "$@"; do
    if query-pr --list-categories | grep "^${i}:" >/dev/null 2>&1; then
      echo "$prog: category \`$i' is still in the categories file, please remove it."
      continue
    fi
    if [ ! -d "$GNATS_DB_DIR/$i" ]; then
      echo "$prog: no directory for category \`$i'"
      continue
    fi
    if [ "`ls $GNATS_DB_DIR/\"$i\"/* 2>/dev/null`" != "" ]; then
      echo "$prog: bug reports are still in \`$i', remove them or recategorize them."
      continue
    fi
    echo -n "Trying to delete \'$i'..."
    # Can't test return value of rmdir on old SunOSes...rrgh
    rmdir "$GNATS_DB_DIR/$i" 2>/dev/null
    if [ -d "$GNATS_DB_DIR/$i" ] ; then
      echo
      echo "$prog: could not remove \`$GNATS_DB_DIR/$i'"
      continue
    fi
    echo 'done.'
done

exit 0
