#! /bin/sh
# Primitive program to diff PRs.
# Copyright (C) 200?,07 Free Software Foundation, Inc.
# Contributed by Bob Manson (manson@juniper.net).
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

# Diff the PRs in files $1 and $2, returning a list of the changed
# fields.  No output is produced if the PRs are identical.

@AWK@ '
BEGIN {
  state = 0;
  nlines = 0;
}

(state == 0) {
  fn = FILENAME;
}

((state == 0 || state == 2) && /^>.*:/) {
  state++;
}


(fn != FILENAME) {
  fn = FILENAME;
  state++;
  arrayindex="";
}

((state == 1 || state == 3) && /^>.*:/) {
  line=$0;
  fieldname=$1;
  sub("^>", "", fieldname);
  sub(":.*$", "", fieldname);
  sub("^[^:]*:[ \t]*", "", line);
  arrayindex=state">"fieldname;
  nlines++;
  numlines[arrayindex] = 1;
  array[arrayindex] = nlines;
  linebuf[nlines] = line;
  fields[fieldname]++;
  next;
}

((state == 1 || state == 3) && arrayindex != "") {
  nlines++;
  numlines[arrayindex]++;
  linebuf[nlines] = $0;
  next;
}

END {
  for (x in fields) {
    if (numlines["1>"x] != numlines["3>"x]) {
      print x;
    } else {
      u=array["1>"x];
      v=array["3>"x];
      n=numlines["1>"x];
      for (i=0; i<n; ++i) {
	if (linebuf[u+i] != linebuf[v+i]) {
          print x;
          break;
        }
      }
    }
  }
}
' $1 $2
