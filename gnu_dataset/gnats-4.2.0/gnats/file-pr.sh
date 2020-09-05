#!/bin/sh
# Program to submit problem reports for GNATS.
# Copyright (C) 1999,2007 Free Software Foundation, Inc.
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

debug_print=false # or echo to get output.

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

EDIT_USER=
EDIT_PASSWD=
version="@VERSION@"

usage="
Usage:  file-pr [-V|--version] [-h|--help] [-d|--database database_name]
                [-H|--host hostname] [-P|--port port_number]
                [-v|--user userid]   [-w|--passwd password]
                [-f|--filename inputfile]
"

# Parse command line.  We don't really need to do this, but that's ok.

while [ $# -gt 0 ]; do
   case "$1" in
     -V|--version|--ve*)
       echo "$version"; exit 0
       ;;

     -d | --database)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; GNATS_DB="--database=$1" ;;
     -d=* | --database=*) GNATS_DB="$1" ;;

     -D | --debug)
       shift ;;

     -H | --host)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; GNATS_HOST="--host=$1" ;;
     -H=* | --host=*) GNATS_HOST="$1" ;;

     -P | --port)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; GNATS_PORT="--port=$1" ;;
     -P=* | --port=*) GNATS_PORT="$1" ;;

     -v | --user)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; EDIT_USER="--user=$1" ;;
     -v=* | --user=*) EDIT_USER="$1" ;;

     -w | --passwd)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; EDIT_PASSWD="--passwd=$1" ;;
     -w=* | --passwd=*) EDIT_PASSWD="$1" ;;
     -f|--filename*)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; FILENAME="$1" ;;
     -h|--help*)
       echo "$usage"; exit 0
       ;;
     *)
       echo "$usage"; exit 1
       ;;
   esac
   shift
done

# set command here to always pass host and port, and directory if supplied
PR_EDIT="pr-edit ${GNATS_HOST} ${GNATS_PORT} ${EDIT_USER} ${EDIT_PASSWD} ${GNATS_DB}"

if [ "$FILENAME" != "" ]; then
    PR_EDIT="$PR_EDIT -f $FILENAME"
fi

$PR_EDIT --submit
