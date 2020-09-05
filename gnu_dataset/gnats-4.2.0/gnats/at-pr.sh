#!/bin/sh
# Check whether a PR has been analyzed within the acknowledgment period.
# Copyright (C) 1993,94,95,99,2002,07 Free Software Foundation, Inc.
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
# along with GNU GNATS; see the file COPYING. If not, see
# <http://www.gnu.org/licenses/>.

prog=at-pr
USAGE="Usage: $prog [-h|--help] [-d|--database database_name] arguments

 Arguments coming in are (all are required):

 1 - response time
 2 - PR number
 3 - submitter-id
 4 - full name of the submitter
 5 - contact for the submitter
 6 - address for GNATS_ADMIN
"

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

QUERY_PR="query-pr"

if [ $# -eq 0 ]
then
  echo "$USAGE" >&2
  exit 1
fi

if [ "x$1" = "x-h" -o "x$1" = "x--help" ]
then
  echo "$USAGE"
  exit 0
fi

# process command line options
while [ $# -gt 6 ]; do
  case "$1" in
    -d | --database)
      if [ $# -eq 7 ]
      then 
        echo "ERROR: value required for $1 option" >&2
        echo "$USAGE" >&2
        exit 1
      fi
      shift 
      GNATSDB="$1" ;;
    -d=* | --database=*) GNATSDB="`echo $1 | sed 's/^[-a-z]*=//'`" ;;

    -*) echo "$USAGE" >&2; exit 1 ;;
  esac
  shift
done

export GNATSDB
VERSION=@VERSION@


eval `$QUERY_PR --print-sh-vars`

if [ "x$GNATSDB_VALID" = x0 ]
then
    echo "Invalid database name $GNATSDB"
    exit 1
fi

if [ $# != 6 ]; then
  echo "ERROR: $prog called with the incorrect number of arguments" >&2
  echo "$USAGE" >&2
  exit 1
fi

# See if the PR number is still valid.

$QUERY_PR --format Number $2 > /dev/null 2>&1

if [ $? != 0 ]
then
    # Maybe the PR is gone?
    exit 0
fi
 
# Grab the current state of the PR, and the synopsis.

# XXX ??? !!! Instead  of calling query-pr 3 times, why not do it once and
# use read or something?

STATE="`$QUERY_PR --format State $2`"
SYNOPSIS="`$QUERY_PR --format Synopsis $2`"
RESPONSIBLE="`$QUERY_PR --format Responsible $2`"
RESP_ADDR=`$QUERY_PR --responsible-address "$RESPONSIBLE"`

# $DEBUG_MODE is set to 0 when debug is off in the config file
if [ "x$DEBUG_MODE" = "x1" ]; then
  STEALTH_HEADER="From: $6 (GNATS Management)
To: $6
Subject: mail output from at-pr


"
fi

if [ "$STATE" = "${DEFAULTSTATE}" ]; then
    mail-agent << __EOF__
${STEALTH_HEADER}From: $6 (GNATS Management)
To: $5, $RESP_ADDR, $6
Subject: PR $2 not analyzed in $1 hours


PR $2 was not analyzed within the acknowledgment period
of $1 business hours.  The pertinent information is:

 Submitter-Id: $3
 Originator: $4
 Synopsis: $SYNOPSIS
 Person responsible for the PR: $RESPONSIBLE

--
The GNU Problem Report Management System (GNATS)
__EOF__
fi

exit 0
