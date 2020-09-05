#!/bin/sh
#
# $Id: check-db.sh,v 1.1 2014/12/28 19:02:34 amanou Exp $
#
# Check the database for old lock files or index inconsistencies
# Copyright (C) 2001 Peter Novodvorsky
# Copyright (C) 2001 Milan Zamazal
# Copyright (C) 1993,97,2007 Free Software Foundation, Inc.
# Contributed by Jonathan Kamens (jik@security.ov.com).
# Further hacked by Milan Zamazal (pdm@zamazal.org).
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

# This script takes no arguments.  It attempts to lock the GNATS
# database for five minutes; if it fails, it sends a mail message
# notifying the administrator of the failure and exits.
# 
# Once the database is locked, the script searches the database for
# lock files that are more than 24 hours old.  Any old lock files are
# reported to the administrator in a mail message.
# 
# After checking for old lock files, it calls gen-index and compares
# the results with gnats-adm/index; any inconsistencies are reported
# to the administrators in a mail message.
# 
# After checking the index file for inconsistencies, the script
# unlocks the database and exits.

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

TMPDIR=${TMPDIR-/tmp}
TMPFILE=$TMPDIR/gnats-check-db-$$
PROGRAM=check-db

USAGE="Usage: $PROGRAM [-h|--help] [-d|--database DATABASENAME | --all-databases]"

#
# Process command line options
#

if [ "$1" = "-h" -o "$1" = "--help" ]; then
  echo "$USAGE"
  exit 0
fi

CHECKALL=no
while [ $# -gt 0 ]; do
  case "$1" in
    --all-databases)
      CHECKALL=yes
      ;;
    -d | --database)
      if [ $# -lt 2 ]; then 
	echo "ERROR: value required for $1 option" >&2
	echo "$USAGE" >&2
	exit 1
      fi
      shift
      GNATSDB="$1"
      ;;
    -d=* | --database=*)
      GNATSDB="`echo $1 | sed 's/^[-a-z]*=//'`"
      ;;
    -*)
      echo "$USAGE" >&2
      exit 1
      ;;
  esac
  shift
done

QUERY_PR="query-pr"

if [ $CHECKALL = yes ]; then
  for D in `$QUERY_PR --list-databases | sed '1,$s/^\([^:][^:]*\):.*$/\1/'`; do
    $PROGRAM -d "$D" $*
  done
  exit
fi

export GNATSDB

if [ "x$GNATS_ROOT" = "x" ]; then
  eval `$QUERY_PR --print-sh-vars`
  GNATS_ROOT=$GNATSDBDIR
fi

if [ "x$GNATSDB_VALID" = x0 ]; then
  mail-agent <<EOF
To: root
Subject: $PROGRAM: invalid database

Unable to continue database check, because the database
\'$GNATSDB\' wasn\'t found. If you don\'t understand what this
message is about, please forward it to the GNATS administrator
of this site.
EOF
  exit 1
fi

# 
# First, try to lock the database
#

i=0
NOTLOCKED=true
while [ $i -lt 30 ]; do
  if pr-edit --lockdb; then
    NOTLOCKED=false
    break
  fi
  i=`expr $i + 1`
  sleep 10
done

GNATS_ADMIN_ADDR=`$QUERY_PR --responsible-address gnats-admin`
if $NOTLOCKED; then
  mail-agent <<EOF
To: $GNATS_ADMIN_ADDR
Subject: $0: can\'t lock database

Unable to continue database check, because database in
$GNATS_ROOT could not be locked for five minutes.
EOF
  exit 1
fi

#
# Now, check for old lock files
#

find $GNATS_ROOT/gnats-adm/locks -type f -name '[0-9]*.lock' -mtime +1 -print > $TMPFILE
if [ -s $TMPFILE ]; then
  cat - $TMPFILE <<EOF | mail-agent
To: $GNATS_ADMIN_ADDR
Subject: $0: found old lock files

The following lock files in the database $GNATS_ROOT
are more than a day old:

EOF
fi

#
# Now, check for inconsistencies in the index file
#

# TODO: Handle all the databases.
INDEX=$GNATS_ROOT/gnats-adm/index
if [ ! -f $INDEX ]; then
  cat - <<EOF
$INDEX not found.

Perhaps GNATS is not configured yet?
See Gnats 'info' on how to set up the categories, responsible, & submitters
files.  In Gnats-info, type 'g' for goto, followed by 'Local configuration'.
EOF
  if [ "`tty`" = 'not a tty' ]; then
    if [ -f /etc/cron.daily/gnats ]; then
      CRONTAB=' in /etc/cron.daily/gnats'
    else
      CRONTAB=''
    fi
    cat - <<EOF

(It seems you have received this message from cron.  If this is annoying for
you, you can comment out the line in GNATS crontab$CRONTAB.
But do not forget to enable it again after GNATS is configured!)

EOF
  fi
fi

# TODO: Handle all the databases.
gen-index --import --export --numerical > $TMPFILE
gen-index --export --numerical > $TMPFILE.2
if diff $TMPFILE $TMPFILE.2 > $TMPFILE.3; then
  true
else
  cat - $TMPFILE.3 <<EOF | mail-agent
To: $GNATS_ADMIN_ADDR
Subject: $0: possible inconsistencies in database index

The following possible inconsistencies were found in
$GNATS_ROOT/gnats-adm/index.

Lines prefixed by '<' are from the current index file.  Lines
prefixed by '>' are from a fresh index generated with
@pkglibexecdir@/gen-index.

EOF
fi

rm -f ${TMPFILE}*
pr-edit --unlockdb
exit 0
