#!/bin/sh
# @configure_input@
#
# Update the SUBMITTER variable in the site or personal send-pr config file
# Copyright (C) 1993,2004,07 Free Software Foundation, Inc.
# Contributed by Brendan Kehoe (brendan@cygnus.com), based on a
# version written by Heinz G. Seidl (hgs@ide.com).
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

VERSION="@VERSION@"

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

COMMAND=`echo $0 | sed -e 's,.*/,,g'`
usage() {
	cat << __EOF__ >&2
Usage: $COMMAND [OPTIONS] submitter-id

OPTION		DESCRIPTION
--help		This message
--version	Print version and exit
--site-config	Update the site configuration file

$COMMAND updates the SUBMITTER variable with 'submitter-id' in the site or
personal send-pr config file.  You may need to have root permissions in order
to update the site configuration file.  This application requires the sed script to work.
__EOF__
}

# Is the mktemp command available?
MKTEMP="@MKTEMP@"

# TEMP: Temporary copy of the config file, to be edited by the user.
if [ -z "$TMPDIR" ]; then
  TMPDIR=/tmp
else
  if [ "`echo $TMPDIR | grep '/$'`" != "" ]; then
    TMPDIR="`echo $TMPDIR | sed -e 's,/$,,'`"
  fi
fi

if [ $MKTEMP = yes ]; then
  TEMP=`mktemp $TMPDIR/pXXXXXX` || exit 1
else
  TEMP=$TMPDIR/p$$
  : > $TEMP || exit 1
fi

if [ $# -eq 0 ]; then
  usage
  exit 1
fi

# Default configuration file
CONFIGFILE="$HOME/.send-pr.conf"

# Leave this blank
SUBMITTER=

# Process the commandline
while [ $# -gt 0 ]; do
  case "$1" in
    --site-config) CONFIGFILE="${sysconfdir}/send-pr.conf" ;;
    --version) echo $COMMAND version $VERSION ; exit 1 ;;
    -*) usage; exit 1 ;;
    *) SUBMITTER=$1 ;;
  esac
  shift
done

trap 'rm -f $TEMP ; exit 0' 0
trap 'echo "$COMMAND: Aborting ..."; rm -f $TEMP ; exit 1' 1 2 3 13 15

if [ `echo ${SUBMITTER}| \
	sed -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'` = "unknown" ] ;
then
	echo "$COMMAND: SUBMITTER cannot be 'unknown'" >&2
	exit 1
fi

if [ ! -e $CONFIGFILE ] ; then
	echo "$COMMAND: No configuration file was found.  Creating." >&2
	cat << __EOF__ > $CONFIGFILE
# Configuration file for send-pr
# ** CREATED BY $COMMAND **
SUBMITTER="$SUBMITTER"
__EOF__
	if [ $? -ne 0 ] ; then
		echo "$COMMAND: Error in creating $CONFIGFILE" >&2
		exit 1
	fi
else 
	sed -e "s/^#\?SUBMITTER=.*/SUBMITTER=${SUBMITTER}/" $CONFIGFILE > $TEMP
	if ! ( grep $SUBMITTER $TEMP > /dev/null ) ; then
		exit 1
	fi
	if ! ( cat $TEMP > $CONFIGFILE ) ; then
		echo "$COMMAND: Error in updating $CONFIGFILE" >&2
		exit 1
	fi	
fi

echo "$COMMAND: \`$SUBMITTER' has been updated in $CONFIGFILE for send-pr" >&2

exit 0
