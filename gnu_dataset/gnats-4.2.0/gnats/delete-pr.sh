#! /bin/sh
# Program to delete problem reports for GNATS.
# Copyright (C) 2007  Free Software Foundation, Inc.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU GNATS; see the file COPYING. If not, see
# <http://www.gnu.org/licenses/>.
#
# Contributed by Kevin Hopkins (K.Hopkins@cs.nott.ac.uk).

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

GNATS_USER="@GNATS_USER@"

locked=
closed=
closed_state=

usage="Usage: $0 [-fhV] [--version] [--help] [ -c | --closed | PR ]"
version="@VERSION@"

# parse command line.
# for the non-flag argument, assume it's correct.  if it's a full
# id number, use that; if not, find the full id.
# only continue if $full_id is an actual file.

if [ $# -eq 0 ]; then
  echo "$usage" ; exit 1
fi
case "$1" in
  -V|-v|--version|--ve*)
    echo "$version"; exit 0
    ;;
  -h|--help*)
    echo "$usage"; exit 0
    ;;
  -d|--database)
    if [ "$#" -le 1 ] ; then echo "$usage"; exit 1; fi
    shift ; GNATS_DB="--database=$1" ;;
  -d=*|--database=*) GNATS_DB="$1" ;;
  -c|--closed)
    if [ "$#" -eq "1" ] ; then
      closed=t
    else
      echo "$usage"; exit 1
    fi
    ;;
  -*)
    echo "$usage"; exit 1
    ;;
  *)
    pr=$1
    ;;
esac

if [ "$closed" = "t" ]
then
    prs=`query-pr $GNATS_DB --format Number --state closed`
else
    if [ "x$pr" = "x" ]
    then
	echo "Must specify either a PR to delete or the --closed option.";
	exit 1
    fi
    prs=$pr
fi

if [ "`echo -n`" = "-n" ]
then	echon () { echo $* \\c ; }
else	echon () { echo -n $* ; }
fi

if [ -z "$HOSTNAME" ]; then
  if [ -f /bin/hostname ] ; then HOSTNAME=`/bin/hostname`
  elif [ -f /usr/bin/hostname ] ; then HOSTNAME=`/usr/bin/hostname`
  # Solaris et al.
  elif [ -f /usr/ucb/hostname ] ; then HOSTNAME=`/usr/ucb/hostname`
  # Irix
  elif [ -f /usr/bsd/hostname ] ; then HOSTNAME=`/usr/bsd/hostname`
  fi
fi

# find a user name
if [ "$USER" != "" ]; then
  me=$USER
else
  if [ "$LOGNAME" != "" ]; then
    me=$LOGNAME
  else
    echo "edit-pr: no user name found---set LOGNAME." ; exit 1
  fi
fi

if [ "x$me" != "x$GNATS_USER" ]
then
    echo "To delete a PR, you must be $GNATS_USER";
    exit 1
fi

if [ -n "$HOSTNAME" ]; then
  full_me="$me@$HOSTNAME"
else
  full_me="$me"
fi

# start of loop
for prnum in $prs
do
    # XXX ??? !!! Should use State[type] here instead, but then we'd have
    # to change the check for a valid PR number, because non-closed states
    # are usually returned as the empty string for a type.
    state=`query-pr $GNATS_DB --format State $prnum`

    case $state in
	"")
	    echo "No such PR as $prnum" 1>&2
	    continue
	    ;;
	[Cc]losed)	closed_state=t ;;
	*)		closed_state="" ;;
    esac

    if [ "$closed_state" = "t" ]
    then
	echon "Do you want to delete problem $prnum? [n]"
	read answer junk < /dev/tty
	case $answer in
	    [yY]*)
		;;
	    [qQ]*)
		exit 0
		;;
	    *)
		continue;
	        ;;
	esac
    else
	echo "To delete $prnum, it must be closed.";
	exit 1
    fi

    pr-edit $GNATS_DB --email-addr "$full_me" --delete-pr $prnum
    if [ $? != 0 ]
    then
	continue;
    fi
# end of loop
done

exit 0
