#!/bin/sh
# Program to process GNATS queries via email.
# Copyright (C) 1993,94,95,2007 Free Software Foundation, Inc.
# Contributed by Jason Merrill (jason@cygnus.com).
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

GNATS_ADMIN_ADDR="`query-pr --responsible-address gnats-admin`"

PATH="@pkglibexecdir@:@sbindir@:@bindir@:/bin:/usr/bin:${PATH}"
export PATH

# Don't expand globs for the arguments to query-pr.
set -f

if [ -n "$GNATSDB" ]; then
  DATABASE="--database=\"$GNATSDB\""
fi
to=
args=
oldIFS="$IFS"
while true; do
  IFS=:
  read header contents
  IFS="$oldIFS"
  [ -z "$header" ] && break;

  contents="`echo $contents | sed 's/^  *//'`"
  
  [ "$header" = "From" -a -z "$to" ] && to="$contents"
  [ "$header" = "Reply-To" ] && to="$contents"
  [ "$header" = "Subject" ] && args="$contents"
done

mail=/tmp/query$$

exec 3>&1 4>&2 > $mail 2>&1

if [ -n "$to" ]; then
  echo "To: $to"
  echo "Subject: query-pr output [$args]"
  echo
  case $args in
    "")
      cat << __EOF__
Your query specified no constraints.  This is probably not what you wanted;
unconstrained queries get very large very fast.  If you really want to see
every non-confidential, non-closed PR in the database, specify some dummy
constraint like \`--category='.

To use this mail server, just include arguments to query-pr in the Subject:
line.  The options for query-pr are outlined below.

__EOF__
      query-pr --help
      ;;
    *query-pr*)
      query-pr --restricted --state 'o|a|f|s' \
	`echo $args | sed 's/^.*query-pr//'` $DATABASE $*;;
    *)
      query-pr --restricted --state 'o|a|f|s' $args $DATABASE $*;;
  esac
else
  echo "To: $GNATS_ADMIN_ADDR"
  echo "Subject: query-pr request failed"
  echo
  echo "Subject line:$args"
  echo "Body of message:"
  cat
fi

exec >&- 1>&3 2>&4

if [ `wc $mail | awk '{print $1}'` -lt 4 ]; then
  cat >> $mail 2>&1 << __EOF__
Your query produced no output.  Here are some of the possible causes:

1) There are no matching PRs.  You may want to try different parameters.

2) All matching PRs are confidential.  For security reasons, this mail server
   does not display confidential PRs.

3) All matching PRs are closed.  By default, this mail server does not display
   closed PRs; to override this behavior, specify \`--state=' (i.e. match any
   state) in your query.

__EOF__
  query-pr --help >> $mail 2>&1
fi

mail-agent < $mail

[ $? -eq 0 ] && rm -f $mail
