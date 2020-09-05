#!/bin/sh
# Program to edit problem reports for GNATS.
# Copyright (C) 1993,94,95,96,98,99,2000,01,02,07 Free Software
# Foundation, Inc.
# Contributed by Jeffrey Osier (jeffrey@cygnus.com).
# Majorly revised by Bob Manson (manson@juniper.net).
# Further improvements by Dirk Bergstrom (dirk@juniper.net).
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

debug_print="false" # or echo to get output.
GNATS_PORT=
locked=
version="@VERSION@"

usage="
Usage: edit-pr  [-V|--version] [-h|--help] [-d|--database database_name]
                [-H|--host hostname] [-P|--port port_number]
                [-v|--user userid]   [-w|--passwd password] PR
"

# get current host name
if [ -z "$HOSTNAME" ]; then
  if [ -f /bin/hostname ] ; then HOSTNAME=`/bin/hostname`
  elif [ -f /usr/bin/hostname ] ; then HOSTNAME=`/usr/bin/hostname`
  # Solaris et al.
  elif [ -f /usr/ucb/hostname ] ; then HOSTNAME=`/usr/ucb/hostname`
  # Irix
  elif [ -f /usr/bsd/hostname ] ; then HOSTNAME=`/usr/bsd/hostname`
  fi
fi

# check to see if there is a $EDITOR; if not, use vi
[ -z "$VISUAL" ] &&
  if [ -z "$EDITOR" ]; then
    VISUAL=vi
  else
    VISUAL="$EDITOR"
  fi

# Parse command line. For the non-flag argument, assume it's pr PR id.

if [ $# -eq 0 ]; then
  echo "$usage" ; exit 1
fi

while [ $# -gt 0 ]; do
   case "$1" in
     -V|--version|--ve*)
       echo "$version"; exit 0
       ;;
     -d | --database)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; GNATS_DB="--database=$1" ;;
     -d=* | --database=*) GNATS_DB="$1" ;;

     -H | --host)
       if [ $# -eq 1 ]; then echo "$usage"; exit 1; fi
       shift ; GNATS_HOST="--host=$1" ;;
     -H=* | --host=*) GNATS_HOST="--host=`echo $1 | sed 's/^[-a-z]*=//'`" ;;

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

     -h|--help*)
       echo "$usage"; exit 0
       ;;
     -*)
       echo "$usage"; exit 1
       ;;

     *)
       if [ "`echo $1 | grep /`" != "" ]; then
         pr_id=`echo $1 | awk -F"/" '{print $2}' -`
       else
         pr_id=$1
       fi
       ;;
   esac
   shift
done

# set command here to always pass host and port, and directory if supplied
QUERY_PR="query-pr $GNATS_HOST $GNATS_PORT $GNATS_DB $EDIT_USER $EDIT_PASSWD"
PR_ADDR="$QUERY_PR --responsible-address"
PR_EDIT="pr-edit $GNATS_HOST $GNATS_PORT $EDIT_USER $EDIT_PASSWD $GNATS_DB"

# These traps take care of deleting all the /tmp files
trap 'rm -f $new.old $change_msg $fixfil' 0
# Don't delete $new on error signals
trap 'if [ "$locked" != "" ]; then \
        $PR_EDIT --unlock $pr_id ; \
	locked= ; \
      fi' 1 2 3 13 15

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

if [ -n "$HOSTNAME" ]; then
  full_me="$me@$HOSTNAME"
else
  full_me="$me"
fi

# new = temp file to use for editing
new="/tmp/ep$$"
newtmp="/tmp/ep$$.tp"
change_msg="/tmp/ep$$.ch"
fixfil="/tmp/ep$$.fx"

# lock the pr
$debug_print "Locking $pr_id."
lock=`$PR_EDIT --lock=$full_me --process=$$ $pr_id 2>&1 > $new`
locked=t

if [ "$lock" != "" ] ; then
  echo $lock
  exit 1
fi

rm -f $fixfil

# Now add any missing fields, along with a description.

$QUERY_PR --list-fields | while read field
do
    grep -i "^>${field}:" "$new" > /dev/null 2>&1
    if [ $? != 0 ]
    then
	$QUERY_PR --field-flags "$field" | grep -i readonly > /dev/null 2>&1
	if [ $? != 0 ]
	then
	    type="`$QUERY_PR --field-type $field`"
	    case $type in
		[Ee][Nn][Uu][Mm])
		    values=`
		        $QUERY_PR --valid-values $field |
			    tr '\n' ' ' |
			    sed 's/ *$//g; s/ / | /g;s/^/[ /;s/$/ ]/;'
		    `
		    valslen=`echo "$values" | wc -c`
		    if [ "$valslen" -gt 160 ]
		    then
			desc="<`$QUERY_PR --field-description $field` (one line)>";
		    else
			desc="<${values} (one line)>";
		    fi
		    dpat=`echo "$desc" | LC_ALL=C tr ']\133*+^$|\134()&/' '............'`
		    echo "/^>${field}:/ s/${dpat}//" >> $fixfil
		    echo "/>${field}: ${desc}" >> $new;
		    ;;
		[Mm][Uu][Ll][Tt][Ii][Tt][Ee][Xx][Tt])
		    desc="	<`$QUERY_PR --field-description $field` (multiple lines)>";
		    dpat=`echo "$desc" | LC_ALL=C tr ']\133*+^$|\134()&/' '............'`
		    echo "s/^${dpat}//" >> $fixfil
		    echo ">${field}:" >> $new;
		    echo "$desc" >> $new;
		    ;;
		*)
		    desc="<`$QUERY_PR --field-description $field` (one line)>"
		    dpat=`echo "$desc" | LC_ALL=C tr ']\133*+^$|\134()&/' '............'`
		    echo "/^>${field}:/ s/${dpat}//" >> $fixfil
		    echo ">${field}: ${desc}" >> $new
		    ;;
	    esac
	else
	    prevfld="$field";
	fi
    fi
done

# here's where we actually call the editor.
cp $new $new.old
$VISUAL $new
if cmp -s $new.old $new ; then
  echo "edit-pr: PR not changed"
  $PR_EDIT --unlock $pr_id
  exit 0
fi

if [ -f $fixfil ]
then
    sed -f $fixfil < $new > $newtmp
    mv $newtmp $new
    sed -f $fixfil < $new.old > $newtmp
    mv $newtmp $new.old
    rm -f $fixfil
fi

# error-check output by calling pr-edit --check; if mistakes exist,
# call $VISUAL or exit
checking=t
while [ "$checking" != "" ]; do
  errors="`$PR_EDIT --check < $new 2>&1`"
  if [ "$errors" != "" ]; then
    echo "Hit \`return\` to fix the following errors, or type \'quit\' to quit:"
    echo "$errors"
    read fixme
    case "$fixme" in
      q* | Q*) 
        echo "PR $pr_id not updated: changed file is in $new.changed"
        mv $new $new.changed
	$PR_EDIT --unlock $pr_id
        exit 0
        ;;
    esac
    $VISUAL $new
  else
    checking=
  fi
done

exec 3<&0

#
# Find out what fields have changed; if the changed field requires a
# change reason, then ask about it.
#
diff-prs $new.old $new | while read field
do
    flags=`$QUERY_PR --field-flags $field` || echo "edit-pr: Invalid field name $field" 1>&2;

    if echo "$flags" | grep -i "requirechangereason" > /dev/null 2>&1
    then
        echo ">${field}-Changed-Why:" >> $change_msg;
	echo "Why did the $field field change? (Ctrl-D to end)";
	cat 0<&3  >> $change_msg;
    fi
done

if [ -f $change_msg ]
then
    cat $change_msg >> $new
fi

if email=`$QUERY_PR \
    --adm-field=Responsible \
    --adm-key="$me" \
    --adm-subfield=alias 2> /dev/null` &&
    [ -n "$email" ]; then
  : using email from responsible database
else
  email=$full_me
fi

#
# Submit the changed PR.
#
while true; do
    if $PR_EDIT --email-addr "$full_me" $pr_id < $new
    then
	echo "Edit successful"
	# we need to unlock the PR
	$PR_EDIT --unlock $pr_id
	exit 0
    else
	echo "Problems with edit submission."
    fi
    while true; do
	echo "a)bort or r)etry? "
	read input
	case "$input" in
	    a*)
		echo "Cancelling edit.  Changed PR is in $new."
		# we need to ulock the PR no matter what
		$PR_EDIT --unlock $pr_id
		exit 1 ;;
	    r*)
		break ;;
	     *)
		echo "Unrecognized input '$input'"
	        ;;
	esac
    done
done

rm -f $new

exit 0
