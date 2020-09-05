#! /bin/sh
# Create initial database structure.
# Copyright (C) 1999,2001,02,07 Free Software Foundation, Inc.
# Contributed by Bozo Bob (manson@juniper.net).
# Further hacked by Milan Zamazal (pdm@zamazal.org).
#
# This file is part of GNU GNATS.  
# (GNNOUUUUUUUUU GNATS!  GNOUUUUUUUUU GNATS!  Doesn't that have a nice
# sound?  Especially if you say all the Gs?)
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

GNATS_USER=@GNATS_USER@
DATABASES=@GLOBAL_DB_LIST_FILE@
DEFAULTSDIR="@sysconfdir@/gnats/defaults"

domkdir() {
    mkdir -p "$1" || { echo "Can't create directory $1, exiting"; exit 1 ; }
    chown "${GNATS_USER}" "$1"
}

docp() {
    cp $DEFAULTSDIR/"$1" "$2" || { echo "Can't copy file $DEFAULTSDIR/$1 to $2, exiting"; exit 1 ; }
    chown "${GNATS_USER}" "$2"
}

prog=mkdb
USAGE="Usage: $prog [--help] DATABASE-NAME"
while [ $# -gt 0 ]; do
  case "$1" in
    -*)  echo "$USAGE" ; exit 1 ;;
     *)  if [ -n "$database" ] ; then
	    echo "$USAGE" 1>&2 ; exit 1 ;
	 else
	    database="$1" ;
	 fi ;;
  esac
  shift
done

if [ -z "${database}" ]
then
    echo "$USAGE" 1>&2 ;
    exit 1;
fi

dbdir=`grep "^${database}:" $DATABASES | sed -n 's/^'${database}':[^:]*:\([^:]*\)$/\1/p'`
if [ -z "${dbdir}" ]
then
    echo "$prog: No proper entry for ${database} in ${DATABASES}:"
    grep "^${database}:" $DATABASES
  exit 1
fi

if [ -d "${dbdir}" ]
then
  echo "$prog: There's already a directory ${dbdir}, exiting" !>&2
  exit 1
fi

domkdir "${dbdir}"

domkdir "${dbdir}/gnats-adm"
domkdir "${dbdir}/gnats-adm/locks"
domkdir "${dbdir}/gnats-queue"
domkdir "${dbdir}/pending"

echo "Copying default files from ${DEFAULTSDIR}"

docp categories "${dbdir}/gnats-adm/categories"
docp submitters "${dbdir}/gnats-adm/submitters"
docp responsible "${dbdir}/gnats-adm/responsible"
docp gnatsd.user_access "${dbdir}/gnats-adm/gnatsd.user_access"
chmod 600 "${dbdir}/gnats-adm/gnatsd.user_access"
docp addresses "${dbdir}/gnats-adm/addresses"
docp states "${dbdir}/gnats-adm/states"
docp classes "${dbdir}/gnats-adm/classes"
docp dbconfig "${dbdir}/gnats-adm/dbconfig"

gen-index -d "${database}" -o "${dbdir}/gnats-adm/index"
