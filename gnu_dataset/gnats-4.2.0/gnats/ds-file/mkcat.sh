#!/bin/sh -
# Create directories for each category in a GNATS categories file. 
# Copyright (C) 1993,94,95,2007 Free Software Foundation, Inc.
# Contributed by Brendan Kehoe (brendan@cygnus.com) and
# Tim Wicinski (wicinski@barn.com).
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

prog=mkcat
USAGE="Usage: $prog [--help] [--database=databasename]"

# process command line options
while [ "$#" -gt 0 ]; do
  case "$1" in
    -d | --database)
      if [ $# -eq 1 ]; then echo "$USAGE" >&2; exit 1; fi
      shift ; GNATSDB="$1" ; export GNATSDB ;;
    --database=*) GNATSDB=`echo "$1" | cut -d= -f2-` ; export GNATSDB ;;
    -*)  echo "$USAGE" >&2; exit 1 ;;
  esac
  shift
done

QUERY_PR="query-pr"
GNATS_DB_DIR=`"$QUERY_PR" --print-directory-for-database` || exit

# verify gnats root
if [ ! -d "${GNATS_DB_DIR}" ] ; then
   echo "$prog: No directory $GNATS_DB_DIR" >&2
   exit 1
fi

# get permission for new category directories
perm=`
  awk < "$GNATS_DB_DIR/gnats-adm/dbconfig" '
    $1 == "category-dir-perms" {print $2; exit}' | tr -d '"'
`
perm=${perm:-755}

$QUERY_PR --list-categories | awk -F: '{print $1}' | while read i
do
    if [ ! -d "$GNATS_DB_DIR/$i" ]; then
	mkdir "$GNATS_DB_DIR/$i" &&
	  chmod "$perm" "$GNATS_DB_DIR/$i" &&
	  echo "Category \`$GNATS_DB_DIR/$i' created."
    fi
done

exit 0
