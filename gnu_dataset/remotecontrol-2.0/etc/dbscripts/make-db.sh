#!/bin/bash

# Copyright (C) 2012-2013 GNU remotecontrol authors.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

DB_NAME=$1
DB_USER=$2
DB_PASS=$3
DB_TEST=$4
DB_ROOT=$5

read_password() {
  echo -n "Enter MySQL root password: "
  stty -echo
  read password;
  stty echo
  echo ""
  if [ "$password" = "" ]; then exit 1; fi
}

read_db_name() {
  if [ -n "$DB_NAME" ]; then
    db_name=$DB_NAME
  else
    echo -n "Enter GNUrc database name (default: gnurc): "
    read db_name;
    echo ""
    if [ "$db_name" = "" ]; then db_name="gnurc"; fi
  fi
}

read_db_user() {
  if [ -n "$DB_USER" ]; then
    db_user=$DB_USER
  else
    echo -n "Enter GNUrc database user (default: gnurc_user): "
    read db_user;
    echo ""
    if [ "$db_user" = "" ]; then db_user="gnurc_user"; fi
  fi
}

read_db_pass() {
  if [ -n "$DB_PASS" ]; then
    db_pass=$DB_PASS
  else
    echo -n "Enter GNUrc database user password: "
    stty -echo
    read db_pass;
    echo ""
    if [ "$db_pass" = "" ]; then
      stty echo
      exit 2
    fi
    echo -n "Confirm password: "
    read db_pass_confirm;
    echo ""
    stty echo
    if [ "$db_pass" != "$db_pass_confirm" ]; then
      echo "Passwords do not match.";
      exit 3;
    fi
  fi
}

read_load_test_data() {
  if [ -n "$DB_TEST" ]; then
    load_test_data=$DB_TEST
  else
    echo -n "Load test data? (Y,n): "
    read load_test_data;
    echo ""
    if [ "$load_test_data" = "" ]; then load_test_data='y'; fi
    case "$load_test_data" in
      [yY] | [yY][eE][sS] )
        load_test_data='y'
        ;;
      [nN] | [nN][oO] )
        load_test_data='n'
        ;;
      *) echo "You must specify yes or no." && exit 4
    esac
  fi
}

if [ -f "/root/mysql.root" ]; then
  password="`cat /root/mysql.root`"
elif [ -n "$DB_ROOT" ]; then
  password=$DB_ROOT
else
  read_password
fi

read_db_name
read_db_user
read_db_pass
read_load_test_data

cd `dirname "$0"`

mysql="mysql -u root -p$password $db_name "

mysql -u root -p$password <<EOF

drop database if exists \`$db_name\`;

CREATE DATABASE $db_name
        CHARACTER SET utf8
        COLLATE utf8_general_ci;
EOF

[ "$?" -ne "0" ] && echo "Error creating database." && exit 5

$mysql < ./3-v2-schema.sql
[ "$?" -ne "0" ] && echo "Error loading database schema." && exit 6

$mysql < ./4-v2-default-languages.sql
[ "$?" -ne "0" ] && echo "Error loading default languages." && exit 7

if [ "$load_test_data" = "y" ]; then
  $mysql < ./5-v2-test-data.sql
  [ "$?" -ne "0" ] && echo "Error loading test data." && exit 8
fi

mysql -u root -p$password <<EOF
create user '$db_user' identified by '$db_pass';
EOF

# ignore error as user may already exist...
mysql -u root -p$password >/dev/null 2>&1 <<EOF
grant all on \`$db_name\`.* to '$db_user';
flush privileges;
EOF

[ "$?" -ne "0" ] && echo "Error creating database user." && exit 9

true
