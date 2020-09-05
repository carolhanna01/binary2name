#!/bin/sh
#
# Copyright (C) 1990-2003 Free Software Foundation, Inc.
# Written by the Dominion project.
#
# This file is part of Dominion.
#
# Dominion is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 1, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#

##
## This script runs what needs to be run to create a new world for
## dominion.  The usage is
##      make-new-world.sh [options] [game_path] [CN_file]
## (not yet: for now it is "make-new-world.sh game_path CN_file")

if [ $# != 2 ]
then
    echo usage: $0 game_path CN_file
    exit 1
fi

GAME_PATH=$1
CN_FILE=$2

if [ -d ${GAME_PATH} -o -f ${GAME_PATH} ]
then
    echo "directory ${GAME_PATH} already exists;"
    echo "please remove it with"
    echo "    /bin/rm -rf ${GAME_PATH}"
    echo "or use a different path for this game."
    exit 1
fi

./mkinstalldirs ${GAME_PATH}
./mkinstalldirs ${GAME_PATH}/misc
./mkinstalldirs ${GAME_PATH}/exec
./mkinstalldirs ${GAME_PATH}/options
./mkinstalldirs ${GAME_PATH}/params
./mkinstalldirs ${GAME_PATH}/magic
./mkinstalldirs ${GAME_PATH}/mail
./mkinstalldirs ${GAME_PATH}/news

./install-sh -c races ${GAME_PATH}/misc
./install-sh -c army_types ${GAME_PATH}/misc
./install-sh -c cns ${GAME_PATH}/misc
./install-sh -c cns.small ${GAME_PATH}/misc
for i in mag_*
do
    echo -n $i " "
    ./install-sh -c $i ${GAME_PATH}/magic
done

echo; echo

dom_make -d ${GAME_PATH} || exit 1
dom_add -d ${GAME_PATH} -f ${CN_FILE} || exit 1

echo
echo "The world has been created, and the computer nations from"
echo "the file " ${CN_FILE} " have been added."
echo
