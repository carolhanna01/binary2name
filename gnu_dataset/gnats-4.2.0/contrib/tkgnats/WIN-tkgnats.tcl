# TkGnats startup file for Win95/98/NT.
# Don't use this for UNIX; it only makes sense for Windows ShortCuts.

# Each individual user gets a copy of this file.

# Copy it to $TkGnats(UserDir)/tkgnats.tcl

# Windows: TkGnats(UserDir) is {home}/tkgnats
#          where {home} is any directory that belongs to an individual.

# Make a shortcut to this file and set the "Start In:" field to
# the directory where TkGnats itself resides.

# No need to edit anything in this file.

set TkGnats(UserDir)    [file dirname [info script]]
set TkGnats(UserSubdir) [file tail    $TkGnats(UserDir)]
set env(HOME)           [file dirname $TkGnats(UserDir)]
set TkGnats(TKGNATSINI) $TkGnats(UserDir)/tkgnats.ini

source tkgnats
