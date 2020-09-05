#    Copyright (C) 1990, 1993, 1996, 1997, 1999 Aladdin Enterprises.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU General Public License for full details.
# 
# Everyone is granted permission to copy, modify and redistribute GNU
# Ghostscript, but only under the conditions described in the GNU General
# Public License.  A copy of this license is supposed to have been given
# to you along with GNU Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# $RCSfile: unixhead.mak,v $ $Revision: 1.2.2.1 $
# Partial makefile common to all Unix configurations.

# This part of the makefile gets inserted after the compiler-specific part
# (xxx-head.mak) and before gs.mak, devs.mak, and contrib.mak.

# ----------------------------- Generic stuff ----------------------------- #

# Define the platform name.  For a "stock" System V platform,
# use sysv_ instead of unix_.

PLATFORM=unix_

# Define the syntax for command, object, and executable files.

# Work around the fact that some `make' programs drop trailing spaces
# or interpret == as a special definition operator.
NULL=

CMD=
C_=-c
D_=-D
_D_=$(NULL)=
_D=
I_=-I
II=-I
_I=
NO_OP=@:
O_=-o $(NULL)
OBJ=o
Q=
XE=
XEAUX=

# Define the current directory prefix and command invocations.

CAT=cat
D=/
EXP=
SHELL=/bin/sh
SH=$(SHELL)

# Define generic commands.

CP_=cp
RM_=rm -f
RMN_=rm -f

# Define the arguments for genconf.

CONFILES=-p "%s&s&&" -pl "&-l%s&s&&" -pL "&-L%s&s&&"
CONFLDTR=-ol

# Define the compilation rules and flags.

CC_D=$(CC_)
CC_INT=$(CC_)

BEGINFILES=

# Patch a couple of PC-specific things that aren't relevant to Unix builds,
# but that cause `make' to produce warnings.

PCFBASM=

# Define the default build rule, so the object directories get created
# automatically.

std: STDDIRS default
	$(NO_OP)
