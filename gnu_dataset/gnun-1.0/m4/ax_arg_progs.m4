# Copyright (C) 2017 Free Software Foundation, Inc.

# This file is part of GNUnited Nations.

# GNUnited Nations is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# GNUnited Nations is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNUnited Nations.  If not, see <http://www.gnu.org/licenses/>.

# SYNOPSIS
#
#   AX_ARG_PROGS(VAR, DESCRIPTION, PROGS)
#
# DESCRIPTION
#
#  Set variable VAR as a precious variable with DESCRIPTION,
#  when it's set, use its value in AC_PATH_PROGS; otherwise
#  use PROGS.

AC_DEFUN([AX_ARG_PROGS], [
  AC_ARG_VAR([$1], [$2])
  AS_CASE(["x$$1"], [x], [ax_arg_progs="$3"], [ax_arg_progs=$$1])
  AC_PATH_PROGS([$1], [$ax_arg_progs])
]) dnl AX_ARG_PROG
