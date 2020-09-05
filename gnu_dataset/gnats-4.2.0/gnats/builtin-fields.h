/* Define the full set of builtin fields.
   Copyright (C) 1999,2000,07 Free Software Foundation, Inc.
   Contributed by Bob Manson (manson@juniper.net).

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#ifndef BUILTIN_FIELDS_H
#define BUILTIN_FIELDS_H

#define NUM_BUILTIN_FIELDS 16

typedef struct pr_builtin_field *PR_BuiltinField;

#include "field.h"

/* Make sure these definitions match the ones in pr-init.c!  (In
   particular, the numbers have to match the order that the names
   appear in the list.)  */
#define NUMBER(DATABASE) (getBuiltinField ((DATABASE), 0))
#define CATEGORY(DATABASE)  (getBuiltinField ((DATABASE), 1))
#define SYNOPSIS(DATABASE) (getBuiltinField ((DATABASE), 2))
#define CONFIDENTIAL(DATABASE) (getBuiltinField ((DATABASE), 3))
#define SEVERITY(DATABASE) (getBuiltinField ((DATABASE), 4))
#define PRIORITY(DATABASE) (getBuiltinField ((DATABASE), 5))
#define RESPONSIBLE(DATABASE) (getBuiltinField ((DATABASE), 6))
#define STATE(DATABASE) (getBuiltinField ((DATABASE), 7))
#define SUBMITTER(DATABASE) (getBuiltinField ((DATABASE), 8))
#define ARRIVAL_DATE(DATABASE) (getBuiltinField ((DATABASE), 9))
#define CLOSED_DATE(DATABASE) (getBuiltinField ((DATABASE), 10))
#define LAST_MODIFIED(DATABASE) (getBuiltinField ((DATABASE), 11))
#define ORIGINATOR(DATABASE) (getBuiltinField ((DATABASE), 12))
#define DESCRIPTION(DATABASE) (getBuiltinField ((DATABASE), 13))
#define AUDIT_TRAIL(DATABASE) (getBuiltinField ((DATABASE), 14))
#define UNFORMATTED(DATABASE) (getBuiltinField ((DATABASE), 15))

#endif
