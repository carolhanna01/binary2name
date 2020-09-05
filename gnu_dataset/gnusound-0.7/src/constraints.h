/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

#ifndef CONSTRAINTS_H
#define CONSTRAINTS_H

#include <config.h>
#include <glib.h>
#include "region.h"

enum constraints_props {
    /** Region position may not change. */
    CONSTRAIN_POSITION = 1 << 0,
    /** Region length may not change. */
    CONSTRAIN_LENGTH = 1 << 1,
    /** Region data may not change. */
    CONSTRAIN_CONTENTS = 1 << 2
};

enum constraints_oper {
    CONSTRAINTS_OPER_INSERT,
    CONSTRAINTS_OPER_DELETE,
    CONSTRAINTS_OPER_REPLACE
};

struct constraints {
    GList *reasons;
    GList *regions;
    GList *props;
};

struct constraints *
constraints_new();

void 
constraints_destroy(struct constraints *cs);

int 
constraints_push(struct constraints *cs, 
                 const char *reason, 
                 struct region *r, 
                 enum constraints_props props);
int
constraints_pop(struct constraints *cs);

const char *
constraints_test(struct constraints *cs,
                 struct region *r,
                 enum constraints_oper oper);

#endif /* ! CONSTRAINTS_H */
