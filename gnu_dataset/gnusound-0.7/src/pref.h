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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef PREF_H
#define PREF_H

/**
 * When this flag is set, returned value of pref doesn't change
 * between pref_init() and pref_exit(), but changes will be written to
 * disk on pref_sync(). See pref_set().
 */

#define PREF_WRITEONCE  (1 << 0)

/** Pref has been set. */

#define PREF_HASBEENSET (1 << 1)

#include <config.h>
#include <stdlib.h>

#define PREF_BOOL(name, def, desc) \
 { name, desc, PREF_INT_T, 0, 0, { .i = def }, { .i = def }, { .i = def }, 0, 1, NULL }
#define PREF_BOOL_WO(name, def, desc) \
 { name, desc, PREF_INT_T, PREF_WRITEONCE, 0, { .i = def }, { .i = def }, { .i = def }, 0, 1, NULL }
#define PREF_BOOL_FULL(name, flags, def, constrain, desc) \
 { name, desc, PREF_INT_T, flags, 0, { .i = def }, { .i = def }, { .i = def }, 0, 1, constrain }
#define PREF_INT_FULL(name, flags, def, min, max, constrain, desc) \
 { name, desc, PREF_INT_T, flags, 0, { .i = def }, { .i = def }, { .i = def }, min, max, constrain }
#define PREF_INT(name, def, min, max, desc) \
 { name, desc, PREF_INT_T, 0, 0, { .i = def }, { .i = def }, { .i = def }, min, max, NULL }
#define PREF_INT_WO(name, def, min, max, desc) \
 { name, desc,  PREF_INT_T, PREF_WRITEONCE, 0, { .i = def }, { .i = def }, { .i = def }, min, max, NULL }
#define PREF_FLOAT_FULL(name, flags, def, min, max, constrain, desc) \
 { name, desc,  PREF_FLOAT_T, flags, 0, { .f = def }, { .f = def }, { .f = def }, min, max, constrain }
#define PREF_FLOAT(name, def, min, max, desc) \
 { name, desc,  PREF_FLOAT_T, 0, 0, { .f = def }, { .f = def }, { .f = def }, min, max, NULL }
#define PREF_STRING_FULL(name, flags, def, constrain, desc) \
 { name, desc,  PREF_STRING_T, flags, 0, { .s = NULL }, { .s = def }, { .s = NULL }, 0, 0, constrain }
#define PREF_STRING(name, def, desc) \
 { name, desc,  PREF_STRING_T, 0, 0, { .s = NULL }, { .s = def }, { .s = NULL }, 0, 0, NULL }
#define PREF_STRING_WO(name, def, desc) \
 { name, desc,  PREF_STRING_T, PREF_WRITEONCE, 0, { .s = NULL }, { .s = def }, { .s = NULL }, 0, 0, NULL }

enum pref_type {
    PREF_FLOAT_T,
    PREF_INT_T,
    PREF_STRING_T
};

union pref_value {
    char *s;
    float f;
    int i;
};

struct pref { 
    char *name;
    const char *description;
    enum pref_type type;
    int flags;
    int accessed;

    /* The current value. */

    union pref_value value;

    /* The default value. */

    union pref_value def;

    /* The value that gets written to disk on pref_sync(). */

    union pref_value sync;

    /*
     * Minimum and maximum values for float and int types. If min and
     * max are equal, then no checking is performed.
     */

    double min;
    double max;

    /*
     * Optional constraint function, should return 0 if the value is
     * within constraints or was successfully clamped to a valid
     * value, or a non-zero value otherwise.
     *
     * The constraint function is free to modify the new_value to
     * conform to the constraints. If it wants to setup a new string
     * value, then it must free() the current string in new_value and
     * malloc() the new string.
     */

    int (*constrain)(const struct pref *pref,
                     union pref_value *new_value);
};

int
pref_init();

void
pref_exit();

int
pref_get_as_int(const char *name);

float
pref_get_as_float(const char *name);

const char *
pref_get_as_string(const char *name);

int 
pref_set_int(const char *name,
             int i);

int 
pref_set_float(const char *name,
               float f);

int 
pref_set_string(const char *name,
                const char *s);

void 
pref_sync();

void
pref_list();

void
pref_optimize();

char **
pref_match(const char *pattern);

int
pref_register(int count, struct pref *new);

int 
pref_load(const char *pattern);

int
pref_tests();

#endif /* ! PREF_H */

