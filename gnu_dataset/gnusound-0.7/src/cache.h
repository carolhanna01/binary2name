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

#ifndef CACHE_H
#define CACHE_H

#include <config.h>
#include <stdlib.h>

typedef enum {
    CACHE_REAL,
    CACHE_NULL
} cache_type;

typedef struct _cache {
    cache_type type;
    size_t sz;
    void *data;
} cache;

cache *
cache_new(cache_type type,
          size_t sz);

cache *
cache_clone(cache *c,
            size_t off,
            size_t sz);

void 
cache_destroy(cache *c);

void
cache_clear(cache *c);

int
cache_resize(cache *c, 
             size_t sz);

void 
cache_move(cache *c,
           size_t new_off,
           size_t old_off,
           size_t sz);

cache *
cache_join(cache *c1,
           cache *c2);

void
cache_dump(cache *c);

#endif /* ! CACHE_H */
