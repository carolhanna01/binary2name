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

#include <config.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "mem.h"
#include "track.h"
#include "cache.h"

void
cache_dump(cache *c) {
    /*
    INFO("%10"CONVSPEC_SIZE_T"/%10"CONVSPEC_SIZE_T"(%10"CONVSPEC_SIZE_T") %p %s\n", 
         c->low,
         c->high,
         c->sz,
         c->data, 
         c->type == CACHE_NULL ? "null cache" : "");
    */
}


cache *
cache_clone(cache *c,
            size_t off,
            size_t sz) {
    cache *c2;

    assert(sz != 0);
    assert(off + sz <= c->sz);

    c2 = cache_new(c->type, sz);

    if(!c2){ 
        FAIL("cannot create new cache for clone (%ld bytes)\n",
             (long)sz);
        return NULL;
    }

    switch(c->type) {
    case CACHE_NULL:
        return c2;
    case CACHE_REAL:
        memcpy(c2->data, c->data + off, sz);
        return c2;
    }

    return NULL;
}

void 
cache_move(cache *c,
           size_t new_off,
           size_t old_off,
           size_t sz) {
    switch(c->type) {
    case CACHE_NULL:
        return;
    case CACHE_REAL:
        memmove(c->data + new_off, c->data + old_off, sz);
    }
}

int
cache_resize(cache *c, 
             size_t sz) {
    void *buf;

    switch(c->type) {
    case CACHE_NULL:
        c->sz = sz;
        return 0;
    case CACHE_REAL:

        /* Promise that resize always succeeds if the new size is smaller
           than the old size. */
        
        buf = c->data;
        if(sz > c->sz) {
            buf = mem_realloc(c->data, sz);
            if(!buf) {
                FAIL("could not grow cache (old size: %ld, "
                     "new size: %ld)\n", (long)c->sz, (long)sz);
                return 1;
            }
        }
        c->data = buf;
        c->sz = sz;
        return 0;
    }
    return 0;
}

cache *
cache_new(cache_type type,
          size_t sz) {
    cache *c = mem_alloc(sizeof(cache));
    if(!c) {
        DEBUG("failed to create cache object (%ld bytes)\n", 
              (long)sz);
        return NULL;
    }
    c->type = type;
    c->sz = sz;

    switch(c->type) {
    case CACHE_NULL:
        c->data = NULL;
        return c;
    case CACHE_REAL:
        c->data = mem_alloc(sz);
        if(!c->data) {
            DEBUG("failed to allocate %ld bytes\n", (long)sz);
            mem_free(c);
            c = NULL;
            return NULL;
        }
        return c;
    }
    return c;
}

void
cache_destroy(cache *c) {
    switch(c->type) {
    case CACHE_NULL:
        break;
    case CACHE_REAL:
        if(c->data) {
            mem_free(c->data);
            c->data = NULL;
        }
    }
    mem_free(c);
    c = NULL;
}

#if 0
cache *
cache_join(cache *c1,
           cache *c2) {
    void *d;
    if(c1->type != c2->type) {
        FAIL("cannot join caches of different types\n");
        return NULL;
    }
    if(c1->type == CACHE_NULL) {
        c1->sz += c2->sz;
        c1->high = c1->sz;
        return c1;
    }
    if(c1->type != CACHE_REAL) {
        FAIL("cannot join caches of type %d\n", c1->type);
        return NULL;
    }
    if(c1->high != c1->sz) {
        FAIL("first cache must be full for join c1: \n");
        cache_dump(c1);
        FAIL("c2: \n");
        cache_dump(c2);
        return NULL;
    }

    //    DEBUG("before join:\n");
    //    DEBUG("c1:\n");
    //    cache_dump(c1);
    //    DEBUG("c2:\n");
    //    cache_dump(c2);
    d = realloc(c1->data, c1->sz + c2->sz);

    if(!d) {
        FAIL("cannot get enough memory to join caches (need %"CONVSPEC_SIZE_T" bytes joined)\n", 
             c1->sz + c2->sz);
        return NULL;
    }

    c1->data = d;
    memcpy(c1->data + c1->sz, c2->data, c2->sz);
    c1->sz += c2->sz;
    c1->high += c2->high;
    //    DEBUG("after join:\n");
    //    DEBUG("c1:\n");
    //    cache_dump(c1);
    return c1;
}
#endif
