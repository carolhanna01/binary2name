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

#ifndef BLOCKLIST_H
#define BLOCKLIST_H

#include <config.h>
#include <glib.h>
#include "audiofile.h"
#include "sample.h"

#define MAX_BCACHE_ENTRIES 25

struct bcache {
    AFframecount offset;
    GList *block;
    short is_valid;
    short hits;
};

struct blocklist {
    AFframecount count;
    GList *l;

    /* Tail pointer, points at last block in list. */

    GList *tail;
    struct bcache bc[MAX_BCACHE_ENTRIES];
};

struct blocklist *
blocklist_new(GList *l);

void
blocklist_destroy(struct blocklist *bl);

struct blocklist *
blocklist_clone(struct blocklist *bl);

int
blocklist_find_block(struct blocklist *bl,
                     AFframecount *offset,
                     GList **block);

int
blocklist_delete(struct blocklist *bl,
                 GList **deleted,
                 AFframecount offset,
                 AFframecount count);

int
blocklist_insert_buffer(struct blocklist *bl,
                        enum sample_type sample_type,
                        const void *frame_bits,
                        AFframecount offset,
                        AFframecount count);

int
blocklist_insert_blocks(struct blocklist *bl,
                        const GList *l,
                        AFframecount offset);

void
blocklist_blocks_destroy(GList *l);

void
blocklist_blocks_set(struct blocklist *bl, 
                     GList *l);

void
blocklist_dump(struct blocklist *bl);

void
blocklist_bcache_dump(struct blocklist *bl);

AFframecount
blocklist_count(struct blocklist *bl);

AFframecount
blocklist_trim_right(struct blocklist *bl);

#endif /* ! BLOCKLIST_H */
