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

#ifndef TRACK_H
#define TRACK_H

#include <glib.h>
#include <config.h>
#include "snd.h"
#include "cache.h"
#include "block.h"
#include "sample.h"
#include "rwlock.h"
#include "blocklist.h"

typedef enum {

    /* DUMMY is filler to equalize values with snd_clone_method */

    CLONE_DUMMY1,
    CLONE_DUMMY2,
    CLONE_DATA
} track_clone_method;

typedef struct _track {
    /** 
     * Read/write lock. This lock protects the track data structures
     * from concurrent access by multiple threads. The track_*
     * functions acquire this lock when necessary, but if you require
     * atomicity across multiple operations, you can acquire this lock
     * for reading/writing yourself. 
     */

    rwlock rwl;
    enum sample_type sample_type;
    float graph_hres;
    struct blocklist *bl;

} track;

track *
track_new(enum sample_type sample_type);

track *
track_clone(track *tr,
            track_clone_method method);

void
track_destroy(track *tr);

int
track_delete(track *tr,
             GList **deleted,
             AFframecount frame_offset,
             AFframecount frame_count);

int
track_insert(track *tr,
             GList *l,
             AFframecount frame_offset);

int
track_insert_silence(track *tr,
                     AFframecount frame_offset,
                     AFframecount frame_count);

AFframecount
track_count(track *tr);


/*** New API ***/

int
track_insert_samples(track *tr,
                     const void *buf,
                     AFframecount offset,
                     AFframecount count);

int
track_insert_samples_from(track *tr,
                          enum sample_type type,
                          const void *buf,
                          AFframecount offset,
                          AFframecount count);

int
track_replace_samples(track *tr,
                      const void *buf,
                      AFframecount offset,
                      AFframecount count);

int
track_replace_samples_from(track *tr,
                           enum sample_type type,
                           const void *buf,
                           AFframecount offset,
                           AFframecount count);

AFframecount
track_get_samples_as(track *tr,
                     enum sample_type type,
                     void *buf,
                     AFframecount offset,
                     AFframecount count);

AFframecount
track_get_samples(track *tr,
                  void *buf,
                  AFframecount frame_offset, 
                  AFframecount frame_count);

AFframecount
track_get_peaks(track *tr, 
                peak_unit_t *lows,
                peak_unit_t *highs,
                AFframecount frame_offset,
                AFframecount frame_count,
                float scale);

AFframecount
track_get_rms(track *tr, 
              rms_unit_t *rms,
              AFframecount offset,
              AFframecount count,
              float scale);

#endif /* ! TRACK_H */
