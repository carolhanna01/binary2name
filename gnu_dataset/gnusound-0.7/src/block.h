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

#ifndef BLOCK_H
#define BLOCK_H

#include <audiofile.h>
#include <config.h>
#include "snd.h"
#include "cache.h"
#include "sample.h"

enum block_flags {
    BLOCK_HAS_RMS = (1 << 0),
};

/* FIXME: this thing is too damn big, we can have tens of thousands of
   blocks per track in which case we spend megabytes just on
   these. This needs to be rethought when we implement disk flushing
   anyway. */

typedef struct _block {
    cache_type type;
    int ref;
    enum block_flags flags;
    AFframecount count;
    enum sample_type sample_type;
    cache *samples;
    cache *peak_lows;
    cache *peak_highs;
    cache *rms;
} block;

void
block_set_calculate_rms(int state);

int
block_put_samples(block *block,
                  const void *buf,
                  AFframecount offset,
                  AFframecount count);

AFframecount
block_get_samples(block *block,
                  void *buf,
                  AFframecount offset,
                  AFframecount count);

AFframecount
block_peek_samples(block *block,
                   const void **buf,
                   AFframecount offset,
                   AFframecount count);

AFframecount 
block_get_peaks(block *block,
                peak_unit_t *lows,
                peak_unit_t *highs,
                AFframecount offset,
                AFframecount count);

AFframecount 
block_peek_peaks(block *block,
                 const peak_unit_t **lows,
                 const peak_unit_t **highs,
                 AFframecount offset,
                 AFframecount count);

AFframecount 
block_peek_rms(block *block,
               const rms_unit_t **rms,
               AFframecount offset,
               AFframecount count);

block *
block_resize(block *block,
             AFframecount count);

void
block_move(block *block,
           AFframecount new_offset,
           AFframecount old_offset,
           AFframecount count);

void
block_addref(block *block);

void 
block_unref(block *block);

block *
block_new(cache_type type,
          enum sample_type sample_type,
          AFframecount count);

block *
block_clone(const block *block,
            AFframecount offset,
            AFframecount count);

block *
block_split(block *block,
            AFframecount offset);

block *
block_join(block *b1,
           block *b2);

#endif /* ! BLOCK_H */
