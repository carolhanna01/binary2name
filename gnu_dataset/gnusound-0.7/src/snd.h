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

#ifndef SND_H
#define SND_H

#include <config.h>
#include <audiofile.h>
#include "track.h"
#include "mixer.h"
#include "rwlock.h"
#include "sample.h"
#include "error.h"

#define MAP_ALL 4294967295U

#define RESULT_ERROR_NOMEM NULL
#define RESULT_IS_ERROR(s) (!(s) || ((s) && ((snd *)s)->error_str))
#define SND_HAS_ERROR(s) (!(s) || ((s) && ((snd *)s)->error_str))

typedef enum {
    CLONE_STRUCTURE,
    CLONE_TRACK_STRUCTURE,
    CLONE_TRACK_DATA
} snd_clone_method;

typedef struct _snd {

    /** Error conditions. */

    struct error error;
    
    /** Name/identifier (may be NULL). */

    char *namep;

    /**
     * Number of channels, i.e. number of valid tracks in tracks
     * array below. 
     */

    int channels;

    /** Rate in frames per second. */

    double rate;

    /** Sample type. */

    enum sample_type sample_type;

    /* Duration in seconds. */

    double time;

    /**
     * Read/write lock. This lock protects the snd object from
     * manipulation by multiple threads. Read lock must be acquired
     * before traversing the tracks array below. 
     */

    rwlock rwl;

    /**
     * Array for the tracks. The channels field above
     * specifies how many tracks there actually are. 
     */

    struct _track *tracks[MAX_TRACKS];

} snd;

struct snd_shadow {
    char *what;
    snd *src;
    snd *dst;
    track_map_t map;
    AFframecount pivot;
    AFframecount offset;
    AFframecount count;
};

snd *
snd_new(int tracks,
        float rate,
        enum sample_type sample_type);

snd *
snd_clone(snd *sr,
          snd_clone_method method);

snd *
snd_copy(snd *sr,
         track_map_t channel_map,
         AFframecount frame_offset,
         AFframecount frame_count);

void 
snd_destroy(snd *sr);

int
snd_insert(snd *target,
           track_map_t *target_map,
           snd *source,
           track_map_t *source_map,
           AFframecount offset);

snd *
snd_erase(snd *sr,
          track_map_t channel_map,
          AFframecount frame_offset,
          AFframecount frame_count);

snd *
snd_delete(snd *sr,
           track_map_t channel_map,
           AFframecount frame_offset,
           AFframecount frame_count);

void
snd_insert_tracks(snd *sr,
                  snd *src,
                  track_map_t channel_map);

snd *
snd_delete_tracks(snd *sr,
                  track_map_t channel_map);

void
snd_set_name(snd *sr,
             const char *filename);

int
snd_track_count(snd *sr);

AFframecount
snd_frame_count(snd *sr,
                track_map_t map);

track_map_t
snd_map_avail(snd *sr);

double 
snd_frames_to_time(const snd *sr, 
                   AFframecount c);

void
snd_convert(snd *sr,
            enum sample_type sample_type,
            float sample_rate);

void
snd_info_dump(snd *sr);

void
snd_dump(snd *sr);

int
snd_verify(snd *sr);

AFframecount
snd_geti(snd *sr,
         void *fb_i,
         void *fb_n[],
         track_map_t channel_map,
         AFframecount frame_offset,
         AFframecount frame_count);

AFframecount
snd_getn(snd *sr,
         void * const fb_n[],
         track_map_t channel_map,
         AFframecount frame_offset,
         AFframecount frame_count);

int
snd_putn(snd *sr,
         const void * const fb_sources[],
         int channels,
         track_map_t insert_map,
         AFframecount frame_offset,
         AFframecount frame_count);

int
snd_puti(snd *sr,
         const void *fb_i,
         void * const fb_n[],
         int channels,
         track_map_t insert_map,
         AFframecount frame_offset,
         AFframecount frame_count);

int
snd_move_track(snd *sr,
               int from,
               int to);

void
snd_set_callbacks(snd *sr,
                  void (*modified)(snd *sr),
                  void (*destroyed)(snd *sr));

struct snd_shadow *
snd_shadow_new(snd *src,
               track_map_t map,
               AFframecount offset,
               AFframecount count);

int
snd_shadow_extend(struct snd_shadow *shadow,
                  AFframecount offset,
                  AFframecount count);

void
snd_shadow_destroy(struct snd_shadow *shadow);

#endif /* ! SND_H */
