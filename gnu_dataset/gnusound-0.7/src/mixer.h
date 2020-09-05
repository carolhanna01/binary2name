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

#ifndef MIXER_H
#define MIXER_H

#include <config.h>
#include <audiofile.h>
#include "sample.h"
#include "error.h"

/**
 * Mixer.
 */
typedef struct _mixer {
    struct error error;

    /** Number of input channels (unused, will be used for input assign). */
    int input_channels;
    /** Number of output channels. */
    int output_channels;
    /** Number of source tracks. */
    int source_tracks;
    /** Whether this is a unity mixer. */
    int is_unity;

    /** The number of tracks that have "solo" activated. */
    int num_solos; 

    /* Maybe it should be considered to have two arrays of structs,
       one of source track properties and one for output channel
       properties. This would make the delete/insert routines 
       easier. */

    /** Array specifying for each source track whether it is 
        in the "solo" set or not. */
    int32_t *source_is_solo;
    /** Array of source tracks with either 0xFFFFFFFF or 0 for
        non-mute or mute respectively.  */
    int32_t *source_is_mute;

    /** Array specifying flags for each output channel. */
    int *output_flags;

    /** Array specifying peaks for each output channel. */
    float *output_peaks;

    /**
     * The mixing table, a 2D array mapping source tracks to 
     * output channels. E.g.:
     *
     *     |  source tracks
     *     |  1    2    3    4
     *  ---+-------------------
     *  o 1|0.5    1  0.5  0,5
     *  u 2|  0    0    0    0
     *  t 3|  1    0  .25    1
     *
     * 
     */
    float **mixtable;
} mixer;

#include "snd.h"

mixer *
mixer_new(int output_channels,
          int source_tracks);

mixer *
mixer_clone(mixer *m);

void 
mixer_destroy(mixer *m);

int
mixer_is_source_solo(mixer *m,
                     unsigned int source_track);

int
mixer_is_source_mute(mixer *m,
                     unsigned int source_track);

void
mixer_toggle_source_solo(mixer *m,
                         unsigned int source_track);

void
mixer_toggle_source_mute(mixer *m,
                         unsigned int source_track);

void
mixer_configure(mixer *m,
                int target_channels,
                int source_tracks);

int
mixer_load(mixer *m,
           const char *path);

int
mixer_save(mixer *m,
           const char *path);

int
mixer_load_compat(mixer *m,
                  const char *path);

int
mixer_save_compat(mixer *m,
                  const char *path);

void
mixer_mixi(mixer *m,
           enum sample_type sample_type,
           void *fb_target,
           void *fb_sources[],
           AFframecount frame_count);

void
mixer_mixn(mixer *m,
           enum sample_type sample_type,
           void *fb_targets[],
           void *fb_sources[],
           AFframecount frame_count);

void 
mixer_buffers_free(int tracks,
                   void *fb_downmix,
                   void **fb_sources);

void *
mixer_buffers_alloc(int frame_width,
                    int tracks,
                    void **fb_downmix,
                    void **fb_sources,
                    AFframecount frame_count);
void 
mixer_dump(mixer *m);

void
mixer_insert_source_tracks(mixer *m,
                           mixer *src,
                           track_map_t map);

mixer *
mixer_delete_source_tracks(mixer *m,
                           track_map_t map);

mixer *
mixer_delete_output_channels(mixer *m,
                             track_map_t map);

int
mixer_move_source_track(mixer *m,
                        int from,
                        int to);

#endif /* ! MIXER_H */
