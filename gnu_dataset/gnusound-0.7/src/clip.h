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

#ifndef CLIP_H
#define CLIP_H

#include <config.h>
#include "snd.h"
#include "mixer.h"
#include "marker.h"
#include "constraints.h"
#include "error.h"
#include "sample.h"
#include "msg.h"

#define CLIP_MARKERS  (1 << 0)
#define CLIP_MIXER    (1 << 1)
#define CLIP_SND      (1 << 2)

struct clip {
    struct error error;
    snd *sr;
    mixer *mixer;
    struct marker_list_array *markers;
    struct msg *msg;
};

void
clip_set_snd(struct clip *clip,
             snd *sr);

int
clip_move_track(struct clip *clip,
                int from,
                int to,
                int map);

struct clip *
clip_delete_tracks(struct clip *clip,
                   track_map_t map);

int
clip_insert_tracks(struct clip *clip,
                   struct clip *src,
                   track_map_t map);

void
clip_destroy(struct clip *clip);

struct clip *
clip_new(const char *name,
         int tracks,
         float rate,
         enum sample_type sample_type);

#endif /* ! CLIP_H */
