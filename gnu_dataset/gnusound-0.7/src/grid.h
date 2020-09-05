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

#ifndef GRID_H
#define GRID_H

#include <config.h>
#include <audiofile.h>

enum grid_measurement {
    GRID_FRAMES,
    GRID_SECONDS,
    GRID_BEATS
};

enum grid_format_type {
    GRID_FORMAT_SHORT,
    GRID_FORMAT_LONG
};

struct grid {
    enum grid_measurement measurement;
    double units;
    double bpm;
    double rate;
    AFframecount gap;
};

void
grid_save(struct grid *g,
          const char *path);

void
grid_load(struct grid *g,
          const char *path);

AFframecount
grid_find_near_left(struct grid *g,
                    AFframecount offset);

AFframecount
grid_find_near_right(struct grid *g,
                     AFframecount offset);

void
grid_measurement_set(struct grid *g,
                     enum grid_measurement m);

void
grid_units_set(struct grid *g,
               double u);

void
grid_bpm_set(struct grid *g,
             double b);

void
grid_bpm_from_frames_set(struct grid *g,
                         AFframecount frame_count);

void
grid_rate_set(struct grid *g,
              double r);

void
grid_frames_to_bpm(struct grid *g,
                   AFframecount frame_count);

void
grid_format(struct grid *g,
            AFframecount frame_offset,
            char *s,
            int n,
            enum grid_format_type type);

AFframecount
grid_get_frames_per_beat(struct grid *g);

#endif /* ! GRID_H */

