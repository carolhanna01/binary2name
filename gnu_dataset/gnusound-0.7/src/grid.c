/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2005  Pascal Haakmat <a.haakmat@chello.nl>
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
#include <stdio.h>
#include <math.h>
#include <audiofile.h>
#include <gnome.h>
#include "grid.h"

void
grid_save(struct grid *g,
          const char *path) {
    char key[512];
    snprintf(key, 512, "=%s=/Grid/BPM", path);
    gnome_config_set_float(key, g->bpm);
    snprintf(key, 512, "=%s=/Grid/Units", path);
    gnome_config_set_float(key, g->units);
    snprintf(key, 512, "=%s=/Grid/Scale", path);
    gnome_config_set_int(key, g->measurement);
}

void
grid_load(struct grid *g,
          const char *path) {
    char key[512];
    snprintf(key, 512, "=%s=/Grid/BPM=120", path);
    g->bpm = gnome_config_get_float(key);
    snprintf(key, 512, "=%s=/Grid/Units=1", path);
    g->units = gnome_config_get_float(key);
    snprintf(key, 512, "=%s=/Grid/Scale=1", path);
    g->measurement = gnome_config_get_int(key);
    if(g->measurement != GRID_FRAMES && 
       g->measurement != GRID_SECONDS && 
       g->measurement != GRID_BEATS)
        g->measurement = GRID_SECONDS;
}

AFframecount
grid_find_near_left(struct grid *g,
                    AFframecount offset) {
    return (offset / g->gap) * g->gap;
}

AFframecount
grid_find_near_right(struct grid *g,
                     AFframecount offset) {
    return ((offset + g->gap) / g->gap) * g->gap;
}

void
grid_configure(struct grid *g) {
    switch(g->measurement) {
    case GRID_FRAMES:
        g->gap = g->units;
        break;
    case GRID_SECONDS:
        g->gap = (g->units * g->rate);
        break;
    case GRID_BEATS:
        g->gap = g->units * ((60 / g->bpm) * g->rate);
        break;
    }
}

void
grid_measurement_set(struct grid *g,
                     enum grid_measurement m) {
    g->measurement = m;
    grid_units_set(g, g->units);
    grid_configure(g);
}

void
grid_units_set(struct grid *g,
               double u) {
    switch(g->measurement) {
    case GRID_FRAMES:
        if(u <= 1)
            u = 1;
        /* There is no such thing as a half frame. */
        u = floor(u);
        break;
    case GRID_BEATS:
        if(u <= 0.03125)
            u = 0.03125;
        break;
    case GRID_SECONDS:
        if(u <= 0.01)
            u = 0.01;
        break;
    }
    g->units = u;
    grid_configure(g);
}

void
grid_bpm_from_frames_set(struct grid *g,
                         AFframecount frame_count) {
    g->bpm = 60 / (frame_count / g->rate);
    grid_configure(g);
}

void
grid_bpm_set(struct grid *g,
             double b) {
    g->bpm = b;
    grid_configure(g);
}

void
grid_rate_set(struct grid *g,
              double r) {
    g->rate = r;
    grid_configure(g);
}

AFframecount
grid_get_frames_per_beat(struct grid *g) {
    return (60 / g->bpm) * g->rate;
}

void
grid_format(struct grid *g,
            AFframecount frame_count,
            char *str,
            int n,
            enum grid_format_type type) {
    int h, m, s, ms;
    double secs, beats;

    switch(g->measurement) {
    case GRID_FRAMES:
        snprintf(str, n, "%ld", frame_count);
        break;
    case GRID_SECONDS:
        secs = (double)frame_count / g->rate;
        h = floor(secs / 86400);
        secs -= h * 86400;
        m = floor(secs / 60);
        secs -= m * 60;
        s = floor(secs);
        ms = (secs - s) * 100;
        if((type == GRID_FORMAT_LONG)) 
            if(h) 
                snprintf(str, n, "%02d:%02d:%02d.%02d", h, m, s, ms);
            else
                snprintf(str, n, "%02d:%02d.%02d", m, s, ms);
        else 
            if(h) 
                snprintf(str, n, "%02d:%02d:%02d   ", h, m, s);
            else 
                snprintf(str, n, "%02d:%02d   ", m, s);
        break;
    case GRID_BEATS:
        beats = (double)frame_count / grid_get_frames_per_beat(g);
        if(beats == floor(beats))
            snprintf(str, n, "%d", (int)beats);
        else 
            snprintf(str, n, "%.2f", beats);
            
        break;
    }
}
