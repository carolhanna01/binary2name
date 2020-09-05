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

#ifndef DRAW_H
#define DRAW_H

#include <gdk/gdk.h>
#include "view.h"

#define DASH_FONT_BIG_WIDTH 16
#define DASH_FONT_BIG_HEIGHT 16
#define DASH_INDICATOR_WIDTH 32
#define DASH_INDICATOR_HEIGHT 12

#define TRACK_Y_SPACING                    1
#define MIXER_TOGGLES_MUTE_HEIGHT         15
#define MIXER_TOGGLES_MUTE_WIDTH          15
#define MIXER_TOGGLES_SOLO_HEIGHT         15
#define MIXER_TOGGLES_SOLO_WIDTH          15
#define MIXER_LEVEL_OFFSET_X              20 /* pixels */
#define MIXER_LEVEL_WIDTH                 40 /* pixels */
#define MIXER_LEVEL_HEIGHT                 9 /* pixels */
#define ENVELOPE_HANDLE_SIZE               8 /* pixels */
#define MARKER_CUEPOINT_HANDLE_SIZE       12 /* pixels */
#define MARKER_MAX_WIDTH                  12 /* pixels */

struct view;

struct draw_hook_track_data {
    struct view *view;
    int track;
};

void
draw_mixercanvas(struct view *view,
                 GtkWidget *widget,
                 GdkRectangle *area);

void
draw_infocanvas(struct view *view,
                GtkWidget *widget,
                GdkRectangle *area);

void
draw_wavecanvas(struct view *view,
                GtkWidget *widget,
                GdkRectangle *area);

void
draw_peaks_from_buffers(struct view *view,
                        GdkDrawable *drawable, 
                        GdkGC *gc, 
                        GdkRectangle *area, 
                        int track,
                        graph_bits_unit_t *lows, 
                        graph_bits_unit_t *highs,
                        AFframecount offset,
                        AFframecount count);

void
draw_tracks(GtkWidget *widget,
            GdkDrawable *drawable,
            GdkGC *gc,
            GdkRectangle *area,
            void *user_data);

void
draw_peaks(GtkWidget *widget,
           GdkDrawable *drawable,
           GdkGC *gc,
           GdkRectangle *area,
           void *user_data);

void
draw_pointer(GtkWidget *widget,
             GdkDrawable *drawable,
             GdkGC *gc,
             GdkRectangle *area,
             void *user_data);

void
draw_cursor(GtkWidget *widget,
            GdkDrawable *drawable,
            GdkGC *gc,
            GdkRectangle *area,
            void *user_data);

void
draw_zero(GtkWidget *widget,
          GdkDrawable *drawable,
          GdkGC *gc,
          GdkRectangle *area,
          void *user_data);

void
draw_grid(GtkWidget *widget,
          GdkDrawable *drawable,
          GdkGC *gc,
          GdkRectangle *area,
          void *user_data);

void
draw_track_dividers(GtkWidget *widget,
                    GdkDrawable *drawable,
                    GdkGC *gc,
                    GdkRectangle *area,
                    void *user_data);

void 
draw_loop(GtkWidget *widget,
          GdkDrawable *drawable,
          GdkGC *gc,
          GdkRectangle *area,
          void *user_data);

void 
draw_background(GtkWidget *widget,
                GdkDrawable *drawable,
                GdkGC *gc,
                GdkRectangle *area,
                void *user_data);

void
draw_blocks(GtkWidget *widget,
            GdkDrawable *drawable,
            GdkGC *gc,
            GdkRectangle *area,
            void *user_data);

void
draw_constraints(GtkWidget *widget,
                 GdkDrawable *drawable,
                 GdkGC *gc,
                 GdkRectangle *area,
                 void *user_data);

void
draw_rms(GtkWidget *widget,
         GdkDrawable *drawable, 
         GdkGC *gc, 
         GdkRectangle *area,
         void *user_data);

#endif /* ! DRAW_H */
