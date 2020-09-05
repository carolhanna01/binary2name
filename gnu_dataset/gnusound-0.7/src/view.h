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

#ifndef VIEW_H
#define VIEW_H

#include <config.h>
#include <gnome.h>
#include "shell.h"
#include "draw.h"
#include "draw_hooks.h"
#include "tool.h"
#include "dialog.h"

/** Info level for view_set_transient(). */
#define MSG_INFO 0
/** Warning level for view_set_transient(). */
#define MSG_WARN 1
/** Error level for view_set_transient(). */
#define MSG_ERR 2

#define SLOPE_ENABLED(track, type) \
  ((shl->markers[track]->marker_types_enabled & type) == type)

#define RELATIVE_Y_TO_VALUE(view, relative_y) \
  ((-(relative_y) / ((view)->vres-1) * 2) + 1)

#define PIXEL_TO_RELATIVE_Y(view, track, y) \
  ((y) - ((track) - (view)->vadjust->value) * ((view)->vres + TRACK_Y_SPACING))

#define PIXEL_TO_TRACK(view, y) (y < 0 ? -1 : \
 ((((int)(((y) / ((view)->vres + TRACK_Y_SPACING) + 1)) * \
    ((view)->vres + TRACK_Y_SPACING)) - (y)) <= TRACK_Y_SPACING ? -1 : \
  ((y) / ((view)->vres + TRACK_Y_SPACING) + (view)->vadjust->value)))

#define PIXEL_TO_FRAME(view, x) ((view)->hadjust->value + ((x) * (view)->hres))

#define DISTANCE(x, y) (((x) - (y)) < 0 ? -((x) - (y)) : ((x) - (y)))

struct view {

    /* The shell (model) for this view. */

    struct _shell *shl;

    /* The pane (widget collection) for this view. */

    struct pane *pane;

    /* Adjustments for the scrollbars on the waveform canvas. */

    GtkAdjustment *hadjust;
    GtkAdjustment *vadjust;

    /* Horizontal resolution, power of 2 OR divisible by
       GRAPH_BITS_HRES. E.g. 128 specifies a scale of 1:128, while
       0.25 specifies 4:1 magnification. */

    float hres;

    /* Vertical resolution, e.g. 128 or 256. */

    int vres;

    /* Need to keep track of mouse position over waveform. */

    AFframecount last_mouse_offset;
    gdouble last_mouse_x;
    gdouble last_mouse_y;
    gdouble last_mouse_x_root;

    /* Tracks last mouse wheel movement so that rapid succession of
       mouse wheel movement means to zoom in/out on same frame. */

    struct timeval last_wheel_movement;
    AFframecount last_wheel_pos;
    double last_wheel_x;

    /* Buffers for waveform drawing. */

    GdkPixmap *wavepixmap;
    peak_unit_t *peaks_low;
    peak_unit_t *peaks_high;
    rms_unit_t *rms;

    /* Pixmap for mixer canvas drawing. */

    GdkPixmap *mixerpixmap;
    int target_channel_being_dragged;
    int source_channel_being_dragged;

    /* Default mouse cursor. */

    GdkCursorType default_cursor;

    /* Draw hooks for the wave canvas. */

    struct draw_hooks *wavecanvas_hooks;

    /* Draw hooks for the tracks. */

    struct draw_hooks *track_hooks[MAX_TRACKS];

    /* Current message, for example when dragging mixer level. */
    
    char *transient_info;
    int transient_level;

    /* Last tool panel inserted on the notebook. */

    int last_tool_page;

    /* Flags. */

    unsigned int draw_blocks:1;
    unsigned int draw_regionlock:1;
    unsigned int draw_lengths:1;
    unsigned int scrolling:1;
    unsigned int scrubbing:1;
    unsigned int show_zero:1;
    unsigned int show_grid:1;
    unsigned int follow_playback:1;

    /* Operations stack, contains the names for currently executing
       operations. */

    GPtrArray *opstack;

    int tool_count;
    
    GList *dialogs;

    /* The properties dialog. */

    struct dialog *dialog_props;
    
    int wavecanvas_auto_scroll_enabled;
    int wavecanvas_emit_motion_notify;
    int wavecanvas_motion_notify_pending;

    int msg_count;
    int msg_ids[10];
};

struct view *
view_new(struct _shell *shl);

void
view_shell_changed(struct view *view);

void
view_set_wavecanvas_auto_scroll(struct view *view,
                                gboolean enable);

void
view_attach_dialog(struct view *view,
                   GtkWidget *w);

void
view_detach_dialog(struct view *view,
                   GtkWidget *w);

void
view_wavecanvas_process_motion_notify(struct view *view,
                                      GdkEventMotion *event);

void
view_show(struct view *view);

void
view_set_transient(struct view *view,
                   int level,
                   const char *fmt,
                   ...);

void
view_clear_transient(struct view *view);

GtkWidget *
view_get_widget(struct view *view,
                const char *name);

void 
view_set_cursor(struct view *view,
                GdkCursorType cursor);

GdkCursorType 
view_get_default_cursor(struct view *view);

void
view_set_default_cursor(struct view *view,
                        GdkCursorType cursor);

void 
view_set_vpos(struct view *view, 
              double value);

void 
view_set_hpos(struct view *view, 
              AFframecount value);

void 
view_set_hpos_internal(struct view *view,
                       AFframecount value);

void 
view_set_hres(struct view *view, 
              float zoom);

void 
view_set_vres(struct view *view, 
              int zoom);

void 
view_pop_status(struct view *view);

void 
view_push_status(struct view *view, 
                 const char *format, 
                 ...);

void 
view_reset_status(struct view *view);

void 
view_update_timeline(struct view *view);

void 
view_redraw(struct view *view);

void 
view_disable_widgets(struct view *view, 
                     const char *names[]);

void
view_zoom_in(struct view *view);

void
view_zoom_out(struct view *view);

void
view_sync_grid_display(struct view *view);

void
view_sync_display(struct view *view);

void
view_center_hrange(struct view *view,
                   AFframecount start,
                   AFframecount end);

void
view_scroll_hcenter(struct view *view,
                    AFframecount from,
                    AFframecount to);

void 
view_disable_draw_hook(struct view *view,
                       const char *name);

void 
view_enable_draw_hook(struct view *view,
                      const char *name);

void
view_add_draw_hook(struct view *view,
                   struct draw_hook *hooks);

void
view_bind_tool(struct view *view,
               struct tool *tool);

void
view_activate_tool(struct view *view,
                   const char *name);

void
view_set_progress(struct view *view,
                  float percent);

float
view_get_zoom_step(float current_zoom);

void 
view_destroy(struct view *view);

void
view_start_operation(struct view *view,
                     const char *operation);

void
view_end_operation(struct view *view);

void
view_set_title(struct view *view,
               const char *title);

void
view_set_default_track_draw_hooks(struct view *v,
                                  int track);

void
view_reset_track_draw_hooks(struct view *view);

void
view_clip_changed(struct view *view);

#endif /* ! VIEW_H */
