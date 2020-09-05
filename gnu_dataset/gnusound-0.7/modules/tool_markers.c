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
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

#include <gnusound.h>

struct tool_markers {
    struct tool tool;

    struct marker *marker_being_dragged;
    int marker_dragged_on_track;

    /*
     * This flag gets set to 1 in the button_press handler when a
     * double click has been detected. The button_release handler
     * tests it to see if it should open the marker editor dialog.
     *
     * It is necessary to open the marker editor dialog from the
     * button_release handler because it is a modal dialog. If it is
     * opened from the button_press handler, it soaks up our
     * button_release event. We need this button_release event,
     * however, to ungrab the mouse pointer.
     */

    int open_marker_editor;    
};

/*
 * Marker dialog.
 */

struct tool_markers_dialog_marker {
    struct dialog dialog;
    struct tool *tool;
    struct marker *marker;
};

static void
tool_markers_dialog_markers_apply(struct dialog *dialog,
                                  void *user_data) {
    struct tool_markers_dialog_marker *dm = 
        (struct tool_markers_dialog_marker *)dialog;
    shell *shl;
    GtkEntry *entry;
    const char *label;

    g_return_if_fail(dialog != NULL);

    shl = dm->tool->shl;
    entry = GTK_ENTRY(pane_get_widget(dialog->pane, "marker_label"));
    label = gtk_entry_get_text(entry);

    g_return_if_fail(label != NULL);

    if(!strlen(label))
        return;

    if(!strcmp(label, "<not applicable>"))
       return;

    marker_set_label(dm->marker, label);

    view_redraw(shl->view);
}

static void
tool_markers_dialog_markers_open(struct dialog *dialog,
                                 void *user_data) {
    struct tool_markers_dialog_marker *dm = 
        (struct tool_markers_dialog_marker *)dialog;
    GtkEntry *entry = GTK_ENTRY(pane_get_widget(dialog->pane, "marker_label"));
    char *s;

    s = dm->marker->label ? dm->marker->label : "<not applicable>";
    
    gtk_entry_set_text(entry, s);
    gtk_editable_select_region(GTK_EDITABLE(entry), 0, -1);
    
    if(!dm->marker->label) 
        gtk_widget_set_sensitive(GTK_WIDGET(entry), FALSE);
}

static struct dialog *
tool_markers_dialog_marker_new(struct tool *tool,
                               struct marker *marker) {
    struct tool_markers_dialog_marker *dm;
    
    g_return_val_if_fail(tool != NULL, NULL);
    g_return_val_if_fail(marker != NULL, NULL);

    dm = mem_alloc(sizeof *dm);

    g_return_val_if_fail(dm != NULL, NULL);

    dialog_init((struct dialog *)dm, 
                GLADE_FILE, "markerdialog", tool->shl, 0, NULL, dm);
    
    ((struct dialog *)dm)->open = tool_markers_dialog_markers_open;
    ((struct dialog *)dm)->apply = tool_markers_dialog_markers_apply;
    ((struct dialog *)dm)->close = 
        (void (*)(struct dialog *, void *))dialog_destroy;
    
    dm->tool = tool;
    dm->marker = marker;
    
    return (struct dialog *)dm;
}

/**
 * Tool functions.
 */

static void
tool_markers_draw_envelope(struct view *view,
                           GdkDrawable *drawable,
                           GdkGC *gc,
                           enum marker_type type,
                           AFframecount frame_offset,
                           AFframecount frame_count) {
    int i;
    struct marker *m;
    shell *shl = view->shl;
    int x, y, px = -1, py = -1, has_prev, lstyle, t;
    float step_count, step_size;
    AFframecount frame_offset_old = 0;
    GdkGCValues gc_vals;
    GdkColor *active_color, *inactive_color;
    int is_active;

    gdk_gc_get_values(gc, &gc_vals);
    lstyle = gc_vals.line_style;

    if(strcmp(shl->active_tool, "markers"))
        lstyle = GDK_LINE_ON_OFF_DASH;
    
    gdk_gc_set_line_attributes(gc, 
                               2,
                               lstyle,
                               gc_vals.cap_style, 
                               gc_vals.join_style);

    active_color = gui_get_color(type == MARKER_SLOPE ? 
                                 "marker_slope_main" :
                                 "marker_slope_aux");
    inactive_color = gui_get_color(type == MARKER_SLOPE ? 
                                   "marker_slope_main_disabled" :
                                   "marker_slope_aux_disabled");
    
    gdk_gc_set_function(gc, GDK_OR);

    for(i = view->vadjust->value, t = 0; i < shl->clip->markers->len; i++, t++) {
        has_prev = 0;
        frame_offset = frame_offset_old;
        
        /* Only draw envelopes for this track if the marker tool 
           is active or the envelope type is enabled. */
        
        is_active = shl->clip->markers->lists[i]->marker_types_enabled & type;

        if(strcmp(shl->active_tool, "markers") && !is_active) 
            continue;

        gdk_gc_set_line_attributes(gc, 
                                   is_active ? 2 : 1,
                                   lstyle,
                                   gc_vals.cap_style, 
                                   gc_vals.join_style);

        gdk_gc_set_foreground(gc, is_active ? active_color : inactive_color);

        m = marker_list_previous(shl->clip->markers->lists[i], 
                                 frame_offset,
                                 type);

        if(m) {
            has_prev = 1;
            px = ((m->frame_offset / view->hres)) -
                (view->hadjust->value / view->hres);
            py = ((view->vres * t) + (view->vres / 2) - 
                  m->multiplier * (view->vres / 2)) + (t * TRACK_Y_SPACING);
            gdk_draw_rectangle(drawable,
                               gc,
                               FALSE,
                               px - (ENVELOPE_HANDLE_SIZE / 2), 
                               py - (ENVELOPE_HANDLE_SIZE / 2),
                               ENVELOPE_HANDLE_SIZE,
                               ENVELOPE_HANDLE_SIZE);
            frame_offset = m->frame_offset + 1;
        }

        m = marker_list_next(shl->clip->markers->lists[i], 
                             frame_offset,
                             type);
        
        for(; m; m = marker_list_next(shl->clip->markers->lists[i], 
                                      frame_offset,
                                      type)) {
            x = ((m->frame_offset / view->hres)) - 
                (view->hadjust->value / view->hres);
            y = ((view->vres * (i - view->vadjust->value)) + (view->vres / 2) - 
                 m->multiplier * (view->vres / 2)) + (t * TRACK_Y_SPACING);
            if(x + (ENVELOPE_HANDLE_SIZE / 2) > 0 &&
               x - (ENVELOPE_HANDLE_SIZE / 2) < view_get_widget(view, "wavecanvas")->allocation.width)
                gdk_draw_rectangle(drawable,
                                   gc,
                                   FALSE,
                                   x - (ENVELOPE_HANDLE_SIZE / 2), 
                                   y - (ENVELOPE_HANDLE_SIZE / 2),
                                   ENVELOPE_HANDLE_SIZE, 
                                   ENVELOPE_HANDLE_SIZE);

            /* Clip x,y position to prevent wraparound and strange
               drawing artifacts. */
            
            if(has_prev && px < 0) {
                step_count = -px;
                step_size = (float)(y - py) / (x - px);
                py = py + (step_count * step_size);
                px = 0;
            }

            if(x > view_get_widget(view, "wavecanvas")->allocation.width) {
                step_count = x - px - (x - view_get_widget(view, "wavecanvas")->allocation.width);
                step_size = (float)(y - py) / (x - px);
                y = py + (step_count * step_size);
                x = view_get_widget(view, "wavecanvas")->allocation.width;
            }
            
            if(x < 0) {

                /* Don't draw if behind us, just remember the coord and 
                   draw it next time around. */

                px = x; 
                py = y;
                has_prev = 1;
                frame_offset = m->frame_offset + 1;
                if(m->frame_offset > frame_offset + frame_count)
                    break;
                continue;
            }

            if(has_prev)
                gdk_draw_line(drawable,
                              gc,
                              x, 
                              y,
                              px, 
                              py);
            has_prev = 1;
            px = x;
            py = y;

            frame_offset = m->frame_offset + 1;

            if(m->frame_offset > frame_offset + frame_count)
                break;
        }
    }

    gdk_gc_set_function(gc, gc_vals.function);
    gdk_gc_set_line_attributes(gc, 
                               gc_vals.line_width,
                               gc_vals.line_style,
                               gc_vals.cap_style, 
                               gc_vals.join_style);
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

static void
tool_markers_draw_cuepoints(struct view *view,
                            GtkWidget *widget,
                            GdkDrawable *drawable,
                            GdkGC *gc,
                            AFframecount frame_offset,
                            AFframecount frame_count) {
    int i, t;
    shell *shl = view->shl;
    struct marker *m;
    int x = 0, y = 0;
    int previous_width = 0, previous_x = 0, elevator = 0;
    int lstyle;
    GdkPoint handle[4];
    AFframecount frame_offset_old = frame_offset;
    GdkGCValues gc_vals;
    struct gui_letterbox *lb;
    struct gui_letterbox_extents ink_lbe, logical_lbe;
    char mask[] = { 0x9 };
    static GdkPixmap *stipple = NULL;
    int is_active = !strcmp(shl->active_tool, "markers");

    gdk_gc_get_values(gc, &gc_vals);
    lstyle = gc_vals.line_style;

    if(!is_active) {

        if(!stipple) 
            stipple = gdk_bitmap_create_from_data(NULL, mask, 2, 2);

        gdk_gc_set_line_attributes(gc, 
                                   2,
                                   gc_vals.line_style,
                                   gc_vals.cap_style, 
                                   gc_vals.join_style);
        gdk_gc_set_stipple(gc, stipple);

    }

    for(i = view->vadjust->value, t = 0; i < shl->clip->markers->len; i++, t++) {
        elevator = 0;
        previous_x = previous_width = 0;
        frame_offset = frame_offset_old;

        for(m = marker_list_next(shl->clip->markers->lists[i], 
                                 frame_offset,
                                 MARKER_TEXT);
            m; 
            m = marker_list_next(shl->clip->markers->lists[i], 
                                 frame_offset,
                                 MARKER_TEXT)) {
            
            if(m->frame_offset > frame_offset + frame_count)
                break;

            x = ((m->frame_offset / view->hres)) - 
                (view->hadjust->value / view->hres);
            y = (view->vres * t) + (t * TRACK_Y_SPACING);

            gdk_gc_set_foreground(gc, gui_get_color("marker_text_background"));

            lb = gui_letterbox_new(widget, m->label);
            
            gui_letterbox_get_extents(lb, &ink_lbe, &logical_lbe);

            handle[0].x = x;
            handle[0].y = y + view->vres;
            handle[1].x = x + MARKER_CUEPOINT_HANDLE_SIZE;
            handle[1].y = y + view->vres;
            handle[2].x = x;
            handle[2].y = (y + view->vres) - MARKER_CUEPOINT_HANDLE_SIZE;
            handle[3].x = x;
            handle[3].y = y + view->vres;
            
            gdk_gc_set_fill(gc, is_active ? GDK_SOLID : GDK_STIPPLED);

            gdk_draw_polygon(drawable,
                             gc,
                             1,
                             handle,
                             4);
            gdk_draw_line(drawable,
                          gc,
                          x, y + (view->vres / 1.3),
                          x, y + (view->vres - 1));
            
            gdk_gc_set_foreground(gc, gui_get_color(is_active ? 
                                                    "marker_text" :
                                                    "marker_text_nonactive"));

            gdk_gc_set_fill(gc, GDK_SOLID);

            if(previous_x + previous_width > x + 2) 
                elevator += ink_lbe.height + 2;
            else 
                elevator = 0;

            gui_letterbox_draw(lb, drawable,
                               x + 1,
                               (y - 2 - elevator) + (view->vres));
    
            gui_letterbox_destroy(lb);

            previous_x = x;
            previous_width = ink_lbe.width;
            frame_offset = m->frame_offset + 1;
        }
    }

    gdk_gc_set_line_attributes(gc, 
                               gc_vals.line_width,
                               gc_vals.line_style,
                               gc_vals.cap_style, 
                               gc_vals.join_style);
    gdk_gc_set_function(gc, gc_vals.function);
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
    gdk_gc_set_fill(gc, gc_vals.fill);
}

void
tool_markers_draw(GtkWidget *widget,
                  GdkDrawable *drawable,
                  GdkGC *gc,
                  GdkRectangle *area,
                  void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    int i;
    int old_state[shl->clip->markers->len];

    tool_markers_draw_cuepoints(view,
                                widget,
                                drawable,
                                gc,
                                view->hadjust->value,
                                view_get_widget(view, "wavecanvas")->allocation.width * view->hres);

    tool_markers_draw_envelope(view,
                               drawable,
                               gc,
                               MARKER_SLOPE,
                               view->hadjust->value,
                               view_get_widget(view, "wavecanvas")->allocation.width * view->hres);
    tool_markers_draw_envelope(view,
                               drawable,
                               gc,
                               MARKER_SLOPE_AUX,
                               view->hadjust->value,
                               view_get_widget(view, "wavecanvas")->allocation.width * view->hres);
    
}

static int
tool_markers_hit_envelope_handle(struct tool *tool,
                                 struct marker *m,
                                 AFframecount offset,
                                 float multiplier) {
    struct view *view = tool->shl->view;
    AFframecount offset_tolerance;
    float multiplier_tolerance;

    offset_tolerance = view->hres * (ENVELOPE_HANDLE_SIZE / 2);

    if(offset < m->frame_offset - offset_tolerance ||
       offset > m->frame_offset + offset_tolerance)
        return 0;

    multiplier_tolerance = 
        (1.0f / (view->vres / 2)) * (ENVELOPE_HANDLE_SIZE / 2);

    if(multiplier < m->multiplier - multiplier_tolerance ||
       multiplier > m->multiplier + multiplier_tolerance)
        return 0;

    return 1;
}

static int
tool_markers_hit_cuepoint(struct tool *tool,
                          struct marker *m,
                          AFframecount offset,
                          float multiplier) {
    struct view *view = tool->shl->view;
    AFframecount offset_tolerance;
    
    offset_tolerance = view->hres * MARKER_CUEPOINT_HANDLE_SIZE;

    if(offset < m->frame_offset - offset_tolerance ||
       offset > m->frame_offset + offset_tolerance)
        return 0;

    if(multiplier > -0.8)
        return 0;

    return 1;
}

GList *
tool_markers_find_nearby_markers(shell *shl,
                                 enum marker_type mask,
                                 int track,
                                 AFframecount frame_offset) {
    GList *result = NULL;
    struct marker *mn, *mp, *m = NULL;
    enum marker_type types[] = { MARKER_SLOPE, 
                                 MARKER_SLOPE_AUX,
                                 MARKER_TEXT };
    enum marker_type type;
    AFframecount offset_tolerance = shl->view->hres * MARKER_MAX_WIDTH;
    int i;

    for(i = 0; i < sizeof(types) / sizeof(types[0]); i++) {

        type = types[i];

        mn = marker_list_next(shl->clip->markers->lists[track],
                              MAX(0, frame_offset - 1),
                              type);
        mp = marker_list_previous(shl->clip->markers->lists[track],
                                  MAX(0, frame_offset - 1),
                                  type);
        
        if(mn && !mp)
            m = mn;
        else if(mp && !mn)
            m = mp;
        else if(mp && mn)
            m = DISTANCE(frame_offset, mp->frame_offset) > 
                DISTANCE(frame_offset, mn->frame_offset) ?
                mn : mp;
        else if(!mp && !mn)
            continue;

        if(m->frame_offset < frame_offset - offset_tolerance ||
           m->frame_offset > frame_offset + offset_tolerance)
            continue;

        if(!(m->type & mask))
            continue;

        result = g_list_append(result, m);

    }

    return result;
}

/**
 * The marker priority is a measure of the likelihood that the user
 * clicks on a particular type of marker, inverted. That is, the more
 * likely a user is to select a particular marker type, the lower it's
 * priority.
 */

static int
tool_markers_get_marker_priority(const struct marker *m) {
    switch(m->type) {
    case MARKER_TEXT:
        return 30;
    case MARKER_SLOPE:
        return 20;
    case MARKER_SLOPE_AUX:
        return 10;
    default:
        return 0;
    }
}
    
static gint
tool_markers_prioritize_markers(gconstpointer a,
                               gconstpointer b) {
    const struct marker *m1 = a, *m2 = b;
    
    return 
        tool_markers_get_marker_priority(m1) - 
        tool_markers_get_marker_priority(m2);
}

static struct marker *
tool_markers_get_marker_at_position(struct tool *tool,
                                    enum marker_type mask,
                                    int track,
                                    AFframecount offset,
                                    float multiplier) {
    shell *shl = tool->shl;
    struct marker *m = NULL;
    GList *near, *l;
    int r = 0;

    /* 
     * Find all markers within a small range of the given offset, then
     * sort them so that the marker which has the lowest probability
     * of being selected (e.g. because it is very small) becomes first
     * in the list. Then we walk the list and pick the first marker
     * for which we can establish that the user intended to click
     * it. This way markers which are difficult to hit cannot be edged
     * out by markers which are easy to hit.
     */

    near = tool_markers_find_nearby_markers(shl, mask, track, offset);

    if(!near)
        return NULL;

    near = g_list_sort(near, tool_markers_prioritize_markers);

    for(l = near; l; l = l->next) {
        
        m = l->data;
        
        switch(m->type) {
        case MARKER_SLOPE:
        case MARKER_SLOPE_AUX:
            r = tool_markers_hit_envelope_handle(tool, m, offset, multiplier);
            break;
        case MARKER_TEXT:
            r = tool_markers_hit_cuepoint(tool, m, offset, multiplier);
            break;
        }

        if(r)
            break;
    }

    g_list_free(near);

    return r ? m : NULL;
}

static void
tool_markers_update_transient(struct tool *tool,
                              struct marker *m) {
    shell *shl = tool->shl;
    char infostr[40];

    switch(m->type) {
    case MARKER_SLOPE:
    case MARKER_SLOPE_AUX:
        view_set_transient(shl->view, MSG_INFO, "%f", m->multiplier);
        break;
    case MARKER_TEXT:
        grid_format(&shl->grid, m->frame_offset,
                    infostr, sizeof infostr, GRID_FORMAT_LONG);
        view_set_transient(shl->view, MSG_INFO, "%s", infostr);
        break;
    }
}

struct cmd_value *
tool_markers_button_release(struct tool *tool,
                            GdkEventButton *event) {
    struct tool_markers *tm = (struct tool_markers *)tool;
    struct marker *m;

    if(!tm->open_marker_editor)
        return cmd_new_void_val();

    tm->open_marker_editor = 0;

    if(!tm->marker_being_dragged)
        return cmd_new_void_val();

    m = tm->marker_being_dragged;
    
    if(m->flags & MARKER_IS_DISABLED)
        return cmd_new_void_val();

    dialog_open(tool_markers_dialog_marker_new(tool, m));
    
    return cmd_new_void_val();
}

struct cmd_value *
tool_markers_button_press(struct tool *tool,
                          GdkEventButton *event) {
    shell *shl = tool->shl;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    double rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    double multiplier = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    AFframecount offset = PIXEL_TO_FRAME(shl->view, event->x);
    enum marker_type type, mask;
    struct marker *m;
    struct tool_markers *tm = (struct tool_markers *)tool;
    int t;

    if(track < 0 || track > shl->clip->markers->len - 1)
        return cmd_new_void_val();

    /* Check for pending marker drag on previous button press/motion
       and delete the marker if it was disabled. */

    if(tm->marker_being_dragged) {

        m = tm->marker_being_dragged;
        t = tm->marker_dragged_on_track;

        if((m->flags & MARKER_IS_DISABLED) == MARKER_IS_DISABLED) 
            marker_list_delete_marker(shl->clip->markers->lists[t],
                                      tm->marker_being_dragged);

        tm->marker_being_dragged = NULL;
        tm->marker_dragged_on_track = -1;
    }

    /* 
     * Button  Manipulates                 Creates
     * ----------------------------------------------------
     * 1       MARKER_SLOPE, MARKER_TEXT   MARKER_SLOPE
     * 2       MARKER_SLOPE_AUX            MARKER_SLOPE_AUX
     * 3       MARKER_TEXT                 MARKER_TEXT
     */
    
    switch(event->button) {
    case 1:
        type = MARKER_SLOPE;
        mask = MARKER_SLOPE | MARKER_TEXT;
        break;
    case 2:
        type = MARKER_SLOPE_AUX;
        mask = MARKER_SLOPE_AUX;
        break;
    case 3:
        type = MARKER_TEXT;
        mask = MARKER_TEXT;
        break;
    default:
        return cmd_new_void_val();
    }

    m = tool_markers_get_marker_at_position(tool, mask, track, offset, 
                                            multiplier);

    if(m) {

        /* Existing marker. */

        switch(event->type) {
        case GDK_2BUTTON_PRESS:
            tm->open_marker_editor = 1;
        case GDK_BUTTON_PRESS:
            tm->marker_being_dragged = m;
            tm->marker_dragged_on_track = track;
            break;
        default:
            break;
        }

    } else {

        /* New marker. */

        m = marker_list_insert(shl->clip->markers->lists[track],
                               offset,
                               type,
                               multiplier,
                               (type == MARKER_TEXT ? 
                                shell_get_next_label(shl) : NULL));
        
        if(m) {
            tm->marker_being_dragged = m;
            tm->marker_dragged_on_track = track;
        }
    }
    
    if(!m)
        return cmd_new_void_val();

    tool_markers_update_transient(tool, m);

    //    view_redraw();

    return cmd_new_void_val();
}

void
tool_markers_drag(struct tool *tool,
                  int track,
                  AFframecount offset,
                  float multiplier) {
    shell *shl = tool->shl;
    int t;
    struct tool_markers *tm = (struct tool_markers *)tool;
    struct marker *m;
    
    if(shl->snap_to_grid) 
        offset -= offset % shl->grid.gap;
    if(offset < 0)
        offset = 0;
    
    m = tm->marker_being_dragged;
    t = tm->marker_dragged_on_track;

    if(track != tm->marker_dragged_on_track) {
        view_clear_transient(shl->view);
        m->flags |= MARKER_IS_DISABLED;
        return;
    }

    if(track < 0 || track > shl->clip->markers->len - 1)
        return;
    
    m->multiplier = multiplier;
    marker_list_set_marker_position(shl->clip->markers->lists[t], m, offset);

    tool_markers_update_transient(tool, m);

    m->flags &= ~MARKER_IS_DISABLED;
}

struct cmd_value *
tool_markers_motion(struct tool *tool,
                    GdkEventMotion *event) {
    shell *shl = tool->shl;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    float rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    float multiplier = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    AFframecount offset = PIXEL_TO_FRAME(shl->view, event->x);
    struct tool_markers *tm = (struct tool_markers *)tool;

    if(!(event->state & (GDK_BUTTON1_MASK | 
                         GDK_BUTTON2_MASK |
                         GDK_BUTTON3_MASK)))
        return cmd_new_void_val();
    
    if(tm->marker_being_dragged) {
        tool_markers_drag(tool, track, offset, multiplier);
        view_redraw(shl->view);
        return cmd_new_void_val();
    }
    
    return cmd_new_void_val();
}

void
tool_markers_destroy(struct tool *tool) {
    struct tool_markers *tm = (struct tool_markers *)tm;

    tool_destroy(tool);
}

struct tool *
tool_markers_new(shell *shl) {
    struct tool_markers *tm = mem_calloc(sizeof(*tm), 1);
    static struct tool_funcs tool_markers = {
        tool_markers_destroy,
        
        NULL, // activate,
        NULL, // deactivate,
        
        tool_markers_button_press,
        tool_markers_button_release,
        tool_markers_motion,
        
        NULL, // key_press,
        NULL, // key_release,

        NULL, // enter
        NULL, // leave

        {
            "tool_markers",
            0,
            30,
            tool_markers_draw
        }
    };

    if(!tm)
        return NULL;

    ((struct tool *)tm)->name = "markers";
    ((struct tool *)tm)->displayname = "Envelopes";
    ((struct tool *)tm)->tooltip = "Manipulates envelopes and cuepoints.";    
    ((struct tool *)tm)->ordinal = 10;
    ((struct tool *)tm)->cursor = GDK_PLUS;
    ((struct tool *)tm)->accel_key = GDK_w;
    ((struct tool *)tm)->accel_mods = GDK_MOD1_MASK;
    ((struct tool *)tm)->funcs = &tool_markers;

    tm->open_marker_editor = 0;
    tm->marker_being_dragged = NULL;
    tm->marker_dragged_on_track = -1;

    return (struct tool *)tm;
}

static int
tool_markers_init(int id) {
    return tool_register("markers", tool_markers_new);
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Marker Tool",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    tool_markers_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

