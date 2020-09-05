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

#include <gnusound.h>

struct tool_select {
    struct tool tool;

    AFframecount select_pivot;
    AFframecount select_flex;
};

void 
tool_select_draw(GtkWidget *widget,
                 GdkDrawable *drawable,
                 GdkGC *gc,
                 GdkRectangle *area,
                 void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    AFframecount frame_offset, frame_count, x1, x2, i, t;
    //    int u;
    GdkGCValues gc_vals;
    char mask[] = { 0x9 };
    static GdkPixmap *stipple = NULL;

    frame_offset = view->hadjust->value;
    frame_count = view_get_widget(view, "wavecanvas")->allocation.width * view->hres;

    if(shl->select_start == shl->select_end)
        return;

    if(shl->select_start < frame_offset && shl->select_end < frame_offset)
        return;

    if(shl->select_start > frame_offset + frame_count && 
       shl->select_end > frame_offset + frame_count)
        return;

    /* At least one visible. */

    x1 = shl->select_start - frame_offset;
    x2 = shl->select_end - frame_offset;

    x1 = ceil(x1 / view->hres);
    x2 = ceil(x2 / view->hres);

    if(x1 < 0)
        x1 = 0;
    if(x2 > frame_count / view->hres)
        x2 = frame_count / view->hres;

    if(x2 < 0)
        return;

    gdk_gc_get_values(gc, &gc_vals);

    //    DEBUG("x1: %ld, x2: %ld\n", x1, x2);
    gdk_gc_set_foreground(gc, gui_get_color("selection"));
    
    if(strcmp(shl->active_tool, "select")) {
        if(!stipple) 
            stipple = gdk_bitmap_create_from_data(NULL, mask, 2, 2);
        gdk_gc_set_background(gc, gui_get_color("selection.background"));
        gdk_gc_set_stipple(gc, stipple);
        gdk_gc_set_fill(gc, GDK_STIPPLED);
    }

    for(t = 0, i = view->vadjust->value; i < shl->clip->sr->channels; i++, t++)
        if((1 << i) & shl->select_channel_map) 
            gdk_draw_rectangle(drawable,
                               gc,
                               TRUE,
                               x1, 
                               (t * view->vres) +
                               (((t - 1) * TRACK_Y_SPACING) + TRACK_Y_SPACING),
                               MAX(x2 - x1, 1),
                               view->vres);

    /*4 beat selection
    for(u = 0; u < 4; u++) 
        for(t = 0, i = shl->vadjust->value; i < shl->clip->sr->channels; i++, t++)
            if((1 << i) & shl->select_channel_map) 
                gdk_draw_rectangle(drawable,
                                   gc,
                                   FALSE,
                                   x1 + (u * (MAX(x2 - x1, 1) / 4)),  
                                   t * shl->vres,
                                   MAX(x2 - x1, 1) / 4,
                                   shl->vres);
    */
    gdk_gc_set_fill(gc, gc_vals.fill);
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
    gdk_gc_set_background(gc, &gc_vals.background);

}

void
tool_select_drag(struct tool *tool,
                 AFframecount flex) {
    struct tool_select *ts = (struct tool_select *)tool;
    shell *shl = tool->shl;
    struct view *view = shl->view;

    ts->select_flex = flex;

    shl->select_start = MIN(ts->select_pivot, ts->select_flex);
    shl->select_end = MAX(ts->select_pivot, ts->select_flex);

    if(shl->select_start < 0)
        shl->select_start = 0;

    gtk_widget_queue_draw(view_get_widget(view, "wavecanvas"));
}

struct cmd_value *
tool_select_motion(struct tool *tool,
                   GdkEventMotion *event) {
    shell *shl = tool->shl;

    if(!(event->state & GDK_BUTTON1_MASK))
        return cmd_new_void_val();

    if(event->state & GDK_CONTROL_MASK) 
        return cmd_new_void_val();

    /* No idea why this happens. */

    if((int)event->x == 0)
        return cmd_new_void_val();

    tool_select_drag(tool, PIXEL_TO_FRAME(shl->view, event->x));

    return cmd_new_void_val();
}

struct cmd_value *
tool_select_button_press(struct tool *tool,
                         GdkEventButton *event) {
    shell *shl = tool->shl;
    struct view *view = shl->view;
    struct tool_select *ts = (struct tool_select *)tool;
    int sc = shl->select_channel_map;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    AFframecount ss, se;

    if(track < 0 || track > shl->clip->sr->channels - 1)
        return cmd_new_void_val();

    /* Waveform selection */
    /* Click + control key modifies which channels are selected. */
    
    if(event->state & GDK_CONTROL_MASK) {
        shl->select_channel_map = (shl->select_channel_map & (1 << track)) ? 
            shl->select_channel_map & ~(1 << track) : 
            shl->select_channel_map | (1 << track);
        gtk_widget_queue_draw(view_get_widget(view, "wavecanvas"));
        return cmd_new_void_val();
    }

    ts->select_flex = PIXEL_TO_FRAME(shl->view, event->x);
    
    if(ts->select_flex < 0)// || shl->select_flex > snd_frame_count(shl->clip->sr))
        return cmd_new_void_val();
    
    ts->select_pivot = (DISTANCE(ts->select_flex, shl->select_start) >
                        DISTANCE(ts->select_flex, shl->select_end) ? 
                        shl->select_start : shl->select_end);
    
    if(!(event->state & GDK_SHIFT_MASK)) {
        
        sc = (1 << track);
        ts->select_pivot = ts->select_flex = PIXEL_TO_FRAME(shl->view, 
                                                            event->x);
        
    }

    ss = MIN(ts->select_pivot, ts->select_flex);
    se = MAX(ts->select_pivot, ts->select_flex);

    if(ss < 0)
        ss = 0;
    
    shell_dispatch(shl,
                   CMD_NEW("set-selection",
                           cmd_new_shellp_val(shl),
                           cmd_new_int_val(sc),
                           cmd_new_long_val(ss),
                           cmd_new_long_val(se - ss)));
    return cmd_new_void_val();
}

struct cmd_value *
tool_select_button_release(struct tool *tool,
                           GdkEventButton *event) {
    shell *shl = tool->shl;

    if(event->state & GDK_CONTROL_MASK) 
        return cmd_new_void_val();

    tool_select_drag(tool, PIXEL_TO_FRAME(shl->view, event->x));
    return cmd_new_void_val();
}

struct tool *
tool_select_new(shell *shl) {
    struct tool_select *ts = mem_calloc(sizeof(*ts), 1);
    static struct tool_funcs tool_select = {
        tool_destroy,
        
        NULL, // activate
        NULL, // deactivate
        
        tool_select_button_press,
        tool_select_button_release,
        tool_select_motion,
        
        NULL, // key_press
        NULL, // key_release

        NULL, // enter
        NULL, // leave

        {
            "tool_select",
            0,
            10,
            tool_select_draw
        }
    };

    if(!ts)
        return NULL;

    ((struct tool *)ts)->name = "select";
    ((struct tool *)ts)->displayname = "Select";
    ((struct tool *)ts)->tooltip = "Makes selections on the waveform.";    
    ((struct tool *)ts)->ordinal = 0;
    ((struct tool *)ts)->cursor = GDK_LEFT_PTR;
    ((struct tool *)ts)->accel_key = GDK_q;
    ((struct tool *)ts)->accel_mods = GDK_MOD1_MASK;
    ((struct tool *)ts)->funcs = &tool_select;    

    return (struct tool *)ts;
}

int
tool_select_init(int id) {
    return tool_register("select", tool_select_new);
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Selection Tool",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    tool_select_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

