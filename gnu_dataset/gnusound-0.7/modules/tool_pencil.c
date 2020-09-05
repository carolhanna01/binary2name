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

/**
 * @file
 * Pencil. 
 * FIXME: while pencilling the window size should not change, but
 * this is not enforced.
 */

#include <gnusound.h>

struct tool_pencil {
    struct tool tool;

    AFframecount pencil_modify_start;
    AFframecount pencil_modify_end;
    int pencil_track;
    graph_bits_unit_t *pencil_lows_buf;
    graph_bits_unit_t *pencil_highs_buf;
};

void
tool_pencil_draw_peaks(GtkWidget *widget,
                       GdkDrawable *drawable,
                       GdkGC *gc,
                       GdkRectangle *area,
                       void *user_data) {
    struct draw_hook_track_data *dhtd = user_data;
    struct view *view = dhtd->view;
    shell *shl = view->shl;
    struct tool_pencil *tp = (struct tool_pencil *)shell_get_active_tool(shl);
    AFframecount offset, count;

    offset = view->hadjust->value;
    count = view_get_widget(view, "wavecanvas")->allocation.width * view->hres;
    
    draw_peaks_from_buffers(view, drawable, gc, area, dhtd->track, 
                            tp->pencil_lows_buf,
                            tp->pencil_highs_buf, 
                            offset, count);
}

void
tool_pencil_set_value(struct tool *tool,
                      AFframecount x,
                      double value) {
    int i;
    struct tool_pencil *tp = (struct tool_pencil *)tool;
    shell *shl = tool->shl;
    struct view *view = shl->view;

    if(value < -1 || value > 1)
        return;

    if(x > tp->pencil_modify_end || tp->pencil_modify_end == -1)
        tp->pencil_modify_end = x + MAX(1, view->hres);
    else if(x < tp->pencil_modify_start || tp->pencil_modify_start == -1)
        tp->pencil_modify_start = x;

    x = ((x - view->hadjust->value) / view->hres);
    tp->pencil_lows_buf[x] = tp->pencil_highs_buf[x] = 
        (PEAK_VRES >> 1) - ((value + 1) * (PEAK_VRES >> 1));
    
    if(view->hres < 1) {
        for(i = 0; i < (1 / view->hres); i++) {
            tp->pencil_lows_buf[x+i] = tp->pencil_highs_buf[x+i] = 
                (PEAK_VRES >> 1) - ((value + 1) * 
                                          (PEAK_VRES >> 1));
        }
    }
}

void
tool_pencil_deactivate(struct tool *tool) {
    struct tool_pencil *tp = (struct tool_pencil *)tool;
    if(tp->pencil_track != -1)
        constraints_pop(tool->shl->constraints);
}

struct cmd_value *
tool_pencil_begin(struct tool *tool,
                  int track) {
    int r;
    shell *shl = tool->shl;
    struct view *view = shl->view;
    struct tool_pencil *tp = (struct tool_pencil *)tool;
    const char *s;
    static struct draw_hook pencil_hook = 
        { "pencil", 0, 30, tool_pencil_draw_peaks };

    if(tp->pencil_lows_buf)
        mem_free(tp->pencil_lows_buf);

    if(shl->player->player_running)
        return cmd_new_error_val("Cannot pencil while playing");

    if((s = constraints_test(shl->constraints,
                             region_new((1 << track),
                                        REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_REPLACE))) 
        return cmd_new_error_val("Cannot pencil here because region is locked "
                                 "(%s)", s);
    
    tp->pencil_lows_buf = mem_calloc(1, sizeof(graph_bits_unit_t) * 
                                     view_get_widget(view, "wavecanvas")->allocation.width * 2);
    if(!tp->pencil_lows_buf) 
        return cmd_new_error_val("Cannot allocate pencil buffer");
    
    constraints_push(shl->constraints,
                     "Pencil",
                     region_new((1 << track),
                                REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING),
                     (CONSTRAIN_POSITION | CONSTRAIN_LENGTH |
                      CONSTRAIN_CONTENTS));

    tp->pencil_highs_buf = &tp->pencil_lows_buf[view_get_widget(view, "wavecanvas")->allocation.width];

    /*
     * Load the peaks for the visible area into the pencil buffers. We
     * temporarily switch off normal peaks display for the track, and
     * display the pencil buffers instead. When the user finishes
     * drawing, we convert the pencil buffer back to samples.
     */

    r = track_get_peaks(shl->clip->sr->tracks[track],
                        tp->pencil_lows_buf,
                        tp->pencil_highs_buf,
                        view->hadjust->value,
                        view_get_widget(view, "wavecanvas")->allocation.width * view->hres,
                        view->hres);
    /*    if(r < 0) {
        FAIL("cannot get peaks for penciling\n");
        mem_free(shl->pencil_lows_buf);
        return 1;
        }*/

    draw_hooks_disable_all(view->track_hooks[track]);
    draw_hooks_add_hook(view->track_hooks[track], &pencil_hook);
    
    tp->pencil_track = track;
    tp->pencil_modify_start = -1;
    tp->pencil_modify_end = -1;

    view_set_wavecanvas_auto_scroll(view, FALSE);

    return cmd_new_void_val();
}

struct cmd_value *
tool_pencil_commit(struct tool *tool) {
    AFframecount i, work, done;
    int r = 0, off;
    float *samples = mem_alloc(sizeof(float) * MAX_BLOCK_SIZE);
    shell *shl = tool->shl;
    struct view *view = shl->view;
    struct tool_pencil *tp = (struct tool_pencil *)tool;
    struct cmd *cmd;
    struct cmd_value *err;

    if(tp->pencil_track == -1)
        return cmd_new_void_val();

    draw_hooks_remove_hook(view->track_hooks[tp->pencil_track], "pencil");
    draw_hooks_enable_all(view->track_hooks[tp->pencil_track]);
    constraints_pop(shl->constraints);

    if(!samples) {
        tp->pencil_track = -1;
        return cmd_new_error_val("Cannot allocate buffer to store "
                                 "pencil drawing\n");
    }

    history_begin(shl->history, "Pencil");
    
    /* Preserve the region. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val((1 << tp->pencil_track)),
                  cmd_new_long_val(tp->pencil_modify_start),
                  cmd_new_long_val(tp->pencil_modify_end -
                                   tp->pencil_modify_start));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &err)) 
        return err;

    cmd_destroy_value(err);

    view_set_cursor(shl->view, GDK_WATCH);

    /* Convert the pencil buffers to samples and insert them. */

    work = tp->pencil_modify_end - tp->pencil_modify_start;
    done = 0;

    while(work) {
        off = (tp->pencil_modify_start - view->hadjust->value) + done;
        for(i = 0; i < MIN(work, MAX_BLOCK_SIZE); i++) {
            samples[i] = (float)tp->pencil_lows_buf[(int)((off+i)/view->hres)] 
                / (PEAK_HRES-1);
        }
        if(track_replace_samples_from(shl->clip->sr->tracks[tp->pencil_track],
                                      SAMPLE_TYPE_FLOAT_32,
                                      samples,
                                      tp->pencil_modify_start + done,
                                      i)) {
            FAIL("could not replace pencil drawing\n");
            r = 1;
            break;
        }
        done += i;
        work -= i;
    }
    
    view_set_cursor(shl->view, view_get_default_cursor(shl->view));

    mem_free(samples);

    tp->pencil_track = -1;

    if(r) {
        history_rollback(shl->history);
        return cmd_new_error_val("Cannot replace data with drawing");
    }

    history_commit(shl->history);

    view_set_wavecanvas_auto_scroll(view, TRUE);

    return cmd_new_void_val();
}

struct cmd_value *
tool_pencil_button_press(struct tool *tool,
                         GdkEventButton *event) {
    shell *shl = tool->shl;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    double rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    double value = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    AFframecount x = PIXEL_TO_FRAME(shl->view, event->x);
    struct cmd_value *r;

    if(track < 0 || track > shl->clip->sr->channels - 1)
        return cmd_new_void_val();
    
    r = tool_pencil_begin(tool, track);
    if(cmd_is_error(r)) 
        return r;

    tool_pencil_set_value(tool, x, value);

    gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
    
    return r;
}

struct cmd_value *
tool_pencil_motion(struct tool *tool,
                   GdkEventMotion *event) {
    shell *shl = tool->shl;
    struct view *view = shl->view;
    struct tool_pencil *tp = (struct tool_pencil *)tool;
    int track = PIXEL_TO_TRACK(shl->view, event->y), i;
    double rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    double value = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    double coeff = 0;
    int left, right, bottom;
    AFframecount x = PIXEL_TO_FRAME(shl->view, event->x);

    if(track < 0)
        return cmd_new_void_val();

    if(!(event->state & GDK_BUTTON1_MASK))
        return cmd_new_void_val();

    if(track != tp->pencil_track)
        return cmd_new_void_val();

    event->x = 
        CLAMP(event->x,
              0,
              view_get_widget(shl->view, "wavecanvas")->allocation.width);

    /* Interpolate between previous position and current. */

    left = MIN(event->x, view->last_mouse_x);
    right = (left == event->x ? view->last_mouse_x : event->x);
    if(view->last_mouse_x - event->x == 0)
        coeff = 1;
    else
        coeff = (double)(view->last_mouse_y - event->y) / 
            (double)(view->last_mouse_x - event->x);
    bottom = event->x < view->last_mouse_x ? event->y : view->last_mouse_y;

    if(left == right)
        right = left + 1;

    for(i = 0; i + left <= right; i++) {
        x = PIXEL_TO_FRAME(shl->view, i + left);
        value = RELATIVE_Y_TO_VALUE(shl->view,
                                    PIXEL_TO_RELATIVE_Y(shl->view, 
                                                        track, bottom + 
                                                        (i * coeff)));
        tool_pencil_set_value(tool, x, value);
    }

    gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
    return cmd_new_void_val();
}

struct cmd_value *
tool_pencil_button_release(struct tool *tool,
                           GdkEventButton *event) {
    shell *shl = tool->shl;
    struct tool_pencil *tp = (struct tool_pencil *)tool;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    double rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    double value = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    AFframecount x = PIXEL_TO_FRAME(shl->view, event->x);
    struct cmd_value *r;

    if(track == tp->pencil_track) {
        x = CLAMP(x,
                  shl->view->hadjust->value,
                  shl->view->hadjust->value + view_get_widget(shl->view, "wavecanvas")->allocation.width * shl->view->hres);
        tool_pencil_set_value(tool, x, value);
    }
    arbiter_yield();
    r = tool_pencil_commit(tool);
    
    view_redraw(shl->view);
    
    return r;
}

struct cmd_value *
tool_pencil_leave(struct tool *tool,
                  GdkEventCrossing *event) {
    struct cmd_value *r = tool_pencil_commit(tool);
    
    view_redraw(tool->shl->view);
    
    return r;
}

struct tool *
tool_pencil_new(shell *shl) {
    struct tool_pencil *tp = mem_calloc(sizeof(*tp), 1);
    static struct tool_funcs tool_pencil = {
        tool_destroy,
        
        NULL, // activate
        tool_pencil_deactivate,
        
        tool_pencil_button_press,
        tool_pencil_button_release,
        tool_pencil_motion,
        
        NULL, // key_press,
        NULL, // key_release,

        NULL, // enter
        tool_pencil_leave, // leave

        {
            "tool_pencil",
            0,
            20,
            NULL
        }
    };

    if(!tp)
        return NULL;

    ((struct tool *)tp)->name = "pencil";
    ((struct tool *)tp)->displayname = "Pencil";
    ((struct tool *)tp)->tooltip = "Sample-accurate waveform manipulation.";
    ((struct tool *)tp)->ordinal = 20;
    ((struct tool *)tp)->cursor = GDK_PENCIL;
    ((struct tool *)tp)->accel_key = GDK_a;
    ((struct tool *)tp)->accel_mods = GDK_MOD1_MASK;
    ((struct tool *)tp)->funcs = &tool_pencil;

    tp->pencil_track = -1;

    return (struct tool *)tp;
}

static int
tool_pencil_init(int id) {
    return tool_register("pencil", tool_pencil_new);
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Pencil Tool",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    tool_pencil_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

