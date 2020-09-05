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

#define SRC_ENV_1 (1 << 0)
#define SRC_ENV_2 (1 << 1)
#define DST_ENV_1 (1 << 2)
#define DST_ENV_2 (1 << 3)

struct tool_mix {
    struct tool tool;

    int pending;

    AFframecount mix_pivot;
    AFframecount mix_modify_start;
    AFframecount mix_modify_end;
    int mix_track;

    double src_gain;
    double dst_gain;

    int src_env_map;
    int dst_env_map;

    struct snd_shadow *mix_shadow;
};

struct tool_mix_source {
    int destroy_notification_id;
    int track;
    struct clip *clip;
    AFframecount offset;
    AFframecount current_pos;
};

static struct tool_mix_source tms = { 0 };

static struct cmd_value *
tool_mix_source_destroyed(const char *name,
                          struct cmd_argv *args) {
    struct clip *clip = cmd_clipp(args->argv[0]);

    if(tms.clip == clip) 
        tms.clip = NULL;
    
    return cmd_new_void_val();
}

struct tool_mix_source *
tool_mix_get_source() {
    if(!tms.clip)
        return NULL;
    if(tms.track < 0 || tms.track >= tms.clip->sr->channels)
        return NULL;
    return &tms;
}

void 
tool_mix_release_source() {
    struct tool_mix_source *tms = tool_mix_get_source();
    if(tms) {
        msg_unsubscribe(tms->clip->msg,
                        tms->destroy_notification_id);
        tms->clip = NULL;
    }
}

void
tool_mix_acquire_source(struct clip *clip,
                        int track,
                        AFframecount offset) {
    tool_mix_release_source();

    tms.clip = clip;
    tms.track = track;
    tms.offset = offset;
    tms.current_pos = -1;
    
    tms.destroy_notification_id = 
        msg_subscribe(clip->msg,
                      "clip::destroy",
                      "tool-mix::source-destroyed", 
                      NULL);

}

void
tool_mix_update_source(AFframecount current_pos) {
    struct tool_mix_source *tms = tool_mix_get_source();

    if(!tms)
        return;

    tms->current_pos = current_pos;
}


void 
tool_mix_draw(GtkWidget *widget,
              GdkDrawable *drawable,
              GdkGC *gc,
              GdkRectangle *area,
              void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    struct tool_mix_source *tms;
    int yo;
    GdkPixmap *pixmap;

    tms = tool_mix_get_source();

    if(!tms || tms->clip != shl->clip) 
        return;

    if(tms->offset < view->hadjust->value ||
       (tms->offset > view->hadjust->value + 
        view_get_widget(view, "wavecanvas")->allocation.width * view->hres))
        return;
    
    yo = (((tms->track - view->vadjust->value) * view->vres) + 
          (view->vres / 2) + 
          ((tms->track - view->vadjust->value) * TRACK_Y_SPACING)) - 1;
    
    gui_get_pixmap("mix_tool.source", &pixmap, NULL);
    gdk_draw_pixmap(drawable,
                    gc,
                    pixmap,
                    0, 
                    0,
                    ((tms->offset - view->hadjust->value) / view->hres) - 1, 
                    yo,
                    -1, -1);

    if(tms->current_pos >= 0) { 
        gui_get_pixmap("mix_tool.current_pos", &pixmap, NULL);
        gdk_draw_pixmap(drawable,
                        gc,
                        pixmap,
                        0, 
                        0,
                        ((tms->current_pos - view->hadjust->value) / 
                         view->hres) - 1, 
                        yo,
                        -1, -1);
    }

    return;
}

void
tool_mix_doit(struct tool_mix *tm,
              AFframecount src_offset,
              int src_track,
              struct clip *src,
              AFframecount dst_offset,
              int dst_track,
              struct clip *dst,
              float *srcbuf,
              float *dstbuf,
              AFframecount count) {
    AFframecount i;
    float src_env1_gain, src_env2_gain, dst_env1_gain, dst_env2_gain;

    /* Just straight mix. */

    if(tm->src_gain == 1 && tm->dst_gain == 1 &&
       tm->src_env_map == 0 && tm->dst_env_map == 0) {
        for(i = 0; i < count; i++) 
            dstbuf[i] += srcbuf[i];
        return;
    }

    /* Straight mix with attenuation. */

    if(tm->src_env_map == 0 && tm->dst_env_map == 0) {
        for(i = 0; i < count; i++) 
            dstbuf[i] = dstbuf[i] * tm->dst_gain + srcbuf[i] * tm->src_gain;
        return;
    }
    
    /* Mix through envelopes. */

    for(i = 0; i < count; i++) {
        src_env1_gain = marker_list_slope_value(src->markers->lists[src_track],
                                                src_offset + i,
                                                MARKER_SLOPE) + 1;
        src_env2_gain = marker_list_slope_value(src->markers->lists[src_track],
                                                src_offset + i,
                                                MARKER_SLOPE_AUX) + 1;
        dst_env1_gain = marker_list_slope_value(dst->markers->lists[dst_track],
                                                dst_offset + i,
                                                MARKER_SLOPE) + 1;
        dst_env2_gain = marker_list_slope_value(dst->markers->lists[dst_track],
                                                dst_offset + i,
                                                MARKER_SLOPE_AUX) + 1;
        if(tm->src_env_map & SRC_ENV_1)
            srcbuf[i] *= src_env1_gain;
        if(tm->src_env_map & SRC_ENV_2)
            srcbuf[i] *= src_env2_gain;
        if(tm->src_env_map & DST_ENV_1)
            srcbuf[i] *= dst_env1_gain;
        if(tm->src_env_map & DST_ENV_2)
            srcbuf[i] *= dst_env2_gain;

        if(tm->dst_env_map & SRC_ENV_1)
            dstbuf[i] *= src_env1_gain;
        if(tm->dst_env_map & SRC_ENV_2)
            dstbuf[i] *= src_env2_gain;
        if(tm->dst_env_map & DST_ENV_1)
            dstbuf[i] *= dst_env1_gain;
        if(tm->dst_env_map & DST_ENV_2)
            dstbuf[i] *= dst_env2_gain;
        
        dstbuf[i] = dstbuf[i] * tm->dst_gain + srcbuf[i] * tm->src_gain;
    }
}

void
tool_mix_range(struct tool *tool,
               int track,
               struct tool_mix_source *tms,
               AFframecount src_offset,
               AFframecount start,
               AFframecount end,
               float y_start,
               float y_end) {
    AFframecount work, done, count;
    int i;
    float *srcbuf, *dstbuf;
    shell *shl = tool->shl;

    srcbuf = mem_alloc(sizeof(float) * MAX_BLOCK_SIZE);
    dstbuf = mem_alloc(sizeof(float) * MAX_BLOCK_SIZE);

    if(!srcbuf || !dstbuf) {
        FAIL("can't allocate buffers\n");
        if(srcbuf)
            mem_free(srcbuf);
        if(dstbuf)
            mem_free(dstbuf);
        return;
    }

    src_offset = tms->offset + src_offset;

    if(src_offset < 0) 
        src_offset = -tms->offset;

    work = end - start;
    done = 0;

    while(work) {
        count = MIN(work, MAX_BLOCK_SIZE);
        memset(srcbuf, '\0', sizeof(float) * count);
        memset(dstbuf, '\0', sizeof(float) * count);

        /* FIXME: we have to do sample rate conversion on the
           source. FIXME2: error checking */

        if(track_get_samples_as(tms->clip->sr->tracks[tms->track],
                                SAMPLE_TYPE_FLOAT_32,
                                srcbuf,
                                src_offset + done,
                                count) < 0) {
            FAIL("get source failed\n");
        }

        i = track_get_samples_as(shl->clip->sr->tracks[track],
                                 SAMPLE_TYPE_FLOAT_32,
                                 dstbuf,
                                 start + done,
                                 count);

        tool_mix_doit((struct tool_mix *)tool, 
                      src_offset + done,
                      tms->track,
                      tms->clip,
                      start + done,
                      track,
                      shl->clip,
                      srcbuf,
                      dstbuf,
                      count);
        
        if(track_replace_samples_from(shl->clip->sr->tracks[track],
                                      SAMPLE_TYPE_FLOAT_32,
                                      dstbuf,
                                      start + done,
                                      count)) {
            FAIL("replace failed\n");
        }

        work -= i;
        done += i;
    }
    mem_free(srcbuf);
    mem_free(dstbuf);
}

struct cmd_value *
tool_mix_begin(struct tool *tool,
               int track,
               AFframecount x,
               double value) {
    struct tool_mix_source *tms;
    struct tool_mix *tm = (struct tool_mix *)tool;
    shell *shl = tool->shl;
    const char *s;

    tms = tool_mix_get_source();
    
    if(!tms) 
        return cmd_new_error_val("Set a source first by using "
                                 "control-click");
    
    tm->pending = 1;
    if(tm->mix_shadow)
        snd_shadow_destroy(tm->mix_shadow);
    tm->mix_shadow = snd_shadow_new(shl->clip->sr, (1 << track),
                                    x, shl->view->hres);
    
    if(!tm->mix_shadow) 
        return cmd_new_error_val("Cannot create undo storage");
    
    if((s = constraints_test(shl->constraints,
                             region_new((1 << track),
                                        REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot mix because region is locked "
                                 "(%s)", s);

    
    tm->mix_track = track;
    tm->mix_pivot = x;
    tm->mix_modify_start = x;
    tm->mix_modify_end = x + shl->view->hres;
    
    tm->src_gain = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(tool->panel, "tool_mix_source_volume")))->value / 100.0;
    tm->dst_gain = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(tool->panel, "tool_mix_destination_volume")))->value / 100.0;
    tm->src_env_map = tm->dst_env_map = 0;
    
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_srcenv1_to_src"))))
        tm->src_env_map |= SRC_ENV_1;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_srcenv2_to_src"))))
        tm->src_env_map |= SRC_ENV_2;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_dstenv1_to_src"))))
        tm->src_env_map |= DST_ENV_1;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_dstenv2_to_src"))))
        tm->src_env_map |= DST_ENV_2;
    
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_srcenv1_to_dst"))))
        tm->dst_env_map |= SRC_ENV_1;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_srcenv2_to_dst"))))
        tm->dst_env_map |= SRC_ENV_2;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_dstenv1_to_dst"))))
        tm->dst_env_map |= DST_ENV_1;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(tool->panel, "tool_mix_dstenv2_to_dst"))))
        tm->dst_env_map |= DST_ENV_2;
    
    DEBUG("src_gain: %f, dst_gain: %f, src_env_map: %d, dst_env_map: %d\n",
          tm->src_gain, tm->dst_gain, tm->src_env_map, tm->dst_env_map);

    tool_mix_update_source(tms->offset + (x - tm->mix_pivot));
    tool_mix_range(tool, track, tms, 0, x, x + shl->view->hres, value, value);
    gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
    return cmd_new_void_val();
}

struct cmd_value *
tool_mix_commit(struct tool *tool) {
    struct tool_mix *tm = (struct tool_mix *)tool;
    struct tool_mix_source *tms;
    shell *shl = tool->shl;

    tms = tool_mix_get_source();

    if(!tm->pending)
        return cmd_new_void_val();

    if(!tms) 
        return cmd_new_void_val();

    tool_mix_update_source(-1);

    history_begin(shl->history, "Mix Tool");
    history_remember(shl->history,
                     CMD_NEW("delete-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(tm->mix_shadow->map),
                             cmd_new_long_val(tm->mix_shadow->offset),
                             cmd_new_long_val(tm->mix_shadow->count)));
    history_remember(shl->history,
                     CMD_NEW("insert-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(tm->mix_shadow->map),
                             cmd_new_sndp_val_with_dtor(tm->mix_shadow->dst,
                                                        cmd_sndp_dtor),
                             cmd_new_int_val(tm->mix_shadow->map),
                             cmd_new_long_val(tm->mix_shadow->offset)));
    history_commit(shl->history);

    snd_dump(tm->mix_shadow->dst);
    tm->mix_shadow->dst = NULL;
    snd_shadow_destroy(tm->mix_shadow);
    tm->mix_shadow = NULL;

    view_redraw(shl->view);

    tm->pending = 0;

    return cmd_new_void_val();
}

struct cmd_value *
tool_mix_button_press(struct tool *tool,
                      GdkEventButton *event) {
    shell *shl = tool->shl;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    double rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    double value = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    AFframecount x = PIXEL_TO_FRAME(shl->view, event->x);

    if(track < 0 || track > shl->clip->sr->channels - 1)
        return cmd_new_void_val();

    if(event->state & GDK_CONTROL_MASK) {
        tool_mix_acquire_source(shl->clip, track, x);
        gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
        return cmd_new_void_val();
    }

    return tool_mix_begin(tool, track, x, value);
}

static int
shell_owns_tms(shell *shl,
               void *user_data) {
    struct tool_mix_source *tms = user_data;
    if(shl->clip == tms->clip)
        return 1;
    return 0;
}

struct cmd_value *
tool_mix_motion(struct tool *tool,
                GdkEventMotion *event) {
    shell *shl = tool->shl;
    shell *src_shl;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    double rel_y = PIXEL_TO_RELATIVE_Y(shl->view, track, event->y);
    double value = RELATIVE_Y_TO_VALUE(shl->view, rel_y);
    struct tool_mix_source *tms;
    struct tool_mix *tm = (struct tool_mix *)tool;

    AFframecount x = PIXEL_TO_FRAME(shl->view, event->x), x1, x2;

    if(track != tm->mix_track)
        return cmd_new_void_val();

    if(!(event->state & GDK_BUTTON1_MASK)) 
        return cmd_new_void_val();

    if(event->state & GDK_CONTROL_MASK) {
        tool_mix_acquire_source(shl->clip, track, x);
        gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
        return cmd_new_void_val();
    }

    tms = tool_mix_get_source();

    if(!tms) 
        return cmd_new_error_val("Set source first (control-click)");

    x1 = PIXEL_TO_FRAME(shl->view, event->x) < shl->view->last_mouse_offset ? 
        PIXEL_TO_FRAME(shl->view, event->x) : shl->view->last_mouse_offset;
    x2 = x1 == PIXEL_TO_FRAME(shl->view, event->x) ? 
        shl->view->last_mouse_offset : PIXEL_TO_FRAME(shl->view, event->x);
    x2 = x2 + shl->view->hres;

    if(x1 < 0)
        x1 = 0;
    if(x2 < 0)
        x2 = 0;

    tool_mix_update_source(tms->offset + (x - tm->mix_pivot));
        
    if(x1 >= tm->mix_modify_start && x1 <= tm->mix_modify_end)
        x1 = tm->mix_modify_end;
    if(x2 >= tm->mix_modify_start && x2 <= tm->mix_modify_end)
        x2 = tm->mix_modify_start;

    /* Only modify section if it wasn't already modified, to allow for
       shaky user hands and wavering artistic judgment. */

    if(x2 > x1) {

        if(snd_shadow_extend(tm->mix_shadow, x1, x2 - x1)) 
            return cmd_new_error_val("Cannot update undo storage");

        tool_mix_range(tool, track, tms, x1 - tm->mix_pivot, x1, x2, value,
                       value);

        if(x1 < tm->mix_modify_start) 
            tm->mix_modify_start = x1;
        if(x2 > tm->mix_modify_end) 
            tm->mix_modify_end = x2;

    }

    gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));

    /* Redraw the shell of the source as well. */

    src_shl = arbiter_find_shell(shell_owns_tms, tms);

    if(src_shl && src_shl != shl)
        gtk_widget_queue_draw(view_get_widget(src_shl->view, "wavecanvas"));

    return cmd_new_void_val();
}

                
struct cmd_value *
tool_mix_button_release(struct tool *tool,
                        GdkEventButton *event) {
    shell *shl = tool->shl;
    int track = PIXEL_TO_TRACK(shl->view, event->y);
    AFframecount x = PIXEL_TO_FRAME(shl->view, event->x);

    if(event->state & GDK_CONTROL_MASK) {
        tool_mix_acquire_source(shl->clip, track, x);
        gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
        return cmd_new_void_val();
    }
    
    return tool_mix_commit(tool);
}

struct cmd_value *
tool_mix_leave(struct tool *tool,
               GdkEventCrossing *event) {
    return tool_mix_commit(tool);
}

struct tool *
tool_mix_new(shell *shl) {
    struct tool_mix *tm = mem_calloc(sizeof(*tm), 1);
    static struct tool_funcs tool_mix = {
        tool_destroy,
        
        NULL, // activate
        NULL, // deactivate
        
        tool_mix_button_press,
        tool_mix_button_release,
        tool_mix_motion,
        
        NULL, // key_press
        NULL, // key_release

        NULL, // enter
        tool_mix_leave, // leave

        {
            "tool_mix",
            0,
            40,
            tool_mix_draw
        }
    };

    if(!tm)
        return NULL;

    ((struct tool *)tm)->name = "mix";
    ((struct tool *)tm)->displayname = "Mix";
    ((struct tool *)tm)->tooltip = "Mixes audio.";
    ((struct tool *)tm)->ordinal = 20;
    ((struct tool *)tm)->cursor = GDK_CROSSHAIR;
    ((struct tool *)tm)->accel_key = GDK_s;
    ((struct tool *)tm)->accel_mods = GDK_MOD1_MASK;
    ((struct tool *)tm)->funcs = &tool_mix;    

    return (struct tool *)tm;
}

static int
tool_mix_init(int id) {
    int i;
    struct cmd_signature f[] = {

        /* Clip callbacks. */

        { "tool-mix::source-destroyed", "Clip destroy event.",
          tool_mix_source_destroyed, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_clipp_T) },

    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);


    return tool_register("mix", tool_mix_new);
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Mix Tool",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004,2005",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    tool_mix_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

