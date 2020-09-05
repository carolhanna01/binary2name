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

/**
 * @file
 * This file contains the drawing routines responsible for drawing on the
 * waveform canvas, the mixer canvas, and the info canvas.
 */

#include <config.h>
#include <math.h>
#include <gnome.h>
#include <gtk/gtk.h>
#include "pref.h"
#include "shell.h"
#include "gui.h"
#include "player.h"
#include "draw.h"
#include "arbiter.h"

void
draw_pointer(GtkWidget *widget,
             GdkDrawable *drawable,
             GdkGC *gc,
             GdkRectangle *area,
             void *user_data) {
    int t, x, b;
    struct view *view = user_data;
    shell *shl = view->shl;
    AFframecount pos = shl->player->state->audio_pos,
        record_pos = shl->player->state->record_pos;
    GdkGCValues gc_vals;

    if(!shl->player->player_running)
        return;

    pos = shl->player->state->audio_pos;

    if(pos < view->hadjust->value ||
       pos > view->hadjust->value + 
       (view_get_widget(view, "wavecanvas")->allocation.width * view->hres))
        return;

    gdk_gc_get_values(gc, &gc_vals);

    gdk_gc_set_foreground(gc, gui_get_color("point_play"));
    
    x = (pos - view->hadjust->value) / view->hres;
    if(!shl->player->state->record_mode) {
        for(b = 0, t = view->vadjust->value; t < shl->clip->sr->channels; t++, b++) 
            gdk_draw_line(drawable,
                          gc,
                          x, (b * view->vres) + (b * TRACK_Y_SPACING),
                          x, (b * view->vres) + (b * TRACK_Y_SPACING) + view->vres);
    }

    if(shl->player->state->record_mode) {
        gdk_gc_set_foreground(gc, gui_get_color("point_record"));
        x = (record_pos - view->hadjust->value) / view->hres;
        for(b = 0, t = view->vadjust->value; t < shl->clip->sr->channels; t++, b++) {
            if(!((1 << t) & shl->player->state->target_tracks_map))
                gdk_gc_set_line_attributes(gc,
                                           gc_vals.line_width,
                                           GDK_LINE_ON_OFF_DASH,
                                           gc_vals.cap_style,
                                           gc_vals.join_style);
            gdk_draw_line(drawable,
                          gc,
                          x, (b * view->vres) + (b * TRACK_Y_SPACING),
                          x, (b * view->vres) + (b * TRACK_Y_SPACING) + view->vres);
            gdk_gc_set_line_attributes(gc,
                                       gc_vals.line_width,
                                       gc_vals.line_style,
                                       gc_vals.cap_style,
                                       gc_vals.join_style);
        }
    }
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void
draw_cursor(GtkWidget *widget,
            GdkDrawable *drawable,
            GdkGC *gc,
            GdkRectangle *area,
            void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    int i, t, pos = (shl->select_start - 
                     ceil(view->hadjust->value)) / view->hres;
    GdkGCValues gc_vals;
    GdkColor white;

    if(!shl->clip->sr)
        return;

    if(shl->select_start != shl->select_end) 
        return;
    
    if(shl->select_start < view->hadjust->value ||
       shl->select_start > view->hadjust->value + 
       (view_get_widget(view, "wavecanvas")->allocation.width * view->hres))
        return;

    gdk_gc_get_values(gc, &gc_vals);
    gdk_color_white(gdk_colormap_get_system(), &white);
    gdk_gc_set_foreground(gc, &white);
    
    for(i = 0, t = view->vadjust->value; t < shl->clip->sr->channels; t++, i++)
        if((1 << t) & shl->select_channel_map)
            gdk_draw_line(drawable,
                          gc,
                          pos, ((i * view->vres) + 
                                (i * TRACK_Y_SPACING)) + 1,
                          pos, ((i * view->vres) + 
                                (i * TRACK_Y_SPACING) + 
                                view->vres));
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void 
draw_loop(GtkWidget *widget,
          GdkDrawable *drawable,
          GdkGC *gc,
          GdkRectangle *area,
          void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    AFframecount x1, x2, i, t;
    AFframecount offset = view->hadjust->value, 
        count = area->width * view->hres;
    GdkGCValues gc_vals;

    if(shl->loop_start == shl->loop_end)
        return;

    if(shl->loop_start < offset && shl->loop_end < offset)
        return;

    if(shl->loop_start > offset + count && 
       shl->loop_end > offset + count)
        return;

    /* At least one visible. */

    gdk_gc_get_values(gc, &gc_vals);

    x1 = shl->loop_start - offset;
    x2 = shl->loop_end - offset;

    x1 /= view->hres;
    x2 /= view->hres;

    if(x1 > count / view->hres)
        x1 = -1;
    if(x2 > count / view->hres)
        x2 = -1;
    
    gdk_gc_set_foreground(gc, gui_get_color("loop"));
    if(!shl->loop)
        gdk_gc_set_line_attributes(gc, 
                                   gc_vals.line_width, 
                                   GDK_LINE_ON_OFF_DASH, 
                                   gc_vals.cap_style, 
                                   gc_vals.join_style);
    for(t = 0, i = view->vadjust->value; i < shl->clip->sr->channels; i++, t++) { 
        if(x1 >= 0)
            gdk_draw_line(drawable,
                          gc,
                          x1, (t * view->vres) + (t * TRACK_Y_SPACING),
                          x1, ((t + 1) * view->vres) + ((t + 1) * 
                                                        TRACK_Y_SPACING));
        if(x2 >= 0) 
            gdk_draw_line(drawable,
                          gc,
                          (x2 - 1), (t * view->vres) + (t * TRACK_Y_SPACING),
                          (x2 - 1), ((t + 1) * view->vres) + ((t + 1) *
                                                              TRACK_Y_SPACING));
    }
    
    if(!shl->loop)
        gdk_gc_set_line_attributes(gc, 
                                   gc_vals.line_width, 
                                   gc_vals.line_style,
                                   gc_vals.cap_style, 
                                   gc_vals.join_style);

    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void 
draw_zero(GtkWidget *widget,
          GdkDrawable *drawable,
          GdkGC *gc,
          GdkRectangle *area,
          void *user_data) {
    unsigned int c, d;
    struct view *view = user_data;
    shell *shl = view->shl;
    GdkGCValues gc_vals;

    if(!view->show_zero)
        return;

    gdk_gc_get_values(gc, &gc_vals);
    
    /* Draw zero line. */
    
    gdk_gc_set_foreground(gc, gui_get_color("zero"));
    
    for(d = 0, c = view->vadjust->value; c <= shl->clip->sr->channels; c++, d++) 
        gdk_draw_line(drawable,
                      gc,
                      0, 
                      ((view->vres * d) - ((view->vres) / 2) + 
                       (d * TRACK_Y_SPACING)) - 1,
                      area->width, 
                      ((view->vres * d) - ((view->vres) / 2) + 
                       (d * TRACK_Y_SPACING)) - 1) ;
    
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void
draw_rms(GtkWidget *widget,
         GdkDrawable *drawable, 
         GdkGC *gc, 
         GdkRectangle *area,
         void *user_data) {
    struct draw_hook_track_data *dhtd = user_data;
    struct view *view = dhtd->view;
    shell *shl = view->shl;
    int x, y, yo, first_selected_pixel, last_selected_pixel;
    AFframecount r, offset, count;
    GdkColor *normal, *selected;
    GdkGCValues gc_vals;
    float fact = (float)PEAK_VRES / (float)view->vres;

    if(!pref_get_as_int("view.wave.draw.rms.enable"))
        return;

    /* Below a certain zoom factor RMS approaches peak and actually
       obscures it, so we want to avoid drawing it. */

    if(view->hres < pref_get_as_int("view.wave.draw.rms.minimum_zoom"))
        return;

    if(!view->rms)
        return;

    gdk_gc_get_values(gc, &gc_vals);

    normal = gui_get_color("wave.rms");
    selected = gui_get_color("wave.rms.selected");
    gdk_gc_set_foreground(gc, normal);
    
    /* Draw track. */

    yo = (((dhtd->track - view->vadjust->value) * view->vres) + 
          (view->vres / 2) + 
          ((dhtd->track - view->vadjust->value) * TRACK_Y_SPACING));
    offset = view->hadjust->value + (area->x * view->hres);
    count = area->width * view->hres;
    
    r = track_get_rms(shl->clip->sr->tracks[dhtd->track],
                      view->rms,
                      offset,
                      count,
                      view->hres);

    first_selected_pixel = MAX(0, (shl->select_start - offset) / view->hres);
    last_selected_pixel = ((shl->select_start - offset) / view->hres) + 
        ((shl->select_end - shl->select_start) / view->hres);

    for(x = area->x; x < area->x + floor(r / view->hres); x++) {
        y = view->rms[x - area->x] / fact;

        if(((1 << dhtd->track) & shl->select_channel_map) && 
           x >= first_selected_pixel &&
           x <= last_selected_pixel) {
            gdk_gc_set_foreground(gc, selected);
        }

        gdk_draw_line(drawable, 
                      gc, 
                      x, yo + y,
                      x, yo - y);
        
        gdk_gc_set_foreground(gc, normal);
    }
    
    gdk_gc_set_foreground(gc, &gc_vals.foreground);

}

void
draw_peaks_from_buffers(struct view *view,
                        GdkDrawable *drawable, 
                        GdkGC *gc, 
                        GdkRectangle *area, 
                        int track,
                        peak_unit_t *lows, 
                        peak_unit_t *highs,
                        AFframecount offset,
                        AFframecount count) {
    shell *shl = view->shl;
    int yo, x, max_x, t, in_selection = 0;
    float fact;
    GdkColor *normal, *selected;
    peak_unit_t prev_low = 0, prev_high = 0;
    AFframecount sfv = shl->select_start, slv = shl->select_end;

    if(shl->select_start != shl->select_end) {
        sfv = MAX(0, (shl->select_start - offset) / view->hres);
        slv = ((shl->select_start - offset) / view->hres) + 
            ((shl->select_end - shl->select_start) / view->hres);
    }

    normal = gui_get_color("wave.peaks");
    selected = gui_get_color("wave.peaks.selected");

    gdk_gc_set_foreground(gc, normal);
    
    /* Draw track. */
    
    t = track - view->vadjust->value;
    max_x = area->x + MIN(area->width, floor(count / view->hres) - area->x);
    yo = (t * view->vres) + (view->vres / 2) + (t * TRACK_Y_SPACING);
    fact = (float)PEAK_VRES / (float)view->vres;

    for(x = area->x; x < max_x; x++) {

        /* Determine whether we're inside the selection. */

        if(((1 << track) & shl->select_channel_map) && 
           sfv != slv && x >= sfv && x < slv && slv > 0) {
            gdk_gc_set_foreground(gc, selected);
            in_selection = 1;
        }

        if(x != area->x) {
            if(prev_high < lows[x])
                lows[x] = prev_high;
            
            if(prev_low > highs[x])
                highs[x] = prev_low;
        }
        
        gdk_draw_line(drawable, 
                      gc, 
                      x, (lows[x] / fact) + yo,
                      x, (highs[x] / fact) + yo);

        prev_low = lows[x];
        prev_high = highs[x];

        if(in_selection)
            gdk_gc_set_foreground(gc, normal);
    }
}

void
draw_peaks(GtkWidget *widget,
           GdkDrawable *drawable,
           GdkGC *gc,
           GdkRectangle *area,
           void *user_data) {
    struct draw_hook_track_data *dhtd = user_data;
    struct view *view = dhtd->view;
    shell *shl = view->shl;
    AFframecount offset, count, r;

    offset = view->hadjust->value;
    count = view_get_widget(view, "wavecanvas")->allocation.width * view->hres;
    
    r = track_get_peaks(shl->clip->sr->tracks[dhtd->track], 
                        shl->view->peaks_low,
                        shl->view->peaks_high,
                        offset, 
                        count, 
                        shl->view->hres);
    
    if(r == -1)
        return;

    draw_peaks_from_buffers(view, drawable, gc, area, dhtd->track, 
                            shl->view->peaks_low, 
                            shl->view->peaks_high, 
                            offset, r);
}

void
draw_tracks(GtkWidget *widget,
            GdkDrawable *drawable,
            GdkGC *gc,
            GdkRectangle *area,
            void *user_data) {
    struct view *view = user_data;
    struct draw_hook_track_data dhtd;
    int i;
    GdkGCValues gc_vals;
    gdk_gc_get_values(gc, &gc_vals);

    for(i = view->vadjust->value; i < view->shl->clip->sr->channels; i++) {
        dhtd.track = i;
        dhtd.view = view;
        draw_hooks_dispatch(view->track_hooks[i],
                            widget,
                            drawable,
                            gc,
                            area,
                            &dhtd);
    }
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void
draw_constraints(GtkWidget *widget,
                 GdkDrawable *drawable,
                 GdkGC *gc,
                 GdkRectangle *area,
                 void *user_data) {
#if 0
    struct view *view = user_data;
    unsigned int t, i;
    shell *shl = view->shl;
    AFframecount offset, count;
    GdkGCValues gc_vals;
    struct rgnlock *region_lock = shl->clip->region_lock;

    if(!view->draw_regionlock)
        return;

    if(!region_lock->enabled)
        return;
    
    gdk_gc_get_values(gc, &gc_vals);

    offset = view->hadjust->value;
    count = view_get_widget(view, "wavecanvas")->allocation.width * view->hres;
    
    gdk_gc_set_foreground(gc, gui_get_color("block"));
    for(t = 0, i = view->vadjust->value; 
        i < shl->clip->sr->channels; i++, t++) {
        if((1 << i) & region_lock->map)
            gdk_draw_rectangle(drawable,
                               gc,
                               FALSE,
                               (region_lock->offset - offset) / view->hres, 
                               ((t * view->vres) + 
                                (t * TRACK_Y_SPACING)) + 1,
                               (region_lock->count) / view->hres,
                               view->vres);
        
    }
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
#endif

}

void
draw_blocks(GtkWidget *widget,
            GdkDrawable *drawable,
            GdkGC *gc,
            GdkRectangle *area,
            void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    AFframecount frame_offset = view->hadjust->value;
    AFframecount frame_count = view_get_widget(view, "wavecanvas")->allocation.width * view->hres;
    unsigned int yo, i, t = 0;
    AFframecount cur;
    GdkGCValues gc_vals;
    GList *l;
    
    if(!view->draw_blocks)
        return;

    gdk_gc_get_values(gc, &gc_vals);
    
    if(frame_count <= 1) {
        FAIL("frame_count: %ld, frame_offset: %ld\n", 
             frame_count, frame_offset);
        return;
    }
    
    /* Draw the internal block representation of the sample. */

    gdk_gc_set_foreground(gc, gui_get_color("block"));
    for(i = view->vadjust->value; i < shl->clip->sr->channels; i++) {

        l = shl->clip->sr->tracks[i]->bl->l;

        if(!l)
            continue;

        cur = ((block *)l->data)->count;
        yo = t++ * view->vres;

        while(l && cur < frame_offset + frame_count) {
            if(cur >= frame_offset) 
                gdk_draw_line(drawable,
                              gc,
                              (cur - frame_offset) / view->hres, 
                              yo + (t * TRACK_Y_SPACING),
                              (cur - frame_offset) / view->hres, 
                              (yo + view->vres) + (t * TRACK_Y_SPACING));

            l = l->next;
            if(l)
                cur += ((block *)l->data)->count;
        }
    }
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void
draw_track_dividers(GtkWidget *widget,
                    GdkDrawable *drawable,
                    GdkGC *gc,
                    GdkRectangle *area,
                    void *user_data) {
    struct view *view = user_data;
    int i;
    GdkGCValues gc_vals;
    shell *shl = view->shl;
    gdk_gc_get_values(gc, &gc_vals);

    gdk_gc_set_foreground(gc, gui_get_color("track_divider"));
    
    for(i = 1; i < shl->clip->sr->channels - view->vadjust->value; i++) {
            gdk_draw_rectangle(drawable,
                               gc,
                               TRUE,
                               0, 
                               ((i * view->vres) + ((i-1) * TRACK_Y_SPACING)),
                               view_get_widget(view, 
                                               "wavecanvas")->allocation.width,
                               TRACK_Y_SPACING);
            
            /*
        gdk_draw_line(drawable,
                      gc,
                      0, 
                      ((i * view->vres) + (i * TRACK_Y_SPACING) + 
                       (TRACK_Y_SPACING >> 1)),
                      shell_canvas_width_get(shl),
                      ((i * view->vres) + (i * TRACK_Y_SPACING) + 
                       (TRACK_Y_SPACING >> 1)));
            */
        
    }
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void
draw_grid(GtkWidget *widget,
          GdkDrawable *drawable,
          GdkGC *gc,
          GdkRectangle *area,
          void *user_data) {
    struct view *view = user_data;
    unsigned int x, visible_tracks, height;
    AFframecount cur, offset, count;
    GdkGC *gc_copy;
    shell *shl = view->shl;
    gint8 dashes[] = { 1, 1 };

    if(!view->show_grid)
        return;
    
    offset = view->hadjust->value;
    count = area->width * view->hres;

    gc_copy = gdk_gc_new(widget->window);

    gdk_gc_copy(gc_copy, gc);

    gdk_gc_set_foreground(gc, gui_get_color("grid"));

    /*

    FIXME: Setting dashes makes the grid display look better but it's
    really slow under GTK+2. Disable until workaround.

    gdk_gc_set_dashes(gc, 0, dashes, 2);
    gdk_gc_set_line_attributes(gc, 
                               1,
                               GDK_LINE_ON_OFF_DASH,
                               GDK_CAP_BUTT,
                               GDK_JOIN_MITER);
    */

    for(cur = offset - (offset % shl->grid.gap);
        cur < offset + count;
        cur += MAX(view->hres, shl->grid.gap)) {

        x = (cur - offset) / view->hres;
        visible_tracks = shl->clip->sr->channels - view->vadjust->value;
        
        /* We just draw blithely across the track dividers, but
           that's all right since a) the track divider is just a measly 
           1 pixel high line right now and b) it gets painted over us
           which restores the damage. */

        height = (visible_tracks * view->vres) + 
            (visible_tracks * (TRACK_Y_SPACING));
        
        gdk_draw_line(drawable,
                      gc,
                      x, 
                      0,
                      x, 
                      height - 2);
    }
    
    gdk_gc_copy(gc, gc_copy);
    gdk_gc_unref(gc_copy);

}

void 
draw_background(GtkWidget *widget,
                GdkDrawable *drawable,
                GdkGC *gc,
                GdkRectangle *area,
                void *user_data) {
    struct view *view = user_data;
    shell *shl = view->shl;
    GdkGCValues gc_vals;
    GdkColor white;
    GdkPixmap *pixmap;

    gdk_gc_set_clip_rectangle(gc, area);

    gdk_gc_get_values(gc, &gc_vals);
    gdk_gc_set_foreground(gc, gui_get_color("background"));
    
    gui_get_pixmap("wave.background", &pixmap, NULL);
    gdk_gc_set_fill(gc, GDK_TILED);
    gdk_gc_set_tile(gc, pixmap);
    gdk_draw_rectangle(view->wavepixmap,
                       gc,
                       TRUE,
                       0,                            
                       ((shl->clip->sr->channels - view->vadjust->value) *
                        view->vres) + (shl->clip->sr->channels - 
                                       view->vadjust->value - 1) * 
                       TRACK_Y_SPACING,
                       area->width,
                       area->height -
                       ((shl->clip->sr->channels - view->vadjust->value) *
                        view->vres) + (shl->clip->sr->channels - 
                                       view->vadjust->value - 1) * 
                           TRACK_Y_SPACING);
    gdk_gc_set_fill(gc, GDK_SOLID);
    
    gdk_draw_rectangle(drawable,
                       gc,
                       TRUE,
                       0, 0,
                       area->width,
                       (view->vres * (shl->clip->sr->channels - 
                                      view->vadjust->value)) + 
                       ((shl->clip->sr->channels - view->vadjust->value - 1) * 
                        TRACK_Y_SPACING));
    /*
                       (widget->allocation.width < 
                        snd_frame_count(shl->clip->sr) / view->hres) ? 
                       widget->allocation.width : 
                       snd_frame_count(shl->clip->sr) / view->hres,
                       view->vres * (shl->clip->sr->channels - 
                                    (int) view->vadjust->value));
    */

    gdk_color_white(gdk_colormap_get_system(), &white);
    gdk_gc_set_foreground(gc, &white);

    /* Draw demarcation lines separating wave from background. */
    
    gdk_draw_line(drawable,
                  gc,
                  0, (view->vres * (shl->clip->sr->channels - 
                                   view->vadjust->value)) +
                  ((shl->clip->sr->channels - view->vadjust->value - 1) * 
                   TRACK_Y_SPACING),
                  area->width,
                  /*
                  (widget->allocation.width < 
                   snd_frame_count(shl->clip->sr) / view->hres) ? 
                  widget->allocation.width : 
                  snd_frame_count(shl->clip->sr) / view->hres,
                  */
                  (view->vres * (shl->clip->sr->channels - view->vadjust->value)) +
                  ((shl->clip->sr->channels - view->vadjust->value - 1) * 
                   TRACK_Y_SPACING));
    
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
    gdk_gc_set_clip_rectangle(gc, NULL);

}

/*
void 
draw_mix_tool_source(struct view *view,
                     GdkDrawable *drawable,
                     GdkGC *gc) {
    struct mix_tool_source *mts;
    shell *shl = view->shl;

    mts = arbiter_get_mix_tool_source();

    if(!mts || mts->sr != shl->clip->sr) 
        return;
    
    if(mts->offset < view->hadjust->value ||
       (mts->offset > view->hadjust->value + 
        view_get_widget(view, "wavecanvas")->allocation.width * view->hres))
        return;
                  
    gdk_draw_drawable(drawable,
                    gc,
                    gui_get_pixmap("mix_tool.source"),
                    0, 
                    0,
                    ((mts->offset - view->hadjust->value) / view->hres) - 1, 
                    (view->vres/2) - 8,
                    -1, -1);

    if(mts->current_pos >= 0) 
        gdk_draw_drawable(drawable,
                        gc,
                        gui_get_pixmap("mix_tool.current_pos"),
                        0, 
                        0,
                        ((mts->current_pos - view->hadjust->value) / 
                         view->hres) - 1, 
                        (view->vres/2) - 8,
                        -1, -1);
        
    return;
}
*/

void 
draw_mixer_sliders(struct view *view,
                   GtkWidget *widget,
                   GdkRectangle *area) {
    int i, j, x = MIXER_LEVEL_OFFSET_X, y = 0, y2 = 0, mix_lvl_height = MIXER_LEVEL_HEIGHT, have_room_for_text = 1, t;
    char s[8];
    GdkGC *gc = widget->style->fg_gc[widget->state];
    GdkDrawable *drawable = view->mixerpixmap;
    GdkPixmap *mixer_level;
    GdkGCValues gc_vals;
    shell *shl = view->shl;
    struct gui_letterbox *lb;
    struct gui_letterbox_extents ink_lbe, logical_lbe;

    gdk_gc_get_values(gc, &gc_vals);

    gui_get_pixmap("mixer.level", &mixer_level, NULL);
    
    while(mix_lvl_height > 1 && 
          (shl->clip->mixer->output_channels) +
          (shl->clip->mixer->output_channels * mix_lvl_height) > view->vres - 4) 
        mix_lvl_height--; // = mix_lvl_height > 1 ? (mix_lvl_height - 1) / 2 : 1;
    
    if(mix_lvl_height != MIXER_LEVEL_HEIGHT)
        have_room_for_text = 0;

    if(mix_lvl_height < 1)
        mix_lvl_height = 1;

    y += 2;
    for(i = view->vadjust->value, t = 0; i < shl->clip->sr->channels; i++, t++) {
        for(j = 0; j < shl->clip->mixer->output_channels; j++) {
            y2 = y + j * mix_lvl_height + j;
            s[1] = '\0';
            if(have_room_for_text)
                snprintf(s, 128, "%d", j + 1);
            else
                s[0] = '-';

            lb = gui_letterbox_new(widget, s);
    
            gui_letterbox_get_extents(lb, &ink_lbe, &logical_lbe);
    
            gui_letterbox_draw(lb, drawable,
                               x + 1,
                               (y2 + mix_lvl_height + 
                                (have_room_for_text ? 0 : 2)));
    
            gui_letterbox_destroy(lb);

            /*
            output_peaks[j][current_output_position[j]] = MAX(shl->clip->mixer->output_peaks[j], MAX(shl->clip->mixer->output_peaks[j + pref_get_as_int("max_tracks")], shl->clip->mixer->output_peaks[j + (2*pref_get_as_int("max_tracks"))]));

            if(current_output_position[j] < output_peak_delay)
                current_output_position[j]++;
            else
                memmove(&output_peaks[j][0], &output_peaks[j][1], 
                        output_peak_delay * sizeof(float));
            */
            //            DEBUG("current_output_position[%d]: %d\n", j, current_output_position[j]);

            if(mixer_level)
                gdk_draw_drawable(drawable,
                                gc,
                                mixer_level,
                                0, 0,
                                x + 8, y2,
                                MIXER_LEVEL_WIDTH * shl->clip->mixer->mixtable[j][i],
                                mix_lvl_height);
            else
                gdk_draw_rectangle(drawable,
                                   gc,
                                   TRUE,
                                   x + 8, y2,
                                   MIXER_LEVEL_WIDTH * shl->clip->mixer->mixtable[j][i],
                                   mix_lvl_height);
        }
        y += view->vres + TRACK_Y_SPACING;
    }
}

void 
draw_mixer_toggles(struct view *view,
                   GtkWidget *widget,
                   GdkRectangle *area) {
    int i, x = 0, y = 1, y2 = 1, mute_enabled, solo_enabled, t;
    GdkGC *gc = widget->style->fg_gc[widget->state];
    GdkDrawable *drawable = view->mixerpixmap;
    GdkPixmap *mixer_mute_on, *mixer_mute_off, *mixer_solo_on, *mixer_solo_off;
    GdkBitmap *mute_on_mask, *mute_off_mask, *solo_on_mask, *solo_off_mask;
    GdkGCValues gc_vals;
    shell *shl = view->shl;

    gdk_gc_get_values(gc, &gc_vals);

    gui_get_pixmap("mixer.mute_on", &mixer_mute_on, &mute_on_mask);
    gui_get_pixmap("mixer.mute_off", &mixer_mute_off, &mute_off_mask);
    gui_get_pixmap("mixer.solo_on", &mixer_solo_on, &solo_on_mask);
    gui_get_pixmap("mixer.solo_off", &mixer_solo_off, &solo_off_mask);

    for(i = view->vadjust->value, t = 0; 
        i < shl->clip->sr->channels;
        i++, t++) {

        x = 2;

        solo_enabled = mixer_is_source_solo(shl->clip->mixer, i);
        mute_enabled = ((shl->clip->mixer->num_solos && !solo_enabled) ||
                        (mixer_is_source_mute(shl->clip->mixer, i) && 
                         !solo_enabled)) ? 1 : 0;
        
        if(view->vres >= (MIXER_TOGGLES_MUTE_HEIGHT * 2) &&
           mixer_mute_on && mixer_mute_off && 
           mixer_solo_on && mixer_solo_off) {

            gdk_gc_set_clip_origin(gc, x, y);
            gdk_gc_set_clip_mask(gc, (mute_enabled ? 
                                      mute_on_mask : mute_off_mask));

            /* Draw buttons using images. */

            gdk_draw_drawable(drawable,
                              gc,
                              mute_enabled ? mixer_mute_on : mixer_mute_off,
                              0, 0,
                              x, y,
                              MIXER_TOGGLES_MUTE_WIDTH,
                              MIXER_TOGGLES_MUTE_WIDTH);
            y += MIXER_TOGGLES_MUTE_WIDTH + 1;
            gdk_gc_set_clip_origin(gc, x, y);
            gdk_gc_set_clip_mask(gc, (solo_enabled ? 
                                      solo_on_mask : solo_off_mask));
            gdk_draw_drawable(drawable,
                            gc,
                            solo_enabled ? mixer_solo_on : mixer_solo_off,
                            0, 0,
                            x, y,
                            MIXER_TOGGLES_SOLO_WIDTH,
                            MIXER_TOGGLES_SOLO_WIDTH);


        } else {

            /* Draw buttons manually. */

            gdk_gc_set_foreground(gc, gui_get_color("toggles_mute"));
            /*
            gdk_draw_rectangle(drawable,
                               gc,
                               mute_enabled ? TRUE : FALSE,
                               x, y,
                               MIXER_TOGGLES_MUTE_WIDTH + 
                               (mute_enabled ? 1 : 0),
                               MIN((view->vres - 1) / 2,
                                   MIXER_TOGGLES_MUTE_HEIGHT));
            */
            
            gdk_draw_arc(drawable,
                         gc,
                         mute_enabled ? TRUE : FALSE,
                         x, y,
                         MIN((view->vres - 1) / 2, 
                             MIXER_TOGGLES_MUTE_HEIGHT),
                         MIN((view->vres - 1) / 2, 
                             MIXER_TOGGLES_MUTE_HEIGHT),
                         0,
                         23040);

            gdk_gc_set_foreground(gc, mute_enabled ?
                                  gui_get_color("background") :
                                  gui_get_color("info_font"));
            /*
            gdk_draw_string(drawable,
                            gc_vals.font,
                            gc,
                            x + (MIXER_TOGGLES_MUTE_WIDTH / 2) - 1,
                            y + (MIN((view->vres - 1) / 2, 
                                     MIXER_TOGGLES_MUTE_HEIGHT) - 2),
                                     mute_enabled ? "M" : "m");
            */
            
            y += MIN(view->vres / 2, MIXER_TOGGLES_MUTE_WIDTH) + 1;
            
            /* Draw solo button. */

            gdk_gc_set_foreground(gc, gui_get_color("toggles_solo"));
            /*
            gdk_draw_rectangle(drawable,
                               gc,
                               solo_enabled ? TRUE : FALSE,
                               x, y,
                               MIXER_TOGGLES_SOLO_WIDTH + 
                               (solo_enabled ? 1 : 0),
                               MIN((view->vres - 1) / 2, 
                                   MIXER_TOGGLES_SOLO_HEIGHT));
            */
            
            gdk_draw_arc(drawable,
                         gc,
                         solo_enabled ? TRUE : FALSE,
                         x, y,
                         MIN((view->vres - 1) / 2, 
                             MIXER_TOGGLES_SOLO_HEIGHT),
                         MIN((view->vres - 1) / 2, 
                             MIXER_TOGGLES_SOLO_HEIGHT),
                         0,
                         23040);
            gdk_gc_set_foreground(gc, solo_enabled ?
                                  gui_get_color("background") :
                                  gui_get_color("info_font"));
            /*
            gdk_draw_string(drawable,
                            gc_vals.font,
                            gc,
                            x + (MIXER_TOGGLES_SOLO_WIDTH / 2) - 1,
                            y + (MIN((view->vres - 1) / 2,
                                     MIXER_TOGGLES_SOLO_HEIGHT) - 2),
                            solo_enabled ? "S" : "s");
            */

        }

        y2 += view->vres + TRACK_Y_SPACING;
        y = y2;

    }
    
    gdk_gc_set_clip_origin(gc, gc_vals.clip_x_origin, gc_vals.clip_y_origin);
    gdk_gc_set_clip_mask(gc, gc_vals.clip_mask);
}


void
draw_mixer_track_dividers(struct view *view,
                          GtkWidget *widget,
                          GdkRectangle *area) {
    int i;
    shell *shl = view->shl;
    GdkGCValues gc_vals;
    GdkGC *gc = widget->style->fg_gc[widget->state];
    GdkDrawable *drawable = view->mixerpixmap;

    gdk_gc_get_values(gc, &gc_vals);

    gdk_gc_set_foreground(gc, gui_get_color("track_divider"));
    
    for(i = 1; i < shl->clip->sr->channels - view->vadjust->value; i++) {
            gdk_draw_rectangle(drawable,
                               gc,
                               TRUE,
                               0, 
                               ((i * view->vres) + ((i-1) * TRACK_Y_SPACING)) - 1,
                               view_get_widget(view, 
                                               "wavecanvas")->allocation.width,
                               TRACK_Y_SPACING);
            
    }
    gdk_gc_set_foreground(gc, &gc_vals.foreground);
}

void
draw_mixercanvas(struct view *view,
                 GtkWidget *widget,
                 GdkRectangle *area) {
    GdkGC *gc = widget->style->fg_gc[widget->state];
    GdkGCValues gc_vals;
    GdkDrawable *drawable = view->mixerpixmap;
    shell *shl = view->shl;

    if(!shl->clip->sr)
        return;

    gdk_gc_get_values(gc, &gc_vals);
    gdk_draw_rectangle(drawable,
                       widget->style->bg_gc[widget->state],
                       TRUE,
                       0, 0,
                       widget->allocation.width,
                       widget->allocation.height);
    //    gdk_gc_set_tile(gc, mixer_bg);
    //    gdk_gc_set_fill(gc, GDK_TILED);
    //gdk_gc_set_foreground(gc, &colors[COLOR_BACKGROUND]);
    /*    gdk_draw_rectangle(drawable,
                       gc,
                       TRUE,
                       0, 0,
                       widget->allocation.width,
                       widget->allocation.height);
    */
    //    gdk_gc_set_fill(gc, GDK_SOLID);

    gdk_gc_set_foreground(gc, gui_get_color("background"));

    //    draw_mixer_track_dividers(view, widget, area);
    draw_mixer_sliders(view, widget, area);
    draw_mixer_toggles(view, widget, area);

    gdk_gc_set_foreground(gc, &gc_vals.foreground);

    /* Draws black border line separating background from our stuff. */

    /*
    gdk_draw_line(drawable,
                  widget->style->black_gc,
                  0, view->vres * (shl->clip->sr->channels - 
                                  view->vadjust->value) + 1,
                  widget->allocation.width, view->vres * 
                  (shl->clip->sr->channels - view->vadjust->value) + 1);
    */
    gdk_draw_drawable(widget->window,
                    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
                    view->mixerpixmap,
                    area->x, area->y,
                    area->x, area->y,
                    area->width, area->height);
}

void
draw_info_line(GtkWidget *widget,
               int *x,
               int *y,
               const char *s) {
    struct gui_letterbox *lb;
    struct gui_letterbox_extents ink_lbe, logical_lbe;
    
    lb = gui_letterbox_new(widget, s);
    
    gui_letterbox_get_extents(lb, &ink_lbe, &logical_lbe);

    gui_letterbox_draw(lb, widget->window, 
                       *x - (ink_lbe.width >> 1),
                       *y);
    
    gui_letterbox_destroy(lb);

    *x = ink_lbe.width;
    *y = *y + logical_lbe.ascent;
}

void
draw_info_range(GtkWidget *widget,
                int *x,
                int *y,
                shell *shl,
                AFframecount start,
                AFframecount end,
                const char *separator) {
    char f1[20], f2[20];
    char s[40];
    grid_format(&shl->grid, start, f1, sizeof f1, GRID_FORMAT_LONG);
    grid_format(&shl->grid, end, f2, sizeof f2, GRID_FORMAT_LONG);
    snprintf(s, sizeof s, "%s%s%s", f1, separator, f2);
    draw_info_line(widget, x, y, s);
}

#define DRAW_INDICATOR(cond, name) \
    gui_get_pixmap(cond ? name "_on" : name "_off", &pixmap, &mask); \
    gdk_gc_set_clip_origin(widget->style->fg_gc[widget->state], x_dst, y_dst); \
    gdk_gc_set_clip_mask(widget->style->fg_gc[widget->state], mask); \
    gdk_draw_drawable(widget->window, \
                      widget->style->fg_gc[widget->state], \
                      pixmap, \
                      0, \
                      0, \
                      x_dst, \
                      y_dst, \
                      -1, -1); 

void
draw_infocanvas(struct view *view,
                GtkWidget *widget,
                GdkRectangle *area) {
    int i, x_dst, y_dst, y_old, x_src;
    char s[40];
    shell *shl = view->shl;
    GdkPixmap *pixmap;
    GdkBitmap *mask;
    GdkGCValues gc_values;

    gdk_gc_get_values(widget->style->fg_gc[widget->state],
                      &gc_values);

    gdk_draw_rectangle(widget->window,
                       widget->style->bg_gc[widget->state],
                       TRUE,
                       0, 0,
                       widget->allocation.width,
                       widget->allocation.height);
    grid_format(&shl->grid, 
                (shl->player->player_running ? 
                 shl->player->state->audio_pos : 
                 MAX(0, (view->last_mouse_x * view->hres) + 
                     view->hadjust->value)), s, 40, 
                GRID_FORMAT_LONG);
    y_dst = 4;
    x_dst = (widget->allocation.width / 2) -
        (strlen(s) * (DASH_FONT_BIG_WIDTH >> 1));

    for(i = 0; s[i]; i++) {

        switch(s[i]) {
        case ':':
            x_src = 10;
            break;
        case '.':
            x_src = 11;
            break;
        case ' ':
            x_src = 12;
            break;
        default:
            x_src = s[i] - '0';
            break;
        }

        x_src *= DASH_FONT_BIG_WIDTH;
        gui_get_pixmap("dash.font_big", &pixmap, &mask);

        gdk_gc_set_clip_origin(widget->style->fg_gc[widget->state], 
                               x_dst - x_src, y_dst);
        gdk_gc_set_clip_mask(widget->style->fg_gc[widget->state], mask); 

        gdk_draw_rectangle(widget->window,
                           widget->style->fg_gc[widget->state],
                           TRUE,
                           x_dst, y_dst,
                           DASH_FONT_BIG_WIDTH,
                           DASH_FONT_BIG_HEIGHT);
        
        x_dst += DASH_FONT_BIG_WIDTH;

    }

    gdk_gc_set_clip_mask(widget->style->fg_gc[widget->state], 
                         gc_values.clip_mask); 
    gdk_gc_set_clip_origin(widget->style->fg_gc[widget->state], 
                           gc_values.clip_x_origin, 
                           gc_values.clip_y_origin);

    /* Selection. */
    
    x_dst = (widget->allocation.width >> 1);
    y_dst += DASH_FONT_BIG_HEIGHT+14;
    draw_info_range(widget, &x_dst, &y_dst, shl, 
                    shl->select_start,
                    (view->draw_lengths ? 
                     shl->select_end - shl->select_start :
                     shl->select_end),
                    (view->draw_lengths ?
                     "  #  " : " ... "));

    x_dst = (widget->allocation.width >> 1);
    y_dst += 2;
    draw_info_range(widget, &x_dst, &y_dst, shl,
                    shl->loop_start,
                    (view->draw_lengths ? 
                     shl->loop_end - shl->loop_start :
                     shl->loop_end),
                    (view->draw_lengths ?
                     "  #  " : " ... "));

    /* Transient info. */

    if(view->transient_info) {
        x_dst = (widget->allocation.width >> 1);
        y_dst += 2;
        y_old = y_dst;
        if(view->transient_level != MSG_INFO) 
            x_dst += 8;
        draw_info_line(widget, &x_dst, &y_dst, view->transient_info);
        if(view->transient_level != MSG_INFO) {
            x_dst = (widget->allocation.width >> 1) - (x_dst >> 1) - 10;
            y_dst = y_dst - ((y_dst - y_old) * 2);
            gui_get_pixmap(view->transient_level == MSG_WARN ?
                           "icons.small.warning" :
                           "icons.small.error",
                           &pixmap, 
                           NULL);
            gdk_draw_drawable(widget->window,
                              widget->style->fg_gc[widget->state],
                              pixmap,
                              0, 0,
                              x_dst, y_dst,
                              -1, -1);
        }
    }
    
    /* Indicators. */

    x_dst = widget->allocation.width - DASH_INDICATOR_WIDTH;
    y_dst = 0;

    DRAW_INDICATOR(shl->player->player_running && 
                   !shl->player->state->record_mode, "dash.play");
    y_dst += DASH_INDICATOR_HEIGHT;
    DRAW_INDICATOR(shl->player->player_running && 
                   shl->player->state->record_mode, "dash.record");
    y_dst += DASH_INDICATOR_HEIGHT;
    DRAW_INDICATOR(LOOP_IS_ACTIVE(shl), "dash.loop");
    y_dst += DASH_INDICATOR_HEIGHT;
    DRAW_INDICATOR(pref_get_as_int("record_replace"), "dash.replace");
    y_dst += DASH_INDICATOR_HEIGHT;    
    
    gdk_gc_set_clip_mask(widget->style->fg_gc[widget->state], 
                         gc_values.clip_mask); 
    gdk_gc_set_clip_origin(widget->style->fg_gc[widget->state], 
                           gc_values.clip_x_origin, 
                           gc_values.clip_y_origin);

}

void
draw_wavecanvas(struct view *view,
                GtkWidget *widget,
                GdkRectangle *area) {
    shell *shl = view->shl;

    if(!shl->clip->sr)
        return;

    /* Draw unto pixmap. */

    //    DEBUG("area->x: %d, area->y: %d, area->width: %d, area->height: %d\n",
    //          area->x, area->y, area->width, area->height);
    
    if(view->wavepixmap) {
        rwlock_rlock(&shl->clip->sr->rwl);
        draw_hooks_dispatch(view->wavecanvas_hooks,
                            widget,
                            view->wavepixmap,
                            widget->style->fg_gc[widget->state],
                            area,
                            view);
        rwlock_runlock(&shl->clip->sr->rwl);
    }
    
    /* Show the pixmap. */
    
    if(view->wavepixmap) 
        gdk_draw_drawable(widget->window,
                          widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
                          view->wavepixmap,
                          area->x, area->y,
                          area->x, area->y,
                          area->width, area->height);

    gtk_widget_queue_draw(view_get_widget(view, "infocanvas"));

}
