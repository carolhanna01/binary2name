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

#include <config.h>
#include "cmd.h"


/* Called when the SND changes on the clip we're viewing. */

static struct cmd_value *
view_clip_snd_changed(const char *name,
                      struct cmd_argv *args) {
    struct clip *clip = cmd_clipp(args->argv[0]);
    struct view *view = cmd_voidp(args->argv[1]);
    
    view_reset_track_draw_hooks(view);
    view_reset_status(view);
    view_redraw(view);
    gtk_widget_queue_draw(view_get_widget(view, "mixercanvas"));

    return cmd_new_void_val();
}

/* Called when a track has been inserted into the clip we're viewing. */

static struct cmd_value *
view_track_inserted(const char *name,
                    struct cmd_argv *args) {
    struct clip *clip = cmd_clipp(args->argv[0]);
    int track = cmd_int(args->argv[1]);
    struct view *view = cmd_voidp(args->argv[2]);
    int i;

    DEBUG("got track-inserted message\n");

    if(view->track_hooks[MAX_TRACKS - 1])
        draw_hooks_destroy(view->track_hooks[MAX_TRACKS - 1]);

    for(i = MAX_TRACKS - 1; i > track; i--)
        view->track_hooks[i] = view->track_hooks[i-1];

    view->track_hooks[track] = draw_hooks_new();

    view_set_default_track_draw_hooks(view, track);

    DEBUG("connected drawing hook for track %d\n", track);

    return cmd_new_void_val();
}

/* Called when a track has been deleted from the clip we're viewing. */

static struct cmd_value *
view_track_deleted(const char *name,
                    struct cmd_argv *args) {
    struct clip *clip = cmd_clipp(args->argv[0]);
    int track = cmd_int(args->argv[1]);
    struct view *view = cmd_voidp(args->argv[2]);
    int i;

    draw_hooks_destroy(view->track_hooks[track]);

    for(i = track; i < MAX_TRACKS - 1; i++) 
        view->track_hooks[i] = view->track_hooks[i+1];

    view->track_hooks[MAX_TRACKS - 1] = draw_hooks_new();

    return cmd_new_void_val();
}

/* Called when a track has been moved in the clip we're viewing. */

static struct cmd_value *
view_track_moved(const char *name,
                 struct cmd_argv *args) {
    struct clip *clip = cmd_clipp(args->argv[0]);
    int from = cmd_int(args->argv[1]);
    int to = cmd_int(args->argv[2]);
    int what = cmd_int(args->argv[3]);
    struct view *view = cmd_voidp(args->argv[4]);
    struct draw_hooks *tmp;
    
    tmp = view->track_hooks[to];
    view->track_hooks[to] = view->track_hooks[from];
    view->track_hooks[from] = tmp;

    return cmd_new_void_val();
}


static struct cmd_value *
cmd_view_set_zoom(const char *name,
                  struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    float hres = cmd_double(args->argv[1]);
    view_set_hres(shl->view, hres);
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_smaller(const char *name,
                 struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int delta = 16;
    if(shl->view->vres < 64)
        delta = 8;
    view_set_vres(shl->view, shl->view->vres - delta);
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_bigger(const char *name,
                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int delta = 16;
    if(shl->view->vres < 64)
        delta = 8;
    view_set_vres(shl->view, shl->view->vres + delta);
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_zoom_in(const char *name,
                 struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    view_zoom_in(shl->view);
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_zoom_out(const char *name,
                  struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    view_zoom_out(shl->view);
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_fit_selection(const char *name,
                         struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    AFframecount size = shl->select_end - shl->select_start;
    int canvas_size = view_get_widget(shl->view, 
                                      "wavecanvas")->allocation.width;
    float hres;

    for(hres = HRES_MIN; 
        hres <= HRES_MAX && (size / hres) > canvas_size; 
        hres += (hres < PEAK_HRES ? hres : PEAK_HRES));
    view_set_hres(shl->view, hres);
    view_center_hrange(shl->view, shl->select_start, shl->select_end);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_center_selection_start(const char *name,
                                  struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    view_center_hrange(shl->view, shl->select_start, shl->select_start);
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_view_center_selection_end(const char *name,
                                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    view_center_hrange(shl->view, shl->select_end, shl->select_end);
    return cmd_new_void_val();
}


int
cmd_view_init() {
    int i; 
    struct cmd_signature f[] = {

        { "set-zoom", 
          "Sets horizontal zoom.",
          cmd_view_set_zoom, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_double_T) },

        { "zoom-in", 
          "Sets horizontal zoom.",
          cmd_view_zoom_in, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "zoom-out", 
          "Sets horizontal zoom.",
          cmd_view_zoom_out, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "bigger", 
          "Sets horizontal zoom.",
          cmd_view_bigger, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "smaller", 
          "Sets horizontal zoom.",
          cmd_view_smaller, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "fit-selection", 
          "Fits selection to view.",
          cmd_view_fit_selection, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


        { "center-selection-start", 
          "Centers view on selection start.",
          cmd_view_center_selection_start, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "center-selection-end", 
          "Centers view on selection end.",
          cmd_view_center_selection_end, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        /* Clip callbacks. */

        { "view-track-inserted", "Insert event.",
          view_track_inserted, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_clipp_T, CMD_int_T, CMD_voidp_T) },
        
        { "view-track-deleted", "Delete event.",
          view_track_deleted, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_clipp_T, CMD_int_T, CMD_voidp_T) },
        
        { "view-track-moved", "Move event.",
          view_track_moved, CMD_VOID_T, 
          cmd_new_paramdecl(5, CMD_clipp_T, CMD_int_T, CMD_int_T, CMD_int_T, CMD_voidp_T) },

        { "view-clip-snd-changed", "SND change event.",
          view_clip_snd_changed, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_clipp_T, CMD_voidp_T) },

    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
