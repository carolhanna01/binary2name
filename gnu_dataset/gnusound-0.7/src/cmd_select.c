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
#include <combo_box.h>
#include "cmd.h"
#include "clip.h"

static struct cmd_value *
cmd_select_selection_to_4_beats(const char *name,
                                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    GtkWidget *measurement, *bpm;

    if(shl->select_end - shl->select_start < 4)
        return cmd_new_error_val("The selection is too small.");
 
    measurement = view_get_widget(shl->view, "grid_measurement");
    bpm = view_get_widget(shl->view, "grid_bpm");
    
    combo_box_set_active(COMBO_BOX(measurement), GRID_BEATS);
    grid_bpm_from_frames_set(&shl->grid,
                             (shl->select_end - shl->select_start) / 4);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(bpm), shl->grid.bpm);

    view_sync_display(shl->view);
    view_redraw(shl->view);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_move_left(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    AFframecount ss;
    struct marker *m = NULL;
    struct cmd *cmd;
    struct cmd_value *r;

    ss = (!count ? MAX(0, offset - MAX(1, shl->view->hres)) : offset);
    if(shl->snap_to_grid) 
        ss -= ss % shl->grid.gap;
    if(shl->snap_to_cuepoints) 
        m = marker_list_array_find_previous(shl->clip->markers, 
                                            map, ss, MARKER_TEXT);
    if(m)
        ss = m->frame_offset;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(ss),
                  cmd_new_long_val(0));
    if(cmd_do_or_fail(cmd, "Cannot move left (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_move_right(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long count = shl->select_end - shl->select_start;
    AFframecount ss;
    struct marker *m = NULL;
    struct cmd *cmd;
    struct cmd_value *r;

    ss = (!count ? 
          shl->select_end + MAX(1, shl->view->hres) : 
          shl->select_end);

    if(shl->snap_to_grid) {
        ss -= ss % shl->grid.gap;
        ss += shl->grid.gap;
    }
    if(shl->snap_to_cuepoints) 
        m = marker_list_array_find_next(shl->clip->markers, map, ss, 
                                        MARKER_TEXT);
    if(m)
        ss = m->frame_offset;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(ss),
                  cmd_new_long_val(0));
    if(cmd_do_or_fail(cmd, "Cannot insert time (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_move_up(const char *name,
                   struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    long offset = shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    int first_selected_track = 1, i;
    for(i = 0; i < shl->clip->sr->channels; i++) {
        if((1 << i) & shl->select_channel_map) {
            first_selected_track = i;
            break;
        }
    }
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1 << MAX(first_selected_track - 1, 0)),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(0));
    if(cmd_do_or_fail(cmd, "Cannot move up (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_move_down(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    long offset = shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    int last_selected_track = 0, i;
    for(i = 0; i < shl->clip->sr->channels; i++)
        if((1 << i) & shl->select_channel_map)
            last_selected_track = i;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1 << MIN(last_selected_track + 1, 
                                           shl->clip->sr->channels - 1)),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(0));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_grow_left(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    AFframecount ss, se;
    struct marker *m = NULL;
    struct cmd *cmd;
    struct cmd_value *r;

    ss = MAX(0, shl->select_start - MAX(1, shl->view->hres));
    se = shl->select_end;
    
    if(shl->snap_to_grid) 
        ss = MAX(0, ((ss + shl->grid.gap) - 
                     (ss % shl->grid.gap)) - shl->grid.gap);
    if(shl->snap_to_cuepoints) 
        m = marker_list_array_find_previous(shl->clip->markers, 
                                            map, ss, MARKER_TEXT);
    if(m)
        ss = m->frame_offset;

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(ss),
                  cmd_new_long_val(se - ss));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_grow_right(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    AFframecount ss, se;
    struct marker *m = NULL;
    struct cmd *cmd;
    struct cmd_value *r;

    ss = shl->select_start;
    se =shl->select_end + MAX(1, shl->view->hres);
    
    if(shl->snap_to_grid)
        se = (se - (se % shl->grid.gap)) + shl->grid.gap;
    if(shl->snap_to_cuepoints) 
        m = marker_list_array_find_next(shl->clip->markers, map, se,
                                        MARKER_TEXT);
    if(m)
        se = m->frame_offset;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(ss),
                  cmd_new_long_val(se - ss));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_grow_up(const char *name,
                   struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    int next_selected_track = shl->clip->sr->channels - 1, i;
    for(i = 0; i < shl->clip->sr->channels; i++) {
        if((1 << i) & map) {
            next_selected_track = MAX(0, i - 1);
            break;
        }
    }
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val((1 << next_selected_track) | map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_grow_down(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    int next_selected_track = 0, i;
    for(i = 0; i < shl->clip->sr->channels; i++)
        if((1 << i) & shl->select_channel_map)
            next_selected_track = i + 1;
    
    if(next_selected_track >= shl->clip->sr->channels)
        return cmd_new_void_val();
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val((1 << next_selected_track) | map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_nudge_left(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(MAX(0, offset - MAX(1, shl->view->hres))),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_nudge_right(const char *name,
                       struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset + MAX(1, shl->view->hres)),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_jump_left(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(MAX(0, offset - count)),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_jump_right(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(MAX(0, offset + count)),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot move down (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_all(const char *name,
               struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(0),
                  cmd_new_long_val(snd_frame_count(shl->clip->sr, map)));
    if(cmd_do_or_fail(cmd, "Cannot select all (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_selection_to_loop(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    
    shl->loop_start = shl->select_start;
    shl->loop_end = shl->select_end;

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_select_loop_to_selection(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->loop_start),
                  cmd_new_long_val(shl->loop_end - shl->loop_start));
    if(cmd_do_or_fail(cmd, "Cannot select all (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}


int
cmd_select_init() {
    int i; 
    struct cmd_signature f[] = {

        { "selection-to-4-beats", 
          "Calculates and sets the number of beats per minute by assuming "
          "the selection covers 4 beats.",
          cmd_select_selection_to_4_beats, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "move-left", "Sets the selection.",
          cmd_select_move_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "move-right", "Sets the selection.",
          cmd_select_move_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "move-up", "Sets the selection.",
          cmd_select_move_up, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "move-down", "Sets the selection.",
          cmd_select_move_down, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


        { "grow-left", "Grows the selection to the left.",
          cmd_select_grow_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "grow-right", "Grows the selection to the right.",
          cmd_select_grow_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "grow-up", "Grows the selection up.",
          cmd_select_grow_up, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "grow-down", "Grows the selection down.",
          cmd_select_grow_down, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


        { "nudge-left", 
          "Nudges selection to the left without changing its size.",
          cmd_select_nudge_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "nudge-right", 
          "Nudges selection to the right without changing its size.",
          cmd_select_nudge_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


        { "jump-left", 
          "Subtracts size of selected region from selection start and end.",
          cmd_select_jump_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "jump-right", 
          "Adds size of selected region to selection start and end.",
          cmd_select_jump_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


        { "select-all", 
          "Selects entire sound object.",
          cmd_select_all, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


        { "selection-to-loop", 
          "Sets loop to selection.",
          cmd_select_selection_to_loop, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "loop-to-selection", 
          "Sets selection to loop.",
          cmd_select_loop_to_selection, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
