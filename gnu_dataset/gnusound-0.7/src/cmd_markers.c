/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2004-2005  Pascal Haakmat <a.haakmat@chello.nl>
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
#include "marker.h"
#include "arbiter.h"

#undef MARKER_CHANGES_IN_HISTORY 

static struct cmd_value *
cmd_markers_insert_markers(shell *shl,
                           enum marker_type type) {
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct marker_list_array *mla;
    struct cmd *cmd;
    struct cmd_value *r;
    int err;
    const char *label;

    mla = marker_list_array_new(shl->clip->markers->len);
    if(!mla) 
        return cmd_new_error_val("Cannot create marker list array");

    label = shell_get_next_label(shl);
    
    marker_list_array_insert_marker(mla, 1, 0, type, 0, label);
    if(error_thrown(ERROR(mla))) {
        r = cmd_error_cascade(cmd_new_error_val("Unable to insert "
                                                "start markers"),
                              ERROR(mla));
        marker_list_array_destroy(mla);
        return r;
    }
    
    if(count > 0) {
        marker_list_array_insert_marker(mla, 1, count, type, 0, label);
        if(error_thrown(ERROR(mla))) {
            r = cmd_error_cascade(cmd_new_error_val("Unable to insert "
                                                    "end markers"),
                                  ERROR(mla));
            marker_list_array_destroy(mla);
            return r;
        }
    }
    
    cmd = CMD_NEW("insert-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_marker_list_arrayp_val_with_dtor(mla,
                                                           cmd_marker_list_arrayp_dtor),
                  cmd_new_int_val(type));
    if(cmd_do_or_fail(cmd, "Cannot insert markers (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}


static struct cmd_value *
cmd_markers_delete_markers(shell *shl,
                           enum marker_type type) {
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("delete-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_int_val(type));
    if(cmd_do_or_fail(cmd, "Cannot delete markers (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}


static struct cmd_value *
cmd_markers_insert_cuepoints(const char *name,
                             struct cmd_argv *args) {
    return cmd_markers_insert_markers(cmd_shellp(args->argv[0]),
                                      MARKER_TEXT);
}


static struct cmd_value *
cmd_markers_delete_cuepoints(const char *name,
                             struct cmd_argv *args) {
    return cmd_markers_delete_markers(cmd_shellp(args->argv[0]),
                                      MARKER_TEXT);
}

static struct cmd_value *
cmd_markers_insert_envelopes(const char *name,
                             struct cmd_argv *args) {
    return cmd_markers_insert_markers(cmd_shellp(args->argv[0]),
                                      MARKER_SLOPE);
}


static struct cmd_value *
cmd_markers_delete_envelopes(const char *name,
                             struct cmd_argv *args) {
    return cmd_markers_delete_markers(cmd_shellp(args->argv[0]),
                                      MARKER_SLOPE | MARKER_SLOPE_AUX);
}

static struct cmd_value *
cmd_markers_copy_envelopes(const char *name,
                           struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("copy-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_marker_list_arrayp_val(arbiter_get_envelopes_clipboard()),
                  cmd_new_int_val(MARKER_SLOPE | MARKER_SLOPE_AUX));
    if(cmd_do_or_fail(cmd, "Cannot copy envelopes (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_markers_paste_envelopes(const char *name,
                            struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("paste-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_marker_list_arrayp_val(arbiter_get_envelopes_clipboard()),
                  cmd_new_int_val(MARKER_SLOPE | MARKER_SLOPE_AUX));
    if(cmd_do_or_fail(cmd, "Cannot paste envelopes (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_markers_copy_cuepoints(const char *name,
                           struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("copy-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_marker_list_arrayp_val(arbiter_get_cuepoints_clipboard()),
                  cmd_new_int_val(MARKER_TEXT));
    if(cmd_do_or_fail(cmd, "Cannot copy cuepoints (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_markers_paste_cuepoints(const char *name,
                            struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("paste-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_marker_list_arrayp_val(arbiter_get_cuepoints_clipboard()),
                  cmd_new_int_val(MARKER_TEXT));
    if(cmd_do_or_fail(cmd, "Cannot paste cuepoints (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_markers_insert_time(const char *name,
                        struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("move-markers-left",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_int_val(MARKER_TEXT | 
                                  MARKER_SLOPE | 
                                  MARKER_SLOPE_AUX));
    if(cmd_do_or_fail(cmd, "Cannot insert time (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

#ifdef MARKER_CHANGES_IN_HISTORY
    history_remember(shl->history,
                     CMD_NEW("move-markers-right",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count),
                             cmd_new_int_val(MARKER_TEXT | 
                                             MARKER_SLOPE | 
                                             MARKER_SLOPE_AUX)));
#endif

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_markers_delete_time(const char *name,
                        struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = shl->select_channel_map;
    long offset = shl->select_start;
    long count = shl->select_end - shl->select_start;
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("move-markers-right",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_int_val(MARKER_TEXT | 
                                  MARKER_SLOPE | 
                                  MARKER_SLOPE_AUX));
    if(cmd_do_or_fail(cmd, "Cannot delete time (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
#ifdef MARKER_CHANGES_IN_HISTORY
    history_remember(shl->history,
                     CMD_NEW("move-markers-left",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count),
                             cmd_new_int_val(MARKER_TEXT | 
                                             MARKER_SLOPE |
                                             MARKER_SLOPE_AUX)));
#endif
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_markers_invert_envelopes(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int t;
    for(t = 0; t < shl->clip->markers->len; t++) 
        if((1 << t) & shl->select_channel_map) 
            marker_list_invert(shl->clip->markers->lists[t],
                               shl->select_start,
                               shl->select_end - shl->select_start,
                               MARKER_SLOPE | MARKER_SLOPE_AUX);
    
    return cmd_new_void_val();
}

int
cmd_markers_init() {
    int i; 
    struct cmd_signature f[] = {
        
        { "insert-cuepoints", "Inserts cuepoint(s) at the selection edges.",
          cmd_markers_insert_cuepoints, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "delete-cuepoints", "Deletes cuepoints in the selection.",
          cmd_markers_delete_cuepoints, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "copy-cuepoints", 
          "Places cuepoints in selected region onto cuepoint clipboard.",
          cmd_markers_copy_cuepoints, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "paste-cuepoints", "Pastes cuepoints onto selection.",
          cmd_markers_paste_cuepoints, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "insert-envelopes", "Inserts envelope(s) at selection edges.",
          cmd_markers_insert_envelopes, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "delete-envelopes", "Removes envelopes from selection.",
          cmd_markers_delete_envelopes, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "copy-envelopes", 
          "Places envelopes in selected region onto envelope clipboard.",
          cmd_markers_copy_envelopes, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "paste-envelopes", "Pastes envelopes onto selection.",
          cmd_markers_paste_envelopes, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "invert-envelope", 
          "Inverts the envelopes in selected region.",
          cmd_markers_invert_envelopes, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "insert-time", 
          "Moves envelopes and cuepoints to the right.",
          cmd_markers_insert_time, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "delete-time", 
          "Moves envelopes and cuepoints to the left.",
          cmd_markers_delete_time, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "invert-envelopes", 
          "Inverts envelopes.",
          cmd_markers_invert_envelopes, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },
    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
