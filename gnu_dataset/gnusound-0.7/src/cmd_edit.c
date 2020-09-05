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

#include <config.h>
#include "cmd.h"
#include "clipboard.h"
#include "resample.h"

static struct cmd_value *
cmd_edit_undo(const char *name,
              struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd_value *r = history_go_back(shl->history);
    view_sync_display(shl->view);
    view_redraw(shl->view);
    return r;
}

static struct cmd_value *
cmd_edit_redo(const char *name,
              struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd_value *r = history_go_forward(shl->history);
    view_sync_display(shl->view);
    view_redraw(shl->view);
    return r;
}

static struct cmd_value *
cmd_edit_copy(const char *name,
              struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    snd *copy;

    copy = snd_copy(shl->clip->sr, shl->select_channel_map,
                    shl->select_start, 
                    shl->select_end - shl->select_start);
    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Cannot copy selection"),
                                 ERROR(shl->clip->sr));
    
    clipboard_replace_snd(copy);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_cut(const char *name,
             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    snd *del_sr, *new_clipboard;
    struct cmd_value *r;
    struct cmd *cmd;
    const char *s;

    if(shell_is_clipboard(shl))
        return cmd_new_error_val("Cannot cut on clipboard");

    if(shl->select_end - shl->select_start < 1) 
        return cmd_new_error_val("Cannot cut empty selection");

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_DELETE)))
        return cmd_new_error_val("Cannot cut because region is locked "
                                 "(%s)", s);
    
    /* Cut the selection. */
    
    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end -
                                   shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot cut (%s)", &r))
        return r;
    del_sr = cmd_sndp(r);
    cmd_destroy_value(r);

    /* Clone the segment. */

    new_clipboard = snd_clone(del_sr, 
                              CLONE_STRUCTURE |
                              CLONE_TRACK_STRUCTURE | 
                              CLONE_TRACK_DATA);    
    if(error_thrown(ERROR(del_sr))) 
        return cmd_error_cascade(cmd_new_error_val("Cannot cut"), 
                              ERROR(del_sr));
    
    /* Place the segment onto the clipboard. */

    clipboard_replace_snd(new_clipboard);

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(0));

    if(cmd_do_or_fail(cmd, "Cannot cut (%s)", &r))
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_paste(const char *name,
               struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd_value *r;
    struct cmd *cmd;
    snd *copy;
    const char *s;

    if(shell_is_clipboard(shl)) 
        return cmd_new_error_val("Cannot paste in clipboard");

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_INSERT)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    /* Get clipboard. */
    
    copy = clipboard_copy_snd(shl->clip->sr->sample_type,
                              shl->clip->sr->rate);
    if(!copy)
        return cmd_new_error_val("Cannot get clipboard");

    /* Paste it. */
    
    cmd = CMD_NEW("paste-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_sndp_val(copy),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot paste (%s)", &r)) {
        snd_destroy(copy);
        return r;
    }
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_paste_over(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    snd *copy;
    long offset;
    const char *s;

    if(shell_is_clipboard(shl))
        return cmd_new_error_val("Cannot paste to clipboard");
    
    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    copy = clipboard_copy_snd(shl->clip->sr->sample_type,
                              shl->clip->sr->rate);
    if(!copy)
        return cmd_new_error_val("Unable to get clipboard");
    
    /* Limit clipboard to selection length. */
    
    offset = MIN(snd_frame_count(copy, MAP_ALL), 
                 shl->select_end - shl->select_start);
    snd_destroy(snd_delete(copy,
                           MAP_ALL,
                           offset,
                           snd_frame_count(copy, MAP_ALL) - offset));
    
    /* Paste it. */
    
    cmd = CMD_NEW("paste-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_sndp_val(copy),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(snd_frame_count(copy, MAP_ALL)),
                  cmd_new_int_val(0));
    if(cmd_do_or_fail(cmd, "Cannot paste (%s)", &r)) {
        snd_destroy(copy);
        return r;
    }
    cmd_destroy_value(r);

    return cmd_new_void_val();

}

/* 
 * FIXME: This one is more than a little ugly. The underlying issue
 * seems to be that we have no practical way of pulling the clipboard
 * through a resampling filter and having the result appear in the
 * shell. We don't have a way to "promise" some rendition of data.
 */

static struct cmd_value *
cmd_edit_paste_fit(const char *name,
                   struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    snd *copy;
    long offset;
    const char *s;
    int t;
    struct filter_stats stats[MAX_TRACKS] = { 0 };
    AFframecount max_written = 0;
    track_map_t target_map, source_map;

    if(shell_is_clipboard(shl))
        return cmd_new_error_val("Cannot paste to clipboard");
    
    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    /* Delete the selection. */
    
    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot paste fit (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    /* Insert the clipboard. */

    copy = clipboard_copy_snd(shl->clip->sr->sample_type,
                              shl->clip->sr->rate);
    if(!copy)
        return cmd_new_error_val("Unable to get clipboard");

    target_map = shl->select_channel_map;
    source_map = snd_map_avail(copy);
    snd_insert(shl->clip->sr,
               &target_map,
               copy,
               &source_map,
               shl->select_start);

    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Cannot paste fit"),
                                 ERROR(shl->clip->sr));
    
    if(source_map == 0)
        return cmd_new_error_val("Clipboard is empty");

    constraints_push(shl->constraints,
                     "Paste Fit",
                     region_new(shl->select_channel_map, 
                                shl->select_start, 
                                REGION_MATCH_ANYTHING),
                     (CONSTRAIN_POSITION | CONSTRAIN_CONTENTS));

    rwlock_rlock(&shl->clip->sr->rwl);

    /* Resample the pasted clipboard so it fits the selection. */

    for(t = 0; t < snd_track_count(shl->clip->sr); t++) {
        if((1 << t) & shl->select_channel_map) {
            resample_track(shl,
                           t,
                           shl->select_start,
                           (shl->select_start + 
                            snd_frame_count(copy, source_map)),
                           shl->select_end - shl->select_start,
                           RESAMPLE_BEST_ALGORITHM,
                           RESAMPLE_GUARANTEE_NEW_FRAME_COUNT,
                           &stats[t]);
            if(stats[t].produced > max_written)
                max_written = stats[t].produced;
            DEBUG("track %d: consumed: %ld, produced: %ld\n",
                  t, stats[t].consumed, stats[t].produced);
        }
    }

    rwlock_runlock(&shl->clip->sr->rwl);

    snd_destroy(copy);

    history_remember(shl->history,
                     CMD_NEW("delete-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(shl->select_channel_map),
                             cmd_new_long_val(shl->select_start),
                             cmd_new_long_val(shl->select_end -
                                              shl->select_start)));
    

    constraints_pop(shl->constraints);

    return cmd_new_void_val();

}

static struct cmd_value *
cmd_edit_clear(const char *name,
               struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_DELETE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot clear (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(0));
    if(cmd_do_or_fail(cmd, "Cannot clear (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_erase(const char *name,
               struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    cmd = CMD_NEW("erase-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot erase (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_crop(const char *name,
              struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    long count = shl->select_end - shl->select_start;
    const char *s;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_end,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_DELETE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        0,
                                        shl->select_start),
                             CONSTRAINTS_OPER_DELETE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    /* Delete tail. */

    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_end),
                  cmd_new_long_val(MAX(0, 
                                       snd_frame_count(shl->clip->sr,
                                                       MAP_ALL) - 
                                       shl->select_end)));
    if(cmd_do_or_fail(cmd, "Cannot crop (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    /* Delete head. */
    
    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(0),
                  cmd_new_long_val(shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot crop (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    /* Adjust selection. */

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(0),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot crop (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_insert_tracks(const char *name,
                       struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_INSERT)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    cmd = CMD_NEW("insert-tracks-clip",
                  cmd_new_shellp_val(shl),
                  cmd_new_clipp_val(NULL),
                  cmd_new_int_val(shl->select_channel_map));
    if(cmd_do_or_fail(cmd, "Cannot insert tracks (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_delete_tracks(const char *name,
                       struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_DELETE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    cmd = CMD_NEW("delete-tracks-clip",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map));
    if(cmd_do_or_fail(cmd, "Cannot delete tracks (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_push_left(const char *name,
                   struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    AFframecount delta = -MAX(1, shl->view->hres);
    struct marker *m = NULL;
    struct cmd *cmd;
    struct cmd_value *r;
    
    if(shl->snap_to_grid) {
        if(shl->select_start == 0)
            delta = -shell_get_grid(shl)->gap;
        else
            delta = -(shl->select_start - 
                      grid_find_near_left(shell_get_grid(shl), 
                                          shl->select_start - 1));
    }

    if(shl->snap_to_cuepoints) 
        m = marker_list_array_find_previous(shl->clip->markers, 
                                            shl->select_channel_map, 
                                            shl->select_start, 
                                            MARKER_TEXT);
    if(m)
        delta = -(shl->select_start - m->frame_offset);
    
    cmd = CMD_NEW("shove-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start),
                  cmd_new_long_val(delta));
    if(cmd_do_or_fail(cmd, "Cannot shove (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_push_right(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    AFframecount delta = MAX(1, shl->view->hres);
    struct marker *m = NULL;
    struct cmd *cmd;
    struct cmd_value *r;
    
    if(shl->snap_to_grid) 
        delta = (grid_find_near_right(shell_get_grid(shl), shl->select_start) -
                 shl->select_start);
    if(shl->snap_to_cuepoints) 
        m = marker_list_array_find_next(shl->clip->markers, 
                                        shl->select_channel_map, 
                                        shl->select_start, 
                                        MARKER_TEXT);
    if(m)
        delta = m->frame_offset - shl->select_start;
    
    cmd = CMD_NEW("shove-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start),
                  cmd_new_long_val(delta));
    if(cmd_do_or_fail(cmd, "Cannot shove (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_shove_left(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("shove-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start),
                  cmd_new_long_val(-(shl->select_end - shl->select_start)));
    if(cmd_do_or_fail(cmd, "Cannot shove (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_shove_right(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    
    cmd = CMD_NEW("shove-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot shove (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_edit_show_clipboard(const char *name,
                        struct cmd_argv *args) {

    clipboard_show();

    return cmd_new_void_val();
}

int
cmd_edit_init() {
    int i; 
    struct cmd_signature f[] = {
        
        { "undo", "Undoes the previous action.",
          cmd_edit_undo, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "redo", "Redoes the previously undone action.",
          cmd_edit_redo, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },

        { "cut", 
          "Deletes selection and places it onto the clipboard.",
          cmd_edit_cut, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },

        { "copy",
          "Copies selection onto clipboard.",
          cmd_edit_copy, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },

        { "paste", 
          "Replaces selection by clipboard.",
          cmd_edit_paste, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },

        { "paste-over", 
          "Replaces selection by clipboard, limiting the replacement "
          "to the selection.",
          cmd_edit_paste_over, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "paste-fit", 
          "Replaces selection by clipboard, fitting the clipboard "
          "to the selection.",
          cmd_edit_paste_fit, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "clear",
          "Deletes the selection.",
          cmd_edit_clear, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "erase",
          "Fills the selection with silence.",
          cmd_edit_erase, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "crop",
          "Deletes everything except the selection.",
          cmd_edit_crop, CMD_VOID_T, cmd_new_paramdecl(1, CMD_shellp_T) },

        { "insert-tracks", "Inserts tracks.",
          cmd_edit_insert_tracks, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "delete-tracks", "Deletes tracks.",
          cmd_edit_delete_tracks, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "push-left", 
          "Pushes the region covered by the selection to the left by "
          "the zoom amount.",
          cmd_edit_push_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "push-right", 
          "Pushes the region covered by the selection to the right by "
          "the zoom amount.",
          cmd_edit_push_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "shove-left", 
          "Shoves the region covered by the selection to the left by "
          "the size of the selection.",
          cmd_edit_shove_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "shove-right", 
          "Shoves the region covered by the selection to the right by "
          "the size of the selection.",
          cmd_edit_shove_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "show-clipboard", "Shows clipboard.",
          cmd_edit_show_clipboard, CMD_VOID_T, 
          cmd_new_paramdecl(0) },

    };
    
    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
