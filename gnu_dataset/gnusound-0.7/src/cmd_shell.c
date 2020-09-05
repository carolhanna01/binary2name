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
#include <dlfcn.h>
#include "cmd.h"
#include "arbiter.h"
#include "pref.h"
#include "module.h"
#include "resample.h"
#include "gui.h"

#undef MARKER_CHANGES_IN_HISTORY 

static struct cmd_value *
cmd_shell_set_selection(const char *name,
                        struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);

    DEBUG("old map: %d, old offset: %ld, old count: %ld\n",
          shl->select_channel_map,
          shl->select_start,
          shl->select_end - shl->select_start);

    DEBUG("map: %d, offset: %ld, count: %ld\n",
          map, offset, count);

    history_remember(shl->history,
                     CMD_NEW(name,
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(shl->select_channel_map),
                             cmd_new_long_val(shl->select_start),
                             cmd_new_long_val(shl->select_end - shl->select_start)));
    
    count = MAX(count, 0);
    offset = MAX(offset, 0);

    shl->select_channel_map = map;
    shl->select_start = offset;
    shl->select_end = offset + count;
    view_redraw(shl->view);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_preserve_snd(const char *name,
                       struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);

    snd *copy_sr = snd_copy(shl->clip->sr, map, offset, count);
    
    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to copy"),
                                 ERROR(shl->clip->sr));

    history_remember(shl->history,
                     CMD_NEW("insert-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_sndp_val_with_dtor(copy_sr, 
                                                        cmd_sndp_dtor),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset)));

    history_remember(shl->history,
                     CMD_NEW("delete-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count)));
    
    /*    history_remember(shl->history,
                     CMD_NEW("set-selection",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(snd_frame_count(copy_sr))));
    */
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_delete_snd(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);
    struct cmd_value *r;
    struct cmd *cmd;
    snd *del_sr;

    DEBUG("deleting %ld at %ld (map: %x)\n", count, offset, map);

    if(count == 0)
        return cmd_new_void_val();

    del_sr = snd_delete(shl->clip->sr, map, offset, count);
    
    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Cannot delete"),
                                 ERROR(shl->clip->sr));

    history_remember(shl->history,
                     CMD_NEW("insert-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_sndp_val_with_dtor(del_sr, cmd_sndp_dtor),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset)));
    
    cmd = CMD_NEW("set-changed",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot delete (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    view_redraw(shl->view);
    view_reset_status(shl->view);

    /* No need for us or for the caller to destroy del_sr manually, or
       to attach a destroy handler, since we added it to the history,
       meaning it gets destroyed automatically when the history entry
       is destroyed. FIXME: Well; except when history_remember()
       destroys its cmd argument of course. Then we return an invalid
       pointer and things might theoretically explode. To solve this
       we need a refcount. This problem is not pressing right now
       because the cmd argument is only destroyed during rollback
       (i.e. when an error has occurred), and currently no commands
       which actually use the return value of delete-snd are ever
       placed in the history. */

    return cmd_new_sndp_val(del_sr);
}

static struct cmd_value *
cmd_shell_insert_snd(const char *name,
                   struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    track_map_t target_map = cmd_int(args->argv[1]);
    snd *source = cmd_sndp(args->argv[2]);
    track_map_t source_map = cmd_int(args->argv[3]);
    long offset = cmd_long(args->argv[4]);
    long count;
    int t, s = 0;
    GList *l = NULL;
    struct cmd *cmd;
    struct cmd_value *r;

    DEBUG("inserting %ld at %ld (target_map: %u, source_map: %u)\n",
          snd_frame_count(source, target_map), offset, target_map, source_map);

    snd_insert(shl->clip->sr,
               &target_map,
               source,
               &source_map,
               offset);

    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to insert"),
                                 ERROR(shl->clip->sr));
    
    /* Figure out how many samples were inserted on each track and
       generate delete-snd commands for each of them. */

    for(t = 0; t < shl->clip->sr->channels; t++) {

        /* Find target track that samples were inserted on. */

        if((1 << t) & target_map) {

            /* Find source track that samples were inserted from. */
            
            for(; s < source->channels; s++)
                if((1 << s) & source_map)
                    break;

            /* Bail out if there are no source tracks left to insert from. */

            if(s >= source->channels)
                break;

            history_remember(shl->history,
                             CMD_NEW("delete-snd",
                                     cmd_new_shellp_val(shl),
                                     cmd_new_int_val(1 << t),
                                     cmd_new_long_val(offset),
                                     cmd_new_long_val(snd_frame_count(source, (1 << s)))));

            s++;

        }

    }

    cmd = CMD_NEW("set-changed",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot insert (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    view_redraw(shl->view);
    view_reset_status(shl->view);

    l = g_list_append(l, GINT_TO_POINTER(target_map));
    l = g_list_append(l, GINT_TO_POINTER(source_map));

    DEBUG("target_map: %u, source_map: %u\n", target_map, source_map);

    return cmd_new_GListp_val_with_dtor(l, cmd_GListp_dtor);
}

static struct cmd_value *
cmd_shell_insert_silence(const char *name,
                         struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    track_map_t map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);
    struct cmd *cmd;
    struct cmd_value *r;
    int t, out_map = 0;
    
    for(t = 0; t < shl->clip->sr->channels; t++) {
        if((1 << t) & map) {
            if(track_insert_silence(shl->clip->sr->tracks[t], offset, count)) {
                /* FIXME: error handling. Really this should probably
                   go in snd_insert_silence() or something. */
                FAIL("unhandled error inserting silence on track %d, offset: %ld, count: %ld\n", t, offset, count);
            } else {
                history_remember(shl->history,
                                 CMD_NEW("delete-snd",
                                         cmd_new_shellp_val(shl),
                                         cmd_new_int_val(1 << t),
                                         cmd_new_long_val(offset),
                                         cmd_new_long_val(count)));
                out_map |= (1 << t);
            }
        }
    }

    return cmd_new_int_val(out_map);
}

static struct cmd_value *
cmd_shell_replace_snd(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    snd *ins_sr = cmd_sndp(args->argv[1]);
    int map = cmd_int(args->argv[2]);
    long offset = cmd_long(args->argv[3]);
    long count = cmd_long(args->argv[4]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot replace (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    cmd = CMD_NEW("insert-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_sndp_val(ins_sr),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset));
    if(cmd_do_or_fail(cmd, "Cannot replace (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_erase_snd(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);
    snd *del_sr;

    if(count == 0)
        return cmd_new_error_val("Require non-zero length");

    del_sr = snd_erase(shl->clip->sr, map, offset, count);

    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to erase"),
                                 ERROR(shl->clip->sr));
    
    history_remember(shl->history,
                     CMD_NEW("replace-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_sndp_val_with_dtor(del_sr, cmd_sndp_dtor),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count)));
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_paste_snd(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    snd *sr = cmd_sndp(args->argv[1]);
    int map = cmd_int(args->argv[2]);
    long offset = cmd_long(args->argv[3]);
    long count = cmd_long(args->argv[4]);
    int adjust_selection = cmd_int(args->argv[5]);
    struct cmd_value *r;
    struct cmd *cmd;
    GList *mapped;
    int target_map, source_map;

    /* Delete the selection. */
    
    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot paste (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    /* Insert clipboard. */
    
    cmd = CMD_NEW("insert-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_sndp_val(sr),
                  cmd_new_int_val(snd_map_avail(sr)),
                  cmd_new_long_val(offset));
    if(cmd_do_or_fail(cmd, "Cannot paste (%s)", &r)) 
        return r;
    mapped = cmd_GListp(r);
    target_map = GPOINTER_TO_INT(g_list_nth_data(mapped, 0));
    source_map = GPOINTER_TO_INT(g_list_nth_data(mapped, 1));
    cmd_destroy_value(r);

    if(source_map == 0)
        return cmd_new_error_val("Clipboard is empty");

    DEBUG("target_map: %u, source_map: %u\n", target_map, source_map);

    /* Adjust selection. */

    if(adjust_selection) {

        /* Set the point immediately after the biggest amount just
           inserted. */

        cmd = CMD_NEW("set-selection",
                      cmd_new_shellp_val(shl),
                      cmd_new_int_val(target_map),
                      cmd_new_long_val(offset + 
                                       snd_frame_count(sr, source_map)),
                      cmd_new_long_val(0));
        if(cmd_do_or_fail(cmd, "Cannot paste (%s)", &r)) 
            return r;
        cmd_destroy_value(r);
    }

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_insert_tracks_clip(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct clip *src = cmd_clipp(args->argv[1]);
    int map = cmd_int(args->argv[2]);
    struct cmd *cmd;
    struct cmd_value *r;

    clip_insert_tracks(shl->clip, src, map);
    if(error_thrown(ERROR(shl->clip)))
        return cmd_error_cascade(cmd_new_error_val("Cannot insert tracks"),
                                 ERROR(shl->clip));
    
    cmd = CMD_NEW("set-changed",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot delete (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    history_remember(shl->history,
                     CMD_NEW("delete-tracks-clip",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map)));
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_delete_tracks_clip(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    struct clip *clip;
    struct cmd *cmd;
    struct cmd_value *r;
    
    clip = clip_delete_tracks(shl->clip, map);
    if(error_thrown(ERROR(shl->clip))) 
        return cmd_error_cascade(cmd_new_error_val("Cannot delete tracks"),
                                 ERROR(shl->clip));
    
    cmd = CMD_NEW("set-changed",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot delete (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    history_remember(shl->history,
                     CMD_NEW("insert-tracks-clip",
                             cmd_new_shellp_val(shl),
                             cmd_new_clipp_val_with_dtor(clip, cmd_clipp_dtor),
                             cmd_new_int_val(map)));
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_delete_markers(const char *name,
                         struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    AFframecount offset = cmd_long(args->argv[2]);
    AFframecount count = cmd_long(args->argv[3]);
    enum marker_type type = cmd_int(args->argv[4]);
    struct marker_list_array *del_mla;

    del_mla = marker_list_array_delete(shl->clip->markers,
                                       map,
                                       offset,
                                       count,
                                       type);
    if(error_thrown(ERROR(shl->clip->markers))) 
        return cmd_error_cascade(cmd_new_error_val("Could not delete region"),
                                 ERROR(shl->clip->markers));

#ifdef MARKER_CHANGES_IN_HISTORY    
    history_remember(shl->history,
                     CMD_NEW("insert-markers",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count),
                             cmd_new_marker_list_arrayp_val_with_dtor(del_mla,
                                                                      cmd_marker_list_arrayp_dtor),
                             cmd_new_int_val(type)));
#endif

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_insert_markers(const char *name,
                         struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    AFframecount offset = cmd_long(args->argv[2]);
    AFframecount count = cmd_long(args->argv[3]);
    struct marker_list_array *src_mla = cmd_marker_list_arrayp(args->argv[4]);
    enum marker_type type = cmd_int(args->argv[5]);

    marker_list_array_insert(shl->clip->markers,
                             src_mla,
                             map,
                             offset,
                             count,
                             type);
    if(error_thrown(ERROR(shl->clip->markers))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to insert"),
                                 ERROR(shl->clip->markers));

#ifdef MARKER_CHANGES_IN_HISTORY    
    history_remember(shl->history,
                     CMD_NEW("delete-markers",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count),
                             cmd_new_int_val(type)));
#endif

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_paste_markers(const char *name,
                        struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    AFframecount offset = cmd_long(args->argv[2]);
    AFframecount count = cmd_long(args->argv[3]);
    struct marker_list_array *marker_clipboard = 
        cmd_marker_list_arrayp(args->argv[4]);
    enum marker_type type = cmd_int(args->argv[5]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("insert-markers",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count),
                  cmd_new_marker_list_arrayp_val(marker_clipboard),
                  cmd_new_int_val(type));
    if(cmd_do_or_fail(cmd, "Cannot paste markers (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_copy_markers(const char *name,
                       struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    AFframecount offset = cmd_long(args->argv[2]);
    AFframecount count = cmd_long(args->argv[3]);
    struct marker_list_array *marker_clipboard = 
        cmd_marker_list_arrayp(args->argv[4]);
    enum marker_type type = cmd_int(args->argv[5]);

    marker_list_array_replace(marker_clipboard,
                              shl->clip->markers,
                              map,
                              offset,
                              count,
                              type);
    if(error_thrown(ERROR(shl->clip->markers))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to copy"),
                                 ERROR(shl->clip->markers));
    
    return cmd_new_void_val();

}

static struct cmd_value *
cmd_shell_move_markers_left(const char *name,
                            struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);
    int type = cmd_int(args->argv[4]);

    marker_list_array_insert_time(shl->clip->markers, 
                                  map, offset, count, type);
    
#ifdef MARKER_CHANGES_IN_HISTORY    
    history_remember(shl->history,
                     CMD_NEW("move-markers-right",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count),
                             cmd_new_int_val(type)));
#endif
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_move_markers_right(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);
    int type = cmd_int(args->argv[4]);
    
    marker_list_array_delete_time(shl->clip->markers,
                                  map, offset, count, type);
    
#ifdef MARKER_CHANGES_IN_HISTORY    
    history_remember(shl->history,
                     CMD_NEW("move-markers-left",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count),
                             cmd_new_int_val(type)));
#endif
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_move_track(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int from = cmd_int(args->argv[1]);
    int to = cmd_int(args->argv[2]);
    int what = cmd_int(args->argv[3]);
    int i, modify_map = 0;
    const char *s;

    /* 
     * move(from, to) reorders all tracks between [from, to]
     * inclusive, so check constraints for all of them. 
     */
    
    for(i = MIN(from, to); i < MAX(from, to); i++)
        modify_map |= i;

    if((s = constraints_test(shl->constraints,
                             region_new(modify_map,
                                        REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    clip_move_track(shl->clip, from, to, what);

    if(error_thrown(ERROR(shl->clip))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to move"),
                                 ERROR(shl->clip));
    
    history_remember(shl->history,
                     CMD_NEW("move-track",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(to),
                             cmd_new_int_val(from),
                             cmd_new_int_val(what)));
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_scrub(const char *name,
                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int direction = cmd_int(args->argv[1]);
    int multiplier = cmd_int(args->argv[2]);
    AFframecount pos;

    if(shl->player->player_running && shl->player->state->record_mode)
        return cmd_new_void_val();

    if(shl->player->player_running) {

        if(shl->player->state->record_mode)
            return cmd_new_void_val();

        pos = CLAMP(shl->player->state->playback_pos +
                    (direction * pref_get_as_int("scrub_playback_amount") * 
                     multiplier),
                    0, snd_frame_count(shl->clip->sr, MAP_ALL));
        player_set_position(shl->player, pos);
        view_center_hrange(shl->view, pos, pos);
    } else {
        pos = shl->view->hadjust->value + 
            MAX(0, (view_get_widget(shl->view, 
                                    "wavecanvas")->allocation.width * 
                    shl->view->hres) / 2);
        view_scroll_hcenter(shl->view, pos, 
                            pos + (direction * 
                                   (shl->view->hres * 
                                    pref_get_as_int("scrub_silent_amount") * 
                                    multiplier)));
    }
    return cmd_new_void_val();
}
                            
static struct cmd_value *
cmd_shell_dispatch_cmd(const char *name,
                       struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd = cmd_cmdp(args->argv[1]);
    char *what = cmd_charp(args->argv[2]);
    struct cmd_value *r, *tmp;
    
    if(history_begin(shl->history, what)) 
        return cmd_new_error_val("Could not begin history transition");

    shl->use++;
    
    r = cmd_do(cmd);

    if(cmd_is_error(r)) {

        tmp = cmd_new_error_val("%s", cmd_get_error_message(r));
        cmd_destroy_value(r);
        r = tmp;
        history_rollback(shl->history);

    } else {

        history_commit(shl->history);

    }

    view_reset_status(shl->view);
    view_sync_display(shl->view);
    view_redraw(shl->view);

    shl->use--;

    if(shl->close_requested) 
        view_push_status(shl->view, 
                         "Quitting, waiting for pending "
                         "commands to complete...");

    if(shl->close_requested && shl->use == 0)
        shell_destroy(shl);

    return r;
}

static struct cmd_value *
cmd_shell_activate_module(const char *name,
                          struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int id = cmd_int(args->argv[1]);
    struct module_state *module_state;
    struct gnusound_module *module;

    DEBUG("id: %d\n", id);

    module_state = shell_get_module_state(shl, id);
    module = module_get(id);

    if(module_state->is_open)
        return cmd_new_error_val("Module %s already open", 
                                 module->name);

    DEBUG("opening module %s\n", module->name);
    
    if(!module->open) {
        DEBUG("module %s doesn't have open(), "
              "trying execute()...\n", module->name);
        return cmd_do(CMD_NEW("execute-module",
                              cmd_new_shellp_val(shl),
                              cmd_new_int_val(id),
                              cmd_new_voidp_val(NULL)));
    }
    
    return module->open(id, shl);
}

static struct cmd_value *
cmd_shell_execute_module(const char *name,
                         struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int id = cmd_int(args->argv[1]);
    void *user_data = cmd_voidp(args->argv[2]);
    struct cmd *cmd;
    struct cmd_value *r;
    struct module_state *module_state;
    struct gnusound_module *module;

    module_state = shell_get_module_state(shl, id);
    module = module_get(id);

    if(!module->execute)
        return cmd_new_error_val("Module %s doesn't provide execute.",
                                 module->name);

    cmd = CMD_NEW("set-changed",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot delete (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    shell_start_operation(shl, module->name);

    r = module->execute(id, shl, user_data);

    shell_end_operation(shl);

    return r;
}

static struct cmd_value *
cmd_shell_convert_snd(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    double sample_rate = cmd_double(args->argv[1]);
    int sample_type = cmd_int(args->argv[2]);
    int do_resample = cmd_int(args->argv[3]);
    int i;
    float old_sample_rate = shl->clip->sr->rate;
    AFframecount new_frame_count, end_offset;
    const char *s;
    struct cmd_value *err;

    if((s = constraints_test(shl->constraints,
                             region_new(REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING,
                                        REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_DELETE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    if(shl->use)
        return cmd_new_error_val("Cannot %s because shell is in use",
                                 name);
    
    /* FIXME: push constraints. */

    view_set_cursor(shl->view, GDK_WATCH);
    arbiter_yield();
    snd_convert(shl->clip->sr, sample_type, sample_rate);
    if(error_thrown(ERROR(shl->clip->sr))) {
        err = cmd_new_error_val("Could not convert: %s", 
                                error_get_message(ERROR(shl->clip->sr)));
        error_free(ERROR(shl->clip->sr));
        view_set_cursor(shl->view, shl->view->default_cursor);
        return err;
    }
    history_clear(shl->history);
    grid_rate_set(&shl->grid, shl->clip->sr->rate);
    
    view_redraw(shl->view);
    view_set_cursor(shl->view, shl->view->default_cursor);

    if(!do_resample)
        return cmd_new_void_val();
    
    if(sample_rate == old_sample_rate)
        return cmd_new_void_val();
    
    /* Resample the tracks. */

    view_set_cursor(shl->view, GDK_WATCH);
    view_push_status(shl->view, "Please wait, resampling to %.2fhz...",
                     sample_rate);
    end_offset = snd_frame_count(shl->clip->sr, MAP_ALL);
    new_frame_count = end_offset * 
        ((double)shl->clip->sr->rate / old_sample_rate);
    for(i = 0; i < shl->clip->sr->channels; i++) {
        resample_track(shl,
                       i,
                       0,
                       end_offset,
                       new_frame_count,
                       RESAMPLE_BEST_ALGORITHM,
                       0,
                       NULL);
    }

    //    shl->select_end = shl->select_start + new_frame_count;

    view_pop_status(shl->view);
    view_set_cursor(shl->view, shl->view->default_cursor);
    view_redraw(shl->view);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_set_changed(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int newval = cmd_int(args->argv[1]);
    int oldval = shl->has_changed;

    shl->has_changed = newval;

    history_remember(shl->history,
                     CMD_NEW("set-changed",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(oldval)));
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_shell_shove_snd(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    AFframecount offset = cmd_long(args->argv[2]);
    AFframecount count = cmd_long(args->argv[3]);
    AFframecount delta = cmd_long(args->argv[4]);
    AFframecount delete_offset, insert_offset, select_offset;
    AFframecount delete_count, insert_count, select_count;
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;

    if(count <= 0) 
        return cmd_new_error_val("Can't shove emptiness");

    if((s = constraints_test(shl->constraints,
                             region_new(map,
                                        MAX(0, offset - ABS(delta)), 
                                        count + ABS(delta)),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    if(delta > 0) {
        
        /*
         * |-----occcccccdd-------------|
         */
        
        delete_offset = offset + count;
        delete_count = delta;
        insert_offset = offset;
        insert_count = delta;
        select_offset = offset + delta;
        select_count = count;

    } else {

        delta = -delta;

        /*
         * |-----ddoccccccc-------------|
         */
        
        delete_offset = MAX(0, offset - delta);
        delete_count = delta;
        insert_offset = offset + count - delta;
        insert_count = delta;
        select_offset = MAX(0, offset - delta);
        select_count = count;
        if(offset - delta < 0)
            select_count = count + (offset - delta);
    }

    DEBUG("offset: %ld, count: %ld, delta: %ld\n", offset, count, delta);
    DEBUG("delete_offset: %ld, delete_count: %ld\n", delete_offset, delete_count);
    DEBUG("insert_offset: %ld, insert_count: %ld\n", insert_offset, insert_count);

    cmd = CMD_NEW("delete-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(delete_offset),
                  cmd_new_long_val(delete_count));
    if(cmd_do_or_fail(cmd, "Cannot delete (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    cmd = CMD_NEW("insert-silence",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(insert_offset),
                  cmd_new_long_val(insert_count));
    if(cmd_do_or_fail(cmd, "Cannot insert silence (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(select_offset),
                  cmd_new_long_val(select_count));
    if(cmd_do_or_fail(cmd, "Cannot set selection (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

int
cmd_shell_init() {
    int i; 
    struct cmd_signature f[] = {
        
        { "dispatch-cmd", 
          "Runs a command in the context of a specified shell. This means "
          "any undo information generated by the command will be added "
          "to the shell's history and the shell's use count is incremented. "
          "Interruptable commands (commands which may invoke arbiter_yield() "
          "at any point during their operation) MUST be executed via "
          "dispatch-cmd.",
          cmd_shell_dispatch_cmd, CMD_ANY_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_cmdp_T, CMD_charp_T) },

        { "set-selection", "Sets the selection.",
          cmd_shell_set_selection, CMD_VOID_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, CMD_long_T, 
                            CMD_long_T) },
        
        { "delete-snd", "Deletes a region.",
          cmd_shell_delete_snd, CMD_sndp_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, CMD_long_T, 
                            CMD_long_T) },
        
        { "insert-snd", 
          "Inserts a snd into the shell."
          "@return Two element list containing the target map and the "
          "source map",
          cmd_shell_insert_snd, CMD_GListp_T, 
          cmd_new_paramdecl(5, CMD_shellp_T, CMD_int_T, CMD_sndp_T, 
                            CMD_int_T, CMD_long_T) },

        { "insert-silence", 
          "Inserts silence into the document.",
          cmd_shell_insert_silence, CMD_int_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, CMD_long_T, 
                            CMD_long_T) },
        
        { "replace-snd", "Replaces a region.",
          cmd_shell_replace_snd, CMD_int_T, 
          cmd_new_paramdecl(5, CMD_shellp_T, CMD_sndp_T, CMD_int_T, 

                            CMD_long_T, CMD_long_T) },

        { "erase-snd",
          "Erases a region, i.e. fills the region with silence.",
          cmd_shell_erase_snd, CMD_int_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, CMD_long_T, 
                            CMD_long_T) },
        
        { "paste-snd", 
          "Deletes given region and inserts given sound object. "
          "If the final parameter is non-zero, then the selection "
          "is adjusted to span the inserted sound object.",
          cmd_shell_paste_snd, CMD_VOID_T, 
          cmd_new_paramdecl(6, CMD_shellp_T, CMD_sndp_T,
                            CMD_int_T, CMD_long_T, CMD_long_T, CMD_int_T) },

        { "convert-snd", "Changes sample rate and type.",
          cmd_shell_convert_snd, CMD_VOID_T,
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_double_T, CMD_int_T, 
                            CMD_int_T) },
        
        { "insert-tracks-clip", 
          "Inserts tracks. ",
          cmd_shell_insert_tracks_clip, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_clipp_T, CMD_int_T) },

        { "delete-tracks-clip", 
          "Deletes tracks. ",
          cmd_shell_delete_tracks_clip, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },
        
        { "delete-markers", 
          "Deletes markers in specified region. ",
          cmd_shell_delete_markers, CMD_VOID_T, 
          cmd_new_paramdecl(5, CMD_shellp_T, CMD_int_T, CMD_long_T,
                            CMD_long_T, CMD_int_T) },

        { "insert-markers", 
          "Inserts markers. ",
          cmd_shell_insert_markers, CMD_VOID_T, 
          cmd_new_paramdecl(6, CMD_shellp_T, CMD_int_T, CMD_long_T,
                            CMD_long_T, CMD_marker_list_arrayp_T, CMD_int_T) },

        { "copy-markers", 
          "Copies markers in region onto supplied marker list array.",
          cmd_shell_copy_markers, CMD_VOID_T, 
          cmd_new_paramdecl(6, CMD_shellp_T, CMD_int_T, CMD_long_T,
                            CMD_long_T, CMD_marker_list_arrayp_T, CMD_int_T) },
        
        { "paste-markers", 
          "Pastes markers from supplied marker list array .",
          cmd_shell_paste_markers, CMD_VOID_T, 
          cmd_new_paramdecl(6, CMD_shellp_T, CMD_int_T, CMD_long_T,
                            CMD_long_T, CMD_marker_list_arrayp_T, CMD_int_T) },

        { "move-markers-left", 
          "Move markers to the left.",
          cmd_shell_move_markers_left, CMD_VOID_T, 
          cmd_new_paramdecl(5, CMD_shellp_T, CMD_int_T, CMD_long_T,
                            CMD_long_T, CMD_int_T) },

        { "move-markers-right", 
          "Move markers to the right. ",
          cmd_shell_move_markers_right, CMD_VOID_T, 
          cmd_new_paramdecl(5, CMD_shellp_T, CMD_int_T, CMD_long_T,
                            CMD_long_T, CMD_int_T) },

        { "move-track", 
          "Move track to another position.",
          cmd_shell_move_track, CMD_VOID_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, CMD_int_T, 
                            CMD_int_T) },
        
        { "scrub",
          "Moves view window or playback position.",
          cmd_shell_scrub, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_int_T, CMD_int_T) },

        { "preserve-snd", "Preserves a region.",
          cmd_shell_preserve_snd, CMD_VOID_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, CMD_long_T, 
                            CMD_long_T) },

        { "activate-module", "Open or execute a module.",
          cmd_shell_activate_module, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },

        { "execute-module", "Execute a module.",
          cmd_shell_execute_module, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_int_T, CMD_voidp_T) },
        
        { "set-changed", 
          "Sets the changed flag which determines whether to query the "
          "user before closing the shell.",
          cmd_shell_set_changed, CMD_VOID_T,
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },
        
        { "shove-snd", 
          "Undocumented.",
          cmd_shell_shove_snd, CMD_VOID_T,
          cmd_new_paramdecl(5, CMD_shellp_T, CMD_int_T, CMD_long_T, 
                            CMD_long_T, CMD_long_T) },
        
    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
