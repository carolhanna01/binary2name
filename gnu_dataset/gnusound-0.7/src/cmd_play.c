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
#include "player.h"
#include "pref.h"
#include "arbiter.h"

static struct cmd_value *
cmd_play_start_player(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    long offset = cmd_long(args->argv[1]);
    
    /*
     * This is racey but that's ok because player_start() and
     * player_stop() are supposed to be robust.
     */
    
    if(!shl->player->player_running) {
        player_set_position(shl->player, offset);
        player_start(shl->player); 
    }

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_seek_player(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    long offset = cmd_long(args->argv[1]);

    player_set_position(shl->player, offset);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_stop_player(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);

    DEBUG("shl->player->player_running: %d\n",
          shl->player->player_running);

    /* This is racey but that's ok because player_start()
       and player_stop() are supposed to be robust. */

    if(shl->player->player_running) 
        player_stop(shl->player);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_start_player_from_cue(const char *name,
                               struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    AFframecount s = (LOOP_IS_ACTIVE(shl) ? shl->loop_start : 
                      shl->select_start);

    if(!shl->player->player_running) {
        cmd = CMD_NEW("start-player",
                      cmd_new_shellp_val(shl),
                      cmd_new_long_val(s));
        if(cmd_do_or_fail(cmd, "Cannot play from cue (%s)", &r)) 
            return r;
        cmd_destroy_value(r);
        view_center_hrange(shl->view, s, s);
    
    } else {
        view_center_hrange(shl->view, shl->select_start, shl->select_start);
        player_set_position(shl->player, shl->select_start);
    }

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_toggle_player(const char *name,
                         struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;

    if(shl->player->player_running) 
        cmd = CMD_NEW("stop-player", cmd_new_shellp_val(shl));
    else 
        cmd = CMD_NEW("start-player",
                      cmd_new_shellp_val(shl),
                      cmd_new_long_val(shl->select_start));

    if(cmd_do_or_fail(cmd, "Cannot toggle player (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_record(const char *name,
                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;
    struct timespec tm;
    struct timeval now;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        (shl->select_end == shl->select_start ?
                                         REGION_MATCH_ANYTHING : 
                                         shl->select_end - shl->select_start)),
                             (shl->record_replace ? 
                              CONSTRAINTS_OPER_REPLACE : 
                              CONSTRAINTS_OPER_INSERT))))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    cmd = CMD_NEW("set-changed",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot set-changed (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    constraints_push(shl->constraints,
                     "Recording",
                     region_new(shl->select_channel_map, 
                                shl->select_start, 
                                (shl->select_end == shl->select_start ?
                                 REGION_MATCH_ANYTHING : 
                                 shl->select_end - shl->select_start)),
                     (CONSTRAIN_POSITION |
                      CONSTRAIN_LENGTH |
                      CONSTRAIN_CONTENTS));
    
    shl->record_mode = 1;

    cmd = CMD_NEW("start-player",
                  cmd_new_shellp_val(shl),
                  cmd_new_long_val(shl->select_start));
    if(cmd_do_or_fail(cmd, "Cannot start player (%s)", &r)) {
        constraints_pop(shl->constraints);
        shl->record_mode = 0;
        return r;
    }
    cmd_destroy_value(r);

    /* Wait for the player to stop. */

    pthread_mutex_lock(&shl->player->player_running_lock);
    while(shl->player->player_running) {
        gettimeofday(&now, NULL);
        tm.tv_sec = now.tv_sec;
        tm.tv_nsec = (now.tv_usec * 1000) + 10000;
        pthread_cond_timedwait(&shl->player->player_running_cond, 
                               &shl->player->player_running_lock, 
                               &tm);
        pthread_mutex_unlock(&shl->player->player_running_lock);
        arbiter_yield();
        pthread_mutex_lock(&shl->player->player_running_lock);
    }
    pthread_mutex_unlock(&shl->player->player_running_lock);

    /* FIXME: This is a bit ugly, maybe the code from
       player_commit_record_undo() should just go here, but that makes
       this function a bit unwieldy. */

    player_commit_record_undo(shl->player);

    shl->record_mode = 0;

    constraints_pop(shl->constraints);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_scrub_right(const char *name,
                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("scrub",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(SCRUB_RIGHT),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot scrub right (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_scrub_left(const char *name,
                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("scrub",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(SCRUB_LEFT),
                  cmd_new_int_val(1));
    if(cmd_do_or_fail(cmd, "Cannot scrub left (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_scrub_right_fast(const char *name,
                     struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("scrub",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(SCRUB_RIGHT),
                  cmd_new_int_val(pref_get_as_int("scrub_fast_multiplier")));
    if(cmd_do_or_fail(cmd, "Cannot scrub right fast (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_play_scrub_left_fast(const char *name,
                    struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("scrub",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(SCRUB_LEFT),
                  cmd_new_int_val(pref_get_as_int("scrub_fast_multiplier")));
    if(cmd_do_or_fail(cmd, "Cannot scrub left fast (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    return cmd_new_void_val();
}


int
cmd_play_init() {
    int i; 
    struct cmd_signature f[] = {
        
        { "start-player", 
          "Starts the playback/record thread.",
          cmd_play_start_player, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_long_T) },

        { "seek-player", 
          "Moves the player to the specified position.",
          cmd_play_seek_player, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_long_T) },

        { "stop-player", 
          "Sets selection to loop.",
          cmd_play_stop_player, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "toggle-player", 
          "Sets selection to loop.",
          cmd_play_toggle_player, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },
        
        { "start-player-from-cue", 
          "Sets selection to loop.",
          cmd_play_start_player_from_cue, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "record", 
          "Sets selection to loop.",
          cmd_play_record, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "scrub-right", 
          "Moves player position or display position.",
          cmd_play_scrub_right, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "scrub-left", 
          "Moves player position or display position.",
          cmd_play_scrub_left, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "scrub-right-fast", 
          "Moves player position or display position.",
          cmd_play_scrub_right_fast, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "scrub-left-fast", 
          "Moves player position or display position.",
          cmd_play_scrub_left_fast, CMD_VOID_T, 
          cmd_new_paramdecl(1, CMD_shellp_T) },


    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
