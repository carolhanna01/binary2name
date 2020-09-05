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

static struct cmd_value *
cmd_mixer_toggle_solo(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int track = cmd_int(args->argv[1]);

    /* FIXME: bounds checking */

    mixer_toggle_source_solo(shl->clip->mixer, track);
    gtk_widget_queue_draw(view_get_widget(shl->view, "mixercanvas"));

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_mixer_toggle_mute(const char *name,
                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int track = cmd_int(args->argv[1]);

    /* FIXME: bounds checking */

    mixer_toggle_source_mute(shl->clip->mixer, track);
    gtk_widget_queue_draw(view_get_widget(shl->view, "mixercanvas"));

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_mixer_set_mix_level(const char *name,
                        struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int source_track = cmd_int(args->argv[1]);
    int target_channel = cmd_int(args->argv[2]);
    double value = cmd_double(args->argv[3]);
    
    /* FIXME: bounds checking */
    
    view_set_transient(shl->view, MSG_INFO, "Channel %d gain: %.3f", 
                       target_channel + 1, value);
    shl->clip->mixer->mixtable[target_channel][source_track] = value;
    
    gtk_widget_queue_draw(view_get_widget(shl->view, "mixercanvas"));
    gtk_widget_queue_draw(view_get_widget(shl->view, "infocanvas"));
    
    return cmd_new_void_val();
}

int
cmd_mixer_init() {
    int i; 
    struct cmd_signature f[] = {
        /*      
        { "solize-track", "Solizes a track",
          cmd_mixer_solo_track, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },

        { "mute-track", "Mutes a track",
          cmd_mixer_mute_track, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },

        { "unsolize-track", "Unsolizes a track",
          cmd_mixer_unsolize_track, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },

        { "unmute-track", "Unmutes a track",
          cmd_mixer_unmute_track, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },
*/
        { "toggle-solo", "Toggles track solo.",
          cmd_mixer_toggle_solo, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },

        { "toggle-mute", "Toggles track mute.",
          cmd_mixer_toggle_mute, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_int_T) },

        { "set-mix-level", "Sets mixer level for a track.",
          cmd_mixer_set_mix_level, CMD_VOID_T, 
          cmd_new_paramdecl(4, CMD_shellp_T, CMD_int_T, 
                            CMD_int_T, CMD_double_T) },


    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
