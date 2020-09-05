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

#include <gnusound.h>

static void
amplitude_process_track(shell *shl,
                        int track,
                        double factor,
                        double slope,
                        AFframecount offset,
                        AFframecount count) {
    int i;
    ITERATORF_INIT(offset, count);
    ITERATORF(shl, shl->clip->sr->tracks[track],
              for(i = 0; i < iter_read; i++) {
                      float_frame_bits[i] = float_frame_bits[i] * 
                      (factor + 
                       (factor * marker_list_slope_value(shl->clip->markers->lists[track],
                                                         iter_frame_offset + i,
                                                         MARKER_SLOPE)) - 
                       (slope * ((float)iter_frames_processed + i)));
              }
              track_replace_samples_from(shl->clip->sr->tracks[track],
                                         SAMPLE_TYPE_FLOAT_32,
                                         float_frame_bits,
                                         iter_frame_offset,
                                         iter_read));
    ITERATORF_EXIT();
}

struct cmd_value *
amplitude_process(const char *name,
                  struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long offset = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);
    double factor = cmd_double(args->argv[4]);
    double slope = cmd_double(args->argv[5]);
    int t;
    struct shell_undo_state *us;
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;
    
    /* Preserve the region. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    if((s = constraints_test(shl->constraints,
                             region_new(map, offset, count),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);

    constraints_push(shl->constraints,
                     "Changing volume",
                     region_new(map, offset, count),
                     (CONSTRAIN_POSITION | 
                      CONSTRAIN_LENGTH | 
                      CONSTRAIN_CONTENTS));

    rwlock_rlock(&shl->clip->sr->rwl);
    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map) 
            amplitude_process_track(shl, t, factor, slope, offset, count);
    rwlock_runlock(&shl->clip->sr->rwl);

    constraints_pop(shl->constraints);

    history_remember(shl->history,
                     CMD_NEW("set-selection",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset),
                             cmd_new_long_val(count)));
    
    return cmd_new_void_val();
}


static int
amplitude_init(int id) {
    int i;
    struct cmd_signature f[] = {
        
        { "process-amplitude", 
          "Changes the amplitude of a waveform or section."
          "@arg 1 The shell."
          "@arg 2 The channel map."
          "@arg 3 The offset."
          "@arg 4 The count."
          "@arg 5 The factor by which to multiply the amplitude."
          "@arg 6 The slope along which to multiply the factor argument.",
          amplitude_process, CMD_VOID_T, 
          cmd_new_paramdecl(6, CMD_shellp_T,
                            CMD_int_T, CMD_long_T, CMD_long_T,
                            CMD_double_T, CMD_double_T) },
    };
    
    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}

static void
amplitude_exit(int id) {
    cmd_unregister("process-amplitude");
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Amplitude Manipulation Tools",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    amplitude_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    amplitude_exit
};
