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

int32_t
find_peak(shell *shl,
          int track,
          AFframecount start_offset,
          AFframecount end_offset) {
    int i;
    int32_t peak = 1, val = 0;
    ITERATOR_INIT(start_offset, end_offset - start_offset);
    ITERATOR(shl, shl->clip->sr->tracks[track],
             for(i = 0; i < iter_read; i++) {
                 val = ABS(int32_frame_bits[i]);
                 if(val > peak)
                     peak = val;
             });
    ITERATOR_EXIT();
    return peak;
}

static struct cmd_value *
normalize_execute(int id,
                  shell *shl,
                  void *data) {
    int32_t peak = 0, p;
    double factor;
    int t, map = shl->select_channel_map;
    AFframecount start = shl->select_start, end = shl->select_end;
    struct cmd *cmd;
    struct cmd_value *r;

    DEBUG("start: %ld, end: %ld\n", start, end);
    rwlock_rlock(&shl->clip->sr->rwl);
    
    for(t = 0; t < snd_track_count(shl->clip->sr); t++) {
        if((1 << t) & map) {
            p = find_peak(shl, t, start, end);
            if(p > peak)
                peak = p;
        }
    }
    
    if(peak == 0) {
        rwlock_runlock(&shl->clip->sr->rwl);
        return cmd_new_error_val("Not enough memory.");
    }
    
    if(shl->cancel_requested) {
        rwlock_runlock(&shl->clip->sr->rwl);
        return cmd_new_void_val();
    }

    factor = (double)INT32_MAX / (double)peak;

    /* Preserve the selection. */

    /*    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(end - start));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) {
        rwlock_runlock(&shl->clip->sr->rwl);        
        return r;
    }
    cmd_destroy_value(r);
    */

    cmd = CMD_NEW("process-amplitude",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(end - start),
                  cmd_new_double_val(factor),
                  cmd_new_double_val(0));
    if(cmd_do_or_fail(cmd, "Cannot process amplitude (%s)", &r)) {
        rwlock_runlock(&shl->clip->sr->rwl);        
        return r;
    }
    cmd_destroy_value(r);

    rwlock_runlock(&shl->clip->sr->rwl);

    return cmd_new_void_val();
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Normalize",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    NULL,
    NULL,
    NULL,
    normalize_execute,
    NULL,
    NULL,
    NULL
};

