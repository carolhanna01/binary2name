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

static AFframecount
find_zero(shell *shl,
          int track,
          AFframecount start_offset,
          AFframecount end_offset) {
    AFframecount r = 0, abs_offset = start_offset, frame_count;
    int i, cur_sign = 0, prev_sign = -1, delta = 1, count_down = 0;
    frame_bits_t frame_bits = mem_alloc(EFFECT_BUF_SIZE * 4);

    if(!frame_bits)
        return start_offset;

    if(start_offset > end_offset)
        count_down = 1;

    frame_count = ABS(end_offset - start_offset);

    if(count_down) {
        delta = -1;
        start_offset = MAX(0, start_offset - MIN(EFFECT_BUF_SIZE, frame_count));
    }

    do {
        r = track_get_samples_as(shl->clip->sr->tracks[track],
                                 SAMPLE_TYPE_INT_32,
                                 frame_bits,
                                 start_offset,
                                 MIN(EFFECT_BUF_SIZE, frame_count));
        if(r <= 0)
            break;
        
        for(i = count_down ? r - 1 : 0; 
            count_down ? i >= 0 : i < r; 
            i += delta, abs_offset += delta) {

            cur_sign = (((int32_t *)frame_bits)[i] > 0 ? 1 : 0);
            
            /* First iteration, fix start point. */
            
            if(prev_sign == -1)
                prev_sign = cur_sign;

            /* Sign change. */
            
            if(cur_sign != prev_sign && prev_sign != -1) {
                DEBUG("found nul: abs_offset: %ld\n", abs_offset);
                mem_free(frame_bits);
                return abs_offset;
            }
            prev_sign = cur_sign;
        }

        start_offset += (delta * r);
        frame_count -= r;
        
    } while(r > 0);

    mem_free(frame_bits);
    return start_offset;
}

struct cmd_value *
autozero_execute(int id,
                 shell *shl,
                 void *data) {
    AFframecount first = 0, last = 0;
    int t;
    struct cmd_value *r;
    struct cmd *cmd;
    
    rwlock_rlock(&shl->clip->sr->rwl);
    
    for(t = 0; t < snd_track_count(shl->clip->sr); t++) {
        if((1 << t) & shl->select_channel_map) {
            first = find_zero(shl,
                              t,
                              shl->select_start,
                              shl->select_end);
            last = find_zero(shl,
                             t,
                             shl->select_end,
                             shl->select_start);
        }
    }

    rwlock_runlock(&shl->clip->sr->rwl);

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(first),
                  cmd_new_long_val(last - first));
    if(cmd_do_or_fail(cmd, "Cannot set selection (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    return cmd_new_void_val();
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Auto Zero",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2004",
    "GPL",
    NULL,
    0,

    NULL,
    NULL,
    NULL,
    autozero_execute,
    NULL,
    NULL,
    NULL
};
