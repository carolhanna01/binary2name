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

void
declip(shell *shl,
       track *tr,
       AFframecount start_offset,
       AFframecount end_offset,
       int32_t upper_trsh,
       int32_t lower_trsh) {
    int i;
    int32_t prev_sample = 0;
    ITERATOR_INIT(start_offset, end_offset - start_offset);
    ITERATOR(shl, tr, 
             for(i = 0; i < iter_read; i++) {
                 if(prev_sample > upper_trsh && 
                    int32_frame_bits[i] < lower_trsh) {
                     DEBUG("declip: clip at sample %ld\n", iter_frame_offset);
                     int32_frame_bits[i] = prev_sample;
                 }
                 prev_sample = int32_frame_bits[i];
             }
             track_replace_samples_from(tr,
                                        SAMPLE_TYPE_INT_32,
                                        int32_frame_bits,
                                        iter_frame_offset,
                                        iter_read));
    ITERATOR_EXIT();
    return;
}

static struct cmd_value *
declip_execute(int id,
               shell *shl,
               void *data) {
    int upper_tresh = INT_MIN + 5000;
    int lower_tresh = INT_MIN + 400;
    int t, map = shl->select_channel_map;
    AFframecount start = shl->select_start, end = shl->select_end;
    struct cmd_value *r;
    struct cmd *cmd;

    /* Preserve the selection. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(end - start));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    rwlock_rlock(&shl->clip->sr->rwl);
    
    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map) 
            declip(shl,
                   shl->clip->sr->tracks[t],
                   start,
                   end,
                   upper_tresh,
                   lower_tresh);
    rwlock_runlock(&shl->clip->sr->rwl);
    
    return cmd_new_void_val();
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Declip",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2004",
    "GPL",
    NULL,
    0,

    NULL,
    NULL,
    NULL,
    declip_execute,
    NULL,
    NULL,
    NULL
};

