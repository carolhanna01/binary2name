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
reverse_track(shell *shl,
              int track,
              AFframecount offset,
              AFframecount count) {
    int i;
    int32_t f;
    AFframecount insert_offset = offset;
    GList *del_blks;
    ITERATOR_INIT(offset, count);
    ITERATOR(shl, shl->clip->sr->tracks[track], 
             for(i = 0; i < (iter_read / 2); i++) {
                 f = int32_frame_bits[iter_read - (i + 1)];
                 int32_frame_bits[iter_read - (i + 1)] = int32_frame_bits[i];
                 int32_frame_bits[i] = f;
             }
             track_delete(shl->clip->sr->tracks[track],
                          &del_blks,
                          iter_frame_offset,
                          iter_read);
             blocklist_blocks_destroy(del_blks);
             
             track_insert_samples_from(shl->clip->sr->tracks[track],
                                       SAMPLE_TYPE_INT_32,
                                       int32_frame_bits,
                                       insert_offset,
                                       iter_read));
    ITERATOR_EXIT();
}

static struct cmd_value *
reverse_execute(int id,
                shell *shl,
                void *data) {
    int t, map = shl->select_channel_map;
    AFframecount offset = shl->select_start, count = shl->select_end - offset;
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;
    
    if((s = constraints_test(shl->constraints,
                             region_new(map, offset, count),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot Reverse because region is locked "
                                 "(%s)", s);
    
    constraints_push(shl->constraints,
                     "Reversing",
                     region_new(map, offset, count),
                     (CONSTRAIN_POSITION | 
                      CONSTRAIN_LENGTH | 
                      CONSTRAIN_CONTENTS));
    
    /* Preserve the selection. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) {
        constraints_pop(shl->constraints);
        return r;
    }
    cmd_destroy_value(r);

    rwlock_rlock(&shl->clip->sr->rwl);
    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map) 
            reverse_track(shl, t, offset, count);
    rwlock_runlock(&shl->clip->sr->rwl);

    constraints_pop(shl->constraints);

    return cmd_new_void_val();
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Reverse",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    NULL,
    NULL,
    NULL,
    reverse_execute,
    NULL,
    NULL,
    NULL
};

