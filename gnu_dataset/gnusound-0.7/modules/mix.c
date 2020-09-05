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
mix(shell *target_shell,
    int target_channel,
    AFframecount target_offset,
    struct clip *source_clip,
    int source_channel,
    AFframecount source_offset,
    AFframecount frame_count) {
    int i;
    int32_t *out_buffer;
    double marker_value = 0;
    AFframecount source_read = 0;
    GList *del_blks = NULL;
    ITERATOR_INIT(target_offset, frame_count);

    out_buffer = mem_calloc(1, EFFECT_BUF_SIZE * sizeof(int32_t));
    if(!out_buffer) {
        FAIL("not enough memory for mix buffer (%"CONVSPEC_SIZE_T" bytes)\n",
             EFFECT_BUF_SIZE * sizeof(int32_t));
        ITERATOR_EXIT();
        return;
    }

    ITERATOR(target_shell, target_shell->clip->sr->tracks[target_channel], 
             for(i = 0; i < iter_read; i++) {
                 marker_value = 
                     marker_list_slope_value(target_shell->clip->markers->lists[target_channel],
                                             iter_frame_offset + i,
                                             MARKER_SLOPE); 
                 out_buffer[i] = int32_frame_bits[i] + 
                     (int32_frame_bits[i] * marker_value);
             }
             source_read = track_get_samples_as(source_clip->sr->tracks[source_channel],
                                                SAMPLE_TYPE_INT_32,
                                                int32_frame_bits,
                                                source_offset,
                                                iter_read);
             for(i = 0; i < source_read; i++) {
                 marker_value = 
                     marker_list_slope_value(source_clip->markers->lists[source_channel],
                                             source_offset + i,
                                             MARKER_SLOPE); 
                 out_buffer[i] += int32_frame_bits[i] + 
                     (int32_frame_bits[i] * marker_value);
             }
             track_delete(target_shell->clip->sr->tracks[target_channel],
                          &del_blks,
                          iter_frame_offset,
                          iter_read);
             blocklist_blocks_destroy(del_blks);
             track_insert_samples_from(target_shell->clip->sr->tracks[target_channel],
                                       SAMPLE_TYPE_INT_32,
                                       out_buffer,
                                       iter_frame_offset,
                                       iter_read);
             source_offset += iter_read;
             memset(out_buffer, '\0', iter_read * sizeof(int32_t)));
    
    free(out_buffer);
    ITERATOR_EXIT();
}

static struct cmd_value *
mix_execute(int id,
            shell *shl,
            void *data) {
    int t, seltrks = 0, target_channel = -1, map = shl->select_channel_map;
    AFframecount start = shl->select_start, end = shl->select_end;
    struct cmd *cmd;
    struct cmd_value *r;

    DEBUG("start: %ld, end: %ld, channel_map: %d\n", 
          start, end, shl->select_channel_map);
    rwlock_rlock(&shl->clip->sr->rwl);

    for(t = 0; t < shl->clip->sr->channels; t++) {
        if((1 << t) & map) {
            seltrks++;
            if(target_channel == -1)
                target_channel = t;
        }
    }

    if(seltrks != 2) {
        rwlock_runlock(&shl->clip->sr->rwl);
        return cmd_new_error_val("Mix mixes the second of 2 selected "
                                 "tracks to the first selected track. "
                                 "You must select 2 tracks.");
    }
    
    /* Preserve the selection. */
    
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
    
    for(t = 0; t < shl->clip->sr->channels; t++) 
        if(t != target_channel && ((1 << t) & map)) 
            mix(shl,
                target_channel,
                start,
                shl->clip,
                t,
                start,
                end - start);
    
    rwlock_runlock(&shl->clip->sr->rwl);
    
    return cmd_new_void_val();
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Mix",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    NULL,
    NULL,
    NULL,
    mix_execute,
    NULL,
    NULL,
    NULL
};

