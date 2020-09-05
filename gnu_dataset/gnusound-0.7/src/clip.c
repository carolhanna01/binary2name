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
#include <stdarg.h>
#include "pref.h"
#include "sample.h"
#include "marker.h"
#include "mixer.h"
#include "snd.h"
#include "player.h"
#include "mem.h"
#include "clip.h"

/**
 * @file
 * A clip is a thin wrapper which keeps together a mixer, markers and 
 * sound data. This API should be improved and expanded in the future.
 */

void
clip_set_snd(struct clip *clip,
             snd *sr) {

    if(clip->sr)
        snd_destroy(clip->sr);

    clip->sr = sr;

    mixer_configure(clip->mixer, 
                    player_get_output_channels(),
                    clip->sr->channels);

    msg_send(clip->msg,
             "clip::snd-changed", 
             CMD_NEW_ARGV(cmd_new_clipp_val(clip)));
    

}

/**
 * Moves a track to another position.
 * @param clip The clip.
 * @param from The track to move.
 * @param to The destination.
 * @param map Mask specifying which elements to move, one or more of
 * CLIP_MARKERS, CLIP_MIXER or CLIP_SND.
 * @return 0 on success, non-zero otherwise.
 */

int
clip_move_track(struct clip *clip,
                int from,
                int to,
                int map) {
    int i;

    if(map & CLIP_SND) {
        if(snd_move_track(clip->sr, from, to)) {
            error_cascade(ERROR(clip),
                          ERROR(clip->sr),
                          "Unable to move sound tracks");
            goto err_snd;
        }
    }

    if(map & CLIP_MIXER) {
        if(mixer_move_source_track(clip->mixer, from, to)) {
            error_cascade(ERROR(clip),
                          ERROR(clip->mixer),
                          "Unable to move mixer tracks");
            goto err_mixer;
        }
    }

    if(map & CLIP_MARKERS) {
        if(marker_list_array_move_list(clip->markers, from, to)) {
            error_cascade(ERROR(clip),
                          ERROR(clip->markers),
                          "Unable to move marker lists");
            goto err_markers;
        }
    }

    msg_send(clip->msg,
             "clip::track-moved", 
             CMD_NEW_ARGV(cmd_new_clipp_val(clip), 
                          cmd_new_int_val(from),
                          cmd_new_int_val(to),
                          cmd_new_int_val(map)));
    
    return 0;
    
 err_markers:
    mixer_move_source_track(clip->mixer, to, from);

 err_mixer:
    snd_move_track(clip->sr, to, from);

 err_snd:
    return 1;
}

int
clip_insert_tracks(struct clip *clip,
                   struct clip *src,
                   track_map_t map) {
    int i;

    snd_insert_tracks(clip->sr, src ? src->sr : NULL, map);
    if(error_thrown(ERROR(clip->sr))) {
        error_cascade(ERROR(clip),
                      ERROR(clip->sr),
                      "Unable to insert sound tracks");
        return 1;
    }
    
    mixer_insert_source_tracks(clip->mixer, src ? src->mixer : NULL, map);
    if(error_thrown(ERROR(clip->mixer))) {
        snd_destroy(snd_delete_tracks(clip->sr, map));
        error_cascade(ERROR(clip),
                      ERROR(clip->mixer),
                      "Unable to insert mixer tracks");
        return 1;
    }

    marker_list_array_insert_lists(clip->markers, src ? src->markers : NULL, 
                                   map);
    if(error_thrown(ERROR(clip->markers))) {
        mixer_destroy(mixer_delete_source_tracks(clip->mixer, map));
        snd_destroy(snd_delete_tracks(clip->sr, map));
        error_cascade(ERROR(clip),
                      ERROR(clip->markers),
                      "Unable to insert marker lists");
        return 1;
    }

    for(i = 0; i < clip->sr->channels; i++) {

        if((1 << i) & map) {

            msg_send(clip->msg,
                     "clip::track-inserted", 
                     CMD_NEW_ARGV(cmd_new_clipp_val(clip), 
                                  cmd_new_int_val(i)));
            

            //            map <<= 1;

        }

    }
    
    return 0;
    
}

struct clip *
clip_delete_tracks(struct clip *clip,
                   track_map_t map) {
    struct clip *r;
    mixer *del_mix;
    snd *del_sr;
    struct marker_list_array *del_mla;
    int i;

    r = clip_new("deleted tracks", 0, clip->sr->rate, clip->sr->sample_type);
    if(!r) {
        error_set(ERROR(clip),
                  "Unable to create clip for deleted tracks");
        return NULL;
    }

    del_sr = snd_delete_tracks(clip->sr, map);
    if(error_thrown(ERROR(clip->sr))) {
        error_cascade(ERROR(clip),
                      ERROR(clip->sr),
                      "Unable to delete sound tracks");
        return NULL;
    }

    del_mix = mixer_delete_source_tracks(clip->mixer, map);
    if(error_thrown(ERROR(clip->mixer))) {
        snd_insert_tracks(clip->sr, del_sr, map);
        error_cascade(ERROR(clip),
                      ERROR(clip->mixer),
                      "Unable to delete mixer tracks");
        return NULL;
    }

    del_mla = marker_list_array_delete_lists(clip->markers, map);
    if(error_thrown(ERROR(clip->markers))) {
        snd_insert_tracks(clip->sr, del_sr, map);
        mixer_insert_source_tracks(clip->mixer, del_mix, map);
        error_cascade(ERROR(clip),
                      ERROR(clip->markers),
                      "Unable to delete marker lists");
        return NULL;
    }
    
    snd_destroy(r->sr);
    mixer_destroy(r->mixer);
    marker_list_array_destroy(r->markers);
    r->sr = del_sr;
    r->mixer = del_mix;
    r->markers = del_mla;

    for(i = 0; i < clip->sr->channels; i++) 
        if((1 << i) & map)
            msg_send(clip->msg,
                     "clip::track-deleted", 
                     CMD_NEW_ARGV(cmd_new_clipp_val(clip), 
                                  cmd_new_int_val(i)));
    
    
    return r;
}


void
clip_destroy(struct clip *clip) {
    GList *l;

    if(!clip)
        return;

    msg_send(clip->msg,
             "clip::destroy", 
             CMD_NEW_ARGV(cmd_new_clipp_val(clip)));
    
    if(clip->msg)
        msg_destroy(clip->msg);

    if(clip->sr)
        snd_destroy(clip->sr);

    if(clip->mixer)
        mixer_destroy(clip->mixer);

    if(clip->markers)
        marker_list_array_destroy(clip->markers);

    mem_free(clip);
}

/*
struct clip *
clip_new_with_snd(snd *sr) {
    struct clip *clip = mem_alloc(sizeof(struct clip));

    if(!clip)
        return NULL;

    assert(sr != NULL);

    clip->destroy_callbacks = NULL;
    error_init(&clip->error);

    clip->constraints = constraints_new();
    if(!clip->constraints) {
        mem_free(clip);
        return NULL;
    }

    clip->mixer = mixer_new(0, 0);
    if(!clip->mixer) {
        clip_destroy(clip);
        return NULL;
    }

    clip->markers = marker_list_array_new(sr->channels);
    if(!clip->markers) {
        clip_destroy(clip);
        return NULL;
]    }
    
    clip->sr = sr;
    mixer_configure(clip->mixer, 
                    player_get_output_channels(NULL),
                    clip->sr->channels);
    
    return clip;
}
*/

struct clip *
clip_new(const char *name,
         int tracks,
         float rate,
         enum sample_type sample_type) {
    int i;
    struct clip *clip = mem_alloc(sizeof(struct clip));
    struct cmd_signature msgs[] = {
        
        { "clip::destroy", "Destruction event.",
          NULL, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_clipp_T, CMD_voidp_T) },
        
        { "clip::snd-changed", "Sound changed event.",
          NULL, CMD_VOID_T,
          cmd_new_paramdecl(2, CMD_clipp_T, CMD_voidp_T) },

        { "clip::track-inserted", "Insert event.",
          NULL, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_clipp_T, CMD_int_T, CMD_voidp_T) },

        { "clip::track-deleted", "Delete event.",
          NULL, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_clipp_T, CMD_int_T, CMD_voidp_T) },

        { "clip::track-moved", "Move event.",
          NULL, CMD_VOID_T, 
          cmd_new_paramdecl(5, CMD_clipp_T, CMD_int_T, CMD_int_T, CMD_int_T, CMD_voidp_T) },
        
    };

    if(!clip)
        return NULL;

    error_init(&clip->error);

    clip->mixer = NULL;
    clip->sr = NULL;
    clip->markers = NULL;

    clip->msg = msg_new();
    if(!clip->msg) {
        clip_destroy(clip);
        return NULL;
    }

    clip->mixer = mixer_new(0, 0);
    if(!clip->mixer) {
        clip_destroy(clip);
        return NULL;
    }

    clip->sr = snd_new(tracks, rate, sample_type);
    if(!clip->sr) {
        clip_destroy(clip);
        return NULL;
    }
    snd_set_name(clip->sr, name);

    clip->markers = marker_list_array_new(tracks);
    if(!clip->markers) {
        clip_destroy(clip);
        return NULL;
    }

    mixer_configure(clip->mixer, 
                    player_get_output_channels(),
                    clip->sr->channels);

    for(i = 0; i < sizeof(msgs) / sizeof(msgs[0]); i++)
        msg_publish(clip->msg, &msgs[i]);

    return clip;
}
