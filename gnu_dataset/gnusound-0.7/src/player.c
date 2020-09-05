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

#include <config.h>
#include <errno.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <sched.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <audiofile.h>
#include "lib/misc.h"
#include "mem.h"
#include "pref.h"
#include "shell.h"
#include "snd.h"
#include "mixer.h"
#include "player.h"

#include "player_dummy.h"

static GHashTable *player_drivers_h;

static struct player_driver player_dummy_driver = {
    "Dummy", player_dummy_new, player_dummy_destroy,
    player_dummy_init, player_dummy_exit, 
    player_dummy_open, player_dummy_close, player_dummy_transfer,
    player_dummy_get_output_channels, player_dummy_get_input_channels,
    player_dummy_get_audio_delay, player_dummy_get_sample_rate,
    player_dummy_get_record_buffer_size, player_dummy_get_playback_buffer_size,
    player_dummy_open_config, player_dummy_commit_config,
    player_dummy_close_config
  
};

static const struct player_driver_container *player_current_driver = NULL;

#if 0
void
player_buffer_convert(frame_bits_t fb,
                      int frame_width,
                      AFframecount frame_count,
                      int format) {
    AFframecount i;

    /* This function should convert the frames in fb to the format
       specified in format. That means byte order, sign and frame
       width translation. The bits in fb are always two's complement.
       Byte order is defined by libaudiofile. FIXME: what kind of byte
       order does libaudiofile return? (think native byte order) */
    
    /* Some cards cannot playback signed 8 bit samples. So we convert
       them to unsigned 8 bit. */

    if(format == AFMT_U8) {
        if(frame_width == 1) {
            for(i = 0; i < frame_count; i++) {
                ((int8_t *)fb)[i] = ((int8_t *)fb)[i] + 128;
            }
        } else {
            FAIL("cannot convert from frame_width %d to 1\n",
                 frame_width);
        }
    }    
}
#endif

int
player_get_driver_count() {
    return g_hash_table_size(player_drivers_h);
}

const struct player_driver_container *
player_get_driver_by_name(const char *name) {
    return g_hash_table_lookup(player_drivers_h, name);
}

struct player_driver_index_request {
    int num;
    struct player_driver_container *pdc;
};

void
player_find_driver_by_index(void *key,
                            void *value,
                            void *user_data) {
    struct player_driver_index_request *req = user_data;
    struct player_driver_container *pdc = value;
    if(req->pdc)
        return;
    if(pdc->num == req->num)
        req->pdc = pdc;
}

const struct player_driver_container *
player_get_driver(int i) {
    struct player_driver_index_request request;
    
    request.num = i;
    request.pdc = NULL;
    
    g_hash_table_foreach(player_drivers_h, 
                         player_find_driver_by_index, &request);

    return request.pdc;
}

const struct player_driver_container *
player_get_default_driver() {
    return player_get_driver(0);
}

const struct player_driver_container *
player_get_current_driver() {
    return player_current_driver;
}

/**
 * Obtains a buffer for playback. The caller requests a number of
 * frames to play back, and this function returns a buffer filled with
 * interleaved samples and the number of frames in that buffer.
 *
 * @param p The player object.
 * @param buf A pointer to the buffer pointer.
 * @param frame_count On entry, the number of frames required,
 * on exit, the number of frames actually available.
 * @return 0 on success, non-zero error code otherwise.
 */

int
player_get_playback_bufi(struct player *p,
                         void **buf,
                         AFframecount *frame_count) {
    AFframecount played_frames;
    shell *shl = p->shl;
    *frame_count = CLAMP(*frame_count, 0, player_get_chunk_size(p));

    //    DEBUG("frame_count: %ld\n", *frame_count);

    played_frames = snd_getn(shl->clip->sr,
                             p->playback_srcbufs,
                             p->state->source_tracks_map,
                             p->state->playback_pos,
                             *frame_count);

    memset(p->playback_muxbuf, '\0', 
           (played_frames *
            sample_get_width(shl->clip->sr->sample_type) * 
            shl->clip->mixer->output_channels));
    
    mixer_mixi(shl->clip->mixer,
               shl->clip->sr->sample_type,
               p->playback_muxbuf,
               p->playback_srcbufs,
               played_frames);
    
    *buf = p->playback_muxbuf;
    *frame_count = played_frames;
    return 0;
}

/**
 * Obtains a buffer for playback. The caller requests a number of
 * frames to play back, and this function returns a buffer filled with
 * non-interleaved samples and the number of frames in that buffer.
 *
 * @param p The player object.
 * @param buf A pointer to the buffer array pointer.
 * @param frame_count On entry, the number of frames required,
 * on exit, the number of frames actually available.
 * @return 0 on success, non-zero error code otherwise.
 */

int
player_get_playback_bufn(struct player *p,
                         void ***buf,
                         AFframecount *frame_count) {
    int i;
    AFframecount played_frames;
    shell *shl = p->shl;
    *frame_count = CLAMP(*frame_count, 0, player_get_chunk_size(p));

    played_frames = snd_getn(shl->clip->sr,
                             p->playback_srcbufs,
                             p->state->source_tracks_map,
                             p->state->playback_pos,
                             *frame_count);
                             
    for(i = 0; i < shl->clip->mixer->output_channels; i++)
        memset(((void **)p->playback_muxbuf)[i], '\0', 
               played_frames * sample_get_width(p->shl->clip->sr->sample_type));

    mixer_mixn(shl->clip->mixer,
               shl->clip->sr->sample_type,
               (void **)p->playback_muxbuf,
               p->playback_srcbufs,
               played_frames);
    
    *buf = (void **)p->playback_muxbuf;
    *frame_count = played_frames;
    return 0;
}

void
player_update_playback_counters(struct player *p,
                                AFframecount frame_count) {
    p->state->playback_pos += frame_count;

   /*
    if(LOOP_IS_ACTIVE(p->shl) && 
       p->state->playback_pos >= p->shl->loop_end) {
        p->state->audio_pos = p->shl->loop_start;
        p->state->playback_pos = p->shl->loop_start;
    }
    */
}

/**
 * Reclaims a buffer previously acquired via
 * player_get_playback_bufi().  The driver calls this function
 * to indicate that the samples have been written to the audio device.
 *
 * @return 0 on success, non-zero error code otherwise.
 */

int
player_flush_playback_bufi(struct player *p,
                           AFframecount frame_count) {
    player_update_playback_counters(p, frame_count);
    return 0;
}

/**
 * Reclaims a buffer previously acquired via
 * player_get_playback_bufn().  The driver calls this function
 * to indicate that the samples have been written to the audio device.
 *
 * @return 0 on success, non-zero error code otherwise.
 */

int
player_flush_playback_bufn(struct player *p,
                           AFframecount frame_count) {
    player_update_playback_counters(p, frame_count);
    return 0;
}


/**
 * Obtains a buffer for use by the player driver as record buffer.
 * The driver uses this buffer to store interleaved recorded data from
 * the audio device.
 *
 * @param p The player object.
 * @param buf A pointer to the buffer pointer.
 * @param frame_count On entry, the number of frames available to
 * store in the buffer, on exit, the number of frames that will fit 
 * in the buffer.
 */

int
player_get_record_bufi(struct player *p,
                       void **buf,
                       AFframecount *frame_count) {
    *buf = p->record_muxbuf;
    *frame_count = CLAMP(*frame_count, 0, player_get_chunk_size(p));
    return 0;
}

/**
 * Obtains buffers for use by the player driver as record buffers.
 * The driver uses these buffers to store the non-interleaved recorded
 * data from the audio device.
 *
 * @param p The player object.
 * @param buf A pointer to the buffer array pointer.
 * @param frame_count On entry, the number of frames available to
 * store in the buffer, on exit, the number of frames that will fit 
 */

int
player_get_record_bufn(struct player *p,
                       void ***buf,
                       AFframecount *frame_count) {
    *buf = (void **)p->record_muxbuf;
    *frame_count = CLAMP(*frame_count, 0, player_get_chunk_size(p));
    return 0;
}


int
player_update_record_undo(struct player *p,
                          AFframecount frame_count) {
    snd *del_sr;
    shell *shl = p->shl;
    track_map_t target_map;

    /* In replace mode we need to delete and preserve the frames that
       we just recorded before we insert the recorded data. */
    
    if(p->state->record_replace && 
       p->state->record_pos < snd_frame_count(shl->clip->sr, MAP_ALL)) {
        
        /* FIXME: error checking. (added but it creaks) */
        del_sr = snd_delete(shl->clip->sr,
                            p->state->undo_channel_map,
                            MAX(0, p->state->record_pos),
                            frame_count);
        if(error_thrown(ERROR(shl->clip->sr))) {
            FAIL("Could not overwrite frames in record: %s\n",
                 error_get_message(ERROR(shl->clip->sr)));
            shl->loop = 0;
            snd_destroy(del_sr);
            return -1;
        }
        
        /* In replace mode we need to keep an undo buffer for the
           frames that we deleted. But in loop mode, we can stop
           making undos after the first loop has completed. */
        
        if(p->state->loop_count == 0) {            
            if(p->state->undo_sr) {
                target_map = p->state->undo_channel_map;
                snd_insert(p->state->undo_sr,
                           &target_map,
                           del_sr,
                           NULL,
                           snd_frame_count(p->state->undo_sr, MAP_ALL));
                if(error_thrown(ERROR(p->state->undo_sr))) {
                    FAIL("cannot insert deleted frames onto undo buffer: %s\n",
                         error_get_message(ERROR(del_sr)));
                    /* FIXME: try to put deleted frames back. */
                    snd_destroy(del_sr);
                    return -1;
                }
                    
            } else {
                p->state->undo_sr = 
                    snd_clone(del_sr,
                              CLONE_STRUCTURE | 
                              CLONE_TRACK_STRUCTURE | 
                              CLONE_TRACK_DATA);
                if(error_thrown(ERROR(del_sr))) {
                    FAIL("cannot clone deleted frames for undo buffer: %s\n",
                         error_get_message(ERROR(del_sr)));
                    shl->loop = 0;
                    snd_destroy(p->state->undo_sr);
                    p->state->undo_sr = NULL;
                    snd_destroy(del_sr);          
                    return -1;
                }
            }
        }
        snd_destroy(del_sr);
    }

    return 0;
}

void
player_update_record_counters(struct player *p,
                              AFframecount frame_count) {

    p->state->record_pos += frame_count;

    /* 
     * There are 4 cases:
     *
     * 1. Loop mode, insert mode.
     * 2. Loop mode, replace mode.
     * 3. Insert.
     * 4. Replace.
     *
     * Case 1: The end offset always increases by frame_count.
     * Case 2: The end offset increases until the end of the loop.
     * Case 3: The end offset always increases by frame_count.
     * Case 4: The end offset always increases by frame_count.
     */

    if(LOOP_IS_ACTIVE(p->shl) && p->state->record_replace && 
       p->state->loop_count > 0) 
        return;

    p->state->undo_end_offset += frame_count;
}

/**
 * Writes the buffer previously obtained thru
 * player_get_record_bufi() to the sound object. The frame_count
 * argument specifies how many frames are in the buffer.
 *
 * This function can only be called when the underlying sound object
 * can be protected by an rwlock. Since rwlock's have limitations on
 * the number of simultaenous threads they can protect (MAX_THREADS in
 * rwlock.h), this function should only be called from a thread
 * accounted for by MAX_THREADS.
 * 
 * @param p The player object.
 * @param frame_count The number of frames in the buffer (should never
 * exceed the number of frames returned by player_get_record_bufi()).
 * @return 0 on success or non-zero error code.
 */

int
player_flush_record_bufi(struct player *p,
                         AFframecount frame_count) {
    int r;
    shell *shl = p->shl;

    assert(frame_count != 0);

    r = player_update_record_undo(p, frame_count);
    if(r)
        return r;

    r = snd_puti(shl->clip->sr,
                 p->record_muxbuf,
                 p->record_srcbufs,
                 p->state->target_tracks,
                 p->state->target_tracks_map,
                 MAX(0, p->state->record_pos),
                 frame_count);
    if(r)
        return r;

    player_update_record_counters(p, frame_count);

    return 0;
}

/**
 * Writes the buffer previously obtained thru
 * player_get_record_bufn() to the sound object. The frame_count
 * argument specifies how many frames are in the buffer.
 *
 * This function can only be called when the underlying sound object
 * can be protected by an rwlock. Since rwlock's have limitations on
 * the number of simultaenous threads they can protect (MAX_THREADS in
 * rwlock.h), this function should only be called from a thread
 * accounted for by MAX_THREADS.
 * 
 * @param p The player object.
 * @param frame_count The number of frames in the buffer (should never
 * exceed the number of frames returned by player_get_record_bufn()).
 * @return 0 on success or non-zero error code.
 */

int
player_flush_record_bufn(struct player *p,
                         AFframecount frame_count) {
    int r;
    shell *shl = p->shl;

    assert(frame_count != 0);

    r = player_update_record_undo(p, frame_count);
    if(r)
        return r;

    r = snd_putn(shl->clip->sr,
                 p->record_muxbuf,
                 p->state->target_tracks,
                 p->state->target_tracks_map,
                 MAX(0, p->state->record_pos),
                 frame_count);
    
    if(r)
        return r;

    player_update_record_counters(p, frame_count);

    return 0;
}

/**
 * Returns the total number of frames available for playback. The
 * driver should use this value to determine when to stop playing.
 * @param p The player object.
 * @return The number of frames left until playback should stop.
 */

AFframecount
player_get_playback_avail(struct player *p) {
    AFframecount t;
    //    DEBUG("p->state->end_offset: %ld, p->state->playback_pos: %ld, snd_frame_count(p->shl->clip->sr): %ld\n", p->state->end_offset, p->state->playback_pos, snd_frame_count(p->shl->clip->sr));
       
    //    if(LOOP_IS_ACTIVE(p->shl)) {
    //        p->state->start_offset = p->shl->loop_start;
    //        p->state->end_offset = p->shl->loop_end;
    //    }
    if(p->state->record_mode && p->state->append_mode) {
        t = snd_frame_count(p->shl->clip->sr, MAP_ALL);
        return MAX(0, t - p->state->playback_pos);
    }

    t = MIN(snd_frame_count(p->shl->clip->sr, MAP_ALL), p->state->end_offset);
    return MAX(0, t - p->state->playback_pos);
}

AFframecount
player_get_record_avail(struct player *p) {
    if(!p->state->record_mode) 
        return 0;
    if(p->state->append_mode)
        p->state->end_offset = p->state->record_pos + player_get_chunk_size(p);
    if(LOOP_IS_ACTIVE(p->shl))
        p->state->end_offset = p->shl->loop_end;
    return MAX(0, p->state->end_offset - p->state->record_pos);
}

/**
 * Returns true if there are still frames to play or record,
 * false otherwise.
 * @param p The player object.
 */

int
player_has_work(struct player *p) {
    if(p->cancel_requested)
        return 0;

    if(p->state->record_mode) {
        if(p->state->append_mode)
            return 1;

        return player_get_record_avail(p);
    }
    
    return player_get_playback_avail(p);
}

void
player_commit_record_undo(struct player *p) {
    shell *shl = p->shl;
    struct cmd *cmd;
    struct cmd_value *r;

    if(p->state->undo_sr) {
        DEBUG("dumping undo_sr info:\n");
        snd_info_dump(p->state->undo_sr);
    }

    if(!p->state->record_mode) 
        return;
    /*
    if(p->player_running && shl->select_start == shl->select_end) {
        if(p->state->record_mode)
            shl->select_start = shl->select_end = p->state->record_pos;
        else 
            shl->select_start = shl->select_end = p->state->playback_pos;
        DEBUG("record_mode; %d, playback pos: %ld, record pos: %ld, select_start: %ld\n",
              p->state->record_mode, p->state->playback_pos, p->state->record_pos, shl->select_start);
        
              }*/
   
    if(p->state->record_replace && p->state->undo_sr) {
        DEBUG("pushing undo for replacement record\n");
        
        history_remember(shl->history,
                         CMD_NEW("paste-snd",
                                 cmd_new_shellp_val(shl),
                                 cmd_new_sndp_val(p->state->undo_sr),
                                 cmd_new_int_val(p->state->undo_channel_map),
                                 cmd_new_long_val(p->state->undo_start_offset),
                                 cmd_new_long_val(p->state->undo_end_offset - 
                                                  p->state->undo_start_offset),
                                 cmd_new_int_val(0)));
        
        
    } else {
        
        DEBUG("pushing undo for insertion record\n");
        
        history_remember(shl->history,
                         CMD_NEW("delete-snd",
                                 cmd_new_shellp_val(shl),
                                 cmd_new_int_val(p->state->undo_channel_map),
                                 cmd_new_long_val(p->state->undo_start_offset),
                                 cmd_new_long_val(p->state->undo_end_offset -
                                                  p->state->undo_start_offset)));

    }

    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(p->state->undo_channel_map),
                  cmd_new_long_val(p->state->undo_start_offset),
                  cmd_new_long_val(p->state->undo_end_offset - 
                                   p->state->undo_start_offset));
    if(cmd_do_or_fail(cmd, "Cannot set selection (%s)", &r)) {
        FAIL("shit, failed in set-selection after record\n");
        cmd_destroy_value(r);
        return;
    }
    cmd_destroy_value(r);
}

/**
 * Thread cleanup handler, called when the playback thread exits.
 */

void
player_cleanup(void *ptr) {
    struct player *p = ptr;
    shell *shl = p->shl;

    DEBUG("entering cleanup, locking mutex...\n");
    pthread_mutex_lock(&p->player_running_lock);

    DEBUG("cancel_requested: %d, shl->loop: %d, shl->loop_start: %ld, shl->loop_end: %ld\n", 
          shl->player->cancel_requested, shl->loop, shl->loop_start, shl->loop_end);

    mixer_buffers_free(pref_get_as_int("max_tracks"),
                       p->playback_muxbuf,
                       p->playback_srcbufs);
    mixer_buffers_free(pref_get_as_int("max_tracks"),
                       p->record_muxbuf,
                       p->record_srcbufs);

    p->player_running = 0;
    p->cancel_honored = 1;
    pthread_cond_broadcast(&p->player_running_cond);
    pthread_mutex_unlock(&p->player_running_lock);
    DEBUG("player cleanup complete\n");
}

/**
 * Stops the running player thread by setting cancel_requested
 * to true (which is polled from the player thread), then waiting for
 * the player thread cleanup handler to signal completion.
 */

void
player_stop(struct player *p) {
    struct sched_param sched_param;

    pthread_mutex_lock(&p->player_running_lock);

    if(!p->player_running) {
        pthread_mutex_unlock(&p->player_running_lock);
        return;
    }
    
    g_source_remove(p->update_view_handler_id);

    DEBUG("requesting cancellation for player thread\n");
    p->cancel_requested = 1;
    p->cancel_honored = 0;
    //    pthread_cancel(p->player_thread);

    while(p->player_running) {
        pthread_cond_wait(&p->player_running_cond, 
                          &p->player_running_lock);
    }
    p->cancel_requested = 0;
    sched_param.sched_priority = 0;
    pthread_setschedparam(pthread_self(), SCHED_OTHER, &sched_param);
    pthread_mutex_unlock(&p->player_running_lock);
    view_redraw(p->shl->view);
    pref_optimize();
}

/**
 * The player thread entry point. This sets up the player driver and
 * dispatches to the driver's transfer() function to do the actual
 * playback/recording.
 */

void *
player_thread(void *ptr) {
    int i;
    struct player *p = (struct player *)ptr;
    const struct player_driver_container *pdc = player_get_current_driver();
    struct player_driver *pd = pdc->pd;
    shell *shl = (shell *)p->shl;

    /*
     * Setup player state. This basically copies a lot of settings
     * from the shell (so that they can be altered in the shell
     * without affecting the player) and does a little bit of
     * interpretation/mangling.
     */

    memset(p->state, '\0', sizeof(*(p->state)));
    p->state->source_rate = shl->clip->sr->rate;
    p->state->sample_type = shl->clip->sr->sample_type;
    p->state->record_mode = shl->record_mode;
    p->state->record_replace = shl->record_replace;
    p->state->source_tracks = shl->clip->sr->channels;
    p->state->source_tracks_map = MAP_ALL;
    p->state->target_tracks = 0;
    p->state->target_tracks_map = 0;
    p->state->loop_count = 0;
    p->state->channel_map = shl->select_channel_map;
    p->state->start_offset = LOOP_IS_ACTIVE(shl) ? shl->loop_start : 
        shl->select_start;
    p->state->end_offset = LOOP_IS_ACTIVE(shl) ? shl->loop_end : 
        shl->select_end;
    p->state->undo_channel_map = shl->select_channel_map;
    p->state->undo_start_offset = p->state->start_offset;
    p->state->undo_end_offset = p->state->start_offset;
    p->state->undo_sr = NULL;
    p->state->append_mode = 0;
    if(p->state->start_offset == p->state->end_offset) {
        p->state->append_mode = 1;
        p->state->end_offset = snd_frame_count(shl->clip->sr, MAP_ALL);
    }

    /* Start at beginning if we're playing back from end. */

    if(p->state->start_offset >= snd_frame_count(shl->clip->sr, MAP_ALL) &&
       !p->state->record_mode) {
        p->state->start_offset = 0;
    }

    p->state->playback_pos = p->state->start_offset;
    p->state->record_pos = p->state->start_offset;
    p->state->audio_chunk_duration = pref_get_as_float("audio_chunk_duration");

    /*
     * In record mode, we mute the channels that we're recording on.
     */

    if(p->state->record_mode) {
        if(!p->state->channel_map)
            p->state->channel_map = 1;
        
        for(i = 0; i < pref_get_as_int("max_tracks"); i++) {
            if((1 << i) & p->state->channel_map) {
                p->state->source_tracks_map -= (1 << i);
                p->state->source_tracks--;
                p->state->target_tracks_map += (1 << i);
                p->state->target_tracks++;
            }
        }
        if(p->state->target_tracks > pref_get_as_int("max_tracks"))
            p->state->target_tracks = pref_get_as_int("max_tracks");

        DEBUG("p->state->source_tracks: %d, "
              "p->state->source_tracks_map: %d, "
              "p->state->target_tracks: %d, "
              "p->state->target_tracks_map: %d, "
              "p->state->channel_map: %d\n",
              p->state->source_tracks,
              p->state->source_tracks_map,
              p->state->target_tracks, 
              p->state->target_tracks_map,
              p->state->channel_map);
    }

    view_clear_transient(shl->view);

    /*
     * Signal that we are up and running now. It's important that this
     * happens before any initializations that may fail, otherwise
     * player_start() deadlocks on failure.
     */

    pthread_mutex_lock(&p->player_running_lock);
    p->player_running = 1;
    p->player_started = 1;
    pthread_cond_broadcast(&p->player_running_cond);
    pthread_cleanup_push(player_cleanup, p);
    pthread_mutex_unlock(&p->player_running_lock);

    if(p->state->target_tracks > player_get_output_channels()) {
        FAIL("cannot record %d tracks, max: %d\n", 
             p->state->target_tracks, player_get_output_channels());
        goto cleanup;
    }

    DEBUG("chunk size: %d\n", player_get_chunk_size(p));
    if(!mixer_buffers_alloc(sample_get_width(shl->clip->sr->sample_type),
                            pref_get_as_int("max_tracks"),
                            &p->playback_muxbuf,
                            p->playback_srcbufs,
                            player_get_chunk_size(p))) {
        view_set_transient(shl->view, MSG_ERR,
                           "No memory for playback buffers");
        FAIL("could not allocate playback mixer buffers\n");
        goto cleanup;
    }

    if(!mixer_buffers_alloc(sample_get_width(shl->clip->sr->sample_type),
                            pref_get_as_int("max_tracks"),
                            &p->record_muxbuf,
                            p->record_srcbufs,
                            player_get_chunk_size(p))) {
        view_set_transient(shl->view, MSG_ERR,
                           "No memory for record buffers");
        FAIL("could not allocate record mixer buffers\n");
        goto cleanup;
    }

    DEBUG("starting driver\n");
    if(pd->start(p)) 
        goto cleanup_start_failed;

    /* Gross hack to signal to the display update callback that we're
       ready for display. */

    p->player_ready = 1;

    DEBUG("player running, record_mode: %d, record_replace: %d\n",
          p->state->record_mode, p->state->record_replace);

    do {
        if(pd->transfer(p)) 
            break;
        p->state->loop_count++;

        if(LOOP_IS_ACTIVE(shl)) {
            p->state->start_offset = shl->loop_start;
            p->state->end_offset = shl->loop_end;        
            p->state->playback_pos = p->shl->loop_start;
            p->state->record_pos = p->shl->loop_start;
            //            p->state->audio_pos = p->shl->loop_start;
        }
    } while(LOOP_IS_ACTIVE(shl) && !p->cancel_requested);

 cleanup_start_failed:    
    pd->stop(p);
 cleanup:
    pthread_cleanup_pop(1);
    return NULL;
}

/**
 * Center on the audio position if necessary and redraw the display.
 */

void
player_do_redraw(struct player *p) {
    shell *shl = p->shl;
    struct view *view = shl->view;
    AFframecount w = view_get_widget(view, "wavecanvas")->allocation.width * view->hres;
    
    if(p->state->record_mode)
        view_reset_status(shl->view);
    
    if(!view->follow_playback)
        goto end;

    /* Don't center when start and end positions fit in the window. */

    if(p->state->start_offset >= view->hadjust->value &&
       p->state->end_offset < view->hadjust->value + w)
        goto end;
    
    view_center_hrange(view, p->state->audio_pos, 
                       p->state->audio_pos);
    
 end:
    view_redraw(view);
}

/**
 * Estimates the current audio position (which sample is being played
 * back right now).
 */

AFframecount
player_estimate_audio_position(struct player *p) {
    float time_delta;
    int slack;
    struct timeval now;
    AFframecount estimate, frame_delta, a, b, c, d, e, f;

    /*
     * The easiest way to estimate the playback position (i.e. what
     * the user hears right now) is to simply return the position of
     * the last chunk that was fed to the driver. This has the problem
     * that the accuracy of the estimate becomes dependant on the
     * chunk size, i.e. the bigger the chunk size, the coarser the
     * estimate. It would be nicer if you could have big chunk sizes
     * (reducing the chance of audio xruns) but still get a fairly
     * accurate playback position estimate.
     *
     * So instead we use the sample rate and the time since we were
     * last called to calculate an estimate of the playback position.
     * We compare this estimate to the position of the last chunk
     * because the hardware clock is probably not exactly 44100hz or
     * whatever, besides there can be underruns etc.  which cause
     * playback interruptions.
     *
     * We make an exception for when we're recording. In that case we
     * return the position of the last chunk that was received from
     * the driver. Otherwise the pointer can be ahead of or behind the
     * data we're recording and this looks very odd.
     */
    
    if(p->state->record_mode) 
        return p->state->record_pos;

    /* First time around just return the actual playback engine
       position. */

    if(!p->audio_pos_estimation_begin) {
        p->audio_pos_estimation_begin = 1;
        return p->state->playback_pos - player_get_audio_delay(p);
    }

    /* Calculate the number of frames that should have been played
       since the last time we were called. */

    gettimeofday(&now, NULL);
    time_delta = tv_diff_secs(&p->audio_pos_previous_estimate_time, &now);
    estimate = p->state->audio_pos + 
        (int)(time_delta * 
              (float)player_get_current_driver()->pd->get_sample_rate(p));
    p->audio_pos_previous_estimate_time = now;

    //    DEBUG("start_offset: %ld, end_offset: %ld\n",
    //          p->state->start_offset, p->state->end_offset);

    /* If the estimate is beyond the end point, wrap. */

    if(estimate > p->state->end_offset)
        estimate = p->state->start_offset + (estimate - p->state->end_offset);

    /* Compare the estimate to the actual playback engine position.
       If it differs by too much, go with the figure from the playback
       engine. */

    slack = player_get_current_driver()->pd->get_playback_buffer_size(p);
    frame_delta = ABS(p->state->playback_pos - estimate);

    a = MIN(estimate, p->state->playback_pos);
    b = MAX(estimate, p->state->playback_pos);

    c = a - p->state->start_offset;
    d = p->state->end_offset - b;

    e = c + d;
    f = b - a;

    frame_delta = MIN(e, f);

    //    DEBUG("a: %ld, b: %ld, c: %ld, d: %ld, e: %ld, f: %ld, frame_delta: %ld, slack: %d, p->state->end_offset: %ld, estimate: %ld, p->state->playback_pos: %ld\n", a, b, c, d, e, f, frame_delta, slack, p->state->end_offset, estimate, p->state->playback_pos);

    //        DEBUG("frame_delta: %ld, slack: %d, p->state->end_offset: %ld, estimate: %ld, p->state->playback_pos: %ld\n", frame_delta, slack, p->state->end_offset, estimate, p->state->playback_pos);
    
    if(frame_delta > slack) {
        //        DEBUG("exceed frame_delta: %ld, slack: %d, p->state->end_offset: %ld, estimate: %ld, p->state->playback_pos: %ld\n", frame_delta, slack, p->state->end_offset, estimate, p->state->playback_pos);
        estimate = p->state->playback_pos - player_get_audio_delay(p);
    }
    
    //    if(estimate < p->state->audio_pos)
    //        estimate = p->state->audio_pos;

    return estimate;
}

/**
 * Timeout callback which updates the display while the player thread
 * is running.
 */

gboolean
player_update_view(void *user_data) {
    struct player *p = user_data;
    shell *shl = p->shl;

    if(!p->player_ready)
        return TRUE;

    /* Since we're being called as a timeout callback we need to
       acquire the GDK lock. */

    gdk_threads_enter();

    if(p->cancel_honored || !p->player_running) {
        view_redraw(shl->view);
        gdk_threads_leave();
        return FALSE;
    }

    p->state->audio_pos = player_estimate_audio_position(p);

    /* Maybe we can skip drawing. */

    if(p->state->audio_pos == p->state->prev_audio_pos) {
        gdk_threads_leave();
        return TRUE;
    }

    p->state->prev_audio_pos = p->state->audio_pos;
    player_do_redraw(p);
    
    gdk_threads_leave();

    return TRUE;
}

/**
 * Spawns a new thread for playback/record and waits for it to start.
 */

void
player_start(struct player *p) {
    struct sched_param sched_param;
    pthread_attr_t attr;
    
    pthread_mutex_lock(&p->player_running_lock);
    
    if(p->player_running) {
        pthread_mutex_unlock(&p->player_running_lock);
        return;
    }

    /* Clean up after previously terminated thread. */

    if(p->cancel_honored) {
        DEBUG("reaping player thread...\n");
        pthread_join(p->player_thread, NULL);
        DEBUG("player thread reaped\n");
    }

    /* Set highest priority for playback thread, lowest priority for
       current (GUI) thread. */

    sched_param.sched_priority = sched_get_priority_min(SCHED_OTHER);
    pthread_setschedparam(pthread_self(), SCHED_OTHER, &sched_param);
    pthread_attr_init(&attr);

    sched_param.sched_priority = sched_get_priority_max(SCHED_OTHER);
    pthread_attr_setschedparam(&attr, &sched_param);
    pthread_attr_setschedpolicy(&attr, SCHED_OTHER);

    if(pthread_create(&p->player_thread, 
                      &attr,
                      player_thread,
                      p)) {
        FAIL("failed to create player thread.\n");
        pthread_mutex_unlock(&p->player_running_lock);
        return;
    }
    p->player_started = 0;
    p->cancel_honored = 0;
    p->player_ready = 0;
    DEBUG("waiting for player thread to initialize...\n");
    while(!p->player_started) {
        pthread_cond_wait(&p->player_running_cond, 
                          &p->player_running_lock);
        
    }
    gettimeofday(&p->audio_pos_previous_estimate_time, NULL);
    p->audio_pos_previous_delay = 0;
    p->audio_pos_estimate_error = 0;
    p->audio_pos_estimation_begin = 0;
    p->update_view_handler_id = 
        g_timeout_add(pref_get_as_int("playback_display_interval"), 
                      player_update_view, p);
    pthread_mutex_unlock(&p->player_running_lock);
    DEBUG("ok, player thread initialized.\n");
}

struct player *
player_new(shell *shl) {
    struct player *p = mem_calloc(sizeof(struct player), 1);
    const struct player_driver_container *pdc = player_get_current_driver();

    if(!p) {
        FAIL("could not allocate new player\n");
        return NULL;
    }
    if(pdc->pd->open(p)) {
        mem_free(p);
        return NULL;
    }

    p->shl = shl;
    p->state = &p->_state_storage;

    pthread_mutex_init(&p->player_running_lock, NULL);
    pthread_cond_init(&p->player_running_cond, NULL);

    return p;
}

void 
player_destroy(struct player *p) {
    if(!p)
        return;
    player_get_current_driver()->pd->close(p);
    mem_free(p);
}

unsigned int
player_get_input_channels(struct player *p) {
    return player_get_current_driver()->pd->get_input_channels();
}

unsigned int
player_get_output_channels(struct player *p) {
    return player_get_current_driver()->pd->get_output_channels();
}

AFframecount
player_get_audio_delay(struct player *p) {
    return player_get_current_driver()->pd->get_audio_delay(p);
}

/**
 * Returns the number of frames per track in the playback and record
 * buffers.  The driver must never exceed this value when manipulating
 * the buffers.
 * @param p The player object.
 * @return The number of frames per track in the playback and record
 * buffers.
 */

unsigned int
player_get_chunk_size(struct player *p) {
    float t = p->state->audio_chunk_duration;
    int s = t * p->state->source_rate;
    unsigned int r = MAX(128, s & ~127);
    //    DEBUG("chunk size: %u\n", r);
    return r;
}

void
player_set_position(struct player *p,
                    AFframecount pos) {
    pos = MAX(0, pos);
    //    p->state->audio_pos = pos;

    /* FIXME: this is more than a bit dangerous since these variables
       need a lock. */

    p->state->record_pos = pos;
    p->state->playback_pos = pos;
}

int
player_register_driver(struct player_driver *pd) {
    struct player_driver_container *pdc = mem_alloc(sizeof *pdc);
    static int num = 0;
    char *name = strdup(pd->name);

    if(!name) {
        if(pdc)
            free(pdc);
        return 1;
    }

    if(!pdc) {
        if(name)
            free(name);
        return 1;
    }

    if(player_drivers_h == NULL)
        player_drivers_h = g_hash_table_new(g_str_hash, g_str_equal);

    if(g_hash_table_lookup(player_drivers_h, name)) {
        FAIL("already registered driver %s\n", name);
        free(pdc);
        free(name);
        return 1;
    }

    pdc->pd = pd;
    pdc->num = num++;
    g_hash_table_insert(player_drivers_h, name, pdc);

    DEBUG("registered player driver %s\n", pd->name);

    return 0;
}

void
player_exit() {
    player_get_current_driver()->pd->destroy();
}

int
player_init() {
    const struct player_driver_container *pdc;
    player_register_driver(&player_dummy_driver);
    pdc = player_get_driver_by_name(pref_get_as_string("audio_driver"));
    if(!pdc) {
        FAIL("cannot locate audio driver %s, trying default...\n", 
             pref_get_as_string("audio_driver"));
        pdc = player_get_default_driver();
        if(!pdc) {
            FAIL("no player drivers available, using dummy driver\n");
            pdc = player_get_driver_by_name("Dummy");
        }
    }
    player_current_driver = pdc;
    DEBUG("initializing audio driver %s\n", pdc->pd->name);
    return pdc->pd->new();
}


    /*

    FIXME: old piece of code for record_discard_frames, keep around
    for future reference.

    if(p->record_discarded_frames < 
       pref_get_as_int("record_discard_frames")) {
        discard_frames = pref_get_as_int("record_discard_frames") - 
            p->record_discarded_frames;
        p->record_discarded_frames += MIN(frame_count, 
                                          discard_frames);
        DEBUG("discarding %ld frames, total discarded: %d\n",
              MIN(frame_count, discard_frames),
              p->record_discarded_frames);
        if(discard_frames >= frame_count)
            return frame_count;
    }

    if(discard_frames) {
        memcpy(muxbuf, 
               muxbuf + (shl->clip->sr->frame_width * channels * discard_frames),
               (frame_count - discard_frames) * channels * shl->clip->sr->frame_width);
        frame_count -= discard_frames;
    }
    */

