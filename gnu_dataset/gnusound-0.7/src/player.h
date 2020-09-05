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

#ifndef PLAYER_H
#define PLAYER_H

#include <config.h>
#include <audiofile.h>
#include "shell.h"

struct _shell;

/*
 * The player state is set up by the player before invoking the player
 * driver's open() function. It basically copies some state from the
 * shell (such as the selection and the record mode) which might
 * otherwise change underneath us. 
 *
 * FIXME: what goes here and what goes in struct player is fairly
 * arbitrary. The idea is that the stuff in here is more transient
 * and the stuff in struct player more structural in some sense, but
 * it's not very clearly delineated.
 */

struct player_state {

    /* Audio buffer size in seconds. */

    float audio_chunk_duration;

    /* Whether we're recording or just playing. */

    int record_mode;
    
    /* Whether the recording replaces or pushes back. */
    
    int record_replace;

    /* Whether to continue recording at the end of the soundfile. */

    int append_mode;

    /* Sample type. */

    int sample_type;

    /* The sample rate of the source. */

    int source_rate;

    /* Which channels were selected when the player was started. */

    int channel_map;

    /* Start position. */
    
    AFframecount start_offset;
    
    /* End position. */
       
    AFframecount end_offset;

    /* The number of source tracks. */

    int source_tracks;

    /* Which tracks to play from. */

    int source_tracks_map;

    /* The number of target (record) tracks. */

    int target_tracks;

    /* Which tracks to record onto. */

    int target_tracks_map;

    /* The number of times we've looped. */

    int loop_count;

    /* Playback position. */
    
    AFframecount playback_pos;

    /* Record position. */

    AFframecount record_pos;
    
    /* The estimate of the playback/record position. */
    
    AFframecount audio_pos;
    AFframecount prev_audio_pos;

    /* Where the undo starts. */

    AFframecount undo_start_offset;

    /* Where the undo ends. */

    AFframecount undo_end_offset;

    /* What channels to save for undo. */

    int undo_channel_map;

    /* The undo data. */

    snd *undo_sr;

};


struct player {
    pthread_t player_thread;
    int player_started;
    int player_running;
    int player_starting;
    int player_ready;
    pthread_mutex_t player_running_lock;
    pthread_cond_t player_running_cond;

    /*
     * Set to true when the player should stop. The player driver
     * needs to check this value in its transfer() function. 
     */
    
    int cancel_requested;
    int cancel_honored;

    /* The ID of the timeout callback for view update. */

    int update_view_handler_id;

    /* When we last estimated the audio position. */

    struct timeval audio_pos_previous_estimate_time;

    /* Error between estimate of playback pos. and actual pos. */
    
    int audio_pos_estimate_error;

    AFframecount audio_pos_previous_delay;

    int audio_pos_estimation_begin;

    /* The shell that this player belongs to. */

    struct _shell *shl;

    /* Buffers. */

    void *playback_muxbuf;
    void *playback_srcbufs[MAX_TRACKS];
    void *record_muxbuf;
    void *record_srcbufs[MAX_TRACKS];
    
    /* Private per-player data for the player driver. */

    void *driver_data;

    /* Player state. */

    struct player_state *state;
    struct player_state _state_storage;

};

/**
 * The player driver abstracts the actual API used to play and record
 * audio.
 *
 * The new(), destroy(), init() and exit() functions are invoked from
 * the main GNUsound thread (i.e. the GUI thread).
 *
 * The open(), close(), start(), stop() and transfer() functions are
 * invoked from the GNUsound playback thread.
 */

struct player_driver {

    /** The driver's name. */

    const char *name;
    
    /** Invoked when driver is (de)activated. */
    
    int (*new)();
    void (*destroy)();

    /** Invoked when a new shell is created that uses this driver. */
    
    int (*open)(struct player *p);
    void (*close)(struct player *p);
    
    /**
     * Invoked before/after playback/record. 
     * stop() will always be called, even if start() fails.
     */
    
    int (*start)(struct player *p);
    void (*stop)(struct player *p);

    /**
     * Invoked when samples should be transferred. The player driver
     * should use player_get_playback_bufi() (or the non-interleaved
     * variant player_get_playback_bufn()) to obtain samples for
     * playback. When they've been fed to the playback hardware, the
     * driver should call player_flush_playback_bufi() (or the
     * non-interleaved variant player_flush_playback_bufn()). An
     * analogous process is used for record. 
     * 
     * To determine how many frames should be played back/recorded,
     * the driver should call player_has_work() and/or
     * player_get_playback_avail() and player_get_record_avail().
     *
     * This should return 0 on success or non-zero on error.
     */

    int (*transfer)(struct player *p);
    
    /** 
     * Should return the number of output channels. This number should
     * not change between calls to new()/destroy().
     */

    unsigned int (*get_output_channels)();
    
    /** Should return the number of input channels. */

    unsigned int (*get_input_channels)();

    /** Should return the audible delay. */
    
    AFframecount (*get_audio_delay)(struct player *p);

    /** Should return the device sample rate. */

    int (*get_sample_rate)(struct player *p);

    /** Should return the record buffer size. */

    size_t (*get_record_buffer_size)(struct player *p);
    
    /** Should return the playback buffer size. */

    size_t (*get_playback_buffer_size)(struct player *p);

    /** Called when the user opens a configuration dialog for this driver. */

    GtkWidget *(*open_config)();
    
    /** Called when the user wishes to commit configuration settings. */

    void (*commit_config)();

    /** Called when the user closes the configuration dialog. */

    void (*close_config)();

};

struct player_driver_container {
    int num;
    struct player_driver *pd;
};

/* Interface for drivers. */

int
player_get_playback_bufn(struct player *p,
                         void ***buf,
                         AFframecount *frame_count);

int
player_flush_playback_bufn(struct player *p,
                           AFframecount frame_count);

int
player_get_record_bufn(struct player *p,
                       void ***buf,
                       AFframecount *frame_count);

int
player_flush_record_bufn(struct player *p,
                         AFframecount frame_count);

int
player_get_playback_bufi(struct player *p,
                         void **buf,
                         AFframecount *frame_count);

int
player_flush_playback_bufi(struct player *p,
                           AFframecount frame_count);

int
player_get_record_bufi(struct player *p,
                       void **buf,
                       AFframecount *frame_count);

int
player_flush_record_bufi(struct player *p,
                         AFframecount frame_count);

AFframecount
player_get_playback_avail(struct player *p);

AFframecount
player_get_record_avail(struct player *p);

int
player_has_work(struct player *p);

/* Normal interface. */

struct player *
player_new(struct _shell *shl);

void
player_destroy(struct player *p);

void
player_start(struct player *p);

void
player_stop(struct player *p);

AFframecount
player_get_audio_delay(struct player *p);

unsigned int
player_get_chunk_size(struct player *p);

unsigned int
player_get_input_channels();

unsigned int
player_get_output_channels();

AFframecount
player_get_audio_delay(struct player *p);

void
player_set_position(struct player *p,
                    AFframecount pos);

const struct player_driver_container *
player_get_driver_by_name(const char *name);

int
player_get_driver_count();

const struct player_driver_container *
player_get_driver(int i);

const struct player_driver_container *
player_get_current_driver();

//const struct player_driver *
//player_get_default_driver();

int
player_register_driver(struct player_driver *drv);

void
player_commit_record_undo(struct player *p);

int
player_init();

void
player_exit();

#endif /* ! PLAYER_H */
