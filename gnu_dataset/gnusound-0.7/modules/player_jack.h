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

#ifndef PLAYER_JACK_H
#define PLAYER_JACK_H

#include <config.h>
#include <signal.h>
#include <audiofile.h>
#include <jack/jack.h>
#include "shell.h"

#define MAX_BUFFERS 32

/* FIXME: use of MAX_TRACKS is arbitrary limitation */
struct jackdrv_globals {
    jack_client_t *client;
    jack_port_t *inputs[MAX_TRACKS];
    jack_port_t *outputs[MAX_TRACKS];
    jack_nframes_t sample_rate;
    jack_nframes_t buffer_size;
    int error;
    sig_atomic_t connected;

    jack_nframes_t delay;
    struct player *active_player;
};
    
struct jackdrv_data {
    size_t recbuf_size;
    size_t recbuf_pos;
    void **recbuf;
    int data_ready;
    pthread_mutex_t data_ready_lock;
    pthread_cond_t data_ready_cond;
};

int
jackdrv_setup();

const char **
jackdrv_get_ports(const char *port_name_pattern,
                  const char *type_name_pattern,
                  unsigned long flags);

void
jackdrv_set_sample_rate(struct player *p);

void
jackdrv_set_sample_type(struct player *p);

size_t 
jackdrv_get_playback_buffer_size(struct player *p);

size_t 
jackdrv_get_record_buffer_size(struct player *p);

int
jackdrv_get_sample_rate(struct player *p);

AFframecount
jackdrv_get_audio_delay(struct player *p);

unsigned int
jackdrv_get_input_channels(struct player *p);

unsigned int
jackdrv_get_output_channels(struct player *p);

int
jackdrv_transfer(struct player *p);

void
jackdrv_close(struct player *p);

int
jackdrv_open(struct player *p);

void
jackdrv_stop(struct player *p);

int
jackdrv_start(struct player *p);

void
jackdrv_destroy();

int
jackdrv_new();

#endif /* ! PLAYER_JACKDRV_H */
