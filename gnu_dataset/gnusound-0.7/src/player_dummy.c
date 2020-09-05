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
#include "player.h"
#include "view.h"

AFframecount
player_dummy_get_audio_delay(struct player *p) {
    return 0;
}

size_t 
player_dummy_get_playback_buffer_size(struct player *p) {
    return 0;
}

size_t 
player_dummy_get_record_buffer_size(struct player *p) {
    return 0;
}

int
player_dummy_get_sample_rate(struct player *p) {
    return 0;
}

unsigned int
player_dummy_get_input_channels() {
    return 0;
}

unsigned int
player_dummy_get_output_channels() {
    return 0;
}

int
player_dummy_transfer(struct player *p) {
    view_set_transient(p->shl->view, MSG_ERR, "No audio driver");
    return 1;
}

void
player_dummy_close(struct player *p) {
}

int
player_dummy_open(struct player *p) {
    return 0;
}

void
player_dummy_exit(struct player *p) {
}

int
player_dummy_init(struct player *p) {
    return 0;
}

GtkWidget *
player_dummy_open_config() {
    return NULL;
}

void
player_dummy_commit_config() {
}

void
player_dummy_close_config() {
}

void
player_dummy_destroy() {
}

int
player_dummy_new() {
    return 0;
}

