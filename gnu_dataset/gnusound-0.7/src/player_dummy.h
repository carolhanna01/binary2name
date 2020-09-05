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

#ifndef PLAYER_DUMMY
#define PLAYER_DUMMY

AFframecount
player_dummy_get_audio_delay(struct player *p);
size_t 
player_dummy_get_playback_buffer_size(struct player *p);
size_t 
player_dummy_get_record_buffer_size(struct player *p);
int
player_dummy_get_sample_rate(struct player *p);
unsigned int
player_dummy_get_input_channels(struct player *p);
unsigned int
player_dummy_get_output_channels(struct player *p);
int
player_dummy_transfer(struct player *p);
int
player_dummy_setup(struct player *p);
void
player_dummy_close(struct player *p);
int
player_dummy_open(struct player *p);
void
player_dummy_exit(struct player *p);
int
player_dummy_init(struct player *p);
GtkWidget *
player_dummy_open_config();
void
player_dummy_commit_config();
void
player_dummy_close_config();
void
player_dummy_destroy();
int
player_dummy_new();

#endif
