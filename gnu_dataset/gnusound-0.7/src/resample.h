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

#ifndef RESAMPLE_H
#define RESAMPLE_H

#include <config.h>
#include "shell.h"

#ifdef HAVE_SAMPLERATE
#include <samplerate.h>
#endif

#define RESAMPLE_BEST_ALGORITHM 0

enum resample_flags {
    RESAMPLE_HONOR_ENVELOPES           = 1 << 0,
    RESAMPLE_GUARANTEE_NEW_FRAME_COUNT = 1 << 1
};

struct filter_stats {
    AFframecount consumed;
    AFframecount produced;
};

const char *
resample_algo_name(int algo);

const char *
resample_algo_description(int algo);

AFframecount
resample_track(shell *shl,
               int track,
               AFframecount start_offset,
               AFframecount end_offset,
               AFframecount new_frame_count,
               int algo,
               enum resample_flags flags,
               struct filter_stats *stats);

#endif /* ! RESAMPLE_H */
